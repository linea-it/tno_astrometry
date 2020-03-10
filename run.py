#!/usr/bin/env python3
import argparse
import fnmatch
import json
import logging
import os
import shutil
import subprocess
import sys
import traceback
from concurrent.futures import ThreadPoolExecutor, as_completed, wait
from datetime import datetime
from tempfile import NamedTemporaryFile

import humanize
import numpy as np
import pandas as pd

from common import (check_bsp_jpl, check_bsp_planetary, check_ccd_images_csv,
                    check_leap_seconds, create_symlink_for_images,
                    get_image_by_id, get_stars_by_ccd, get_targets_by_ccd_id,
                    location_by_obs_code, read_ccd_image_csv,
                    read_stars_catalog, read_targets_offset,
                    remove_symlink_for_images)
from gaia_dr2 import gaia_dr2_to_praia_catalog
from plot_astrometry import plotStarsCCD
from praia_astrometry import (execute_astrometry, get_catalog_code,
                              run_praia_astrometry)
from praia_header import run_praia_header
from praia_target import create_obs_file, create_targets_file, run_praia_target

parser = argparse.ArgumentParser()
parser.add_argument(
    "asteroid", help="Asteroid name without spaces. example: 1999RB216")
parser.add_argument(
    "--path", default=None, help="Path where the inputs are and where the outputs are. if not passed the directory mounted as /data will be used.")
parser.add_argument(
    "--images", default="ccd_images.csv", help="Name of csv file with csv information. default ccd_images.csv")
parser.add_argument(
    "--catalog", default="gaia2", help="Name of the catalog that will be used to identify the targets. example gaia1 or gaia2.")
parser.add_argument(
    "--obs_code", default="W84", help="IAU code of observation site or user-based site designation for target NIMA/MPC output formats only. default W84")
parser.add_argument(
    "--disable_plot", default=False, type=bool, help="Disables plots creation for each ccd. default is False")
parser.add_argument(
    "--max_workers", default=None, type=int, help="if max_workers is None or not given, it will default to the number of processors on the machine, multiplied by 5.")
parser.add_argument(
    "--debug", default=False, help="Set logger level. default is False. use False - level.INFO or True - level.DEBUG")

args = parser.parse_args()

asteroid = args.asteroid
basepath = args.path
obs_code = args.obs_code
catalog = args.catalog
cat_code = get_catalog_code(catalog)
ccd_images_filename = args.images
enable_plot = not args.disable_plot
max_workers = args.max_workers
debug = args.debug

obs_location = location_by_obs_code(obs_code)

t0 = datetime.now()

original_data_path = os.getenv("DATA_DIR")
if basepath is not None and basepath is not '':
    # Quando o container recebe o parametro path, o valor da variavel DATA_DIR deve ser alterado.
    # tem uma limitacao nos programas PRAIA em que os paths nao podem ultrapassar 50 caracteres.
    # para contornar isso e criado um link do diretorio de inputs para o diretorio APP_PATH/asteroid_name
    # por exemplo
    # path = /proccess/4/objects/Eris -> /app/Eris
    complete_path = os.path.join(os.getenv("DATA_DIR"), basepath.strip('/'))

    # Guarda uma copia do path completo para usar no final do programa.
    original_data_path = complete_path

    tmp_dir = os.path.join(os.getenv("APP_PATH"), asteroid.replace(' ', '_'))
    os.symlink(complete_path, tmp_dir)
    os.environ["DATA_DIR"] = tmp_dir


# Setup Log file
logfile = os.path.join(os.getenv("DATA_DIR"), "astrometry.log")
if debug:
    logging.basicConfig(
        filename=logfile,
        level=logging.DEBUG,
        format='%(asctime)s [%(levelname)s] %(message)s')
else:
    logging.basicConfig(
        filename=logfile,
        level=logging.INFO,
        format='%(asctime)s [%(levelname)s] %(message)s')


# Result json
result = dict({
    'asteroid': asteroid,
    'images': 0,
    'available_images': 0,
    'not_available_images': 0,
    'processed_images': 0,
    'reference_catalog_csv': None,
    'bsp_jpl': None,
    'bsp_planetary': None,
    'leap_second': None,
    'header_extraction': None,
    'praia_astrometry': None,
    'praia_targets': None,
    'plots': None,
    'astrometry': None,
    'target_offset': None,
    'targets_file': None,
    'outputs': dict({}),
    'error': None
})


try:
    logging.info(
        "--------------------------------------------------------------------------------------")
    logging.info("Start Astrometry pipeline")
    logging.info("Asteroid: [ %s ]" % asteroid)

    # Verificar todos os Imputs necessários

    # -----------------------------------------------------------------------------------------------------
    # BSP JPL
    # -----------------------------------------------------------------------------------------------------
    bsp_jpl = check_bsp_jpl(asteroid)
    bsp_jpl_filename = os.path.basename(bsp_jpl)
    result['bsp_jpl'] = bsp_jpl_filename
    logging.info("BSP JPL: [%s]" % bsp_jpl)

    # -----------------------------------------------------------------------------------------------------
    # BSP Planetary
    # -----------------------------------------------------------------------------------------------------
    bsp_planets = check_bsp_planetary()
    bsp_planets_filename = os.path.basename(bsp_planets)
    result['bsp_planetary'] = bsp_planets_filename
    logging.info("BSP PLANETARY: [%s]" % bsp_planets)

    # -----------------------------------------------------------------------------------------------------
    # Leap Seconds
    # -----------------------------------------------------------------------------------------------------
    leap_sec = check_leap_seconds()
    leap_sec_filename = os.path.basename(leap_sec)
    result['leap_second'] = leap_sec_filename
    logging.info("LEAP SECONDS: [%s]" % leap_sec)

    # -----------------------------------------------------------------------------------------------------
    # Imagens / CCDs
    # IMPORTANTE: A lista de imagens OBRIGATÓRIAMENTE deve estar ordenada por data.
    # o csv, pode estar em qualquer ordem, mais a função que le o csv e retorna o dataframe já ordena.
    # Esta ordem deve ser mantida durante toda a execução.
    # -----------------------------------------------------------------------------------------------------
    ccd_images_csv = check_ccd_images_csv(ccd_images_filename)
    ccd_images_filename = os.path.basename(ccd_images_csv)

    # Ler o Arquivo ccd_images.csv
    df_ccd_images = read_ccd_image_csv(ccd_images_csv)

    # Criar link para imagens disponiveis, no mesmo diretorio dos dados.
    # Dataframe de ccds + colunas: original_path e current_path (que o link simbolico) e avalilable.
    df_ccd_images = create_symlink_for_images(df_ccd_images, logging)

    # Dataframe com todas os ccds disponiveis.
    df_available_ccds = df_ccd_images[df_ccd_images['available'] == True]
    # Dataframe com todos os ccds que não estão disponiveis
    df_not_available_ccds = df_ccd_images[df_ccd_images['available'] == False]

    # Array com os paths das imagens disponiveis, já considerando os link simbolicos.
    images = df_available_ccds['current_path'].tolist()

    result['images'] = len(df_ccd_images)
    result['available_images'] = len(df_available_ccds)
    result['not_available_images'] = len(df_not_available_ccds)

    logging.info("CCD Images: [%s] Availables: [%s] Not Availables: [%s]" % (
        result['images'], result['available_images'], result['not_available_images']))

    if result['available_images'] == 0:
        raise Exception("No CCD images available for this Asteroid")

   # -----------------------------------------------------------------------------------------------------
   # Catalogo de referencia
   # -----------------------------------------------------------------------------------------------------
    if catalog == 'gaia2':
        # Para o Catalogo GAIA DR2 é necessário converter para o formato do PRAIA.
        convert_catalog_t0 = datetime.now()

        logging.debug(
            "Converting User Catalog GAIA DR2 to PRAIA format.")

        input_catalog = os.path.join(os.getenv("DATA_DIR"), 'gaia_dr2.csv')
        gaia_dr2_catalog = gaia_dr2_to_praia_catalog(input_catalog)

        convert_catalog_t1 = datetime.now()
        convert_catalog_exec_time = convert_catalog_t1 - convert_catalog_t0

        logging.info(
            "Reference catalog was converted to PRAIA format in %s. Filename: [%s]" % (humanize.naturaldelta(convert_catalog_exec_time), gaia_dr2_catalog))

        catalog_name = 'gaia_dr2'
        result['reference_catalog_csv'] = input_catalog
    else:
        catalog_name = catalog

    # -----------------------------------------------------------------------------------------------------
    # Execucao do PRAIA Header Extraction
    # -----------------------------------------------------------------------------------------------------
    header_t0 = datetime.now()
    try:
        praia_header_output = run_praia_header(images)
    except Exception as e:
        logging.error(e)
        raise Exception(e)

    header_t1 = datetime.now()
    header_exec_time = header_t1 - header_t0

    headers = open(praia_header_output, "r")
    count_headers = count = len(open(praia_header_output).readlines())

    result['header_extraction'] = dict({
        'start': header_t0.isoformat(),
        'finish': header_t1.isoformat(),
        'execution_time': header_exec_time.total_seconds(),
        'config': 'hd.dat',
        'filename': os.path.basename(praia_header_output),
        'file_size': os.path.getsize(praia_header_output),
        'extension': os.path.splitext(praia_header_output)[1],
        'headers': count_headers,
        'log': 'praia_header.log'
    })

    logging.info("Praia Header Extraction [%s] results in %s. Filename: [%s]" % (
        count_headers, humanize.naturaldelta(header_exec_time), praia_header_output))

    # -----------------------------------------------------------------------------------------------------
    # Execucao do PRAIA Astrometry
    # -----------------------------------------------------------------------------------------------------
    astrometry_t0 = datetime.now()

    # A etapa de Astrometria pode ser executada em paralelo, cada thread processa um ccd,
    # o limite de threads é definido por max_workers.
    pool = ThreadPoolExecutor(max_workers=max_workers)
    futures = []
    i = 1
    for header in headers:
        logging.info("Running Praia Astrometry %s of %s" % (i, count_headers))
        futures.append(pool.submit(execute_astrometry, i,
                                   header, catalog_name, cat_code, logging))
        i += 1

        result['processed_images'] += 1

    # Esperar todas as execucoes.
    wait(futures)

    for future in futures:
        xy, output = future.result()
        if output is not None:
            # Guardar os outputs no json de resultado
            result['outputs'][output['ccd_id']] = output
            # Adicionar o path dos arquivo de 'astrometria catalogo de refentecia' ao Dataframe na coluna referece_catalog_xy
            df_ccd_images.at[xy['ccd_id'],
                             'reference_catalog_xy'] = xy['reference_catalog_xy']
        # TODO: Alguns CCDs podem falhar e não gerar o XY. 
        # para estes casos é interessante registrar o erro, 
        # hoje está apenas no log e o ccd fica sem outputs, mas não indica em qual parte falhou. 

    # APENAS PARA DEBUG Criar uma lista xy com os resultados xy do catalogo de referencia.
    # TODO: REMOVER ESTE BLOCO
    # ids = []
    # for id, image in df_available_ccds.iterrows():
    #     ids.append(id)

    # print(ids)
    # import random
    # random.shuffle(ids)
    # print(ids)

    # listXY = []
    # for id in ids:
    #     listXY.append(
    #         {'ccd_id': id, 'reference_catalog_xy': '%s.ast_reduction.gaia_dr2.txt' % id})

    # for xy in listXY:
    #     df_ccd_images.at[xy['ccd_id'],
    #                      'reference_catalog_xy'] = xy['reference_catalog_xy']

    # TODO: REMOVER ESTE BLOCO

    # Garantir que o Dataframe está ordenado pelo date_obs
    # df_ccd_images = df_ccd_images.sort_values(by=['date_obs'])

    # Filtrar pelos ccds Available e que geraram XY
    df_list_xy = df_ccd_images[(df_ccd_images['available'] == True) & (
        df_ccd_images.reference_catalog_xy.notnull())]

    # Criar o arquivo de Output da Astrometria, "ast_out.txt",
    # uma lista com os arquivos de Astrometrias gerado para catalogo de referencia.
    praia_astrometry_output = os.path.join(
        os.getenv("DATA_DIR"), "ast_out.txt")
    # este arquivo contem o path para os xy um em cada linha.
    df_list_xy.to_csv(
        praia_astrometry_output, columns=['reference_catalog_xy'], index=False, header=False)

    # Guardar os tempos de execucao total da astrometria.
    astrometry_t1 = datetime.now()
    astrometry_exec_time = astrometry_t1 - astrometry_t0

    result['praia_astrometry'] = dict({
        'start': astrometry_t0.isoformat(),
        'finish': astrometry_t1.isoformat(),
        'execution_time': astrometry_exec_time.total_seconds(),
        "filename": os.path.basename(praia_astrometry_output),
    })

    if len(df_list_xy) == 0:
        raise Exception(
            "PRAIA Astrometry was performed but generated no results.")

    logging.info("Praia Astrometry executed %s CCDs in %s" %
                 (len(df_list_xy), humanize.naturaldelta(astrometry_exec_time)))

    # -----------------------------------------------------------------------------------------------------
    # Execucao do PRAIA Targets
    # -----------------------------------------------------------------------------------------------------
    targets_t0 = datetime.now()

    # Lista com datas em JD extraidas do resultado do Praia Headers
    dates_jd = np.loadtxt(praia_header_output, dtype=str,
                          usecols=(13,), ndmin=1)
    logging.info("DATES JD: [%s]" % len(dates_jd))

    # TODO: usar apenas datas, para ccds que geraram o xy.

    # Criar o arquivo de Targets
    location = [obs_location['lon'], obs_location['lat'], obs_location['ele']]
    targets_file = create_targets_file(
        asteroid, dates_jd, bsp_jpl, bsp_planets, leap_sec, location)

    logging.info("Targets file was generated: [%s]" % targets_file)
    result['targets_file'] = dict({
        'filename': os.path.basename(targets_file),
        'file_size': os.path.getsize(targets_file),
        'extension': os.path.splitext(targets_file)[1]
    })

    # Criar o arquivo Targets Offset
    targets_offset = run_praia_target(praia_astrometry_output, targets_file)
    # Verificar se o arquivo targets tem resultado.
    if os.path.isfile(targets_offset) and os.path.getsize(targets_offset) > 0:
        logging.info(
            "Targets Offset file was generated: [%s]" % targets_offset)

        result['target_offset'] = dict({
            'filename': os.path.basename(targets_offset),
            'file_size': os.path.getsize(targets_offset),
            'extension': os.path.splitext(targets_offset)[1]
        })

        # So Cria o arquivo de Astrometria se tiver resultado no targets_offset.
        # Arquivo de resultado da Astrometria.
        praia_target_output = create_obs_file(
            targets_offset, asteroid, obs_code, cat_code)

        # Verificar se o resultado final da Astrometria foi gerado com sucesso
        if os.path.isfile(praia_target_output) and os.path.getsize(praia_target_output) > 0:
            logging.info(
                "Target Astrometry generated: [%s]" % praia_target_output)

            result['astrometry'] = dict({
                'filename': os.path.basename(praia_target_output),
                'file_size': os.path.getsize(praia_target_output),
                'extension': os.path.splitext(praia_target_output)[1]
            })
        else:
            msg = "Astrometric observed ICRF positions was not generated or is empty."
            result['error'] = msg
            logging.warning(msg)
    else:
        msg = "Object position was not found in xy file. PRAIA targets search was executed but did not generate any results."
        result['error'] = msg
        logging.warning(msg)

    targets_t1 = datetime.now()
    targets_exec_time = targets_t1 - targets_t0

    result['praia_targets'] = dict({
        'start': targets_t0.isoformat(),
        'finish': targets_t1.isoformat(),
        'execution_time': targets_exec_time.total_seconds(),
        'log': 'praia_target.log'
    })

    logging.info("Praia Targets executed in %s" %
                 humanize.naturaldelta(targets_exec_time))
    # -----------------------------------------------------------------------------------------------------
    # Plot Astrometry - CCD X Stars X Target
    # -----------------------------------------------------------------------------------------------------
    # Só executa se o Parametro enable_plot for True, se tiver algum resultado no Target_Offset e tiver 
    # o catalogo de referencia.
    if enable_plot is True and result['target_offset'] is not None and result['reference_catalog_csv'] is not None:
        plot_t0 = datetime.now()

        # Ler o resultado do Targets Offset, que contem apenas os CCDs que tiveram posiçoes detectadas
        df_targets = read_targets_offset(os.path.join(
            os.getenv("DATA_DIR"), result['target_offset']['filename']))

        # Ler o arquivo Catalogo de estrelas
        if result['reference_catalog_csv']:
            df_stars = read_stars_catalog(result['reference_catalog_csv'])
        else:
            df_stars = pd.DataFrame()

        i = 0
        # Para Cada Posição detectada. gerar o plot. 
        for ccd_id, target in df_targets.iterrows():
            t0 = datetime.now()

            # Filtrar no Dataframe com todos os ccds, o CCD relacionado oa Target
            ccd = df_ccd_images.loc[ccd_id]

            # Filtrar o catalogo de estrelas pelo ccd
            stars = df_stars.loc[df_stars['ccd_id'] == int(ccd_id)]

            # Nome original do CCD, sem a extensao .fits
            ccd_filename = os.path.splitext(os.path.basename(ccd['filename']))[0]

            plot_filepath = os.path.join(
                os.getenv("DATA_DIR"), '%s.ast_plot.png' % ccd_filename)
            plot = plotStarsCCD(asteroid, ccd, stars, target, plot_filepath)

            # Guardar o plot nos outputs do ccd
            result['outputs'][str(ccd_id)]['files'].append({
                'catalog': None,
                'filename': os.path.basename(plot),
                'file_size': os.path.getsize(plot),
                'extension': os.path.splitext(plot)[1]
            })

            # Associar este Plot com o ccd no df_ccd_images.
            df_ccd_images.at[ccd_id, 'ast_plot'] = os.path.basename(plot)

            t1 = datetime.now()
            tdelta = t1 - t0

            logging.info("Plot [%s] [%s] created for ccd %s in %s." % (
                i, plot_filepath, ccd_id, humanize.naturaldelta(tdelta)))

            i += 1

        plot_t1 = datetime.now()
        plot_tdelta = plot_t1 - plot_t0

        result['plots'] = dict({
            'start': plot_t0.isoformat(),
            'finish': plot_t1.isoformat(),
            'execution_time': plot_tdelta.total_seconds(),
        })

    dataframe_path = os.path.join(
        os.getenv("DATA_DIR"), "ccd_images_dataframe.csv")
    df_ccd_images.to_csv(dataframe_path, sep=';')        

except Exception as e:
    logging.error("Error: [ %s ]" % e)
    logging.error(traceback.format_exc())

    result['error'] = str(e)

finally:

    # Escrever o arquivo json com os resultados
    with open(os.path.join(os.getenv("DATA_DIR"), 'astrometry.json'), 'w') as json_file:
        json.dump(result, json_file)

    # Remover os links da imagens
    remove_symlink_for_images()

    # Remover o link BSP Planetary
    if result['bsp_planetary'] is not None:
        os.unlink(os.path.join(os.getenv("DATA_DIR"), result['bsp_planetary']))

    # Remover o link do Leap Second
    if result['leap_second'] is not None:
        os.unlink(os.path.join(os.getenv("DATA_DIR"), result['leap_second']))

    # Remover o link para o diretorio data_dir
    if os.path.islink(os.getenv("DATA_DIR")):
        os.unlink(os.getenv("DATA_DIR"))

    t1 = datetime.now()
    t_delta = t1 - t0

    # Exibir o json de resultado na saida do programa
    print(json.dumps(result, indent=2))

    logging.info("Finish in %s" % humanize.naturaldelta(t_delta))
