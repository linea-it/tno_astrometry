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

from common import (create_symlink_for_images, get_image_by_id,
                    get_stars_by_ccd, get_targets_by_ccd_id,
                    read_ccd_image_csv, read_stars_catalog,
                    read_targets_offset, remove_symlink_for_images)
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
logging.basicConfig(
    filename=logfile,
    level=logging.INFO,
    format='%(asctime)s [%(levelname)s] %(message)s')
if debug:
    logging.setLevel(logging.DEBUG)


# Result json
result = dict({
    'asteroid': asteroid,
    'images': 0,
    'available_images': 0,
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
    logging.info("Start Astrometry pipeline")
    logging.info("Asteroid: [ %s ]" % asteroid)

    # -----------------------------------------------------------------------------------------------------
    # Imagens / CCDs
    # -----------------------------------------------------------------------------------------------------
    ccd_images_csv = os.path.join(
        os.getenv("DATA_DIR"), str(ccd_images_filename))
    if not os.path.exists(ccd_images_csv):
        msg = "csv file with ccds information not found. Filename: [%s]" % ccd_images_csv
        logging.error(msg)
        raise Exception(msg)

    # Ler o Arquivo ccd_images.csv
    df_ccd_images = read_ccd_image_csv(ccd_images_csv)
    result['images'] = len(df_ccd_images)
    logging.info("CCD Images: [ %s ]" % result['images'])

    # Criar link para imagens no mesmo diretorio dos dados.
    images = create_symlink_for_images(df_ccd_images)
    result['available_images'] = len(images)
    logging.info(
        "Created Symbolic links for images. Links [ %s ]" % result['available_images'])

    # -----------------------------------------------------------------------------------------------------
    # Catalogo de referencia
    # -----------------------------------------------------------------------------------------------------
    catalog_name = None

    if catalog == 'gaia2':
        # Procurar um arquivo csv no diretorio de data para converter para o formato do PRAIA.
        catalog_filename = 'gaia_dr2.csv'
        input_catalog = os.path.join(os.getenv("DATA_DIR"), catalog_filename)
        gaia_dr2_catalog = os.path.join(
            os.getenv("DATA_DIR"), "%s.cat" % os.path.splitext(catalog_filename)[0])

        if not os.path.exists(input_catalog):
            msg = "Catalog file not found. to use gaia2 must have in /data a csv file with the name gaia_dr2.csv."
            logging.error(msg)
            raise Exception(msg)

        result['reference_catalog_csv'] = input_catalog

        logging.debug(
            "Converting User Catalog GAIA DR2 to PRAIA format. Filename: [%s]" % input_catalog)

        with open(gaia_dr2_catalog, 'w') as fp:
            process = subprocess.Popen(["%s %s" % (os.getenv("CDS2REF"), input_catalog)],
                                       stdin=subprocess.PIPE, stdout=fp, shell=True)
        process.communicate()

        if not os.path.exists(gaia_dr2_catalog):
            msg = "Catalog file was not generated. Filename: [%s]" % gaia_dr2_catalog
            logging.error(msg)
            raise Exception(msg)

        logging.info(
            "Reference catalog was converted to PRAIA format. Filename: [%s]" % gaia_dr2_catalog)

        catalog_name = os.path.splitext(catalog_filename)[0]
    else:
        catalog_name = catalog

    # -----------------------------------------------------------------------------------------------------
    # BSP JPL
    # -----------------------------------------------------------------------------------------------------
    bsp_jpl_filename = "%s.bsp" % asteroid
    bsp_jpl = os.path.join(os.getenv("DATA_DIR"), bsp_jpl_filename)
    if not os.path.exists(bsp_jpl):
        msg = "BSP JPL file not found. Filename: [%s]" % bsp_jpl
        logging.error(msg)
        raise Exception(msg)

    result['bsp_jpl'] = bsp_jpl_filename
    logging.info("BSP JPL: [%s]" % bsp_jpl)

    # -----------------------------------------------------------------------------------------------------
    # BSP Planetary
    # -----------------------------------------------------------------------------------------------------
    bsp_planets_filename = os.getenv("BSP_PLANETARY")
    bsp_planets = os.path.join(os.getenv("DATA_DIR"), bsp_planets_filename)
    if not os.path.exists(bsp_planets):
        # Se nao existir no data, criar link
        os.symlink(os.path.join(os.getenv("BSP_PLANETARY_PATH"),
                                bsp_planets_filename), bsp_planets)

    result['bsp_planetary'] = bsp_planets_filename
    logging.info("BSP PLANETARY: [%s]" % bsp_planets)

    # -----------------------------------------------------------------------------------------------------
    # Leap Seconds
    # -----------------------------------------------------------------------------------------------------
    leap_sec_filename = os.getenv("LEAP_SENCOND")
    leap_sec = os.path.join(os.getenv("DATA_DIR"), leap_sec_filename)
    if not os.path.exists(leap_sec):
        # Se nao existir no data, criar link
        os.symlink(os.path.join(os.getenv("LEAP_SENCOND_PATH"),
                                leap_sec_filename), leap_sec)

    result['leap_second'] = leap_sec_filename
    logging.info("LEAP SECONDS: [%s]" % leap_sec)

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
        'output': os.path.basename(praia_header_output),
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

    listXY = []
    for future in futures:
        xy, output = future.result()

        if output is not None:
            listXY.append(xy)
            result['outputs'][output['ccd_id']] = output

    # guardar o path dos arquivos xy em um arquivo de output.
    praia_astrometry_output = os.path.join(
        os.getenv("DATA_DIR"), "ast_out.txt")
    np.savetxt(praia_astrometry_output, listXY, fmt='%s')

    astrometry_t1 = datetime.now()
    astrometry_exec_time = astrometry_t1 - astrometry_t0

    result['praia_astrometry'] = dict({
        'start': astrometry_t0.isoformat(),
        'finish': astrometry_t1.isoformat(),
        'execution_time': astrometry_exec_time.total_seconds(),
    })

    logging.info("Praia Astrometry executed in %s" %
                 humanize.naturaldelta(astrometry_exec_time))

    # -----------------------------------------------------------------------------------------------------
    # Execucao do PRAIA Targets
    # -----------------------------------------------------------------------------------------------------
    if len(listXY) == 0:
        raise Exception(
            "PRAIA Astrometry was performed but generated no results.")

    targets_t0 = datetime.now()

    # Lista com datas em JD extraidas do resultado do Praia Headers
    dates_jd = np.loadtxt(praia_header_output, dtype=str,
                          usecols=(13,), ndmin=1)
    logging.info("DATES JD: [%s]" % len(dates_jd))

    # Criar o arquivo de Targets
    targets_file = create_targets_file(
        asteroid, dates_jd, bsp_jpl, bsp_planets, leap_sec)

    logging.info("Targets file was generated: [%s]" % targets_file)
    result['targets_file'] = dict({
        'filename': os.path.basename(targets_file),
        'file_size': os.path.getsize(targets_file),
        'extension': os.path.splitext(targets_file)[1]
    })

    # Criar o arquivo Targets Offset
    targets_offset = run_praia_target(praia_astrometry_output, targets_file)
    # Verificar ser o arquivo targets tem resultado.
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
            }),
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
    })

    logging.info("Praia Targets executed in %s" %
                 humanize.naturaldelta(targets_exec_time))

    # -----------------------------------------------------------------------------------------------------
    # Plot Astrometry - CCD X Stars X Target
    # -----------------------------------------------------------------------------------------------------
    if enable_plot is True and result['target_offset'] is not None:

        plot_t0 = datetime.now()

        # Ler o arquivo de ccds
        df_ccds = read_ccd_image_csv(ccd_images_csv)

        # Ler o arquivo Catalogo de estrelas
        if result['reference_catalog_csv']:
            df_stars = read_stars_catalog(result['reference_catalog_csv'])
        else:
            df_stars = pd.DataFrame()

        # Ler arquivo Targets Offset
        df_targets = read_targets_offset(os.path.join(
            os.getenv("DATA_DIR"), result['target_offset']['filename']))

        for idx in result['outputs']:
            ccd_result = result['outputs'][idx]

            t0 = datetime.now()

            # filtrar a lista de ccd pelo id
            ccd = df_ccds.loc[df_ccds['id'] ==
                              int(ccd_result['ccd_id'])].iloc[0]

            # Filtrar o catalogo de estrelas pelo ccd
            stars = df_stars.loc[df_stars['ccd_id'] == int(ccd['id'])]

            targets = df_targets.loc[df_targets['ccd_image'].str.contains(
                str(ccd['id'])) == True]

            plot_filepath = os.path.join(
                os.getenv("DATA_DIR"), '%s.ast_plot.png' % ccd['id'])
            plot = plotStarsCCD(asteroid, ccd, stars, targets, plot_filepath)

            logging.info("Plot [%s] created for ccd %s in %s." % (
                i, ccd['id'], humanize.naturaldelta(tdelta)))

            t1 = datetime.now()
            tdelta = t1 - t0

            i += 1

        plot_t1 = datetime.now()
        plot_tdelta = plot_t1 - plot_t0

        result['plots'] = dict({
            'start': plot_t0.isoformat(),
            'finish': plot_t1.isoformat(),
            'execution_time': plot_tdelta.total_seconds(),
        })


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
