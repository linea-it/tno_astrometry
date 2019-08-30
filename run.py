#!/usr/bin/env python3
import os
import sys
import logging
from praia_header import run_praia_header, create_symlink_for_images, remove_symlink_for_images
from praia_astrometry import run_praia_astrometry, get_catalog_code, execute_astrometry
from praia_target import create_targets_file, run_praia_target, create_obs_file
from ccd_image import read_ccd_image_csv, get_image_by_id, get_stars_by_ccd, get_targets_by_ccd_id, get_ccd_images_from_csv, read_stars_catalog, read_targets_offset
from plot_astrometry import plotStarsCCD
import argparse
from datetime import datetime
import humanize
import traceback
import numpy as np
from concurrent.futures import ThreadPoolExecutor, wait, as_completed
import subprocess
from tempfile import NamedTemporaryFile
import fnmatch
import json
import shutil
import pandas as pd

parser = argparse.ArgumentParser()
parser.add_argument(
    "asteroid", help="Asteroid name without spaces. example: 1999RB216")
parser.add_argument(
    "--path", default=None, help="Path where the inputs are and where the outputs are. if not passed the directory mounted as /data will be used." )    
parser.add_argument(
    "--images", default="ccd_images.csv", help="Name of csv file with csv information. default ccd_images.csv" )    
parser.add_argument(
    "--catalog", default="gaia2", help="Name of the catalog that will be used to identify the targets. example gaia1 or gaia2." )
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
    'astrometry': None,
    'target_offset': None,
    'targets_file': None,
    'dates_jd': None,
    'outputs': dict({}),
    'error': None
})


try:
    logging.info("Start Astrometry pipeline")
    logging.info("Asteroid: [ %s ]" % asteroid)

    # Imagens
    ccd_images_csv = os.path.join(os.getenv("DATA_DIR"), str(ccd_images_filename))
    if not os.path.exists(ccd_images_csv):
        msg = "csv file with ccds information not found. Filename: [%s]" % ccd_images_csv
        logging.error(msg)
        raise Exception(msg)

    ccd_images = read_ccd_image_csv(ccd_images_csv)
    result['images'] = len(ccd_images)
    logging.info("CCD Images: [ %s ]" % len(ccd_images))


    # Criar link para imagens no mesmo diretorio dos dados. 
    images = create_symlink_for_images(ccd_images)
    result['available_images'] = len(images)
    logging.info("Created Symbolic links for images. Links [ %s ]" % len(images))

    catalog_name = None

    # Catalog
    if catalog == 'gaia2':
        # Procurar um arquivo csv no diretorio de data para converter para o formato do PRAIA.
        catalog_filename = 'gaia_dr2.csv'
        input_catalog = os.path.join(os.getenv("DATA_DIR"), catalog_filename)
        gaia_dr2_catalog = os.path.join(os.getenv("DATA_DIR"), "%s.cat" % os.path.splitext(catalog_filename)[0])

        if not os.path.exists(input_catalog):
            msg = "Catalog file not found. to use gaia2 must have in /data a csv file with the name gaia_dr2.csv."
            logging.error(msg)
            raise Exception(msg)

        result['reference_catalog_csv'] = input_catalog

        logging.debug("Converting User Catalog GAIA DR2 to PRAIA format. Filename: [%s]"  % input_catalog)

        with open(gaia_dr2_catalog, 'w') as fp:
            process = subprocess.Popen(["%s %s" % (os.getenv("CDS2REF"), input_catalog)],
                                   stdin=subprocess.PIPE, stdout=fp, shell=True)
        process.communicate()

        if not os.path.exists(gaia_dr2_catalog):
            msg = "Catalog file was not generated. Filename: [%s]" % gaia_dr2_catalog
            logging.error(msg)
            raise Exception(msg)

        logging.debug("Catalog was converted to PRAIA format. Filename: [%s]"  % gaia_dr2_catalog)

        catalog_name = os.path.splitext(catalog_filename)[0]
    else:
        catalog_name = catalog

    #
    # Execucao do Praia Header Extraction
    header_t0 =  datetime.now()
    try:
        praia_header_output = run_praia_header(images)
    except Exception as e:
        logging.error(e)
        raise Exception(e)
        
    header_t1 =  datetime.now()
    header_exec_time = header_t1 - header_t0
    logging.info("Praia Header Extraction executed in %s"  % humanize.naturaldelta(header_exec_time))


    # Execucao do Praia Astrometry 
    astrometry_t0 = datetime.now()
    headers = open(praia_header_output, "r")
    count_headers = count = len(open(praia_header_output).readlines(  ))

    pool = ThreadPoolExecutor(max_workers=max_workers)
    futures = []
    i = 1
    for header in headers:
        logging.info("Running Praia Astrometry %s of %s" % (i, count_headers))
        futures.append(pool.submit(execute_astrometry, i, header, catalog_name, cat_code, logging))
        i += 1
    
        result['processed_images'] += 1

    # Esperar todas as execucoes.
    wait(futures)

    # guardar o path dos arquivos xy em um arquivo de output.
    praia_astrometry_output = os.path.join(os.getenv("DATA_DIR"), "ast_out.txt")
    listXY = []
    for future in futures:
        xy, output = future.result()

        if output is not None:
            listXY.append(xy)
            result['outputs'][output['ccd_id']] = output
   
    np.savetxt(praia_astrometry_output, listXY, fmt='%s')

    astrometry_t1 =  datetime.now()
    astrometry_exec_time = astrometry_t1 - astrometry_t0
    logging.info("Praia Astrometry executed in %s"  % humanize.naturaldelta(astrometry_exec_time))

    # Execucao do Praia Targets

    # BSP JPL
    bsp_jpl_filename = "%s.bsp" % asteroid
    bsp_jpl = os.path.join(os.getenv("DATA_DIR"), bsp_jpl_filename)
    if not os.path.exists(bsp_jpl):
        msg = "BSP JPL file not found. Filename: [%s]" % bsp_jpl
        logging.error(msg)
        raise Exception(msg)
    logging.info("BSP JPL: [%s]"  % bsp_jpl)

    # BSP Planetary
    bsp_planets_filename = os.getenv("BSP_PLANETARY")
    bsp_planets = os.path.join(os.getenv("DATA_DIR"), bsp_planets_filename)
    if not os.path.exists(bsp_planets):
        # Se nao existir no data, criar link 
        os.symlink(os.path.join(os.getenv("BSP_PLANETARY_PATH"), bsp_planets_filename), bsp_planets)
    logging.info("BSP PLANETARY: [%s]"  % bsp_planets)

    # Leap Seconds
    leap_sec_filename = os.getenv("LEAP_SENCOND")
    leap_sec = os.path.join(os.getenv("DATA_DIR"), leap_sec_filename)
    if not os.path.exists(leap_sec):
        # Se nao existir no data, criar link 
        os.symlink(os.path.join(os.getenv("LEAP_SENCOND_PATH"), leap_sec_filename), leap_sec)
    logging.info("LEAP SECONDS: [%s]"  % leap_sec)

    # Lista com datas em JD extraidas do resultado do Praia Headers
    dates_jd = np.loadtxt(praia_header_output, dtype=str, usecols=(13,), ndmin=1)
    logging.info("DATES JD: [%s]"  % len(dates_jd))

    # Criar o arquivo de Targets 
    targets_file = create_targets_file(asteroid, dates_jd, bsp_jpl, bsp_planets, leap_sec)

    logging.info("Targets file was generated: [%s]"  % targets_file)
    result['targets_file'] = dict({
        'filename': os.path.basename(targets_file),
        'file_size': os.path.getsize(targets_file),
        'extension': os.path.splitext(targets_file)[1]
    })        


    # Criar o arquivo Targets Offset
    targets_offset = run_praia_target(praia_astrometry_output, targets_file)
    # Verificar ser o arquivo targets tem resultado. 
    if os.path.isfile(targets_offset) and os.path.getsize(targets_offset) > 0:    
        logging.info("Targets Offset file was generated: [%s]"  % targets_offset)

        result['target_offset'] = dict({
            'filename': os.path.basename(targets_offset),
            'file_size': os.path.getsize(targets_offset),
            'extension': os.path.splitext(targets_offset)[1]
        })

        # So Cria o arquivo de Astrometria se tiver resultado no targets_offset.
        # Arquivo de resultado da Astrometria.
        praia_target_output = create_obs_file(targets_offset, asteroid, obs_code, cat_code)

        # Verificar se o resultado final da Astrometria foi gerado com sucesso
        if os.path.isfile(praia_target_output) and os.path.getsize(praia_target_output) > 0:
            logging.info("Target Astrometry generated: [%s]"  % praia_target_output)

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


    # Plot Astrometry - CCD X Stars X Target
    if enable_plot is True and result['target_offset'] is not None:
        print("Executar thead para criar os plots")

        plot_t0 = datetime.now()

        # Ler o arquivo de ccds 
        df_ccds = get_ccd_images_from_csv(ccd_images_csv)

        # Ler o arquivo Catalogo de estrelas
        if result['reference_catalog_csv']:
            df_stars = read_stars_catalog(result['reference_catalog_csv'])
        else:
            df_stars = pd.DataFrame()

        # Ler arquivo Targets Offset 
        df_targets = read_targets_offset(os.path.join(os.getenv("DATA_DIR"), result['target_offset']['filename']))

        for idx in result['outputs']:
            ccd_result = result['outputs'][idx]

            t0 = datetime.now()

            # filtrar a lista de ccd pelo id
            ccd = df_ccds.loc[df_ccds['id'] == int(ccd_result['ccd_id'])].iloc[0]

            # Filtrar o catalogo de estrelas pelo ccd
            stars = df_stars.loc[df_stars['ccd_id'] == int(ccd['id'])]

            targets = df_targets.loc[df_targets['ccd_image'].str.contains(str(ccd['id'])) == True]

            plot_filepath = os.path.join(os.getenv("DATA_DIR"), '%s.ast_plot.png' % ccd['id'])
            plot = plotStarsCCD(asteroid, ccd, stars, targets, plot_filepath)

            logging.info("Plot [%s] created for ccd %s in %s." % (i, ccd['id'], humanize.naturaldelta(tdelta)))

            t1 = datetime.now()
            tdelta = t1 - t0

            i += 1

        plot_t1 = datetime.now()
        plot_tdelta = plot_t1 - plot_t0


    # # Remover os links da imagens
    # remove_symlink_for_images(images)
   
    # # Remover o link BSP Planetary
    # os.unlink(bsp_planets)

    # # Remover o link do Leap Second
    # os.unlink(leap_sec)

    # # Remover o link para o diretorio data_dir 
    # if os.path.islink(os.getenv("DATA_DIR")):
    #     os.unlink(os.getenv("DATA_DIR"))

    # voltar para o Path original
    # os.environ["DATA_DIR"] = original_data_path


    ###########################################################################
    # Organizar os resultados e salvar no json.
    ###########################################################################
    # targets_file = os.path.join(os.getenv("DATA_DIR"), os.path.basename(targets_file))
    # targets_offset = os.path.join(os.getenv("DATA_DIR"), os.path.basename(targets_offset))

    # praia_target_output = os.path.join(os.getenv("DATA_DIR"), os.path.basename(praia_target_output))

    # result = dict({
    #     'asteroid': asteroid,
    #     'images': len(ccd_images),
    #     'processed_images': len(images),
    #     'astrometry': dict({
    #         'filename': os.path.basename(praia_target_output),
    #         'filepath': praia_target_output,
    #         'file_size': os.path.getsize(praia_target_output),
    #         'extension': os.path.splitext(praia_target_output)[1]
    #     }),
    #     'target_offset': dict({
    #         'filename': os.path.basename(targets_offset),
    #         'filepath': targets_offset,
    #         'file_size': os.path.getsize(targets_offset),
    #         'extension': os.path.splitext(targets_offset)[1]
    #     }),
    #     'targets_file': dict({
    #         'filename': os.path.basename(targets_file),
    #         'filepath': targets_file,
    #         'file_size': os.path.getsize(targets_file),
    #         'extension': os.path.splitext(targets_file)[1]
    #     }),
    #     'outputs': dict({}),
    # })

    # # Entrar no diretorio original para fazer os acertos nos nomes.
    # for image in ccd_images:
    #     listOfFiles = os.listdir(os.getenv("DATA_DIR"))
    #     pattern = "%s*" % image['id']
    #     image_name = image['filename'].split('.')[0]

    #     # TODO: Corrigir o path das imagens nos arquivos de saida. e renomear os arquivos
    #     # substituindo o id da imagem pelo nome

    #     outputs = list()
    #     for old_filename in listOfFiles:
    #         if fnmatch.fnmatch(old_filename, pattern):
    #             new_filename = old_filename.replace(str(image['id']), image_name)

    #             new_path = os.path.join(os.getenv("DATA_DIR"), new_filename)
    #             os.rename(
    #                 os.path.join(
    #                     os.getenv("DATA_DIR"), old_filename), 
    #                     new_path
    #                 ) 
        
    #             extension = os.path.splitext(new_path)[1]
    #             catalog = None

    #             if extension == '.xy':
    #                 catalog = new_filename.split('.')[1]

    #             outputs.append(dict({
    #                 'catalog': catalog,
    #                 'filename': new_filename,
    #                 'filepath': new_path,
    #                 'file_size': os.path.getsize(new_path),
    #                 'extension': extension
    #             }))

    #     result['outputs'][image_name] = outputs


    # with open('astrometry.json', 'w') as json_file:
    #     json.dump(result, json_file)

    # Exibir o json de resultado na saida do programa
    # print(json.dumps(result, indent=2))

except Exception as e:
    logging.error("Error: [ %s ]"  % e)
    logging.error(traceback.format_exc())
    remove_symlink_for_images(images)

    # Remover o link para o diretorio data_dir 
    if os.path.islink(os.getenv("DATA_DIR")):
        os.unlink(os.getenv("DATA_DIR"))
    raise Exception(e)

finally:

    # Escrever o arquivo json com os resultados
    with open(os.path.join(os.getenv("DATA_DIR"), 'astrometry.json'), 'w') as json_file:
        json.dump(result, json_file)

    # Remover os links da imagens
    remove_symlink_for_images(images)
   
    # Remover o link BSP Planetary
    os.unlink(bsp_planets)

    # Remover o link do Leap Second
    os.unlink(leap_sec)

    # Remover o link para o diretorio data_dir 
    if os.path.islink(os.getenv("DATA_DIR")):
        os.unlink(os.getenv("DATA_DIR"))

    t1 =  datetime.now()
    t_delta = t1 - t0

    # Exibir o json de resultado na saida do programa
    print(json.dumps(result, indent=2))

    logging.info("Finish in %s"  % humanize.naturaldelta(t_delta))


