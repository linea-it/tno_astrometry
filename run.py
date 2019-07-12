#!/usr/bin/env python3
import os
import logging
from praia_header import run_praia_header, create_symlink_for_images, remove_symlink_for_images
from praia_astrometry import run_praia_astrometry, create_params_file, get_catalog_code
from praia_target import create_targets_file, run_praia_target, create_obs_file
import argparse
from datetime import datetime
import humanize
import traceback
import numpy as np
from concurrent.futures import ThreadPoolExecutor, wait, as_completed
import subprocess

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

args = parser.parse_args()

asteroid = args.asteroid
basepath = args.path
obs_code = args.obs_code
catalog = args.catalog
cat_code = get_catalog_code(catalog)
ccd_images_filename = args.images

if basepath is not None and basepath is not '':
    complete_path = os.path.join(os.getenv("DATA_DIR"), basepath.strip('/'))
    os.symlink(complete_path, "/data_tmp")
    os.environ["DATA_DIR"] = "/data_tmp"

# Setup Log file
logfile = os.path.join(os.getenv("DATA_DIR"), "astrometry.log")
logging.basicConfig(filename=logfile, level=logging.DEBUG)
logging.info("Start Astrometry pipeline")
logging.info("Asteroid: [ %s ]" % asteroid)

# # TODO Recuperar todas as exposicoes para o Asteroid.
# ccd_images = [
#     # {'filename': '/images/D00232016_g_c36_r2356p02_immasked.fits'},
#     # {'filename': '/images/D00233221_g_c20_r2357p02_immasked.fits'},
#     {'filename': '/images/D00240777_g_c11_r2362p01_immasked.fits'},
#     # {'filename': '/images/D00241125_g_c20_r2362p01_immasked.fits'},
#     # {'filename': '/images/D00246881_g_c41_r2363p01_immasked.fits'},
#     # {'filename': '/images/D00364725_r_c29_r2166p01_immasked.fits'},
#     # {'filename': '/images/D00364726_g_c56_r2166p01_immasked.fits'},
#     # {'filename': '/images/D00364727_i_c29_r2166p01_immasked.fits'},
#     # {'filename': '/images/D00372179_r_c06_r2182p02_immasked.fits'},
#     # {'filename': '/images/D00374550_z_c06_r2262p01_immasked.fits'},
#     # {'filename': '/images/D00382258_r_c10_r2277p01_immasked.fits'},
#     # {'filename': '/images/D00388143_i_c30_r2278p01_immasked.fits'},
#     # {'filename': '/images/D00398226_z_c30_r2284p01_immasked.fits'},
#     # {'filename': '/images/D00398231_z_c33_r2284p01_immasked.fits'},
#     # {'filename': '/images/D00503010_z_c30_r2378p01_immasked.fits'},
#     # {'filename': '/images/D00503041_i_c30_r2378p01_immasked.fits'},
#     # {'filename': '/images/D00506423_i_c35_r2379p01_immasked.fits'},
#     # {'filename': '/images/D00506424_z_c35_r2379p01_immasked.fits'},
#     # {'filename': '/images/D00506425_z_c35_r2379p01_immasked.fits'},
#     # {'filename': '/images/D00507393_i_c35_r2379p01_immasked.fits'},
#     # {'filename': '/images/D00507394_z_c35_r2379p01_immasked.fits'},
#     # {'filename': '/images/D00507395_z_c35_r2379p01_immasked.fits'}
# ]

# Imagens
ccd_images_csv = os.path.join(os.getenv("DATA_DIR"), str(ccd_images_filename))
if not os.path.exists(ccd_images_csv):
    msg = "csv file with ccds information not found. Filename: [%s]" % ccd_images_csv
    logging.error(msg)
    raise msg
ccd_images = np.genfromtxt(ccd_images_csv, dtype=None, delimiter=';', names=True, encoding='utf8')
logging.info("CCD Images: [ %s ]" % len(ccd_images))

# Criar link para imagens no mesmo diretorio dos dados. 
images = create_symlink_for_images(ccd_images)

logging.info("Created Symbolic links for images. Links [ %s ]" % len(images))

# Todo essa funcao pode ir para o praia_astrometry
def execute_astrometry(idx, header, catalog, catalog_code):
    try:
        t0 =  datetime.now()
        # Criar arquivo de input para cada execucao em paralelo.
        input_file = os.path.join(os.getenv("DATA_DIR"), "astrometry_input_%s.txt" % idx)
        logging.debug("Astrometry input IDX[%s] file [ %s ]" %(str(idx).ljust(2), input_file))

        with open(input_file, 'w') as text_file:
            text_file.write(header)
        text_file.close()

        # Criar arquivo de parametros para cada execucao.
        filename = os.path.join(os.getenv("DATA_DIR"), "astrometry_params_%s.dat" % idx)
        params_file = create_params_file(input_file, catalog, catalog_code, filename, idx)

        # Exucao do praia astrometry
        praia_astrometry_output = run_praia_astrometry(idx, input_file, catalog, params_file), 

        # remover os inputs e params.
        # os.remove(input_file)
        # os.remove(params_file)

        t1 = datetime.now()
        tdelta = t1 - t0

        logging.info("Astrometry IDX[%s] Output [ %s ] executed in %s" %(str(idx).ljust(2), praia_astrometry_output, humanize.naturaldelta(tdelta)))
        
        return praia_astrometry_output
    except Exception as e:
        logging.error(e)
        logging.error(traceback.format_exc())
        return None


try:

    t0 = datetime.now()

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
            raise msg

        logging.debug("Converting User Catalog GAIA DR2 to PRAIA format. Filename: [%s]"  % input_catalog)

        with open(gaia_dr2_catalog, 'w') as fp:
            process = subprocess.Popen(["%s %s" % (os.getenv("CDS2REF"), input_catalog)],
                                   stdin=subprocess.PIPE, stdout=fp, shell=True)
        process.communicate()

        if not os.path.exists(gaia_dr2_catalog):
            msg = "Catalog file was not generated. Filename: [%s]" % gaia_dr2_catalog
            logging.error(msg)
            raise msg

        logging.debug("Catalog was converted to PRAIA format. Filename: [%s]"  % gaia_dr2_catalog)

        catalog_name = os.path.splitext(catalog_filename)[0]
    else:
        catalog_name = catalog

    # Execucao do Praia Header Extraction
    header_t0 =  datetime.now()
    try:
        praia_header_output = run_praia_header(images)
    except Exception as e:
        logging.error(e)
        raise(e)
        
    header_t1 =  datetime.now()
    header_exec_time = header_t1 - header_t0
    logging.info("Praia Header Extraction executed in %s"  % humanize.naturaldelta(header_exec_time))


    # Execucao do Praia Astrometry 
    astrometry_t0 = datetime.now()
    headers = open(praia_header_output, "r")
    count_headers = count = len(open(praia_header_output).readlines(  ))

    pool = ThreadPoolExecutor()
    futures = []
    i = 1
    for header in headers:
        logging.info("Running Praia Astrometry %s of %s" % (i, count_headers))
        futures.append(pool.submit(execute_astrometry, i, header, catalog_name, cat_code))
        i += 1
    
    # Esperar todas as execucoes.
    wait(futures)

    # guardar o path dos arquivos xy em um arquivo de output.
    praia_astrometry_output = os.path.join(os.getenv("DATA_DIR"), "praia_astrometry_output.txt")
    listXY = []
    for future in futures:
        xy = future.result()
        if xy is not None:
            listXY.append(xy)
    
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
        raise msg
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

    # Criar o arquivo Targets Offset
    targets_offset = run_praia_target(praia_astrometry_output, targets_file)
    logging.info("Targets Offset file was generated: [%s]"  % targets_offset)

    # Arquivo de resultado da Astrometria.
    praia_target_output = create_obs_file(targets_offset, asteroid, obs_code, cat_code)
    logging.info("Target Astrometry generated: [%s]"  % praia_target_output)

    # Remover os links da imagens
    remove_symlink_for_images(images)
   
    # Remover o link BSP Planetary
    os.unlink(bsp_planets)

    # Remover o link do Leap Second
    os.unlink(leap_sec)

    t1 =  datetime.now()
    t_delta = t1 - t0

    logging.info("Astrometry Output: [%s]"  % praia_target_output)
    logging.info("Finish in %s"  % humanize.naturaldelta(t_delta))


except Exception as e:
    logging.error("Error: [ %s ]"  % e)
    logging.error(traceback.format_exc())
    remove_symlink_for_images(images)
    raise(e)