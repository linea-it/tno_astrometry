#!/usr/bin/env python3
import os
import logging
from praia_header import run_praia_header, create_symlink_for_images, remove_symlink_for_images
from praia_astrometry import run_praia_astrometry
from praia_target import create_targets_file
import argparse
from datetime import datetime
import humanize
import traceback
import numpy as np

parser = argparse.ArgumentParser()
parser.add_argument(
    "asteroid", help="Asteroid name without spaces. example: 1999RB216")
parser.add_argument(
    "--catalog", default="gaia1", help="Name of the catalog that will be used to identify the targets. example gaia1.")    
args = parser.parse_args()


asteroid = args.asteroid

# TODO Catalogo do Usuario (Gaia DR2)
# user_catalog = 'gaia_dr2.cat'
# TODO melhorar essa parte
catalog = args.catalog


# Setup Log file
logfile = os.path.join(os.getenv("DATA_DIR"), "astrometry.log")
logging.basicConfig(filename=logfile, level=logging.DEBUG)
logging.info("Start Astrometry pipeline")
logging.info("Asteroid: [ %s ]" % asteroid)


# TODO Recuperar todas as exposicoes para o Asteroid.
ccd_images = [
    # {'filename': '/data/D00232016_g_c36_r2356p02_immasked.fits'},
    # {'filename': '/data/D00233221_g_c20_r2357p02_immasked.fits'},
    {'filename': '/images/D00232016_g_c36_r2356p02_immasked.fits'},
    # {'filename': '/images/D00233221_g_c20_r2357p02_immasked.fits'},
    # {'filename': '/images/D00240777_g_c11_r2362p01_immasked.fits'},
    # {'filename': '/images/D00241125_g_c20_r2362p01_immasked.fits'},
    # {'filename': '/images/D00246881_g_c41_r2363p01_immasked.fits'},
    # {'filename': '/images/D00364725_r_c29_r2166p01_immasked.fits'},
    # {'filename': '/images/D00364726_g_c56_r2166p01_immasked.fits'},
    # {'filename': '/images/D00364727_i_c29_r2166p01_immasked.fits'},
    # {'filename': '/images/D00372179_r_c06_r2182p02_immasked.fits'},
    # {'filename': '/images/D00374550_z_c06_r2262p01_immasked.fits'},
    # {'filename': '/images/D00382258_r_c10_r2277p01_immasked.fits'},
    # {'filename': '/images/D00388143_i_c30_r2278p01_immasked.fits'},
    # {'filename': '/images/D00398226_z_c30_r2284p01_immasked.fits'},
    # {'filename': '/images/D00398231_z_c33_r2284p01_immasked.fits'},
    # {'filename': '/images/D00503010_z_c30_r2378p01_immasked.fits'},
    # {'filename': '/images/D00503041_i_c30_r2378p01_immasked.fits'},
    # {'filename': '/images/D00506423_i_c35_r2379p01_immasked.fits'},
    # {'filename': '/images/D00506424_z_c35_r2379p01_immasked.fits'},
    # {'filename': '/images/D00506425_z_c35_r2379p01_immasked.fits'},
    # {'filename': '/images/D00507393_i_c35_r2379p01_immasked.fits'},
    # {'filename': '/images/D00507394_z_c35_r2379p01_immasked.fits'},
    # {'filename': '/images/D00507395_z_c35_r2379p01_immasked.fits'}
]

logging.info("CCD Images: [ %s ]" % len(ccd_images))

# Criar link para imagens no mesmo diretorio dos dados. 
images = create_symlink_for_images(ccd_images)
print(images)
logging.info("Created Symbolic links for images")

try:
    # Execucao do Praia Header Extraction
    header_t0 =  datetime.now()
    praia_header_output = run_praia_header(images)
    header_t1 =  datetime.now()
    header_exec_time = header_t1 - header_t0
    logging.info("Praia Header Extraction executed in %s"  % humanize.naturaldelta(header_exec_time))


    # Execucao do Praia Astrometry
    astrometry_t0 = datetime.now()
    praia_astrometry_output = run_praia_astrometry(praia_header_output, catalog)
    astrometry_t1 =  datetime.now()
    astrometry_exec_time = astrometry_t1 - astrometry_t0
    logging.info("Praia Astrometry executed in %s"  % humanize.naturaldelta(astrometry_exec_time))


    # TODO comparar a quantidade de exposures com os xy se for diferente e por que falhou em algum.

    # Execucao do Praia Targets
    # TODO bsp_jpl
    bsp_jpl_filename = "Eris.bsp"
    bsp_jpl = os.path.join(os.getenv("DATA_DIR"), bsp_jpl_filename)
    logging.info("BSP JPL: [%s]"  % bsp_jpl)

    # TODO bsp_planets
    bsp_planets_filename = "de435.bsp"
    bsp_planets = os.path.join(os.getenv("DATA_DIR"), bsp_planets_filename)
    logging.info("BSP PLANETARY: [%s]"  % bsp_planets)

    # TODO leap second
    leap_sec_filename = "naif0012.tls"
    leap_sec = os.path.join(os.getenv("DATA_DIR"), leap_sec_filename)
    logging.info("LEAP SECONDS: [%s]"  % leap_sec)

    # Lista com datas em JD extraidas do resultado do Praia Headers
    dates_jd = np.loadtxt(praia_header_output, dtype=str, usecols=(13,), ndmin=1)
    logging.info("DATES JD: [%s]"  % len(dates_jd))

    # Criar o arquivo de Targets 
    targets_file = create_targets_file(asteroid, dates_jd, bsp_jpl, bsp_planets, leap_sec)
    logging.info("Targets file was generated: [%s]"  % targets_file)


    # Remover os links da imagens
    remove_symlink_for_images(images)

    # TODO precisa de uma lista de arquivos xy gerados pelo praia Astrometry.
    # para gerar esta lista precisa de um parametro informando com o catalogo gaia que esta sendo usado. 
    print("Terminou")

except Exception as e:
    logging.error("Error: [ %s ]"  % e)
    logging.error(traceback.format_exc())
    remove_symlink_for_images(images)
    raise(e)
