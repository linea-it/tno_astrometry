#!/usr/bin/env python3
import os
import logging
from praia_header import run_praia_header
from praia_astrometry import run_praia_astrometry
import argparse
from datetime import datetime
import humanize
parser = argparse.ArgumentParser()
parser.add_argument(
    "asteroid", help="Asteroid name without spaces. example: 1999RB216")
args = parser.parse_args()


asteroid = args.asteroid

# Setup Log file
logfile = os.path.join(os.getenv("DATA_DIR"), "astrometry.log")
logging.basicConfig(filename=logfile, level=logging.DEBUG)
logging.info("Start Astrometry pipeline")
logging.info("Asteroid: [ %s ]" % asteroid)


# TODO Recuperar todas as exposicoes para o Asteroid.
exposures = [
    {'filename': '/data/D00232016_g_c36_r2356p02_immasked.fits'},
    {'filename': '/data/D00233221_g_c20_r2357p02_immasked.fits'}
]

logging.info("Exposures: [ %s ]" % len(exposures))


# TODO Catalogo do Usuario (Gaia DR2)
user_catalog = 'gaia_dr2.cat'

# Execucao do Praia Header Extraction
header_t0 =  datetime.now()
praia_header_output = run_praia_header(exposures)
header_t1 =  datetime.now()
header_exec_time = header_t1 - header_t0
logging.info("Praia Header Extraction executed in %s"  % humanize.naturaldelta(header_exec_time))


# Execucao do Praia Astrometry
astrometry_t0 = datetime.now()
output = run_praia_astrometry(praia_header_output, user_catalog)
astrometry_t1 =  datetime.now()
astrometry_exec_time = astrometry_t1 - astrometry_t0
logging.info("Praia Astrometry executed in %s"  % humanize.naturaldelta(astrometry_exec_time))


print("Terminou")
