import os
import logging
from praia_header import run_praia_header
import argparse
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

# TODO Adicionar logs nessa funcao.
praia_header_output = run_praia_header(exposures)

print("Terminou")
