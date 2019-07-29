import subprocess
import os
from glob import glob
import numpy as np
from datetime import datetime
import traceback
import humanize

file_catalogs = ['gaia1',]

def get_catalog_code(name):
    # Gaia-DR1 = U
    # Gaia-DR2 = V    
    d_catalogs = dict({
        'gaia1': 'U',
        'gaia2': 'V'
    })

    return d_catalogs[name]

def create_params_file(praia_header_output, user_catalog, catalog_code, output, idx):

    with open(os.path.join(os.getenv("APP_PATH"), "src/praia_astrometry.template.dat")) as template:

        data = template.read()
        data = data.replace('{PRAIA_HEADER_OUTPUT}', praia_header_output.ljust(50))

        # Verificar se o catalogo que sera usado e diferente dos catalogos defaults.
        if user_catalog not in file_catalogs:
            catalog = os.path.join(os.getenv("DATA_DIR"), user_catalog + ".cat")
            catalog_xy = user_catalog + ".rad.xy"
            data = data.replace('{USER_CATALOG}', catalog.ljust(50))
            data = data.replace('{USER_CATALOG_XY}', catalog_xy.ljust(50))

        else:
            # se o catalogo passado for um catalogo default o user catalog nao sera usado.
            data = data.replace('{USER_CATALOG}', "user_catalog_palceholder.cat".ljust(50))

        # adicionar identificador para os arquivos intermediarios
        data = data.replace('{IDX}', str(idx).ljust(5))

        # Catalog Reference
        data = data.replace('{CATALOG_REFERENCE}', catalog_code)

        # Data Path 
        data = data.replace('{DATA_DIR}', os.getenv("DATA_DIR"))

        with open(output, 'w') as new_file:
            new_file.write(data)
        new_file.close()

    template.close()

    return output


def execute_astrometry(idx, header, catalog, catalog_code, ccd_images, logging):
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
        praia_astrometry_output = run_praia_astrometry(idx, input_file, catalog, params_file)

        t1 = datetime.now()
        tdelta = t1 - t0

        logging.info("Astrometry IDX[%s] Output [ %s ] executed in %s" %(str(idx).ljust(2), praia_astrometry_output, humanize.naturaldelta(tdelta)))
        
        return praia_astrometry_output
    except Exception as e:
        logging.error(e)
        logging.error(traceback.format_exc())
        return None



def run_praia_astrometry(idx, praia_header_output, catalog,  params_file):

    praia_astrometry = os.getenv("PRAIA_ASTROMETRY")

    exec_log = os.path.join(os.getenv("DATA_DIR"), "praia_astrometry_%s.log" % idx)

    with open(exec_log, 'w') as fp:
        process = subprocess.Popen(["%s < %s" % (praia_astrometry, params_file)],
                                   stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE, shell=True)

        out, error = process.communicate()

        fp.write(out.decode("utf-8"))
        fp.write(error.decode("utf-8"))

        if process.returncode > 0:
            raise Exception("Failed to run PRAIA Astrometry. \n" + error.decode("utf-8"))

    # Verificar se o arquivo xy foi gerado.
    ccd_image_path = np.loadtxt(praia_header_output, dtype=str, usecols=(19,), unpack=True)
    ccd_image = os.path.splitext(os.path.basename(str(ccd_image_path)))[0]

    xy = os.path.join(os.getenv("DATA_DIR"), "%s.%s.rad.xy" % (ccd_image, catalog))
    reg = os.path.join(os.getenv("DATA_DIR"), "%s.reg" % (ccd_image))
    mes = os.path.join(os.getenv("DATA_DIR"), "%s.mes" % (ccd_image))
    
    # Remove .reg e .mes
    # os.remove(reg)
    # os.remove(mes)

    if os.path.exists(xy):
        return xy
    else:
        return None



      