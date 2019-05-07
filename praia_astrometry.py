import subprocess
import os
from glob import glob
import numpy as np

gaia_catalogs = ['gaia1', 'gaia2', 'gaia3', 'gaia4', 'gaia5', 'gaia6']

# Gaia-DR1 = U
# Gaia-DR2 = V    
d_catalogs = dict({
    'gaia1': 'U',
    'gaia2': 'V'
})

def create_params_file(praia_header_output, user_catalog, output, idx):

    with open(os.path.join(os.getenv("APP_PATH"), "src/praia_astrometry.template.dat")) as template:

        data = template.read()
        data = data.replace('{PRAIA_HEADER_OUTPUT}', praia_header_output.ljust(50))

        # Verificar se o catalogo que sera usado e diferente dos catalogos defaults.
        if user_catalog not in gaia_catalogs:
            catalog = user_catalog + ".cat"
            data = data.replace('{USER_CATALOG}', catalog.ljust(50))
        else:
            # se o catalogo passado for um catalogo default o user catalog nao sera usado.
            data = data.replace('{USER_CATALOG}', "user_catalog_palceholder.cat".ljust(50))

        # adicionar identificador para os arquivos intermediarios
        data = data.replace('{IDX}', str(idx).ljust(5))

        # Catalog Reference
        data = data.replace('{CATALOG_REFERENCE}', d_catalogs[user_catalog])

        with open(output, 'w') as new_file:
            new_file.write(data)
        new_file.close()

    template.close()

    return output


def run_praia_astrometry(praia_header_output, catalog, params_file):

    praia_astrometry = os.getenv("PRAIA_ASTROMETRY")

    exec_log = os.path.join(os.getenv("DATA_DIR"), "praia_astrometry.log")

    with open(exec_log, 'w') as fp:
        process = subprocess.Popen(["%s < %s" % (praia_astrometry, params_file)],
                                   stdin=subprocess.PIPE, stdout=fp, shell=True)
        process.communicate()

    # # lista com arquivos de resultado, referente ao user_catalog 
    # keyXYfiles = "%s/*.%s.rad.xy" % (os.getenv("DATA_DIR"), catalog)
    # listXY = sorted(glob(keyXYfiles))

    # # Remove arquivos de resultado que nao serao usados.
    # for filename in os.listdir(os.getenv("DATA_DIR")):
    #     filepath = os.path.join(os.getenv("DATA_DIR"), filename)
    #     if (filename.endswith(".xy") or filename.endswith(".mes") or filename.endswith(".reg")) and filepath not in listXY:
    #         os.remove(filepath)

    # # guardar o path dos arquivos xy em um arquivo de output.
    # output = os.path.join(os.getenv("DATA_DIR"), praia_astrometry_output)
    # np.savetxt(output, listXY, fmt='%s')

    # Verificar se o arquivo xy foi gerado.
    ccd_image_path = np.loadtxt(praia_header_output, dtype=str, usecols=(19,), unpack=True)
    ccd_image = os.path.splitext(os.path.basename(str(ccd_image_path)))[0]

    xy = os.path.join(os.getenv("DATA_DIR"), "%s.%s.rad.xy" % (ccd_image, catalog))
    reg = os.path.join(os.getenv("DATA_DIR"), "%s.reg" % (ccd_image))
    mes = os.path.join(os.getenv("DATA_DIR"), "%s.mes" % (ccd_image))
    
    # Remove .reg e .mes
    os.remove(reg)
    os.remove(mes)

    if (os.path.exists(xy)):
        return xy
    else:
        return None