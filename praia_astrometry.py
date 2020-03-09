import subprocess
import os
from glob import glob
import numpy as np
from datetime import datetime
import traceback
import humanize
import fnmatch

file_catalogs = ['gaia1', ]


def get_catalog_code(name):
    # Gaia-DR1 = U
    # Gaia-DR2 = V
    d_catalogs = dict({
        'gaia1': 'U',
        'gaia2': 'V'
    })

    return d_catalogs[name]


def create_params_file(praia_header_output, user_catalog, catalog_code, output, idx, ccd_id):

    with open(os.path.join(os.getenv("APP_PATH"), "src/praia_astrometry.template.dat")) as template:

        data = template.read()
        data = data.replace('{PRAIA_HEADER_OUTPUT}',
                            praia_header_output.ljust(50))

        # Verificar se o catalogo que sera usado e diferente dos catalogos defaults.
        if user_catalog not in file_catalogs:
            catalog = os.path.join(os.getenv("DATA_DIR"),
                                   user_catalog + ".cat")
            catalog_xy = user_catalog + ".rad.xy"
            data = data.replace('{USER_CATALOG}', catalog.ljust(50))
            data = data.replace('{USER_CATALOG_XY}', catalog_xy.ljust(50))

            data = data.replace('{CUSER}', user_catalog)

        else:
            # se o catalogo passado for um catalogo default o user catalog nao sera usado.
            data = data.replace(
                '{USER_CATALOG}', "user_catalog_placeholder.cat".ljust(50))
            data = data.replace('{CUSER}', 'CUSER')

        # adicionar identificador para os arquivos intermediarios
        data = data.replace('{IDX}', str(idx).ljust(5))

        # Catalog Reference
        data = data.replace('{CATALOG_REFERENCE}', catalog_code)

        # Data Path
        data = data.replace('{DATA_DIR}', os.getenv("DATA_DIR"))

        # Astrometry Photometry
        ast_photometry = "%s/%s.ast_photometry.txt" % (
            os.getenv("DATA_DIR"), ccd_id)
        data = data.replace('{AST_PHOTOMETRY}', ast_photometry.ljust(50))

        # Astrometry Reduction
        # ast_reduction = "%s/%s.ast_reduction" % (os.getenv("DATA_DIR"), ccd_id)
        # data = data.replace('{AST_REDUCTION}', ast_photometry.ljust(50))
        data = data.replace('{CCD_ID}', ccd_id)

        with open(output, 'w') as new_file:
            new_file.write(data)
        new_file.close()

    template.close()

    return output


def execute_astrometry(idx, header, catalog, catalog_code, logging):
    try:
        t0 = datetime.now()

        # Recuperar ccd_id a partir da header.
        ccd_image_path = header[115:167].strip()
        ccd_id = get_ccd_id_from_filepath(ccd_image_path)

        # Criar arquivo de input para cada execucao em paralelo.
        input_file = os.path.join(
            os.getenv("DATA_DIR"), "%s.astrometry_input.txt" % ccd_id)
        logging.debug("Astrometry input IDX[%s] file [ %s ]" % (
            str(idx).ljust(2), input_file))

        with open(input_file, 'w') as text_file:
            text_file.write(header)
        text_file.close()

        # Criar arquivo de parametros para cada execucao.
        filename = os.path.join(os.getenv("DATA_DIR"),
                                "%s.astrometry_params.dat" % ccd_id)
        params_file = create_params_file(
            input_file, catalog, catalog_code, filename, idx, ccd_id)

        # Exucao do praia astrometry
        praia_astrometry_output, output = run_praia_astrometry(
            idx, ccd_id, input_file, catalog, params_file)

        t1 = datetime.now()
        tdelta = t1 - t0

        logging.info("Astrometry IDX[%s] Output [ %s ] executed in %s" % (
            str(idx).ljust(2), praia_astrometry_output, humanize.naturaldelta(tdelta)))

        if praia_astrometry_output is not None:
            output.update({
                'start': t0.isoformat(),
                'finish': t1.isoformat(),
                'execution_time': tdelta.total_seconds()
            })

        return praia_astrometry_output, output

    except Exception as e:
        logging.error(e)
        logging.error(traceback.format_exc())
        return None, None


def run_praia_astrometry(idx, ccd_id, praia_header_output, catalog,  params_file):

    praia_astrometry = os.getenv("PRAIA_ASTROMETRY")

    exec_log = os.path.join(os.getenv("DATA_DIR"),
                            "%s.praia_astrometry.log" % ccd_id)

    with open(exec_log, 'w') as fp:
        process = subprocess.Popen(["%s < %s" % (praia_astrometry, params_file)],
                                   stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE, shell=True)

        out, error = process.communicate()

        fp.write(out.decode("utf-8"))
        fp.write(error.decode("utf-8"))

        if process.returncode > 0:

            raise Exception(
                "Astrometry IDX[%s] CCD ID [%s]- Failed to run PRAIA Astrometry. See log [%s] for more information" % (idx, ccd_id, exec_log))

    # Verificar se o arquivo xy foi gerado.
    ccd_image_path = np.loadtxt(
        praia_header_output, dtype=str, usecols=(19,), unpack=True)
    ccd_image = os.path.splitext(os.path.basename(str(ccd_image_path)))[0]

    files = []

    reference_catalog_xy = os.path.join(os.getenv("DATA_DIR"), "%s.%s.rad.xy" %
                                        (ccd_image, catalog))
    xy = dict({
        'ccd_id': int(ccd_image),
        'reference_catalog_xy': reference_catalog_xy
    })
    # reg = os.path.join(os.getenv("DATA_DIR"), "%s.reg" % (ccd_image))
    # mes = os.path.join(os.getenv("DATA_DIR"), "%s.mes" % (ccd_image))

    listOfFiles = os.listdir(os.getenv("DATA_DIR"))
    pattern = "%s*" % ccd_image

    for filename in listOfFiles:
        if fnmatch.fnmatch(filename, pattern):
            try:
                f = parse_outputs_filename(filename)
                if f['extension'] != ".fits":
                    files.append(f)

            except Exception as e:
                print(e)

    # Remove .reg e .mes
    # os.remove(reg)
    # os.remove(mes)

    if os.path.exists(reference_catalog_xy):
        outputs = dict({
            'id': idx,
            'ccd_id': ccd_image,
            'files': files
        })

        return xy, outputs
    else:
        return None, None


def parse_outputs_filename(filename):
    filepath = os.path.join(os.getenv("DATA_DIR"), filename)
    extension = os.path.splitext(filename)[1]
    catalog = None

    if extension == '.xy':
        catalog = filename.split('.')[1]
    elif extension == '.txt':
        if filename.find('ast_reduction') != -1:
            catalog = filename.split('.')[2]

    return dict({
        'catalog': catalog,
        'filename': filename,
        'file_size': os.path.getsize(filepath),
        'extension': extension
    })


def get_ccd_id_from_filepath(filepath):
    filename = os.path.basename(filepath)
    ccd_id = os.path.splitext(filename)[0]
    return ccd_id
