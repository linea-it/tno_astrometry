import subprocess
import os

praia_astrometry_params = 'praia_astrometry.dat'

def create_params_file(praia_header_output, user_catalog):

    with open(os.path.join(os.getenv("APP_PATH"), "src/praia_astrometry.template.dat")) as template:

        data = template.read()
        data = data.replace('{PRAIA_HEADER_OUTPUT}', praia_header_output.ljust(50))
        data = data.replace('{USER_CATALOG}', user_catalog.ljust(50))

        params_file = os.path.join(os.getenv("DATA_DIR"), praia_astrometry_params)
        with open(params_file, 'w') as new_file:
            new_file.write(data)
        new_file.close()

    template.close()

    return params_file


def run_praia_astrometry(praia_header_output, user_catalog):

    params_file = create_params_file(praia_header_output, user_catalog)

    praia_astrometry = os.getenv("PRAIA_ASTROMETRY")

    exec_log = os.path.join(os.getenv("DATA_DIR"), "praia_astrometry.log")

    with open(exec_log, 'w') as fp:
        process = subprocess.Popen(["%s < %s" % (praia_astrometry, params_file)],
                                   stdin=subprocess.PIPE, stdout=fp, shell=True)
        process.communicate()



    # if os.path.exists(output_file):
    #     return output_file
    # else:
    #     raise Exception(
    #         "Failed to run PRAIA Header Extraction. result file was not generated.")
