import subprocess
import os

praia_header_input = 'images_paths.txt'
praia_header_params = 'praia_header_params.dat'
praia_header_output = 'praia_header_output.txt'


def create_input_file(exposures):
    input_file = os.path.join(os.getenv("DATA_DIR"), praia_header_input)
    with open(input_file, 'w') as inp_file:
        for exposure in exposures:
            filepath = os.path.join(
                os.getenv("DATA_DIR"), exposure['filename'])
            inp_file.write(filepath + "\n")
        inp_file.close()

    return input_file


def create_params_file(input_file, output_file):

    with open(os.path.join(os.getenv("APP_PATH"), "src/praia_header_params.template.dat")) as template:

        data = template.read()
        data = data.replace('{INPUT_FILE}', input_file.ljust(50))
        data = data.replace('{OUTPUT_FILE}', output_file.ljust(50))

        params_file = os.path.join(os.getenv("DATA_DIR"), praia_header_params)
        with open(params_file, 'w') as new_file:
            new_file.write(data)
        new_file.close()

    template.close()

    return params_file


def run_praia_header(exposures):
    input_file = create_input_file(exposures)

    output_file = os.path.join(os.getenv("DATA_DIR"), praia_header_output)

    params_file = create_params_file(input_file, output_file)

    praia_header = os.getenv("PRAIA_HEADER")

    exec_log = os.path.join(os.getenv("DATA_DIR"), "praia_header.log")

    with open(exec_log, 'w') as fp:
        process = subprocess.Popen(["%s < %s" % (praia_header, params_file)],
                                   stdin=subprocess.PIPE, stdout=fp, shell=True)
        process.communicate()

    if os.path.exists(output_file):
        return output_file
    else:
        raise Exception(
            "Failed to run PRAIA Header Extraction. result file was not generated.")
