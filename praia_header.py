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
    print(output_file)


def run_praia_header(exposures):
    print("run praia header")

    input_file = create_input_file(exposures)

    output_file = os.path.join(os.getenv("DATA_DIR"), praia_header_output)

    params_file = create_params_file(input_file, output_file)
