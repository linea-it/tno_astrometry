import subprocess
import os

praia_header_input = 'images_paths.txt'
praia_header_params = 'hd.dat'
praia_header_output = 'hd_out.txt'


def create_input_file(images):
    input_file = os.path.join(os.getenv("DATA_DIR"), praia_header_input)
    with open(input_file, 'w') as inp_file:
        for image in images:

            inp_file.write(image + "\n")
        inp_file.close()

    return input_file


def create_params_file(input_file, output_file):

    with open(os.path.join(os.getenv("APP_PATH"), "src/praia_header.template.dat")) as template:

        data = template.read()
        data = data.replace('{INPUT_FILE}', input_file.ljust(50))
        data = data.replace('{OUTPUT_FILE}', output_file.ljust(50))

        params_file = os.path.join(os.getenv("DATA_DIR"), praia_header_params)
        with open(params_file, 'w') as new_file:
            new_file.write(data)
        new_file.close()

    template.close()

    return params_file

def create_symlink_for_images(images):
    """ 
        Cria Link simbolico para as imagens. 
        para os programas PRAIA funcioanrem e necessario que as imagens estejam 
        no mesmo diretorio onde o programa esta executando, para evitar uma 
        copia, sao criados os links simbolicos. 
        
        Antes de criar o link verifica se a imagem existe. 

        origem: /images/...../filename.fits -> /DATA_DIR/filename.fits

        retorna uma lista com os links criados. 
        obs: se uma imagem nao existir ela nao e retornada. 
    """
    images_list = []
    for image in images:
        origin = os.path.join(os.getenv("IMAGES_PATH"), image['filename'])

        if os.path.exists(origin):
            filename = os.path.basename(origin)
            filename = "%s.fits" % str(image['id'])
            dest = os.path.join(os.getenv("DATA_DIR"), filename)
            os.symlink(origin, dest)

            images_list.append(dest)
        # TODO Baixar a imagem quando nao existir no diretorio

    return images_list

def remove_symlink_for_images(images):
    for image in images:
        os.unlink(image)


def run_praia_header(images):

    input_file = create_input_file(images)

    output_file = os.path.join(os.getenv("DATA_DIR"), praia_header_output)

    params_file = create_params_file(input_file, output_file)

    praia_header = os.getenv("PRAIA_HEADER")

    exec_log = os.path.join(os.getenv("DATA_DIR"), "praia_header.log")

    with open(exec_log, 'wb') as fp:

        # process = subprocess.Popen(["%s < %s" % (praia_header, params_file)],
        #                         stdin=subprocess.PIPE, stdout=fp, stderr=fp, shell=True)
        process = subprocess.Popen(["%s < %s" % (praia_header, params_file)],
                                stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE, shell=True)        
        out, error = process.communicate()

        fp.write(out)
        fp.write(error)

        if process.returncode > 0:
            raise Exception("Failed to run PRAIA Header Extraction. \n" + error.decode("utf-8"))

    if os.path.exists(output_file):
        return output_file
    else:
        raise Exception(
            "Failed to run PRAIA Header Extraction. result file was not generated.")
