import os
import subprocess


def gaia_dr2_to_praia_catalog(filepath):
    """
        Converts the GAIA DR2 catalog to the PRAIA Catalog Format.

        @filepath: caminho para o arquivo csv no formato original do gaia_dr2.
    """

    input_filename = os.path.basename(filepath)
    output_filename = "%s.cat" % os.path.splitext(input_filename)[0]
    output = os.path.join(
        os.getenv("DATA_DIR"), output_filename)

    # Verifica se o arquivo de entrada existe
    if not os.path.exists(filepath):
        raise Exception(
            "Catalog file not found. to use gaia2 must have in /data a csv file with the name gaia_dr2.csv.")

    # Executa o programa que vai converter o catalogo.
    with open(output, 'w') as fp:
        process = subprocess.Popen(["%s %s" % (os.getenv("CDS2REF"), filepath)],
                                   stdin=subprocess.PIPE, stdout=fp, shell=True)
        process.communicate()

    if not os.path.exists(output):
        raise Exception(
            "Catalog file was not generated. Filename: [%s]" % output)

    return output
