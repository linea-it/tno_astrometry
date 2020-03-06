import os

import numpy as np
import pandas as pd
import fnmatch

# def read_ccd_image_csv(filepath):
#     return np.genfromtxt(filepath, dtype=None, delimiter=';', names=True, encoding='utf8')


def get_image_by_id(ccd_images, image_id):
    # rows = read_ccd_image_csv(filepath)
    for row in ccd_images:
        if int(row['id']) == int(image_id):
            return row


def read_ccd_image_csv(filepath):
    df = pd.read_csv(
        filepath,
        delimiter=";",
        usecols=["id", "expnum", "ccdnum", "band", "ra_cent", "dec_cent",
                 "rac1", "rac2", "rac3", "rac4", "decc1", "decc2", "decc3", "decc4", "filename", "date_obs"],
        dtype={
            'id': 'Int64',
        }
    )
    # Adicioanar colunas ao Dataframe.
    # Available = Bolean, inicia vazia, depois recebe True ou False caso a imagem exista no diretório.
    df['available'] = ""
    # Original Path = String, path original para o arquivo, antes de ser criado o link, neste ponto o filename ainda é o nome do CCD.
    df['original_path'] = ""
    # Current Path = String, path atual da imagem, aqui já é um link simbolico onde o nome do arquivo passa a ser o CCD_ID.
    df['current_path'] = ""
    # Reference Catalog xy = String, path para o resultado da astrometria, especifico para o catalogo de referenceia, esta coluna é para facilitar o acesso a este arquivo.
    # e para facilitar a associação do resultado com o CCD.
    df['reference_catalog_xy'] = ""

    # Usar o id como index no Dataframe. é utila para alterar ou incluir colunas nas linhas pelo id.
    df = df.set_index('id')

    # Ordenar os CCDs pela data de obs
    # IMPORTANTE: está ordem é dependencia para o funcionamento dos programas.
    # Nao deve ser alterada.
    df = df.sort_values(by=['date_obs'])
    return df


def create_symlink_for_images(images, logging):
    """ 
        Cria Link simbolico para as imagens. 
        para os programas PRAIA funcioanrem e necessario que as imagens estejam 
        no mesmo diretorio onde o programa esta executando, para evitar uma 
        copia, sao criados os links simbolicos. 

        Antes de criar o link verifica se a imagem existe. 

        origem: /images/...../filename.fits -> /DATA_DIR/filename.fits

        Retorna o mesmo dataframe, atualizado nas colunas available, original_path, current_path
    """
    origins = []
    image_paths = []
    availables = []

    # Resetar o index do Dataframe.
    images = images.reset_index()

    for i, image in images.iterrows():
        origin = os.path.join(os.getenv("IMAGES_PATH"), image['filename'])

        if os.path.exists(origin):

            filename = os.path.basename(origin)
            filename = "%s.fits" % str(image['id'])
            dest = os.path.join(os.getenv("DATA_DIR"), filename)
            os.symlink(origin, dest)

            # Registra se a imagem esta disponivel ou nao.
            availables.append(True)
            origins.append(origin)
            image_paths.append(dest)

            logging.debug(
                "Link Image. Origin [%s] to [%s]" % (origin, dest))
        else:
            availables.append(False)
            origins.append(None)
            image_paths.append(None)

            logging.warning("Image not found. [%s]" % origin)

    images['available'] = availables
    images['original_path'] = origins
    images['current_path'] = image_paths

    # Volta a utilizaro index pelo ID.
    images = images.set_index('id')
    return images


def remove_symlink_for_images():

    listOfFiles = os.listdir(os.getenv("DATA_DIR"))
    pattern = "*.fits"
    for filename in listOfFiles:
        if fnmatch.fnmatch(filename, pattern):
            try:
                os.unlink(os.path.join(os.getenv("DATA_DIR"), filename))
            except Exception as e:
                print(e)

    # for image in images:
    #     os.unlink(image)


def read_stars_catalog(filepath):
    df = pd.read_csv(
        filepath,
        delimiter=";",
        usecols=["ra", "dec", "ccd_id", ],
        dtype={
            'ccd_id': 'Int64',
        })
    return df


def get_stars_by_ccd(filepath, ccd_id):

    df = pd.read_csv(
        filepath,
        delimiter=";",
        usecols=["ra", "dec", "ccd_id", ],
        dtype={
            'ccd_id': 'Int64',
        })

    stars = df.loc[df['ccd_id'] == int(ccd_id)]

    return stars


def get_targets_by_ccd_id(filepath, ccd_id):

    df = pd.read_csv(
        filepath,
        header=None,
        delim_whitespace=True,
        usecols=[35, 36, 49],
        names=['ra', 'dec', 'ccd_image'],
        dtype={
            'ra': 'Float64',
            'dec': 'Float64',
        }
    )

    df['ra'] = df['ra'].apply(lambda x: x*15)

    df = df.loc[df['ccd_image'].str.contains(str(ccd_id)) == True]

    return df


def read_targets_offset(filepath):
    df = pd.read_csv(
        filepath,
        header=None,
        delim_whitespace=True,
        usecols=[35, 36, 49],
        names=['ra', 'dec', 'ccd_image'],
        dtype={
            'ra': 'Float64',
            'dec': 'Float64',
        }
    )

    df['ra'] = df['ra'].apply(lambda x: x*15)

    return df


def location_by_obs_code(obs_code):
    locations = dict({
        'W84': dict({
            'name': 'Cerro Tololo',
            'lon': +289.193583333,
            'lat': -30.16958333,
            'ele': 2202.7
        })
    })

    return locations[obs_code]


def check_bsp_jpl(asteroid_name):
    """
        Verifica se existe o arquivo BSP JPL.
    """
    bsp_jpl_filename = "%s.bsp" % asteroid_name
    bsp_jpl = os.path.join(os.getenv("DATA_DIR"), bsp_jpl_filename)
    if not os.path.exists(bsp_jpl):
        raise Exception("BSP JPL file not found. Filename: [%s]" % bsp_jpl)

    return bsp_jpl


def check_bsp_planetary():
    """
        Verifica se existe o arquivo BSP Planetary.
    """
    bsp_planets_filename = os.getenv("BSP_PLANETARY")
    bsp_planets = os.path.join(os.getenv("DATA_DIR"), bsp_planets_filename)
    if not os.path.exists(bsp_planets):
        # Se nao existir no data, criar link
        os.symlink(os.path.join(os.getenv("BSP_PLANETARY_PATH"),
                                bsp_planets_filename), bsp_planets)

    return bsp_planets


def check_leap_seconds():
    """
        Verifica se existe o arquivo de leap seconds
    """
    leap_sec_filename = os.getenv("LEAP_SENCOND")
    leap_sec = os.path.join(os.getenv("DATA_DIR"), leap_sec_filename)
    if not os.path.exists(leap_sec):
        # Se nao existir no data, criar link
        os.symlink(os.path.join(os.getenv("LEAP_SENCOND_PATH"),
                                leap_sec_filename), leap_sec)

    return leap_sec


def check_ccd_images_csv(ccd_images_filename):
    """
        Verifica se o arquivo csv, com os dados dos CCDs existe.
    """
    ccd_images_csv = os.path.join(
        os.getenv("DATA_DIR"), str(ccd_images_filename))

    if not os.path.exists(ccd_images_csv):
        raise Exception(
            "csv file with ccds information not found. Filename: [%s]" % ccd_images_csv)

    return ccd_images_csv
