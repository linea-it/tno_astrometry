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
                 "rac1", "rac2", "rac3", "rac4", "decc1", "decc2", "decc3", "decc4", "filename"],
        dtype={
            'id': 'Int64',
        }
    )
    return df


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
    for i, image in images.iterrows():
        origin = os.path.join(os.getenv("IMAGES_PATH"), image['filename'])

        if os.path.exists(origin):
            filename = os.path.basename(origin)
            filename = "%s.fits" % str(image['id'])
            dest = os.path.join(os.getenv("DATA_DIR"), filename)
            os.symlink(origin, dest)

            images_list.append(dest)

    return images_list


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
