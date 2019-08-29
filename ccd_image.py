import numpy as np
import pandas as pd

def read_ccd_image_csv(filepath):
    return np.genfromtxt(filepath, dtype=None, delimiter=';', names=True, encoding='utf8')


def get_image_by_id(ccd_images, image_id):
    # rows = read_ccd_image_csv(filepath)
    for row in ccd_images:
        if int(row['id']) == int(image_id):
            return row

def get_ccd_images_from_csv(filepath):
    df = pd.read_csv(
        filepath,
        delimiter=";",
        usecols=["id", "expnum", "ccdnum", "band", "ra_cent", "dec_cent",
                "rac1", "rac2", "rac3", "rac4", "decc1", "decc2", "decc3", "decc4", ],
        dtype={
            'id': 'Int64',
        }
    )
    return df

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
        usecols=[35,36,49],
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
        usecols=[35,36,49],
        names=['ra', 'dec', 'ccd_image'],
        dtype={
            'ra': 'Float64',
            'dec': 'Float64',
        }        
    )

    df['ra'] = df['ra'].apply(lambda x: x*15)

    return df