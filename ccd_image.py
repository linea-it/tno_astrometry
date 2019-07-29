import numpy as np

def read_ccd_image_csv(filepath):
    return np.genfromtxt(filepath, dtype=None, delimiter=';', names=True, encoding='utf8')


def get_image_by_id(ccd_images, image_id):
    # rows = read_ccd_image_csv(filepath)
    for row in ccd_images:
        if int(row['id']) == int(image_id):
            return row
    
