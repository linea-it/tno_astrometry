import os

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

def plotStarsCCD(asteroid, ccd, stars, targets, output):
    try:
        # Stars [0] RA [1] Dec [2] CCD id
        ccdx = []
        ccdy = []
        starx = []
        stary = []        
        tra = []
        tdec = []

        if ccd is not None:
            ccdx = [ccd['rac1'], ccd['rac2'], ccd['rac3'], ccd['rac4'], ccd['rac1']]
            ccdy = [ccd['decc1'], ccd['decc2'],
                ccd['decc3'], ccd['decc4'], ccd['decc1']]

        if len(stars) > 0:
            starx = stars['ra']
            stary = stars['dec']

        if len(targets) > 0:
            tra = targets['ra']
            tdec = targets['dec']
 

        plt.figure()
        # Plot CCD
        plt.plot(ccdx, ccdy, 'k', color='#a3a3c2')
        # Plot Stars
        plt.plot(starx, stary, '.', color='#7575a3', label='Stars positions')
        # Plot Target
        plt.plot(tra, tdec, '.', color='#ff3300', label='%s positions' % asteroid)

        # Plot Labels
        plt.title('CCD %s' % ccd['id'])
        plt.xlabel('RA (deg)')
        plt.ylabel('Dec (deg)')
        plt.legend()

        plt.axes().set_aspect('equal', 'datalim')
        plt.savefig(output)
        plt.close()

        return output

    except Exception as e:
        raise
