import subprocess
import os
import spiceypy as spice
import numpy as np

praia_target_params = 'praia_target.dat'

targets_file = 'targets.txt'
targets_offset_file = 'target_offset.txt'


def findIDSPK(n, key):
    loc = 2 #order setting bsp files (1=DEXXX.bsp, 2=Ast.bsp)
    m, header, flag = spice.dafec(loc, n)
    spk = ''
    for row in header:
        if row[:len(key)] == key:
            spk = row[len(key):].strip()
    return spk


def ra2HMS(radeg='', ndecimals=0):
    raH = int(radeg)
    raM = int((radeg - raH)*60)
    raS = 60*((radeg - raH)*60 - raM)
    style = '{:02d} {:02d} {:0' + str(ndecimals+3) + '.'+ str(ndecimals) + 'f}'
    RA = style.format(raH, raM, raS)
    return RA


def dec2DMS(decdeg='', ndecimals=0):
    ds = '+'
    if decdeg < 0:
        ds, decdeg = '-', abs(decdeg)
    deg = int(decdeg)
    decM = abs(int((decdeg - deg)*60))
    decS = 60*(abs((decdeg - deg)*60) - decM)
    style = '{}{:02d} {:02d} {:0' + str(ndecimals+3) + '.'+ str(ndecimals) + 'f}'
    DEC = style.format(ds, deg, decM, decS)
    return DEC


def create_targets_file(name, dates, bsp_object, bsp_planets, leap_Sec):
    #Load the asteroid and planetary ephemeris and the leap second (in order)
    spice.furnsh(bsp_planets)
    spice.furnsh(leap_Sec)
    spice.furnsh(bsp_object)

    #Values specific for extract all comments of header from bsp files (JPL, NIMA)
    source = {'NIMA':(45, 'ASTEROID_SPK_ID ='), 'JPL':(74, 'Target SPK ID   :')}
    n, key = source['NIMA']
    idspk = findIDSPK(n, key)
    if idspk == '':
        n, key = source['JPL']
        idspk = findIDSPK(n, key)

    #Convert dates from JD to et format. "JD" is added due to spice requirement    
    datesET = [spice.utc2et(jd + " JD") for jd in dates]

    #Compute geocentric positions (x,y,z) for each date with light time correction
    rAst, ltAst = spice.spkpos(idspk, datesET, 'J2000', 'LT', 'EARTH')

    #Create targets file
    output = os.path.join(os.getenv("DATA_DIR"), targets_file)
    with open(output, 'w') as outFile:
        for i, r in enumerate(rAst):
            #Convert rectangular coordinates (x,y,z) to range, right ascension, and declination.
            d, rarad, decrad = spice.recrad(r)

            #Transform RA and Decl. from radians to degrees and then to hexadecimal format.
            ra = ra2HMS(np.degrees(rarad)/15.0, 6)
            dec = dec2DMS(np.degrees(decrad), 5)

            #Save parameters in specific format
            outFile.write(" " + ra + " " + dec + " " + dates[i] + " " + name + "\n")

    outFile.close()

    return output