from astropy.utils import iers
from astropy.time import Time
from astropy.coordinates import GCRS, EarthLocation
import os
import subprocess

import numpy as np
import spiceypy as spice
# Deligar o auto Download do Finals2000
# https://docs.astropy.org/en/stable/utils/iers.html#working-offline
iers.conf.auto_download = False


praia_target_params = 'praia_target.dat'
targets_file = 'targets.txt'
targets_offset_file = 'target_offset.txt'


def findIDSPK(n, key):
    loc = 2  # order setting bsp files (1=DEXXX.bsp, 2=Ast.bsp)
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
    style = '{:02d} {:02d} {:0' + \
        str(ndecimals+3) + '.' + str(ndecimals) + 'f}'
    RA = style.format(raH, raM, raS)
    return RA


def dec2DMS(decdeg='', ndecimals=0):
    ds = '+'
    if decdeg < 0:
        ds, decdeg = '-', abs(decdeg)
    deg = int(decdeg)
    decM = abs(int((decdeg - deg)*60))
    decS = 60*(abs((decdeg - deg)*60) - decM)
    style = '{}{:02d} {:02d} {:0' + \
        str(ndecimals+3) + '.' + str(ndecimals) + 'f}'
    DEC = style.format(ds, deg, decM, decS)
    return DEC


def geoTopoVector(longitude, latitude, elevation, jd):

    loc = EarthLocation(longitude, latitude, elevation)

    time = Time(jd, scale='utc', format='jd')
    itrs = loc.get_itrs(obstime=time)
    gcrs = itrs.transform_to(GCRS(obstime=time))

    r = gcrs.cartesian

    # convert from m to km
    x = r.x.value/1000.0
    y = r.y.value/1000.0
    z = r.z.value/1000.0

    return x, y, z


def create_targets_file(name, dates, bsp_object, bsp_planets, leap_Sec, location):
    # Load the asteroid and planetary ephemeris and the leap second (in order)
    spice.furnsh(bsp_planets)
    spice.furnsh(leap_Sec)
    spice.furnsh(bsp_object)

    # Values specific for extract all comments of header from bsp files (JPL, NIMA)
    source = {'NIMA': (45, 'ASTEROID_SPK_ID ='),
              'JPL': (74, 'Target SPK ID   :')}
    n, key = source['NIMA']
    idspk = findIDSPK(n, key)
    if idspk == '':
        n, key = source['JPL']
        idspk = findIDSPK(n, key)

    # Convert dates from JD to et format. "JD" is added due to spice requirement
    datesET = [spice.utc2et(jd + " JD") for jd in dates]

    # Compute geocentric positions (x,y,z) for each date with light time correction
    rAst, ltAst = spice.spkpos(idspk, datesET, 'J2000', 'LT', 'EARTH')

    # Location
    lon, lat, ele = location[0], location[1], location[2]

    # Create targets file
    output = os.path.join(os.getenv("DATA_DIR"), targets_file)
    with open(output, 'w') as outFile:
        for i, r_geo in enumerate(rAst):
            # Convert from longitude, latitude, elevation to r(x,y,z)
            r = geoTopoVector(lon, lat, ele, float(dates[i]))

            #r_topo = r_geo - r
            r_topo = [r_geo[0]-r[0], r_geo[1]-r[1], r_geo[2]-r[2]]

            # Convert rectangular coordinates (x,y,z) to range, right ascension, and declination.
            d, rarad, decrad = spice.recrad(r_topo)

            # Transform RA and Decl. from radians to degrees and then to hexadecimal format.
            ra = ra2HMS(np.degrees(rarad)/15.0, 6)
            dec = dec2DMS(np.degrees(decrad), 5)

            # Save parameters in specific format
            outFile.write(" " + ra + " " + dec + " " +
                          dates[i] + " " + name + "\n")

    outFile.close()

    return output


def create_params_file(praia_astrometry_output, targets, targets_offset):

    with open(os.path.join(os.getenv("APP_PATH"), "src/praia_target.template.dat")) as template:

        data = template.read()
        data = data.replace('{PRAIA_ASTROMETRY_OUTPUT}',
                            praia_astrometry_output.ljust(50))
        data = data.replace('{TARGET_FILE}', targets.ljust(50))
        data = data.replace('{TARGET_OFFSET_FILE}', targets_offset.ljust(50))

        params_file = os.path.join(os.getenv("DATA_DIR"), praia_target_params)
        with open(params_file, 'w') as new_file:
            new_file.write(data)
        new_file.close()

    template.close()

    return params_file


def run_praia_target(praia_astrometry_output, targets):

    targets_offset = os.path.join(os.getenv("DATA_DIR"), targets_offset_file)

    params_file = create_params_file(
        praia_astrometry_output, targets, targets_offset)

    praia_target = os.getenv("PRAIA_TARGET")

    exec_log = os.path.join(os.getenv("DATA_DIR"), "praia_target.log")

    with open(exec_log, 'w') as fp:
        process = subprocess.Popen(["%s < %s" % (praia_target, params_file)],
                                   stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE, shell=True)

        out, error = process.communicate()

        fp.write(out.decode("utf-8"))
        fp.write(error.decode("utf-8"))

        if process.returncode > 0:
            raise Exception("Failed to run PRAIA Target. \n" +
                            error.decode("utf-8"))

    return targets_offset


def create_obs_file(targets_offset, asteroid, obs_code, cat_code):
    mag, ra, dec, jd = np.loadtxt(targets_offset, usecols=(
        10, 35, 36, 43), ndmin=2, unpack=True)

    raHMS = [ra2HMS(alpha) for alpha in ra]
    decDMS = [dec2DMS(delta) for delta in dec]

    output = os.path.join(os.getenv("DATA_DIR"), "%s.txt" % asteroid)

    with open(output, 'w') as outFile:
        for i in range(len(mag)):
            outFile.write(ra2HMS(ra[i], 4) + "  ")
            outFile.write(dec2DMS(dec[i], 3) + "   ")
            outFile.write('{:06.3f}'.format(mag[i]) + "  ")
            outFile.write('{:016.8f}'.format(jd[i]) + "  ")
            outFile.write(obs_code + "  ")
            outFile.write(cat_code + "\n")

    outFile.close()

    return output
