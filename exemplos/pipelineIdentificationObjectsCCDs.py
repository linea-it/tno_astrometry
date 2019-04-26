from datetime import datetime
import spiceypy as spice
from glob import glob
import numpy as np
import os


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


def createTargetsFile(name, dates, bsp, dexxx, leapSec, nameFile):
    #Load the asteroid and planetary ephemeris and the leap second (in order)
    spice.furnsh(dexxx)
    spice.furnsh(leapSec)
    spice.furnsh(bsp)

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
    outFile = open(nameFile, 'w')

    for i, r in enumerate(rAst):
        #Convert rectangular coordinates (x,y,z) to range, right ascension, and declination.
        d, rarad, decrad = spice.recrad(r)

        #Transform RA and Decl. from radians to degrees and then to hexadecimal format.
        ra = ra2HMS(np.degrees(rarad)/15.0, 6)
        dec = dec2DMS(np.degrees(decrad), 5)

        #Save parameters in specific format
        outFile.write(" " + ra + " " + dec + " " + dates[i] + " " + name + "\n")

    outFile.close()


#Function to set some input parameters to PRAIA_target_search.dat file
def setParameters2PRAIAdat(parameters, inputOutputFile):
    inoutFile = open(inputOutputFile, "r+")
    contents = inoutFile.readlines()
    
    for i in range(len(parameters)):
        newlinei = parameters[i].ljust(49) + contents[i][49:]
        if contents[i] != newlinei:
            contents[i] = newlinei
    
    inoutFile.seek(0)
    inoutFile.writelines(contents)
  
    inoutFile.close()


def createObsFile(inFileName, outFileName):
    mag, ra, dec, jd = np.loadtxt(inFileName, usecols=(10,35,36, 43), unpack=True)

    raHMS = [ra2HMS(alpha) for alpha in ra]
    decDMS = [dec2DMS(delta) for delta in dec]

    outFile = open(outFileName, "w")
    
    for i in range(len(mag)):
        outFile.write(ra2HMS(ra[i], 4) + "  ")
        outFile.write(dec2DMS(dec[i], 3) + "   ")
        outFile.write('{:06.3f}'.format(mag[i]) + "  ")
        outFile.write('{:016.8f}'.format(jd[i]) + "  ")
        outFile.write(obsCode + "  ")
        outFile.write(catCode + "\n")     

    outFile.close()



if __name__ == "__main__":

    #============================================ INPUT ===========================================
    name = "Eris"

    bspObject = "bspFiles/Eris.bsp"
    bspPlanets = "bspFiles/de435.bsp"
    leapSec = "naif0012.tls"
    
    inFileName = "output"
    outFile = 'targets'

    praiaDat = "PRAIA_targets_search_20_03.dat"

    sourceType = "1"
    nameListXY = "list1.xy"
    nameOffsetFile = "offset_target2"
    errorRadius = "01.5000"
    praiaExe = "./PRAIA_targets_search_20_03"

    obsCode = "W84"
    catCode = "V"
    outFileName = "Eris.txt"

    keyXYfiles = "filesXY/*.gaia1.rad.xy"
    #==============================================================================================

    t0 = datetime.now()

    dates_jd = np.loadtxt(inFileName, dtype=str, usecols=(13,))

    createTargetsFile(name, dates_jd, bspObject, bspPlanets, leapSec, outFile)

    listXY = sorted(glob(keyXYfiles))
    np.savetxt(nameListXY, listXY, fmt='%s')

    parameters = [sourceType, nameListXY, outFile, nameOffsetFile, errorRadius]
    setParameters2PRAIAdat(parameters, praiaDat)

    os.system(praiaExe + " < " + praiaDat + " > " + name + ".log")

    createObsFile(nameOffsetFile, outFileName)

    tf = datetime.now()

    print "Duration: ", tf - t0

