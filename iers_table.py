#!/usr/bin/env python3
from astropy.utils import iers

# https://docs.astropy.org/en/stable/utils/iers.html#iers-data-access-astropy-utils-iers
# Download do Arquivo finals2000A.all

finalFile = iers.IERS_A_URL
iers.IERS.iers_table = iers.IERS_A.open(finalFile)