# -----------------------------------------------------------------------------
# Seasonal statistics for ERA5 for EMCV figure

import os
import sys
import datetime
import pandas as pd

# -----------------------------------------------------------------------------

input_folder = "/media/david/civg04/ERA5/raw_netcdf/Monthly_NetCDF/T2M/"
output_folder = "/media/david/civg01/DP/Work/ECVs/ERA5/"

# -----------------------------------------------------------------------------

# Input files
input_files = sorted(os.listdir(input_folder))

# Select 2004-2018 period
input_path = output_folder + "ERA5_SeasonalMean_TimeSeries_01.nc"
output_path = output_folder + "ERA5_SeasonalMean_TimeSeries_2004-2018_01.nc"
cmd = "cdo -f nc selyear,2004/2018 " + input_path + " " + output_path
os.system(cmd)

# Seasonal statistics
input_path = output_folder + "ERA5_SeasonalMean_TimeSeries_2004-2018_01.nc"
output_path = output_folder + "ERA5_SeasonalMeans_2004-2018_01.nc"
cmd = "cdo -f nc -yseasmean " + input_path + " " + output_path
os.system(cmd)

# Unit conversion
input_path = output_folder + "ERA5_SeasonalMeans_2004-2018_01.nc"
output_path = output_folder + "ERA5_SeasonalMeans_2004-2018_degC_01.nc"
cmd = "cdo -f nc -subc,273.15 " + input_path + " " + output_path
os.system(cmd)












