# -----------------------------------------------------------------------------
# Seasonal statistics for HARv2 for EMCV figure

import os
import sys
import datetime
import pandas as pd

# -----------------------------------------------------------------------------

input_folder = "/media/david/civg01/DP/Data/Raw/Reanalysis/HARv2/201211/t2/"
output_folder = "/media/david/civg01/DP/Work/ECVs/HARv2/"

# -----------------------------------------------------------------------------

# Input files
input_files = sorted(os.listdir(input_folder))

# Seasonal time series
os.chdir(input_folder)
output_path = output_folder + "HARv2_SeasonalMean_TimeSeries_01.nc"
cmd = "cdo -f nc -b 64 seasmean -cat " + "'" + "HARv2_d10km_m_2d_t2_*.nc" + "'" + " " + output_path
os.system(cmd)

# Seasonal statistics
input_path = output_folder + "HARv2_SeasonalMean_TimeSeries_01.nc"
output_path = output_folder + "HARv2_SeasonalMeans_01.nc"
cmd = "cdo -f nc -yseasmean " + input_path + " " + output_path
os.system(cmd)

# Unit conversion
input_path = output_folder + "HARv2_SeasonalMeans_01.nc"
output_path = output_folder + "HARv2_SeasonalMeans_degC_01.nc"
cmd = "cdo -f nc -subc,273.15 " + input_path + " " + output_path
os.system(cmd)














