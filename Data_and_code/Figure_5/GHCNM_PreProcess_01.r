# -------------------------------------------------------------------------------------------
# GHCNM pre-processing - monthly files per station and inventory

library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)
library(readr)

# -------------------------------------------------------------------------------------------
# INPUTS

temp_folder = "Z:/GHCNM/Raw/V4/201210/Extracted/"
tavg_sta_path = paste0(temp_folder, "ghcnm.tavg.v4.0.1.20201209.qcf.inv")
tavg_path = paste0(temp_folder, "ghcnm.tavg.v4.0.1.20201209.qcf.dat")

# Temperature columns
temp_colwidths = c(11, 4, 4, rep(c(5,1,1,1), 12))
temp_colnames = c("ID", "Year", "Variable")
for(m in seq(12)) {
  temp_colnames <- c(temp_colnames, paste0("V", m), paste0("M", m), paste0("Q", m),
                      paste0("S", m))
}

# Outputs
output_monthly_folder = "Z:/GHCNM/Processed/Monthly/EMCV/001/"
output_inventory_path = "Z:/GHCNM/Inventory/GHCNM_DataInventory_EMCV_01.csv"
output_data_path = paste0(output_monthly_folder, "GHCNM_Data_EMCV_01.csv") 
domain_limits = c(50.0, 120.0, 10.0, 60.0) # xmin, xmax, ymin, ymax

# -------------------------------------------------------------------------------------------
# FUNCTIONS

# Read/format temperature data
fun1 = function(temp_path, temp_sta_path) {
  
  # Read data
  df2a = read_fwf(temp_path, fwf_widths(temp_colwidths, temp_colnames),
                  col_types = cols(.default = col_character()))
  df2a = as.data.frame(df2a)
  
  # Read station metadata
  # - no alternative elevation in V4 metadata
  df2b = read_fwf(temp_sta_path, 
                  fwf_widths(c(11,9,10,7,31), 
                             c("ID", "Latitude", "Longitude", "Elevation", "Name")),
                  col_types = cols(.default = col_character()))
  df2b = as.data.frame(df2b)
  df2b$Latitude = as.numeric(df2b$Latitude)
  df2b$Longitude = as.numeric(df2b$Longitude)
  df2b$Elevation = as.numeric(df2b$Elevation)
  
  # Filter on stations inside domain
  df2b$DomainFlag = ifelse(df2b$Longitude >= domain_limits[1] &
                             df2b$Longitude <= domain_limits[2] &
                             df2b$Latitude >= domain_limits[3] &
                             df2b$Latitude <= domain_limits[4], 1, 0)
  df2c = df2b[df2b$DomainFlag==1, !names(df2b) %in% c("DomainFlag")]
  
  # Subset data on stations in domain
  df2d = df2a[df2a$ID %in% unique(df2c$ID), ]
  
  # Format df
  tmp = c("ID", "Year", "Variable")
  for(i in seq(12)) {
    tmp = c(tmp, paste0("V", i))
  }
  df2d = df2d[, tmp]
  df2d = gather(df2d, Month, Value, V1:V12)
  df2d$Month2 = sapply(df2d$Month, function(x) substr(x, 2, 2+1))
  df2d = df2d[, !names(df2d) %in% c("Month")]
  names(df2d)[names(df2d)=="Month2"] = "Month"
  df2d$Year = as.integer(df2d$Year)
  df2d$Month = as.integer(df2d$Month)
  df2d$Value = as.numeric(df2d$Value)
  df2d$Value[df2d$Value==-9999] = NA
  df2d$Value = df2d$Value / 100
  
  return(df2d)
  
}

# Read temperature metadata
fun2 = function(temp_sta_path) {
  
  # Read station metadata
  # - no alternative elevation in V4 metadata
  df2b = read_fwf(temp_sta_path, 
                  fwf_widths(c(11,9,10,7,31), 
                             c("ID", "Latitude", "Longitude", "Elevation", "Name")),
                  col_types = cols(.default = col_character()))
  df2b = as.data.frame(df2b)
  df2b$Latitude = as.numeric(df2b$Latitude)
  df2b$Longitude = as.numeric(df2b$Longitude)
  df2b$Elevation = as.numeric(df2b$Elevation)
  
  # Filter on stations inside domain
  df2b$DomainFlag = ifelse(df2b$Longitude >= domain_limits[1] &
                             df2b$Longitude <= domain_limits[2] &
                             df2b$Latitude >= domain_limits[3] &
                             df2b$Latitude <= domain_limits[4], 1, 0)
  df2c = df2b[df2b$DomainFlag==1, !names(df2b) %in% c("DomainFlag")]
  
  return(df2c)
  
}

# -------------------------------------------------------------------------------------------
# STEPS

# Read/format data
df2a = fun1(tavg_path, tavg_sta_path)
gc()

# Order
df4 = df2a[with(df2a, order(ID, Year, Month)), c("ID", "Year", "Month", "Value")]
gc()

# ---
# Inventory
# - count non-na months in relevant time windows (can divide by 12 to give nyears)
# - relax "complete individual years" rule for now

# Summarise annual data availability
df5 = df4 %>%
  group_by(ID) %>%
  summarise(NYears_Post1979 = length(Year[!is.na(Value) & Year>=1979]) / 12,
            NYears_All = length(Year[!is.na(Value)]) / 12)
df5 = as.data.frame(df5)
gc()

# Summarise summer (JJA) data availability
df5z1 = df4[df4$Month %in% seq(6,8), ] %>%
  group_by(ID) %>%
  summarise(NYears_Post1979 = length(Year[!is.na(Value) & Year>=1979]) / 3,
            NYears_All = length(Year[!is.na(Value)]) / 3)
df5z1 = as.data.frame(df5)
gc()

# Summarise winter (DJF) data availability
df5z2 = df4[df4$Month %in% c(12,1,2), ] %>%
  group_by(ID) %>%
  summarise(NYears_Post1979 = length(Year[!is.na(Value) & Year>=1979]) / 3,
            NYears_All = length(Year[!is.na(Value)]) / 3)
df5z2 = as.data.frame(df5)
gc()

# Combine annual and seasonal
#df5$TimeAgg = "Annual"
#df5z1$TimeAgg = "JJA"
#df5z2$TimeAgg = "DJF"
#df5 = rbind(df5, df5z1)
#df5 = rbind(df5, df5z2)

# Join metadata
# - lat/lon, elevation, name
df6a = fun2(tavg_sta_path)
df5b1 = merge(df5, df6a, by=c("ID"), all.x=T)
any(is.na(df5b1$Latitude))
any(is.na(df5b1$Longitude))
any(is.na(df5b1$Name))
unique(df5b1$ID[is.na(df5b1$Latitude)])

# Tidy order
df5c = df5b1
df5c$Source = "GHCNM"
df5c = df5c[with(df5c, order(ID)),
            c("ID", "Latitude", "Longitude", "Elevation", "Name", #"TimeAgg", 
              "NYears_Post1979", "NYears_All")]

# Save inventory
write.csv(x=df5c, file=output_inventory_path, row.names=F, quote=F)

# ---
# Save master dataframe

# Subset on IDs included in inventory
df7 = df4
df7 = df7[df7$ID %in% unique(df5c$ID), ]

# Save
write.csv(x=df7, file=output_data_path, row.names=F, quote=F)







