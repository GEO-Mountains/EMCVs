# -----------------------------------------------------------------------------
# EMCV figure

library(plyr)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(cowplot)
library(rgeos)
library(rgdal)
library(viridis)
library(scales)
library(stringr)
library(pals)

# -----------------------------------------------------------------------------
# Inputs

har_path = "Z:/DP/Work/ECVs/HARv2/HARv2_Mean_JJA_01.csv"
era5_path = "Z:/DP/Work/ECVs/ERA5/ERA5_SeasonalMeans_degC_01.txt"
ghcnm_path = "Z:/GHCNM/Processed/Monthly/EMCV/001/GHCNM_Data_EMCV_01a.csv"
ghcnm_inventory_path = "Z:/GHCNM/Inventory/GHCNM_DataInventory_EMCV_01a.csv"
extractions_path = "Z:/DP/Work/ECVs/EMCV_PointExtractions_01.csv"
output_folder = "Z:/DP/Work/ECVs/"

continents_folder = "Z:/DP/GIS/Data/World/Land/ne_50m_land"
continents_shp_name = "ne_50m_land"

# -----------------------------------------------------------------------------
# Functions

save_plots <- function(output_fol, root_name, plot_ref, pwidth=6.5, pheight=6.0, 
                       dpi=600) {
  output_path <- paste0(output_fol, root_name, "_", dpi, "dpi.png")
  png(output_path, width=pwidth, height=pheight, units="in", res=dpi)
  plot(plot_ref)
  dev.off()
}

# -----------------------------------------------------------------------------
# Steps

# Prepare continents shapefile
continents = readOGR(dsn=path.expand(continents_folder), layer=continents_shp_name)
continents@data$id = rownames(continents@data)
continents.points = fortify(continents, region="id")
continents.df = join(continents.points, continents@data, by="id")

# Read GHCNM data
df = read.csv(ghcnm_path)

# Subset GHCNM and get JJA series
df = df[df$Year >= 1979 & df$Year <= 2018, ]
dfa = ddply(df[df$Month %in% seq(6,8), ], c("ID", "Year"), summarise,
            Count = length(Value),
            Value = mean(Value))
dfa = na.omit(dfa)

# Get JJA mean for 2004-2018 subset
dfb = ddply(dfa[dfa$Year>=2004, ], c("ID"), summarise,
            Count = length(Value),
            Value = mean(Value))
dfb = dfb[dfb$Count>=12, ]

# Get JJA mean for other stations with >= N years of data post-1979
dfc = dfa[!dfa$ID %in% unique(dfb$ID), ]
dfc = ddply(dfc, c("ID"), summarise,
            Count = length(Value),
            Value = mean(Value))
dfc = dfc[dfc$Count>=30, ]

# Combine
dfb$Period = "overlap"
dfc$Period = "all"
dfd = rbind(dfb, dfc)

# Read/merge inventory to get coordinates
df99 = read.csv(ghcnm_inventory_path)
dfd = merge(dfd, df99[, c("ID", "Latitude", "Longitude")], by=c("ID"), all.x=T)

# ---

# Find some example stations
id = "KG000036982"
id = "IN008010200"
##id = "CHM00055228"
id = "CHM00055578"
id = "CHM00056137"
p0a = ggplot(dfa[dfa$ID==id & dfa$Count==3, ], aes(x=Year, y=Value)) +
  geom_line() +
  geom_point() +
  theme_bw()
plot(p0a)

# Coordinates of example stations from inventory
subset(df99, ID %in% c("KG000036982", "IN008010200", "CHM00055578", "CHM00056137"))

# ---

# Read point extractions
df88 = read.csv(extractions_path)

# Process into anomaly series
df88a = gather(df88, ID, Value, CHM00055578:KG000036982)
df88b = ddply(df88a[df88a$Season==3, ], c("Source", "ID"), transform,
              Mean = mean(Value[Year>=2004 & Year<=2018]),
              SD = sd(Value[Year>=2004 & Year<=2018]))
df88b$SA = (df88b$Value - df88b$Mean) / df88b$SD

# Test plot
p88a = ggplot(df88b, aes(x=Year, y=SA, colour=Source)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ID, ncol=2) +
  theme_bw()
plot(p88a)

# SAs for obs
dfz = ddply(dfa[dfa$ID %in% c("KG000036982", "IN008010200", "CHM00055578", 
                              "CHM00056137"), ], c("ID"), transform,
            Mean = mean(Value[Year>=2004 & Year<=2018]),
            SD = sd(Value[Year>=2004 & Year<=2018]))
dfz$SA = (dfz$Value - dfz$Mean) / dfz$SD

# Ensure missing years are represented with NA
tmp = expand.grid(
  ID = unique(dfz$ID),
  Year = seq(1979,2018)
)
dfz = merge(tmp, dfz, by=c("ID", "Year"), all.x=T)

# Merge in obs
dfz$Source = "GHCNM"
df88b = df88b[, !names(df88b) %in% c("Season")]
dfz = dfz[, !names(dfz) %in% c("Count")]
df88c = rbind(df88b, dfz)

# Test plot
p88b = ggplot(df88c, aes(x=Year, y=SA, colour=Source)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ID, ncol=2) +
  theme_bw()
plot(p88b)

# ---

# Read HAR data
df1 = read.csv(har_path)
df1$id = rep(seq(length(df1$t2)/4), each=4)

# Ensure correct plotting
df1a = df1 %>%
  group_by(id) %>%
  mutate(lat2 = ifelse(lat==min(lat), lat*0.999, lat*1.001),
         lon2 = ifelse(lon==min(lon), lon*0.999, lon*1.001))
df1a = as.data.frame(df1a)

# Test plot
p1a = ggplot(df1a) +
  geom_polygon(aes(x=lon2, y=lat2, fill=t2+273.15, group=id)) +
  geom_polygon(aes(x=lon, y=lat, fill=t2+273.15, group=id)) +
  coord_equal() +
  #scale_fill_gradient2() +
  scale_fill_viridis(option="magma") + # magma or inferno? magma for now?
  theme_bw()
plot(p1a)

# ---

# Read ERA5 data
df2 = read.csv(era5_path, sep="\t")

# Test plot
p2a = ggplot(df2[df2$time==1038720, ]) +
  geom_tile(aes(x=longitude, y=latitude, fill=t2m+273.15)) +
  coord_equal(xlim=c(min(df1a$lon2), max(df1a$lon2)), 
              ylim=c(min(df1a$lat2), max(df1a$lat2))) +
  #scale_fill_gradient2() +
  scale_fill_viridis(limits=c(min(df1a$t2)+273.15, max(df1a$t2)+273.15), oob=squish,
                     option="magma") + # magma or inferno? magma for now?
  theme_bw()
plot(p2a)

# ---
# Combine dataframes so can plot as facets

# Dummy fields and renaming etc
df2a = df2[df2$time==1038720, !names(df2) %in% c("time")]
names(df2a)[names(df2a)=="latitude"] = "lat"
names(df2a)[names(df2a)=="longitude"] = "lon"
names(df2a)[names(df2a)=="t2m"] = "t2"
df2a$id = 1
df2a$lat2 = 1
df2a$lon2 = 1
df1a = df1a[, !names(df1a) %in% c("y", "x")]
df1a$source = "HARv2"
df2a$source = "ERA5"
df3 = rbind(df1a, df2a)
df3$t2 = df3$t2 + 273.15

# Labels for points
df99z = df99[df99$ID %in% c("KG000036982", "IN008010200", "CHM00055578", "CHM00056137"), ]
df99z$ID2[df99z$ID=="CHM00055578"] = "iii" # "c"
df99z$ID2[df99z$ID=="CHM00056137"] = "iv" # "d"
df99z$ID2[df99z$ID=="IN008010200"] = "ii" # "b"
df99z$ID2[df99z$ID=="KG000036982"] = "i" # "a"

# Plot
cols = c("overlap"="black", "all"="grey70")
shapes = c("overlap"=21, "all"=22)
lons = seq(60,110,10)
lats = c(30,40)
p3a = ggplot() +
  geom_tile(data=df3[df3$source=="ERA5", ], aes(x=lon, y=lat, fill=t2)) +
  geom_polygon(data=df3[df3$source=="HARv2", ], 
               aes(x=lon2, y=lat2, fill=t2, group=id)) +
  geom_polygon(data=df3[df3$source=="HARv2", ], 
               aes(x=lon, y=lat, fill=t2, group=id)) +
  geom_point(data=dfd, aes(x=Longitude, y=Latitude, fill=Value+273.15, shape=Period),
             size=1) +
  geom_label(data=df99z, aes(x=Longitude, y=Latitude, label=ID2), size=3,
             nudge_x=0.7, nudge_y=-0.7, label.padding=unit(0.1, "line")) +
  coord_equal(xlim=c(min(df1a$lon2), max(df1a$lon2)), 
              ylim=c(min(df1a$lat2), max(df1a$lat2))) +
  facet_wrap(~source, ncol=1) +
  scale_x_continuous(name="", breaks=lons,
                     labels=paste0(lons,'°W')) +
  scale_y_continuous(name="", breaks=lats,
                     labels=paste0(lats,'°N')) +
  scale_fill_viridis(name="Temperature (K)",
                     limits=c(min(df1a$t2)+273.15, max(df1a$t2)+273.15), oob=squish,
                     option="magma") + # magma or inferno? magma for now?
  #scale_colour_manual(values=cols) +
  scale_shape_manual(values=shapes, guide=F) +
  guides(fill = guide_colourbar(title.position="left", title.hjust = 0.5,
                                barwidth=0.6)) +
  theme_bw() +
  theme(legend.title=element_text(size=8, angle=90)) +
  theme(legend.text=element_text(size=8)) +
  #theme(legend.position="bottom") +
  theme(axis.title=element_text(size=9)) +
  theme(axis.text=element_text(size=8)) +
  theme(panel.grid.major=element_blank()) +
  theme(panel.grid.minor = element_blank()) +
  ##theme(plot.title=element_text(size=9)) +
  theme(strip.text=element_text(size=8)) +
  theme(strip.background=element_rect(colour=NA, fill=NA)) +
  theme(panel.border=element_rect(fill = NA, colour = "black"))
#plot(p3a)

# Save
save_plots(output_folder, 'test02a', p3a, pwidth=6.5, pheight=6)

# Adjust IDs for labelling
df88c$ID2 = NA
df88c$ID2[df88c$ID=="CHM00055578"] = "(iii) CHM00055578" # "c"
df88c$ID2[df88c$ID=="CHM00056137"] = "(iv) CHM00056137" # "d"
df88c$ID2[df88c$ID=="IN008010200"] = "(ii) IN008010200" # "b"
df88c$ID2[df88c$ID=="KG000036982"] = "(i) KG000036982" # "a"

# Anomaly series plots - upper
cols = c("GHCNM"="#000000", "ERA5"="#009E73", "HARv2"="#D55E00")
p4a = ggplot(df88c[df88c$ID %in% c("KG000036982", "IN008010200"), ], 
             aes(x=Year, y=SA, colour=Source)) +
  geom_hline(yintercept=0, colour="grey50", size=0.25) +
  geom_line(size=0.25) +
  geom_point(size=1) +
  facet_wrap(~ID2, ncol=2) +
  scale_x_continuous(name="") +
  scale_y_continuous(name="Standardised Anomaly", limits=c(-3.5,3.5)) +
  scale_colour_manual(name="", values=cols) +
  theme_bw() +
  theme(legend.title=element_text(size=8)) +
  theme(legend.text=element_text(size=8)) +
  #theme(legend.position="bottom") +
  ##theme(legend.position = c(0,1)) + 
  ##theme(legend.justification = c(0,1)) +
  theme(legend.background=element_blank()) +
  theme(legend.text=element_text(size=8)) +
  theme(legend.key=element_blank()) +
  theme(legend.key.size=unit(0.8, "line")) +
  theme(axis.title=element_text(size=9)) +
  theme(axis.text=element_text(size=8)) +
  ##theme(plot.title=element_text(size=9)) +
  theme(strip.text=element_text(size=8)) +
  theme(strip.background=element_rect(colour=NA, fill=NA)) +
  theme(panel.border=element_rect(fill = NA, colour = "black"))
plot(p4a)

# Anomaly series plots - lower
cols = c("GHCNM"="#000000", "ERA5"="#009E73", "HARv2"="#D55E00")
p4b = ggplot(df88c[!df88c$ID %in% c("KG000036982", "IN008010200"), ], 
             aes(x=Year, y=SA, colour=Source)) +
  geom_hline(yintercept=0, colour="grey50", size=0.25) +
  geom_line(size=0.25) +
  geom_point(size=1) +
  facet_wrap(~ID2, ncol=2) +
  scale_x_continuous(name="") +
  scale_y_continuous(name="Standardised Anomaly", limits=c(-3.5,3.5)) +
  scale_colour_manual(name="", values=cols) +
  theme_bw() +
  theme(legend.title=element_text(size=8)) +
  theme(legend.text=element_text(size=8)) +
  #theme(legend.position="bottom") +
  ##theme(legend.position = c(0,1)) + 
  ##theme(legend.justification = c(0,1)) +
  theme(legend.background=element_blank()) +
  theme(legend.text=element_text(size=8)) +
  theme(legend.key=element_blank()) +
  theme(legend.key.size=unit(0.8, "line")) +
  theme(axis.title=element_text(size=9)) +
  theme(axis.text=element_text(size=8)) +
  ##theme(plot.title=element_text(size=9)) +
  theme(strip.text=element_text(size=8)) +
  theme(strip.background=element_rect(colour=NA, fill=NA)) +
  theme(panel.border=element_rect(fill = NA, colour = "black"))
plot(p4b)

# Combine plots
p5a = plot_grid(p4a, NULL, p3a, NULL, p4b, nrow=5, labels=c("", "", "", "", ""), 
                align="hv", rel_heights=c(0.8,-0.15,3,-0.15,0.8)) # ,
#plot(p5a)
save_plots(output_folder, 'EMCV_Reanalysis_02', p5a, pwidth=6.5, pheight=9)








