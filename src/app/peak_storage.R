# Analysis steps for Peak Storage
# Project: NASA HiMAT
# Danielle S Grogan
# last updated: 2019-07-01

### R Libraries
library(RCurl)  # enables sourcing R code from github
library(raster)
library(rgeos)
library(rgdal)

### Source functions from other github repos:
# wbm_load()
wbm_load.script = getURL("https://raw.githubusercontent.com/daniellegrogan/WBMr/master/wbm_load.R", ssl.verifypeer=F)
eval(parse(text=wbm_load.script))

# spatial_aggregation()
spatial_aggregation.script = getURL("https://raw.githubusercontent.com/daniellegrogan/WBMr/master/spatial_aggregation.R", ssl.verifypeer=F)
eval(parse(text=spatial_aggregation.script))

# mouth_ts()
mouth_ts.script = getURL("https://raw.githubusercontent.com/daniellegrogan/WBMr/master/mouth_ts.R", ssl.verifypeer=F)
eval(parse(text=mouth_ts.script))

### Source functions from within this project:
file.sources = list.files("src/functions", full.names = T)
sapply(files.sources, source)
rm(wbm_load.script, spatial_aggregation.script, mouth_ts.script, file.sources)  # remove unnecesary variables

### Data within this Project
# River basin shapefile
basin.shape = readOGR("data/basins_hma", "basins_hma")

### File paths to data outside R Project
wbm.path   = "/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/Frontiers/ERA_hist/yearly"
gl.path    =  "/net/nfs/merrimack/raid2/data/glaciers_6.0/HiMAT_full_210_Subset"
netwk.path = "/net/nfs/zero/data3/WBM_TrANS/data"

# years to analyze
years = seq(2000, 2003)

### MAIN ###
# for each step below, produce:
# i.  Time series per basin
# ii. Map (grid cells)  # Not yet coded


# 0. Calculate glacier runoff by basin (km3/year): required input for analysis below
# glacier runoff: output in m3
glacier.runoff = glacier_runoff_subset(gl.path = gl.path,          # path to glacier model output
                                       model   = gcm,              # If rcp != historical, also supply a GCM model name
                                       rcp     = rcp,              # rcp = one of: "historical", "rcp45", "rcp85"
                                       st.yr   = min(years),       # start year to subset
                                       end.yr  = max(years),       # end year to subset
                                       out.yr = 1)                 # 0 = output monthly.  1 for yearly

glacier.runoff.mm = glacier_runoff_m3_to_mm(glacier.runoff, 
                                            out.path = "results", 
                                            out.nm   = "Glacier_runoff_mmYr.nc",
                                            overwrite = T)

# spatial aggregation of glacier runoff: km3 per basin
glacier.runoff.basins = spatial_aggregation(raster.data = glacier.runoff.mm,
                                            shapefile   = shape,
                                            s           = 1, 
                                            cell.area   = 1,
                                            weight      = T, 
                                            poly.out    = F)
colnames(glacier.runoff.basins) = years
rownames(glacier.runoff.basins) = shape$name
write.csv(glacier.runoff.basins, "results/Glacier_runoff_basins_km3Yr.csv")

# 1. Total percolation of glacier runoff into groundwater system
#    a. in mm/year: 
#       time series (1980 - 2099) and 
#    b. as % of glacier runoff:
#       time series (1980 - 2099) and 

glacier_percolation(wbm.path = wbm.path,                           # path to wbm files. for yearly files, stop after "/yearly"
                    shape    = basin.shape,                        # shapefile for basin aggregation
                    glacier.runoff.basins = glacier.runoff.basins, # glacier runoff: km3/year per basin
                    years    = years,                              # years for analysis
                    out.path = "results")                          # path to save all output

# 2. Irrigation water supplied by glacier runoff through groundwater withdrawals
#    a. in mm/year: 
#       time series (1980 - 2099) and 
#    b. as % of Gross Irrigation:
#       time series (1980 - 2099) and 
#    c. as % of irrigation from groundwater sources:
#       time series (1980 - 2099) and 


glacier_gw_irr.R(wbm.path = wbm.path,      # path to wbm files. for yearly files, stop after "/yearly"
                 shape    = basin.shape,   # shapefile for basin aggregation
                 years    = years,         # years for analysis
                 out.path = "results")     # path to save all output
  
# 3. Glacier runoff exported to ocean
#    a. in km3/year 
#       time series (1980 - 2099) and 
#    b. as % of glacier runoff:
#       time series (1980 - 2099) and 

# inputs needed to identify basin mouths
basin.ID = raster(file.path(netwk.path, "HiMAT_full_210_IDs_Subset.asc"))
up.area  = raster(file.path(netwk.path,"flowdirection210_upstrArea.asc"))

# find IDs for exorheic basins
ex.basins = basin.shape[basin.shape$name == "Ganges" |
                          basin.shape$name == "Mekong" |
                          basin.shape$name == "Irawaddy" |
                          basin.shape$name == "Luni_ext" |
                          basin.shape$name == "Indus" |
                          basin.shape$name == "Brahmaputra" |
                          basin.shape$name == "Salween" |
                          basin.shape$name == "Yangtze" |
                          basin.shape$name == "Yellow",]

glacier_to_ocean(wbm.path   = wbm.path,              # path to wbm output. for yearly files, stop after "/yearly"
                 glacier.runoff.basins = glacier.runoff.basins,
                 years      = years,                 # years for analysis
                 basin.ID   = basin.ID,              # basinID file associated with WBM river network
                 basin.list = ex.basins$Basin_ID,    # list of basin IDs from which to extract river mouth data. Use all IDs in basinID file if NA
                 basin.nm   = ex.basins$name,        # list of basin names that match IDs
                 up.area    = up.area,               # upstream area file associated with WBM river network
                 shape      = basin.shape,           # shapefile for basin aggregation
                 out.path   = "results")             # path to save all output
  
# 4. Glacier runoff --> crop ET
#    a. in mm/year: 
#       time series (1980 - 2099) and 
#    b. as % of glacier runoff:
#       time series (1980 - 2099) and 

glacier_to_cropET(wbm.path              = wbm.path,               # path to wbm output. for yearly files, stop after "/yearly"
                  glacier.runoff.basins = glacier.runoff.basins,  # glacier runoff: km3/year per basin
                  years                  = years,                 # years for analysis
                  shape                  = basin.shape,           # shapefile to aggregate basins
                  out.path               = "results")             # path to save all output
