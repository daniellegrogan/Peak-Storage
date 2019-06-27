# Analysis steps for Peak Storage
# Project: NASA HiMAT
# Danielle S Grogan
# last updated: 2019-06-27

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
files.sources = list.files("src/functions", full.names = T)
sapply(files.sources, source)
rm(wbm_load.script, spatial_aggregation.script, mouth_ts.script, file.sources)  # remove unnecesary variables

### Data within this Project
# River basin shapefile
basin.shape = readOGR("data/basins_hma", "basins_hma")

### File paths to data outside Project
wbm.path = "/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/Frontiers/ERA_hist/yearly"
gl.path  =  "/net/nfs/merrimack/raid2/data/glaciers_6.0/HiMAT_full_210_Subset"


### MAIN ###
# for each step below, produce:
# i.  Time series per basin
# ii. Map (grid cells)  # Not yet coded

# 1. Total percolation of glacier runoff into groundwater system
#    a. in mm/year: 
#       time series (1980 - 2099) and 
#       mean annual per climatology (1980 - 2009, 2010 - 2039, 2040 - 2069, 2070 - 2099)
#    b. as % of glacier runoff:
#       time series (1980 - 2099) and 
#       mean annual per climatology (1980 - 2009, 2010 - 2039, 2040 - 2069, 2070 - 2099)

glacier_percolation(wbm.path = wbm.path,         # path to wbm files. for yearly files, stop after "/yearly"
                    shape    = basin.shape,      # shapefile for basin aggregation
                    gl.path  = gl.path,          # path to glacier runoff files
                    gcm      = NA,               # GCM model name, if analyzing future RCP
                    rcp      = 'historical',     # one of: 'histroical', 'rcp45', 'rcp85'
                    years    = seq(2000, 2003),  # years for analysis
                    out.path = "results")        # path to save all output

# 2. Irrigation water supplied by glacier runoff through groundwater withdrawals
#    a. in mm/year: 
#       time series (1980 - 2099) and 
#       mean annual per climatology (1980 - 2009, 2010 - 2039, 2040 - 2069, 2070 - 2099)
#    b. as % of Gross Irrigation:
#       time series (1980 - 2099) and 
#       mean annual per climatology (1980 - 2009, 2010 - 2039, 2040 - 2069, 2070 - 2099)
#    c. as % of irrigation from groundwater sources:
#       time series (1980 - 2099) and 
#       mean annual per climatology (1980 - 2009, 2010 - 2039, 2040 - 2069, 2070 - 2099)

# 3. Glacier runoff exported to ocean
#    a. in mm/year: 
#       time series (1980 - 2099) and 
#       mean annual per climatology (1980 - 2009, 2010 - 2039, 2040 - 2069, 2070 - 2099)
#    b. as % of glacier runoff:
#       time series (1980 - 2099) and 
#       mean annual per climatology (1980 - 2009, 2010 - 2039, 2040 - 2069, 2070 - 2099)

# 4. Glacier runoff --> crop ET
#    a. in mm/year: 
#       time series (1980 - 2099) and 
#       mean annual per climatology (1980 - 2009, 2010 - 2039, 2040 - 2069, 2070 - 2099)
#    b. as % of glacier runoff:
#       time series (1980 - 2099) and 
#       mean annual per climatology (1980 - 2009, 2010 - 2039, 2040 - 2069, 2070 - 2099)


