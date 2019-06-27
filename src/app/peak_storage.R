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
rm(wbm_load.script, spatial_aggregation.script, mouth_ts.script)

### File paths to data
wbm.path = file.path(base.path, "Current_projects/HiMAT/wbm/Frontiers/ERA_hist/yearly/")
gl.path  = file.path(base.path, "Current_projects/AGU_2018/data") 
out.path = file.path(base.path, "Current_projects/HiMAT/wbm/Analysis_2019-06")

# River basin shapefile
basin.shape = readOGR("data/basins_hma", "basins_hma")

irr.vars = c("GrossIrr_pg", 
             "IrrGrwt_pg",
             "IrrFlow_pg")

# for each step below, produce:
# i.  Time series per basin
# ii. Map (grid cells)

# 1. Total percolation of glacier runoff into groundwater system
#    a. in mm/year: 
#       time series (1980 - 2099) and 
#       mean annual per climatology (1980 - 2009, 2010 - 2039, 2040 - 2069, 2070 - 2099)
#    b. as % of glacier runoff:
#       time series (1980 - 2099) and 
#       mean annual per climatology (1980 - 2009, 2010 - 2039, 2040 - 2069, 2070 - 2099)

# 2. Irrigation water supplied by glacier runoff by way of groundwater withdrawals
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


