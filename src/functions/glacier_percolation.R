# glacier_percolation.R
# Function calculates and saves to file:
#  Total percolation of glacier runoff into groundwater 
#    a. in mm/year
#    b. as % of glacier runoff

# Project: NASA HiMAT
# Danielle S Grogan
# last updated: 2019-07-01

library(raster)
library(rgdal)
library(rgeos)

############################################################################################################
glacier_percolation = function(wbm.path,                # path to wbm files. for yearly files, stop after "/yearly"
                               shape,                   # shapefile for basin aggregation
                               glacier.runoff.basins,   # glacier runoff: km3/year per basin
                               years,                   # years for analysis
                               out.path){               # path to save all output
  
  # Note: Uses yearly wbm files, and aggregates glacier runoff to yearly values.
  # edit if monthly or seasonal analysis is needed 
  
  perc.ineff = wbm_load(path    = file.path(wbm.path, "IrrPercIneff_mm_pg"), 
                        varname = "IrrPercIneff_mm_pg", 
                        years   = years)
  
  perc.rice  = wbm_load(path    = file.path(wbm.path, "IrrPercRice_mm_pg"), 
                        varname = "IrrPercRice_mm_pg", 
                        years   = years)
  
  # 1. Total percolation of glacier runoff into groundwater system
  #    a. in mm/year
  #       one reaster layer per year. Output brick (netcdf) to file
  #       time series 
  total.perc.brick = overlay(perc.ineff, 
                             perc.rice, 
                             fun = function(x,y){return(365*(x+y))},  # x365 to convert from mean annual to annual sum 
                             filename = file.path(out.path, "Perc_pg_mmYr.nc"),
                             overwrite=T)
  
  #      spatial aggregation by basin. Output timeseries (csv) to file
  #       time series 
  total.perc.basins = spatial_aggregation(raster.data = total.perc.brick,
                                          shapefile   = shape,
                                          s           = 1,
                                          weight      = T, 
                                          poly.out    = F)
  colnames(total.perc.basins) = years
  rownames(total.perc.basins) = shape$name
  write.csv(total.perc.basins, file.path(out.path, "Perc_pg_basins_km3Yr.csv"))
  
  #    b. as % of glacier runoff:
  perc_percent = 100* total.perc.basins/glacier.runoff.basins
  perc_percent[is.infinite(perc_percent)] = NA  # divide by zero should result in NA, not inf
  write.csv(perc_percent, file.path(out.path, "Perc_pg_basins_percentGlRunoff.csv"))
}
###########################################################################################
