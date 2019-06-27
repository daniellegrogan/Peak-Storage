# glacier_percolation.R
# Function calculates and saves to file:
#  Total percolation of glacier runoff into groundwater 
#    a. in mm/year
#    b. as % of glacier runoff

# Project: NASA HiMAT
# Danielle S Grogan
# last updated: 2019-06-025

library(raster)
library(rgdal)
library(rgeos)

############################################################################################################
glacier_percolation = function(wbm.path,   # path to wbm files. for yearly files, stop after "/yearly"
                               shape,      # shapefile for basin aggregation
                               gl.path,    # path to glacier runoff files
                               gcm,        # GCM model name, if analyzing future RCP
                               rcp,        # one of: 'histroical', 'rcp45', 'rcp85'
                               years,      # years for analysis
                               out.path){  # path to save all output
  
  # Note: Uses yearly wbm files, and aggregates glacier runoff to yearly values.
  # edit if monthly or seasonal analysis is needed 
  
  perc.ineff = wbm_load(path    = file.path(wbm.path, "IrrPercIneff_mm_pg"), 
                        varname = "IrrPercIneff_mm_pg", 
                        years   = years)
  
  perc.rice  = wbm_load(path    = file.path(wbm.path, "IrrPercRice_mm_pg"), 
                        varname = "IrrPercRice_mm_pg", 
                        years   = years)
  
  # glacier runoff: output in m3
  glacier.runoff = glacier_runoff_subset(gl.path = gl.path,          # path to glacier model output
                                         model   = gcm,              # If rcp != historical, also supply a GCM model name
                                         rcp     = rcp,              # rcp = one of: "historical", "rcp45", "rcp85"
                                         st.yr   = min(years),       # start year to subset
                                         end.yr  = max(years),       # end year to subset
                                         out.yr = 1)                 # 0 = output monthly.  1 for yearly
  
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
  #      spatial aggregation by basin. Output timeseries (csv) to file (raster doesn't make sense here)
  #      time series 
  
  # unit conversion: glacier runoff in m3/year to mm/year
  km2_to_m2 = 1e6
  area.glacier.grid = km2_to_m2*raster::area(glacier.runoff) # unit: m2
  glacier.runoff.mm = glacier_runoff_m3_to_mm(glacier.runoff, 
                                              out.path = out.path, 
                                              out.nm = "Glacier_runoff_mmYr.nc",
                                              overwrite = T)
  
  # spatial aggregation of glacier runoff: km3 per basin
  glacier.runoff.basins = spatial_aggregation(raster.data = glacier.runoff.mm,
                                              shapefile   = shape,
                                              s = 1, 
                                              cell.area = 1,
                                              weight      = T, 
                                              poly.out    = F)
  colnames(glacier.runoff.basins) = years
  rownames(glacier.runoff.basins) = shape$name
  write.csv(glacier.runoff.basins, file.path(out.path, "Glacier_runoff_basins_km3Yr.csv"))
  
  # calculate % based on basin time series
  perc_percent = 100* total.perc.basins/glacier.runoff.basins
  perc_percent[is.infinite(perc_percent)] = NA  # divide by zero should result in NA, not inf
  write.csv(perc_percent, file.path(out.path, "Perc_pg_basins_percentGlRunoff.csv"))
}
###########################################################################################
