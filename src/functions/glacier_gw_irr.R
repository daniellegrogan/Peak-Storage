# glacier_gw_irr.R
# Function calculates and saves to file:
#  Irrigation water supplied by glacier runoff through groundwater withdrawals
#    a. in volume/year (spatial agg only needed - wbm output already provides gridded layers in mm/yr)
#    b. as % of irrigation from groundwater sources
#    c. as % of Gross Irrigation


# Project: NASA HiMAT
# Danielle S Grogan
# last updated: 2019-06-025

library(raster)
library(rgdal)
library(rgeos)

############################################################################################################
glacier_gw_irr.R = function(wbm.path,   # path to wbm files. for yearly files, stop after "/yearly"
                            shape,      # shapefile for basin aggregation
                            years,      # years for analysis
                            out.path){  # path to save all output
  
  # Note: Uses yearly wbm files
  # edit if monthly or seasonal analysis is needed 
  
  irr.gw.pg  = wbm_load(path    = file.path(wbm.path, "IrrGrwt_mm_pg"),   # glacier water in groundwater irrigation
                        varname = "IrrGrwt_mm_pg", 
                        years   = years)
  
  irr.gw    = wbm_load(path    = file.path(wbm.path, "irrigationGrwt"),   # all irrigation from groundwater
                       varname = "irrigationGrwt", 
                       years   = years)
  
  irr.gross = wbm_load(path    = file.path(wbm.path, "irrigationGross"),   # gross irrigation
                       varname = "irrigationGross", 
                       years   = years)
  
  # 1. Irrigation water supplied by glacier runoff through groundwater withdrawals
  #      spatial aggregation by basin. Output timeseries (csv) to file
  #       time series 
  irr.gw.pg.basins = spatial_aggregation(raster.data = irr.gw.pg,
                                         shapefile   = shape,
                                         s           = 1,
                                         weight      = T, 
                                         poly.out    = F)
  colnames(irr.gw.pg.basins) = years
  rownames(irr.gw.pg.basins) = shape$name
  write.csv(irr.gw.pg.basins, file.path(out.path, "IrrGWpg_basins_km3Yr.csv"))
  
  #    b. as % of irrigation from groundwater sources
  irr.gw.pg.percent.gw = overlay(irr.gw.pg, 
                                 irr.gw, 
                                 fun = function(x,y){return(100*(x/y))},  
                                 filename = file.path(out.path, "IrrGWpg_percent_of_IrrGW.nc"),
                                 overwrite=T)
  
  # spatial aggregation by basin. Output timeseries (csv) to file 
  irr.gw.basins = spatial_aggregation(raster.data = irr.gw,   # aggregate IrrGW by basin
                                      shapefile   = shape,
                                      s           = 1,
                                      weight      = T, 
                                      poly.out    = F)
  
  irr.gw.pg.percent.gw.basins = 100*irr.gw.pg.basins/irr.gw.basins
  irr.gw.pg.percent.gw.basins[is.infinite(irr.gw.pg.percent.gw.basins)] = NA  # divide by zero should result in NA, not inf
  colnames(irr.gw.pg.percent.gw.basins) = years
  rownames(irr.gw.pg.percent.gw.basins) = shape$name
  write.csv(irr.gw.pg.percent.gw.basins, file.path(out.path, "IrrGWpg_basins_percent_of_irrGW.csv"))
  
  
  #    c. as % of gross irrigation
  irr.gw.pg.percent.irrgross = overlay(irr.gw.pg, 
                                       irr.gross, 
                                       fun = function(x,y){return(100*(x/y))},  
                                       filename = file.path(out.path, "IrrGWpg_percent_of_IrrGross.nc"),
                                       overwrite=T)
  
  # spatial aggregation by basin. Output timeseries (csv) to file 
  irr.gross.basins = spatial_aggregation(raster.data = irr.gross,   # aggregate IrrGross by basin
                                         shapefile   = shape,
                                         s           = 1,
                                         weight      = T, 
                                         poly.out    = F)
  
  irr.gw.pg.percent.irrgross.basins = 100*irr.gw.pg.basins/irr.gross.basins
  irr.gw.pg.percent.irrgross.basins[is.infinite(irr.gw.pg.percent.irrgross.basins)] = NA  # divide by zero should result in NA, not inf
  colnames(irr.gw.pg.percent.irrgross.basins) = years
  rownames(irr.gw.pg.percent.irrgross.basins) = shape$name
  write.csv(irr.gw.pg.percent.irrgross.basins, file.path(out.path, "IrrGWpg_basins_percent_of_irrGross.csv"))
  
}
###########################################################################################
