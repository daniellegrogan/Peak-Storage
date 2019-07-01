# glacier_to_cropET.R  ### WORK IN PROGRESS ###
# Function calculates and saves to file:
# Glacier runoff exported to crop ET
#    a. in km3/year
#    b. as % of glacier runoff

glacier_to_cropET = function(wbm.path,               # path to wbm output. for yearly files, stop after "/yearly"
                             glacier.runoff.basins,  # glacier runoff: km3/year per basin
                             years,                  # years for analysis
                             shape,                  # shapefile to aggregate basins
                             out.path){              # path to save all output
  
  # a. in km3/year 
  
  # mm/year = et.ineff_pg + (etIrrCrops x soilMoist_pg)
  et.ineff = wbm_load(path    = file.path(wbm.path, "IrrEvap_mm_pg"), 
                     varname = "IrrEvap_mm_pg", 
                     years   = years)
  
  et.crops  = wbm_load(path    = file.path(wbm.path, "etIrrCrops"), 
                      varname = "etIrrCrops", 
                      years   = years)
  
  soilM.pg  = wbm_load(path    = file.path(wbm.path, "soilMoist_pg"), 
                       varname = "soilMoist_pg", 
                       years   = years)
  
  # 1. Total ET of glacier runoff 
  #    a. in mm/year
  #       one reaster layer per year. Output brick (netcdf) to file
  #       time series 
  glacier.ET.brick = overlay(et.ineff, 
                             et.crops, 
                             soilM.pg,
                             fun = function(x,y,z){return(365*(x+(y*z)))},  # x365 to convert from mean annual to annual sum 
                             filename = file.path(out.path, "ET_pg_mmYr.nc"),
                             overwrite=T)
  
  #      spatial aggregation by basin. Output timeseries (csv) to file
  #       time series 
  glacier.ET.basins = spatial_aggregation(raster.data = glacier.ET.brick,
                                          shapefile   = shape,
                                          s           = 1,
                                          weight      = T, 
                                          poly.out    = F)
  colnames(glacier.ET.basins) = years
  rownames(glacier.ET.basins) = shape$name
  write.csv(glacier.ET.basins, file.path(out.path, "ET_pg_basins_km3Yr.csv"))


  #    b. as % of glacier runoff
  # calculate % based on basin time series
  et_percent = 100* glacier.ET.basins/glacier.runoff.basins
  et_percent[is.infinite(et_percent)] = NA  # divide by zero should result in NA, not inf
  write.csv(et_percent, file.path(out.path, "ET_pg_basins_percentGlRunoff.csv"))
}
