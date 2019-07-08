# gl_analysis.R  
# Function analyzes and saves to file:
#    glacier runoff
#    Total percolation of glacier runoff into groundwater system
#    Irrigation water supplied by glacier runoff through groundwater withdrawals
#    Glacier runoff exported to ocean
#    Glacier runoff --> crop ET
#    Glacier water storage 

# Project: NASA HiMAT
# Danielle S Grogan
# last updated: 2019-07-03

#######################################################################################################################################
gl_analysis = function(wbm.path,
                       basin.shape,
                       basin.ID,
                       up.area,
                       ex.basins,
                       gl.path,
                       model, 
                       rcp, 
                       years,
                       out.path){
  
  # check if output directory exists.  If not, create it
  f1 = file.path(strsplit(out.path, "/")[[1]][1], strsplit(out.path, "/")[[1]][2])
  if(!file.exists(f1)){
    dir.create(f1)
  }
  if(!file.exists(out.path)){
    dir.create(out.path)
  }
  
  # Calculate glacier runoff by basin (km3/year): required input for analysis below
  # glacier runoff: output in m3
  glacier.runoff = glacier_runoff_subset(gl.path = gl.path,          # path to glacier model output
                                         model   = model,            # If rcp != historical, also supply a GCM model name
                                         rcp     = rcp,              # rcp = one of: "historical", "rcp45", "rcp85"
                                         st.yr   = min(years),       # start year to subset
                                         end.yr  = max(years),       # end year to subset
                                         out.yr = 1)                 # 0 = output monthly.  1 for yearly
  
  glacier.runoff.mm = glacier_runoff_m3_to_mm(glacier.runoff, 
                                              out.path = out.path, 
                                              out.nm   = "Glacier_runoff_mmYr.nc",
                                              overwrite = T)
  
  # spatial aggregation of glacier runoff: km3 per basin
  glacier.runoff.basins = spatial_aggregation(raster.data = glacier.runoff.mm,
                                              shapefile   = basin.shape,
                                              s           = 1, 
                                              cell.area   = 1,
                                              weight      = T, 
                                              poly.out    = F)
  colnames(glacier.runoff.basins) = years
  rownames(glacier.runoff.basins) = basin.shape$name
  write.csv(glacier.runoff.basins, file.path(out.path, "Glacier_runoff_basins_km3Yr.csv"))
  
  
  # 1. Total percolation of glacier runoff into groundwater system
  #    a. in mm/year: 
  #       time series (1980 - 2099) and 
  #    b. as % of glacier runoff:
  #       time series (1980 - 2099) and 
  
  glacier_percolation(wbm.path = wbm.path,                           # path to wbm files. for yearly files, stop after "/yearly"
                      shape    = basin.shape,                        # shapefile for basin aggregation
                      glacier.runoff.basins = glacier.runoff.basins, # glacier runoff: km3/year per basin
                      years    = years,                              # years for analysis
                      out.path = out.path)                           # path to save all output
  
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
                   out.path = out.path)      # path to save all output
  
  # 3. Glacier runoff exported to ocean
  #    a. in km3/year 
  #       time series (1980 - 2099) and 
  #    b. as % of glacier runoff:
  #       time series (1980 - 2099) and 
  
  glacier_to_ocean(wbm.path   = wbm.path,              # path to wbm output. for yearly files, stop after "/yearly"
                   glacier.runoff.basins = glacier.runoff.basins,
                   years      = years,                 # years for analysis
                   basin.ID   = basin.ID,              # basinID file associated with WBM river network
                   basin.list = ex.basins$Basin_ID,    # list of basin IDs from which to extract river mouth data. Use all IDs in basinID file if NA
                   basin.nm   = ex.basins$name,        # list of basin names that match IDs
                   up.area    = up.area,               # upstream area file associated with WBM river network
                   shape      = basin.shape,           # shapefile for basin aggregation
                   out.path   = out.path)              # path to save all output
  
  # 4. Glacier runoff --> crop ET
  #    a. in mm/year: 
  #       time series (1980 - 2099) and 
  #    b. as % of glacier runoff:
  #       time series (1980 - 2099) and 
  
  glacier_to_cropET(wbm.path              = wbm.path,               # path to wbm output. for yearly files, stop after "/yearly"
                    glacier.runoff.basins = glacier.runoff.basins,  # glacier runoff: km3/year per basin
                    years                 = years,                  # years for analysis
                    shape                 = basin.shape,            # shapefile to aggregate basins
                    out.path              = out.path)               # path to save all output
  
  
  # 5. Glacier water storage  ## EXORHEIC BAINS ONLY. NEED TO UPDATE TO INCLUDE ENDORHEIC ###
  gl_runoff = read.csv(file.path(out.path,"/Glacier_runoff_basins_km3Yr.csv"))
  gl_ocean  = read.csv(file.path(out.path,"/Glacier_to_ocean_km3Yr.csv"))
  gl_et     = read.csv(file.path(out.path,"ET_pg_basins_km3Yr.csv"))
  
  # sum runoff and export over exorheic basins
  gl_runoff.ex = gl_runoff[which(gl_runoff$X %in% ex.basins$name),]
  gl_et.ex     = gl_et[which(gl_et$X %in% ex.basins$name),]
  gl_ocean.ex  = gl_ocean[which(gl_ocean$X %in% ex.basins$name),]
  
  gl_stor.ex = cbind(gl_runoff.ex[,1], 
                     gl_runoff.ex[,2:ncol(gl_runoff.ex)] - 
                       (gl_et.ex[,2:ncol(gl_et.ex)] + gl_ocean.ex[,2:ncol(gl_ocean.ex)])
  )
  colnames(gl_stor.ex)[1] = "Storage"
  write.csv(gl_stor.ex, file.path(out.path, "Storage_basins_km3Yr.csv"), row.names = F)
  
}
#######################################################################################################################################
