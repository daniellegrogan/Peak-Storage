# glacier_to_ocean.R  
# Function calculates and saves to file:
# Glacier runoff exported to ocean
#    a. in km3/year
#    b. as % of glacier runoff

# depends on function mouth_ts() from WBMr git repo

glacier_to_ocean = function(wbm.path,        # path to wbm output. for yearly files, stop after "/yearly"
                            gl.path,         # path to glacier runoff files
                            gcm,             # GCM model name, if analyzing future RCP
                            rcp,             # one of: 'histroical', 'rcp45', 'rcp85'
                            years,           # years for analysis
                            basin.ID,        # basinID file associated with WBM river network
                            basin.list,      # list of basin IDs from which to extract river mouth data.
                            basin.nm,        # list of basin names that match basin.list
                            up.area,         # upstream area file associated with WBM river network
                            shape,           # shapefile to aggregate basins
                            out.path){       # path to save all output
  
  #    a. in km3/year 
  # extract brick data at mouth of each basin
  
  # glacier runoff as fraction of discharge at mouth
  q.pg.frac.mouth = lapply(X        = basin.list, # list to apply mouth_ts()
                           FUN      = mouth_ts,   
                           basin.ID = basin.ID,   # inputs to mouth_ts()
                           up.area  = up.area, 
                           path     = file.path(wbm.path, "discharge_pg"),
                           varname  = "discharge_pg",
                           yrs      = years)
  q.pg.frac.mouth = matrix(unlist(q.pg.frac.mouth), nrow = length(basin.list), byrow=T) # list to matrix
  
  # discharge at mouth in average annual m3/s
  q.m3s.mouth = lapply(X        = basin.list,   
                       FUN      = mouth_ts,
                       basin.ID = basin.ID,
                       up.area  = up.area, 
                       path     = file.path(wbm.path, "discharge"),
                       varname  = "discharge",
                       yrs      = years)
  q.m3s.mouth = matrix(unlist(q.m3s.mouth), nrow = length(basin.list), byrow=T) # list to matrix
  
  # unit conversion: annual average m3/s to total km3/year
  # m3/s x s/year x km3/m3  =   km3/year
  seconds_per_year = 3.154e+7
  km3_per_m3 = 1e-9
  
  q.pg.km3yr = q.pg.frac.mouth * q.m3s.mouth * seconds_per_year * km3_per_m3
  rownames(q.pg.km3yr) = basin.nm
  colnames(q.pg.km3yr) = yrs
  write.csv(q.pg.km3yr, file.path(out.path, "Glacier_to_ocean_km3Yr.csv"))

  
  #    b. as % of glacier runoff
  
  # glacier runoff: output in m3
  glacier.runoff = glacier_runoff_subset(gl.path = gl.path,          # path to glacier model output
                                         model   = gcm,              # If rcp != historical, also supply a GCM model name
                                         rcp     = rcp,              # rcp = one of: "historical", "rcp45", "rcp85"
                                         st.yr   = min(years),       # start year to subset
                                         end.yr  = max(years),       # end year to subset
                                         out.yr = 1)                 # 0 = output monthly.  1 for yearly
  
  glacier.runoff.mm = glacier_runoff_m3_to_mm(glacier.runoff, 
                                              out.path = NA, 
                                              out.nm = NA,
                                              overwrite = F)
  
  # spatial aggregation of glacier runoff: km3 per basin
  glacier.runoff.basins = spatial_aggregation(raster.data = glacier.runoff.mm,
                                              shapefile   = shape,
                                              s           = 1, 
                                              cell.area   = 1,
                                              weight      = T, 
                                              poly.out    = F)
  colnames(glacier.runoff.basins) = years
  rownames(glacier.runoff.basins) = shape$name

  glacier.runoff.ex = glacier.runoff.basins[rownames(glacier.runoff.basins) %in% basin.nm,]  
  q.pg.perc = 100*q.pg.km3yr/glacier.runoff.ex
  q.pg.perc[is.infinite(q.pg.perc)] = NA
  
  write.csv(q.pg.perc, file.path(out.path, "Glacier_to_ocean_percentGlRunoff.csv"))
}

