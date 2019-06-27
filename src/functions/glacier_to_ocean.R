# glacier_to_ocean.R  ### WORK IN PROGRESS ###
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
                            basin.list = NA, # list of basin IDs from which to extract river mouth data. Use all IDs in basinID file if NA
                            up.area,         # upstream area file associated with WBM river network
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
  
  # discharge at mouth in average annual m3/s
  q.m3s.mouth = lapply(X        = basin.list,   
                       FUN      = mouth_ts,
                       basin.ID = basin.ID,
                       up.area  = up.area, 
                       path     = file.path(wbm.path, "discharge"),
                       varname  = "discharge",
                       yrs      = years)
  
  # unit conversion: annual average m3/s to total km3/year
  # m3/s x s/year x km3/m3  =   km3/year
  seconds_per_year = 3.154e+7
  km3_per_m3 = 1e-9
  
  q.pg.km3yr = q.pg.frac.mouth * q.m3s.mouth * seconds_per_year * km3_per_m3
  # figure out row and column naming scheme
  
  #    b. as % of glacier runoff
  # glacier runoff: output in m3
  glacier.runoff = glacier_runoff_subset(gl.path = gl.path,          # path to glacier model output
                                         model   = gcm,              # If rcp != historical, also supply a GCM model name
                                         rcp     = rcp,              # rcp = one of: "historical", "rcp45", "rcp85"
                                         st.yr   = min(years),       # start year to subset
                                         end.yr  = max(years),       # end year to subset
                                         out.yr = 1)                 # 0 = output monthly.  1 for yearly
  
  glacier.runoff.mm = glacier_runoff_m3_to_mm(glacier.runoff, 
                                              out.path = out.path, 
                                              out.nm = NA,
                                              overwrite = F)
}

