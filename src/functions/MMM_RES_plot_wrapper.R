# MMM_RES_plot_wrapper.R
# RES := Runoff, Export, Storage

# wrapper for MMM_RES_plot.R
# makes RES plot for sum of all exorheic basins, and each basin individually

# Project: NASA HiMAT
# Danielle S Grogan
# last updated: 2019-07-23

plot.dir = file.path("figures", paste(rcp, "MMM", sep="_"))
MMM_RES_plot_wrapper = function(res.dir.base,   # results directory from which to read, one level up from GCM names
                         gcm.list,       # list names of GCMs, used to generate file paths
                         years,          # vector of years.  INCLUDE HISTORICAL AND FUTURE YEARS (e.g., 2000 - 2099)
                         rcp,            # one of: "rcp45", "rcp85".  NO HISTORICAL - this function will build historical + rcp time series
                         ex.basin.names, # exorheic basin names
                         plot.dir){      # plot directory to which to write plots
  
  # create plot.dir if it does not exist
  f1 = file.path(strsplit(plot.dir, "/")[[1]][1], strsplit(plot.dir, "/")[[1]][2])
  if(!file.exists(f1)){
    dir.create(f1)
  }
  if(!file.exists(plot.dir)){
    dir.create(plot.dir)
  }
  
  # make multi-model data frames for vars of interest
  
  # Sum of all exorheic basins
  basin = "Ex"
  gl_runoff = multi_model_df(res.dir.base, 
                             gcm.list,
                             file.nm = "Glacier_runoff_basins_km3Yr.csv",
                             years,          
                             rcp,           
                             basin)
  gl_storage = multi_model_df(res.dir.base, 
                              gcm.list,
                              file.nm = "Storage_basins_km3Yr.csv",
                              years,          
                              rcp,           
                              basin)
  gl_ocean = multi_model_df(res.dir.base, 
                            gcm.list,
                            file.nm = "Glacier_to_ocean_km3Yr.csv",
                            years,          
                            rcp,           
                            basin)
  gl_et = multi_model_df(res.dir.base, 
                         gcm.list,
                         file.nm = "ET_pg_basins_km3Yr.csv",
                         years,          
                         rcp,           
                         basin)
  # Export = gl_ocean + gl_et
  gl_exp = gl_ocean + gl_et
  
  # Calculate cumulative storage
  storage_c = t(apply(gl_storage, c(1), cumsum))

  plot.nm = paste("Runoff_Exp_Stor_ExorheicAll_", rcp, ".png", sep="")
  MMM_RES_plot(gl_runoff,   # glacier runoff time series (km3/yr), one model per row
               storage_c,   # cumulative glacier storage time series (km3), one model per row
               gl_exp,      # glacier export time series (km3/yr), one model per row
               plot.dir,    # directory to write the plot
               plot.nm)
  
  ### Each exorheic basin
  for(b in 1:length(ex.basin.names)){
    basin = ex.basin.names[i]
    gl_runoff = multi_model_df(res.dir.base, 
                               gcm.list,
                               file.nm = "Glacier_runoff_basins_km3Yr.csv",
                               years,          
                               rcp,           
                               basin)
    gl_storage = multi_model_df(res.dir.base, 
                                gcm.list,
                                file.nm = "Storage_basins_km3Yr.csv",
                                years,          
                                rcp,           
                                basin)
    gl_ocean = multi_model_df(res.dir.base, 
                              gcm.list,
                              file.nm = "Glacier_to_ocean_km3Yr.csv",
                              years,          
                              rcp,           
                              basin)
    gl_et = multi_model_df(res.dir.base, 
                           gcm.list,
                           file.nm = "ET_pg_basins_km3Yr.csv",
                           years,          
                           rcp,           
                           basin)
    # Export = gl_ocean + gl_et
    gl_exp = gl_ocean + gl_et
    
    # Calculate cumulative storage
    storage_c = t(apply(gl_storage, c(1), cumsum))
    
    plot.nm = paste("Runoff_Exp_Stor_", basin, "_" rcp, ".png", sep="")
    MMM_RES_plot(gl_runoff,   # glacier runoff time series (km3/yr), one model per row
                 storage_c,   # cumulative glacier storage time series (km3), one model per row
                 gl_exp,      # glacier export time series (km3/yr), one model per row
                 plot.dir,    # directory to write the plot
                 plot.nm)
    
  }
  
}
