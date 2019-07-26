# MMM_RES_plot_wrapper.R
# RES := Runoff, Export, Storage

# wrapper for MMM_RES_plot.R
# makes RES plot for sum of all exorheic basins, and each basin individually

# Project: NASA HiMAT
# Danielle S Grogan
# last updated: 2019-07-23

MMM_RES_plot_wrapper = function(res.dir,        # results directory from which to read multi-model data frames
                                ex.basin.names, # exorheic basin names
                                plot.dir       # plot directory to which to write plots
                                ){      
  
  # create plot.dir if it does not exist
  f1 = file.path(strsplit(plot.dir, "/")[[1]][1], strsplit(plot.dir, "/")[[1]][2])
  if(!file.exists(f1)){
    dir.create(f1)
  }
  if(!file.exists(plot.dir)){
    dir.create(plot.dir)
  }
  
  # All Exorheic basins
  # load multi-model data frames
  gl_runoff  = read.csv(file.path(res.dir, "Glacier_runoff_basins_km3Yr_ExorheicAll.csv"), header=T, row.names = 1)
  gl_et      = read.csv(file.path(res.dir, "ET_pg_basins_km3Yr_ExorheicAll.csv"),          header=T, row.names = 1)
  gl_ocean   = read.csv(file.path(res.dir, "Glacier_to_ocean_km3Yr_ExorheicAll.csv"),      header=T, row.names = 1)
  gl_storage = read.csv(file.path(res.dir, "Storage_basins_km3Yr_ExorheicAll.csv"),        header=T, row.names = 1)

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
    basin = as.character(ex.basin.names[b])
    
    # load multi-model data frames
    gl_runoff  = read.csv(file.path(res.dir, paste("Glacier_runoff_basins_km3Yr_", basin, ".csv", sep="")), header=T, row.names = 1)
    gl_et      = read.csv(file.path(res.dir, paste("ET_pg_basins_km3Yr_", basin, ".csv", sep="")),          header=T, row.names = 1)
    gl_ocean   = read.csv(file.path(res.dir, paste("Glacier_to_ocean_km3Yr_", basin, ".csv", sep="")),      header=T, row.names = 1)
    gl_storage = read.csv(file.path(res.dir, paste("Storage_basins_km3Yr_", basin, ".csv", sep="")),        header=T, row.names = 1)
    
    # Export = gl_ocean + gl_et
    gl_exp = gl_ocean + gl_et
    
    # Calculate cumulative storage
    storage_c = t(apply(gl_storage, c(1), cumsum))
    
    plot.nm = paste("Runoff_Exp_Stor_", basin, "_", rcp, ".png", sep="")
    MMM_RES_plot(gl_runoff,   # glacier runoff time series (km3/yr), one model per row
                 storage_c,   # cumulative glacier storage time series (km3), one model per row
                 gl_exp,      # glacier export time series (km3/yr), one model per row
                 plot.dir,    # directory to write the plot
                 plot.nm)
    
  }
  
}
