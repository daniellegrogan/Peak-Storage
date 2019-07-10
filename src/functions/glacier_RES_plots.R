# glacier_RES_plots.R
# RES := Runoff, Export, Storage
# wrapper for plot_glacier_TS() and plot_glacier_cumulative()

# Plots results from glacier analysis
# makes time series and cumulative plots of RES results for:
# (a) sum of all exhorheic basins
# (b) Each individual exorheic basin

# Project: NASA HiMAT
# Danielle S Grogan
# last updated: 2019-07-10

glacier_RES_plots = function(res.dir,   # results directory from which to read 
                             plot.dir){ # plot directory to which to write plots
  
  f1 = file.path(strsplit(plot.dir, "/")[[1]][1], strsplit(plot.dir, "/")[[1]][2])
  if(!file.exists(f1)){
    dir.create(f1)
  }
  if(!file.exists(plot.dir)){
    dir.create(plot.dir)
  }
  
  
  gl_runoff = read.csv(file.path(res.dir,"Glacier_runoff_basins_km3Yr.csv"))
  gl_storage= read.csv(file.path(res.dir, "Storage_basins_km3Yr.csv"))
  gl_ocean  = read.csv(file.path(res.dir,"Glacier_to_ocean_km3Yr.csv"))
  gl_et     = read.csv(file.path(res.dir,"ET_pg_basins_km3Yr.csv"))
  
  ### Plot time series of glacier runoff, export, and storage for SUM of all exorheic basins
  # sum runoff and export over exorheic basins
  gl_runoff.ex  = gl_runoff[which(gl_runoff[,1] %in% ex.basins$name),]
  gl_storage.ex = gl_storage[which(gl_storage[,1] %in% ex.basins$name),]
  gl_ocean.ex   = gl_ocean[which(gl_ocean[,1] %in% ex.basins$name),]
  gl_et.ex      = gl_et[which(gl_et[,1] %in% ex.basins$name),]
  
  gl_roff = as.numeric(colSums(gl_runoff.ex[,2:ncol(gl_runoff.ex)]))
  gl_exp  = as.numeric(colSums(gl_ocean.ex[,2:ncol(gl_ocean.ex)]) + colSums(gl_et.ex[,2:ncol(gl_et.ex)]))
  gl_stor = as.numeric(colSums(gl_storage.ex[,2:ncol(gl_storage.ex)]))
  
  years = as.numeric(sub("X", "", c(colnames(gl_runoff)[2:ncol(gl_runoff)])))
  
  name_parts = strsplit(res.dir, "/")
  plot.nm.ts = paste("Runoff_Exp_Stor_ExorheicAll_", name_parts[[1]][2], "_", name_parts[[1]][3], "_TS.png", sep="")
  
  plot_glacier_TS(plot.dir,
                  gl_roff,
                  gl_exp,
                  gl_stor,
                  years,
                  plot.nm.ts)
  
  ### Plot cumulative glacier runoff, export, and storage for SUM of all exorheic basins
  plot.nm.cl = paste("Runoff_Exp_Stor_ExorheicAll_", name_parts[[1]][2], "_", name_parts[[1]][3], "_cumulative.png", sep="")
  plot_glacier_cumulative(plot.dir,
                          gl_roff,
                          gl_exp,
                          gl_stor,
                          years,
                          plot.nm.cl)
  
  ### Plot time series of glacier runoff, export, and storage for EACH exorheic basin
  for(i in 1:length(ex.basins$name)){
    gl_roff = as.numeric(gl_runoff.ex[which(gl_runoff.ex[,1] %in% ex.basins$name[i]),  2:ncol(gl_runoff.ex)])
    gl_stor = as.numeric(gl_storage.ex[which(gl_storage.ex[,1] %in% ex.basins$name[i]),2:ncol(gl_storage.ex)])
    
    gl_oc = as.numeric(gl_ocean.ex[which(gl_ocean.ex[,1] %in% ex.basins$name[i]), 2:ncol(gl_ocean.ex)])
    gl_et = as.numeric(gl_et.ex[which(gl_et.ex[,1] %in% ex.basins$name[i]),2:ncol(gl_et.ex)])
    gl_exp = gl_oc + gl_et
    
    years = as.numeric(sub("X", "", c(colnames(gl_runoff.ex)[2:ncol(gl_runoff.ex)])))
    
    plot.nm.ts = paste("Runoff_Exp_Stor", ex.basins$name[i], name_parts[[1]][2], name_parts[[1]][3], "TS.png", sep="_")
    plot_glacier_TS(plot.dir,
                    gl_roff,
                    gl_exp,
                    gl_stor,
                    years,
                    plot.nm.ts)
    
    plot.nm.cl = paste("Runoff_Exp_Stor", ex.basins$name[i], name_parts[[1]][2], name_parts[[1]][3], "cumulative.png", sep="_")
    plot_glacier_cumulative(plot.dir,
                            gl_roff,
                            gl_exp,
                            gl_stor,
                            years,
                            plot.nm.cl)
  }
}
