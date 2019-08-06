# Analysis steps for Peak Storage
# Project: NASA HiMAT
# Danielle S Grogan
# last updated: 2019-07-10

### R Libraries
library(RCurl)  # enables sourcing R code from github
library(raster)
library(rgeos)
library(rgdal)

### Source functions from other github repos:
# wbm_load()
wbm_load.script = getURL("https://raw.githubusercontent.com/daniellegrogan/WBMr/master/wbm_load.R", ssl.verifypeer=F)
eval(parse(text=wbm_load.script))

# spatial_aggregation()
spatial_aggregation.script = getURL("https://raw.githubusercontent.com/daniellegrogan/WBMr/master/spatial_aggregation.R", ssl.verifypeer=F)
eval(parse(text=spatial_aggregation.script))

# mouth_ts()
mouth_ts.script = getURL("https://raw.githubusercontent.com/daniellegrogan/WBMr/master/mouth_ts.R", ssl.verifypeer=F)
eval(parse(text=mouth_ts.script))

# wbm_model_mean()
wbm_model_mean.script = getURL("https://raw.githubusercontent.com/daniellegrogan/WBMr/master/wbm_model_mean.R", ssl.verifypeer=F)
eval(parse(text=wbm_model_mean.script))

# raster_time_ave()
raster_time_ave.script = getURL("https://raw.githubusercontent.com/daniellegrogan/WBMr/master/raster_time_ave.R", ssl.verifypeer=F)
eval(parse(text=raster_time_ave.script))

### Source functions from within this project:
file.sources = list.files("src/functions", full.names = T)
sapply(file.sources, source)
rm(wbm_load.script, spatial_aggregation.script, mouth_ts.script, file.sources, wbm_model_mean.script, raster_time_ave.script)  # remove unnecesary variables

#######################################################################################################################################
### MAIN ####
#######################################################################################################################################

# Inputs: same for all models, historical, rcps, etc: 
basin.shape = readOGR("data/basins_hma", "basins_hma")
netwk.path = "/net/nfs/zero/data3/WBM_TrANS/data"

# inputs needed to identify basin mouths
basin.ID = raster(file.path(netwk.path, "HiMAT_full_210_IDs_Subset.asc"))
up.area  = raster(file.path(netwk.path,"flowdirection210_upstrArea.asc"))

# find IDs for exorheic basins
ex.basins = basin.shape[basin.shape$name == "Ganges" |
                          basin.shape$name == "Mekong" |
                          basin.shape$name == "Irawaddy" |
                          basin.shape$name == "Luni_ext" |
                          basin.shape$name == "Indus" |
                          basin.shape$name == "Brahmaputra" |
                          basin.shape$name == "Salween" |
                          basin.shape$name == "Yangtze" |
                          basin.shape$name == "Yellow",]


### Call gl_analysis() function:

### Historical ERA-Interim ###
wbm.path   = "/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/Peak_Storage/ERA_hist/yearly"
gl.path    =  "/net/nfs/merrimack/raid2/data/glaciers_6.0/HiMAT_full_210_Subset"

# years to analyze
years = seq(1980, 2016)

gl_analysis(wbm.path,
            basin.shape,
            basin.ID,
            up.area,
            ex.basins,
            gl.path,
            model = NA, 
            rcp = 'historical', 
            years,
            out.path = "results/historical")
  
  
### GCMs ###
gl.path    =  "/net/nfs/merrimack/raid2/data/glaciers_6.0/HiMAT_full_210_Subset"
wbm.base   = "/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/Peak_Storage/"
gcm.list = c("CanESM2", 
             "CCSM4",
             "CNRM-CM5",
             "CSIRO-Mk3-6-0",
             "GFDL-CM3",
             "GFDL-ESM2M",
             #"GISS-E2-R",
             "IPSL-CM5A-LR",
             "MPI-ESM-LR",
             "NorESM1-M")

### Historical GCMs ###
years = seq(2000, 2005)
lapply(gcm.list, 
       FUN = function(x) 
       {gl_analysis(wbm.path = file.path(wbm.base, x, "historical/yearly"),
                    basin.shape,
                    basin.ID,
                    up.area,
                    ex.basins,
                    gl.path,
                    model = x, 
                    rcp = 'historical', 
                    years,
                    out.path = file.path("results", x, "historical"))}
       )

### Future GCMs
# NB - processing can take > 15 min per GCM
years = seq(2006, 2099)
rcp.list = c("rcp45", "rcp85")

for(r in rcp.list){
  lapply(gcm.list, 
         FUN = function(x) 
         {gl_analysis(wbm.path = file.path(wbm.base, x, r, "yearly"),
                      basin.shape,
                      basin.ID,
                      up.area,
                      ex.basins,
                      gl.path,
                      model = x, 
                      rcp = r, 
                      years,
                      out.path = file.path("results", x, r))}
  )
}

################################################################################################################################
### Calculate multi-model means
out.dir = "results/multi_model_mean"
res.dir.base = "results/"

## Time series data
years = seq(2000, 2099)  # for this analysis, append future to historical
rcp.list = c("rcp45", "rcp85")

input.file.list = c("Glacier_runoff_basins_km3Yr.csv",
                    "Storage_basins_km3Yr.csv",
                    "Glacier_to_ocean_km3Yr.csv",
                    "ET_pg_basins_km3Yr.csv")

for(r in rcp.list){
  
  # Sum of all exorheic basins
  basin = "Ex"
  lapply(input.file.list,
         FUN = function(x) 
         {multi_model_df(res.dir.base, 
                         gcm.list,
                         file.nm = x,
                         years,          
                         rcp = r,           
                         basin,
                         out.dir)}
         )
  
  # Each exorheic basin
  ex.basin.names = ex.basins$name
  for(b in 1:length(ex.basin.names)){
    basin = as.character(ex.basin.names[b])
    lapply(input.file.list,
           FUN = function(x) 
           {multi_model_df(res.dir.base, 
                           gcm.list,
                           file.nm = x,
                           years,          
                           rcp = r,           
                           basin,
                           out.dir)}
    )
    
  } # end basin loop
} # end rcp loop


## Raster data (output from gl_analysis())
res.dir.base = "results"
file.names = c("ET_pg_mmYr.nc", 
               "Glacier_runoff_mmYr.nc",
               "IrrGWpg_percent_of_IrrGross.nc",
               "IrrGwpg_percent_of_IrrGW.nc",
               "Perc_pg_mmYr.nc")

h.rcp.list = c("historical", "rcp45", "rcp85")
for(r in h.rcp.list){
  lapply(file.names, 
         fun = function(x) multi_model_raster(res.dir.base, 
                                              gcm.list,     
                                              file.nm = x,       
                                              rcp = r,            
                                              out.dir)
  )
}


### Multi-model mean from raw WBM results
wbm.base   = "/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/Peak_Storage/"
varname.list = c("IrrGrwt_mm_pg", "IrrPercRice_mm_pg")

for(r in h.rcp.list){
  for(v in varname.list){
    file.path.list = lapply(gcm.list, FUN = function(x) file.path(wbm.base, x, r, "yearly", v))
    
    wbm_model_mean(file.path.list, 
                   yrs     = NA,          
                   out.dir = "results/multi_model_mean",         
                   out.nm  = paste(v, "_", r, ".nc", sep=""),        
                   ret = 0)  
  }
}

################################################################################################################################

### Climatologies
# brk.data = raster::brick("results/multi_model_mean/IrrGrwt_mm_pg_historical.nc")
# time.step = 6
# s = 0
# out.dir = "results/multi_model_mean/climatology"
# out.nm = "IrrGrwt_mm_pg_historical_clim.nc"

################################################################################################################################
# Plots #
### WORK IN PROGRESS###

### MMM plots
rcp = 'rcp45'
plot.dir = file.path("figures", paste(rcp, "MMM", sep="_"))

# make RES plots for sum of exorheic basins, and each basin individually
# RES := Runoff, Export, Storage
MMM_RES_plot_wrapper(res.dir        = 'results/multi_model_mean/',         # results directory from which to read
                     ex.basin.names = ex.basins$name,                     # exorheic basin names
                     plot.dir       = plot.dir)                            # plot directory to which to write plots
  


rcp = 'rcp85'
plot.dir = file.path("figures", paste(rcp, "MMM", sep="_"))

# make RES plots for sum of exorheic basins, and each basin individually
# RES := Runoff, Export, Storage
MMM_RES_plot_wrapper(res.dir        = 'results/multi_model_mean/',         # results directory from which to read
                     ex.basin.names = ex.basins$name,                     # exorheic basin names
                     plot.dir       = plot.dir)                            # plot directory to which to write plots



#### individual GCMs
# res.dir  = "results/historical"  # directory from which to read results
# plot.dir = "figures/historical"  # directory to which to save plot
# 
# gcm.list = c("CanESM2",
#              "CCSM4",
#              "CNRM-CM5",
#              "CSIRO-Mk3-6-0",
#              "GFDL-CM3",
#              "GFDL-ESM2M",
#              #"GISS-E2-R",
#              "IPSL-CM5A-LR",
#              "MPI-ESM-LR",
#              "NorESM1-M")
# 
# res.dir = "results/CanESM2/rcp45"
# plot.dir = "figures/CanESM2/rcp45"
# 
# for(i in gcm.list){
#   res.dir = file.path("results", i, "rcp45")
#   plot.dir = file.path("figures", i, "rcp45")
#   glacier_RES_plots(res.dir, plot.dir)
# }
# 
# 
# 
# 
# 
# ### MAPS ###
# # NOTE: MAKE THIS MORE GENERAL
# 
# coastline shapefile
coastline = readOGR("/net/nfs/squam/raid/userdata/dgrogan/data/map_data/land-polygons-generalized-3857/", layer = "land_polygons_z4")
coastline = spTransform(coastline, crs(basin.shape))

# plot boundaries
xl = c(59, 120)
yl = c(9, 49)
# 
# Percolation of glacier water
res.dir = "results/historical/"
plot.dir= "figures/historical/"
plot.nm = "Glacier_percolation_historical_mean_mmYr_1980-2015.png"

perc_pg = brick(file.path(res.dir, "Perc_pg_mmYr.nc"))  # yearly time series
perc_pg_mean = calc(perc_pg, fun = mean)                # mean value over time

# for purposes of plotting, make 0 = NA (no color)
perc_pg_mean[perc_pg_mean == 0] = NA
perc_pg_mean = mask(perc_pg_mean, basin.shape)
perc_pg_mean[perc_pg_mean < 0.1] = NA

library(RColorBrewer)

png(file.path(plot.dir, plot.nm), height=1200, width=1600, res=140)
plot(coastline,  xlim = xl, ylim = yl, border='grey70', lwd=1)
plot(perc_pg_mean, col = brewer.pal(9, "YlGnBu")[3:9], legend=F, add = T)
plot(basin.shape,  add = T, lwd=0.7)
plot(perc_pg_mean, col = brewer.pal(9, "YlGnBu")[3:9],  add=T, box=F, axes=T, las=1,
     legend.only=T, legend.width=0.4, horizontal=T, smallplot=c(0.068, 0.33, 0.15, 0.17),
     axis.args=list(cex.axis=1),
     legend.args=list(text='Percolation of Glacier Runoff (mm/yr)', side=3, font=1, line=0.1, cex=1))
dev.off()
