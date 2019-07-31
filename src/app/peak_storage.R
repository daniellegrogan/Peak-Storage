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
#wbm.path   = "/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/Frontiers/ERA_hist/yearly"
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
#wbm.base   = "/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/Frontiers"
wbm.base   = "/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/Peak_Storage/"
gcm.list = c("CanESM2", 
             "CCSM4",
             "CNRM-CM5",
             "CSIRO-Mk3-6-0",
             "GFDL-CM3",
             "GFDL-ESM2M",
             #"GISS-E2-R",
             "IPSL-CM5A-LR",
             "MPI-ESM-LR")
             "NorESM1-M"

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

# RCP 4.5
lapply(gcm.list, 
       FUN = function(x) 
       {gl_analysis(wbm.path = file.path(wbm.base, x, "rcp45/yearly"),
                    basin.shape,
                    basin.ID,
                    up.area,
                    ex.basins,
                    gl.path,
                    model = x, 
                    rcp = 'rcp45', 
                    years,
                    out.path = file.path("results", x, "rcp45"))}
)

# RCP 8.5
lapply(gcm.list,
       FUN = function(x)
       {gl_analysis(wbm.path = file.path(wbm.base, x, "rcp85/yearly"),
                    basin.shape,
                    basin.ID,
                    up.area,
                    ex.basins,
                    gl.path,
                    model = x,
                    rcp = 'rcp85',
                    years,
                    out.path = file.path("results", x, "rcp85"))}
)

################################################################################################################################
### Calculate multi-model means
out.dir = "results/multi_model_mean"
res.dir.base = "results/"

## Time series data
# Sum of all exorheic basins
basin = "Ex"
rcp = "rcp85"
years = seq(2000, 2099)
gl_runoff = multi_model_df(res.dir.base, 
                           gcm.list,
                           file.nm = "Glacier_runoff_basins_km3Yr.csv",
                           years,          
                           rcp,           
                           basin,
                           out.dir)
gl_storage = multi_model_df(res.dir.base, 
                            gcm.list,
                            file.nm = "Storage_basins_km3Yr.csv",
                            years,          
                            rcp,           
                            basin,
                            out.dir)
gl_ocean = multi_model_df(res.dir.base, 
                          gcm.list,
                          file.nm = "Glacier_to_ocean_km3Yr.csv",
                          years,          
                          rcp,           
                          basin,
                          out.dir)
gl_et = multi_model_df(res.dir.base, 
                       gcm.list,
                       file.nm = "ET_pg_basins_km3Yr.csv",
                       years,          
                       rcp,           
                       basin,
                       out.dir)

# Each exorheic basin
ex.basin.names = ex.basins$name
for(b in 1:length(ex.basin.names)){
  basin = as.character(ex.basin.names[b])
  gl_runoff = multi_model_df(res.dir.base, 
                             gcm.list,
                             file.nm = "Glacier_runoff_basins_km3Yr.csv",
                             years,          
                             rcp,           
                             basin,
                             out.dir)
  gl_storage = multi_model_df(res.dir.base, 
                              gcm.list,
                              file.nm = "Storage_basins_km3Yr.csv",
                              years,          
                              rcp,           
                              basin,
                              out.dir)
  gl_ocean = multi_model_df(res.dir.base, 
                            gcm.list,
                            file.nm = "Glacier_to_ocean_km3Yr.csv",
                            years,          
                            rcp,           
                            basin,
                            out.dir)
  gl_et = multi_model_df(res.dir.base, 
                         gcm.list,
                         file.nm = "ET_pg_basins_km3Yr.csv",
                         years,          
                         rcp,           
                         basin,
                         out.dir)
}

## Raster data
res.dir.base = "results"
file.names = c("ET_pg_mmYr.nc", 
               "Glacier_runoff_mmYr.nc",
               "IrrGWpg_percent_of_IrrGross.nc",
               "IrrGwpg_percent_of_IrrGW.nc",
               "Perc_pg_mmYr.nc")

# Historical GCMs
rcp = 'historical'
lapply(file.names, 
       fun = function(x) multi_model_raster(res.dir.base, 
                                            gcm.list,     
                                            file.nm = x,       
                                            rcp,            
                                            out.dir)
       )

# RCP 4.5
rcp = 'rcp45'
lapply(file.names, 
       fun = function(x) multi_model_raster(res.dir.base, 
                                            gcm.list,     
                                            file.nm = x,       
                                            rcp,            
                                            out.dir)
)

# RCP 8.5
rcp = 'rcp85'
lapply(file.names,
       fun = function(x) multi_model_raster(res.dir.base,
                                            gcm.list,
                                            file.nm = x,
                                            rcp,
                                            out.dir)
)


### Multi-model mean from raw WBM results: IrrGrwt_mm_pg (average mm/year)

wbm.base   = "/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/Peak_Storage/"
varname    = "IrrGrwt_mm_pg"

# Historical GCMs
rcp        = 'historical'
file.path.list = lapply(gcm.list, FUN = function(x) file.path(wbm.base, x, rcp, "yearly", varname))

wbm_model_mean(file.path.list, 
               yrs     = NA,          
               out.dir = "results/multi_model_mean",         
               out.nm  = "IrrGrwt_mm_pg_historical.nc",        
               ret = 0)   

# RCP 4.5
rcp = 'rcp45'
file.path.list = lapply(gcm.list, FUN = function(x) file.path(wbm.base, x, rcp, "yearly", varname))

wbm_model_mean(file.path.list, 
               yrs     = NA,          
               out.dir = "results/multi_model_mean",         
               out.nm  = "IrrGrwt_mm_pg_rcp45.nc",        
               ret = 0) 
  
# RCP 8.5
rcp = 'rcp85'
file.path.list = lapply(gcm.list, FUN = function(x) file.path(wbm.base, x, rcp, "yearly", varname))

wbm_model_mean(file.path.list,
               yrs     = NA,
               out.dir = "results/multi_model_mean",
               out.nm  = "IrrGrwt_mm_pg_rcp85.nc",
               ret = 0)

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
