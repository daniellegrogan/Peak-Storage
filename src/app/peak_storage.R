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

### Source functions from within this project:
file.sources = list.files("src/functions", full.names = T)
sapply(file.sources, source)
rm(wbm_load.script, spatial_aggregation.script, mouth_ts.script, file.sources)  # remove unnecesary variables

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
             "MPI-ESM-LR",
             "NorESM1-M")

gcm.list = c("CCSM4",
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
# NB - processing takes > 15 min per GCM
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
# Plots #
### WORK IN PROGRESS###

### MMM plots
rcp = 'rcp45'
plot.dir = file.path("figures", paste(rcp, "MMM", sep="_"))

# make RES plots for sum of exorheic basins, and each basin individually
# RES := Runoff, Export, Storage
MMM_RES_plot_wrapper(res.dir.base = 'results',         # results directory from which to read, one level up from GCM names
                     gcm.list,                         # list names of GCMs, used to generate file paths
                     years = seq(2000, 2099),          # vector of years.  INCLUDE HISTORICAL AND FUTURE YEARS (e.g., 2000 - 2099)
                     rcp = 'rcp45',                    # one of: "rcp45", "rcp85".  NO HISTORICAL - this function will build historical + rcp time series
                     ex.basin.names = ex.basins$names, # exorheic basin names
                     plot.dir)                         # plot directory to which to write plots
  

#### individual GCMs
res.dir  = "results/historical"  # directory from which to read results
plot.dir = "figures/historical"  # directory to which to save plot

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

res.dir = "results/CanESM2/rcp45"
plot.dir = "figures/CanESM2/rcp45"

for(i in gcm.list){
  res.dir = file.path("results", i, "rcp45")
  plot.dir = file.path("figures", i, "rcp45")
  glacier_RES_plots(res.dir, plot.dir)
}





### MAPS ###
# NOTE: MAKE THIS MORE GENERAL

# coastline shapefile
coastline = readOGR("/net/nfs/squam/raid/userdata/dgrogan/data/map_data/land-polygons-generalized-3857/", layer = "land_polygons_z4")
coastline = spTransform(coastline, crs(basin.shape))

# plot boundaries
xl = c(59, 120)
yl = c(9, 49)

# Percolation of glacier water
plot.nm = "Glacier_percolation_historical_mean_mmYr_1980-2015.png"

perc_pg = brick(file.path(res.dir, "Perc_pg_mmYr.nc"))  # yearly time series
perc_pg_mean = calc(perc_pg, fun = mean)                # mean value over time

# for purposes of plotting, make 0 = NA (no color)
perc_pg_mean[perc_pg_mean == 0] = NA
perc_pg_mean = mask(perc_pg_mean, basin.shape)

png(file.path(plot.dir, plot.nm), res=100, width = 800, height = 600)
plot(coastline,  xlim = xl, ylim = yl, border='grey70', lwd=1)
plot(perc_pg_mean, add = T)
plot(basin.shape,  add = T, lwd=1)
dev.off()
