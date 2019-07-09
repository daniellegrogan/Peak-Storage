# Analysis steps for Peak Storage
# Project: NASA HiMAT
# Danielle S Grogan
# last updated: 2019-07-09

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
wbm.base   = "/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/Frontiers"
gcm.list = c("CanESM2", 
             "CCSM4",
             "CNRM-CM5",
             "CSIRO-Mk3-6-0",
             "GFDL-CM3",
             "GFDL-ESM2M",
             "GISS-E2-R",
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
years = seq(2006, 2099)

gcm.list = c("CanESM2")
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

res.dir  = "results/historical"  # directory from which to read results
plot.dir = "figures/historical"  # directory to which to save plot

gl_runoff = read.csv(file.path(res.dir,"/Glacier_runoff_basins_km3Yr.csv"))
gl_storage= read.csv(file.path(res.dir, "Storage_basins_km3Yr.csv"))
gl_ocean  = read.csv(file.path(res.dir,"/Glacier_to_ocean_km3Yr.csv"))
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
plot.nm = "Runoff_Exp_Stor_ExorheicAll_historical_TS.png"

plot_glacier_TS(plot.dir,
                gl_roff,
                gl_exp,
                gl_stor,
                years,
                plot.nm)

### Plot cumulative glacier runoff, export, and storage for SUM of all exorheic basins
plot.nm = "Runoff_Exp_Stor_ExorheicAll_historical_cumulative.png"
plot_glacier_cumulative(plot.dir,
                        gl_roff,
                        gl_exp,
                        gl_stor,
                        years,
                        plot.nm)

### Plot time series of glacier runoff, export, and storage for EACH exorheic basin
for(i in 1:length(ex.basins$name)){
  gl_roff = as.numeric(gl_runoff.ex[which(gl_runoff.ex[,1] %in% ex.basins$name[i]),  2:ncol(gl_runoff.ex)])
  gl_stor = as.numeric(gl_storage.ex[which(gl_storage.ex[,1] %in% ex.basins$name[i]),2:ncol(gl_storage.ex)])
  
  gl_oc = as.numeric(gl_ocean.ex[which(gl_ocean.ex[,1] %in% ex.basins$name[i]), 2:ncol(gl_ocean.ex)])
  gl_et = as.numeric(gl_et.ex[which(gl_et.ex[,1] %in% ex.basins$name[i]),2:ncol(gl_et.ex)])
  gl_exp = gl_oc + gl_et
  
  years = as.numeric(sub("X", "", c(colnames(gl_runoff.ex)[2:ncol(gl_runoff.ex)])))
  
  plot.nm.ts = paste("Runoff_Exp_Stor", ex.basins$name[i], "historical_TS.png", sep="_")
  plot_glacier_TS(plot.dir,
                  gl_roff,
                  gl_exp,
                  gl_stor,
                  years,
                  plot.nm.ts)
  
  plot.nm.cl = paste("Runoff_Exp_Stor", ex.basins$name[i], "historical_cumulative.png", sep="_")
  plot_glacier_cumulative(plot.dir,
                          gl_roff,
                          gl_exp,
                          gl_stor,
                          years,
                          plot.nm.cl)
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

# Map: irrigated areas, rice paddies, soil properties, rice percolation rates