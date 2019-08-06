# Maps of input data for Peak Storage analysis
# Project: NASA HiMAT
# Danielle S Grogan
# last updated: 2019-07-09

### R Libraries
library(RCurl)  # enables sourcing R code from github
library(raster)
library(rgeos)
library(rgdal)

### Maps made below: 
# irrigated areas
# rice paddies
# rice paddy percolation rates
# inter-basin transfers

### shapefiles for plotting
coastline   = readOGR("/net/nfs/squam/raid/userdata/dgrogan/data/map_data/land-polygons-generalized-3857/", layer = "land_polygons_z4")
coastline   = spTransform(coastline, crs(basin.shape))
coastline.shadow = shift(coastline, x= 0.15, y=-0.08)
basin.shape = readOGR("data/basins_hma", "basins_hma")

### plot limits
xl = c(59, 120)
yl = c(9, 49)

### plot directory:
plot.dir = "figures/atlas"

### data

# irrigated area
irr.area = raster("/net/nfs/bog/raid/data/dgrogan/MIRCA_data/WBM_format/IrrAreaFraction_MIRCA2000_05mn.nc")
irr.area[irr.area == 0] = NA
irr.area = mask(irr.area, basin.shape)

plot.nm = "Irrigated_area_CellFrac.png"
png(file.path(plot.dir, plot.nm),
    height=1200, width=1600, res=140)
par(mar=c(3, 3.2,0,0))
plot(coastline.shadow, xlim = xl, ylim = yl, border='grey90', lwd=4)
plot(coastline,        xlim = xl, ylim = yl, border='grey70', col='white',  lwd=1, add=T)
plot(irr.area,   add=T, legend=F)
plot(basin.shape, add=T, lwd=0.8)
plot(irr.area,   add=T, box=F, axes=T, las=1,
     legend.only=T, legend.width=0.4, horizontal=T, smallplot=c(0.08, 0.28, 0.27, 0.29),
     axis.args=list(cex.axis=1),
     legend.args=list(text='Irrigated Area', side=3, font=1, line=0.1, cex=1))
dev.off()

# irrigated rice paddies
rice.1 = raster("/net/nfs/bog/raid/data/dgrogan/MIRCA_data/WBM_format/IrrCrop_fraction/IrrCrop3_sub1_fraction.nc")
rice.2 = raster("/net/nfs/bog/raid/data/dgrogan/MIRCA_data/WBM_format/IrrCrop_fraction/IrrCrop3_sub2_fraction.nc")
rice.3 = raster("/net/nfs/bog/raid/data/dgrogan/MIRCA_data/WBM_format/IrrCrop_fraction/IrrCrop3_sub3_fraction.nc")

# make NA = 0 for summing
rice.1[is.na(rice.1)] = 0
rice.2[is.na(rice.2)] = 0
rice.3[is.na(rice.3)] = 0

rice.area = overlay(rice.1, rice.2, rice.3,
                    fun = function(x,y,z) x + y + z)
rice.area[rice.area == 0] = NA
rice.area = mask(rice.area, basin.shape)

plot.nm = "Harvested_rice_area_CellFracYr.png"
png(file.path(plot.dir, plot.nm),
    height=1200, width=1600, res=140)
par(mar=c(3, 3.2,0,0))
plot(coastline.shadow, xlim = xl, ylim = yl, border='grey90', lwd=4)
plot(coastline,        xlim = xl, ylim = yl, border='grey70', col='white',  lwd=1, add=T)
plot(rice.area,   add=T, legend=F)
plot(basin.shape, add=T, lwd=0.8)
plot(rice.area,   add=T, box=F, axes=T, las=1,
     legend.only=T, legend.width=0.4, horizontal=T, smallplot=c(0.08, 0.28, 0.27, 0.29),
     axis.args=list(cex.axis=1),
     legend.args=list(text='Harvested Rice Area', side=3, font=1, line=0.1, cex=1))
dev.off()


# rice paddy percolation rates
rice.perc = raster("/net/nfs/zero/home/WBM_TrANS/data/RicePercolationRate.nc")
rice.perc[rice.perc == 0] = NA
rice.perc = rice.perc * (rice.area > 0)
rice.perc = mask(rice.perc, basin.shape)

plot.nm = "Rice_paddy_percolation_rate_mmDay.png"
png(file.path(plot.dir, plot.nm),
    height=1200, width=1600, res=140)
par(mar=c(3, 3.2,0,0))
plot(coastline.shadow, xlim = xl, ylim = yl, border='grey90', lwd=4)
plot(coastline,        xlim = xl, ylim = yl, border='grey70', col='white',  lwd=1, add=T)
plot(rice.perc,   add=T, legend=F)
plot(basin.shape, add=T, lwd=0.8)
plot(rice.perc,   add=T, box=F, axes=T, las=1,
     legend.only=T, legend.width=0.4, horizontal=T, smallplot=c(0.08, 0.28, 0.27, 0.29),
     axis.args=list(cex.axis=1),
     legend.args=list(text='Rice Paddy Percolation Rate (mm/day)', side=3, font=1, line=0.1, cex=1))
dev.off()

# IBT
wbm.path = "/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/Peak_Storage/ERA_hist"
ibt = read.delim(list.files(file.path(wbm.path, "diversion"), full.names=T), header=T, sep="\t")
ibt.data = read.delim("/net/nfs/zero/data3/WBM_TrANS/spreadsheets/InterBasinWaterTransferDatabase_NoFuture.csv",
                      skip = 7, header=T, sep="\t")


# identify rows in IBT database that match simulation IBTs.  Subset to these rows
ibt.IDs = sub("X", "", colnames(ibt)[2:ncol(ibt)])
ibt.rows = ibt.data$ID %in% ibt.IDs 
ibt.data.sub = as.data.frame(ibt.data[ibt.rows,])

ibt.from.coords = subset(ibt.data.sub, select = c("ID", "STN6.From.Longitude", "STN6.From.Latitude"))
ibt.to.coords   = subset(ibt.data.sub, select = c("ID", "STN6.To.Longitude",   "STN6.To.Latitude"))

# Conver to spatial points data set
coordinates(ibt.from.coords) = ~ STN6.From.Longitude + STN6.From.Latitude
coordinates(ibt.to.coords) = ~ STN6.To.Longitude + STN6.To.Latitude

# harmonize projections
crs(ibt.from.coords) = crs(ibt.to.coords) = crs(shape)

# all IBTs in simulation
plot.nm = "IBT_full_netwk.png"
png(file.path(plot.dir, plot.nm),
    height=1200, width=1600, res=140)
plot(coastline,  xlim = xl, ylim = yl, border='grey70', lwd=1)
plot(basin.shape, lwd=0.8, add=T)
plot(ibt.from.coords, pch = 21, cex=0.7, bg='turquoise3', col="turquoise4", add=T)
plot(ibt.to.coords,   pch = 1, cex=0.3,  col='turquoise4',  add=T)
for(i in 1:length(ibt.to.coords)){
  arrows(x0 = ibt.from.coords@coords[i,1], 
         y0 = ibt.from.coords@coords[i,2], 
         x1 = ibt.to.coords@coords[i,1],
         y1 = ibt.to.coords@coords[i,2], 
         length = 0.06,
         lwd = 1.2,
         col = 'darkblue')
}
dev.off()


### Categorize IBTs
ibt.from.basins = over(ibt.from.coords, shape, fn = NULL)
ibt.to.basins   = over(ibt.to.coords, shape, fn = NULL)

ibt.df = as.data.frame(cbind(ibt.from.coords@data$ID, 
                             ibt.from.basins$Basin_ID, 
                             ibt.to.basins$Basin_ID))
colnames(ibt.df) = c("IBT_ID", "From_ID", "To_ID")

ibt.df[is.na(ibt.df)] = 0
ibt.df$CrossBorder = (ibt.df$From_ID != ibt.df$To_ID)
ibt.df$CrossOut    = (ibt.df$From_ID > 0   &  ibt.df$To_ID == 0)
ibt.df$CrossIn     = (ibt.df$From_ID == 0  &  ibt.df$To_ID > 0)
ibt.df$Out         = (ibt.df$From_ID == 0  &  ibt.df$To_ID == 0)

ibt.df$Col = rep("grey", nrow(ibt.df))
ibt.df$lwd = rep(1.2, nrow(ibt.df))
for(i in 1:nrow(ibt.df)){
  if(ibt.df$CrossBorder[i] == T){
    ibt.df$Col[i] = "blue"
    ibt.df$lwd[i] = 2.5
  }
  
  if(ibt.df$CrossOut[i] == T){
    ibt.df$Col[i] = "darkorange2"
  }else if(ibt.df$CrossIn[i] == T){
      ibt.df$Col[i] = "darkblue"
  }else if(ibt.df$Out[i] == T){
    ibt.df$Col[i] = "grey"
  }
}


plot.nm = "IBT_crossing.png"
png(file.path(plot.dir, plot.nm),
    height=1200, width=1600, res=140)
plot(coastline.shadow, xlim = xl, ylim = yl, border='grey90', lwd=4)
plot(coastline,        xlim = xl, ylim = yl, border='grey70', col='white', lwd=1, add=T)
plot(basin.shape, lwd=0.8, border='grey30', add=T)
plot(ibt.from.coords, pch = 21, cex=0.7, bg='turquoise3', col="turquoise4", add=T)
plot(ibt.to.coords,   pch = 1, cex=0.3,  col='turquoise4',  add=T)
for(i in 1:length(ibt.to.coords)){
  arrows(x0 = ibt.from.coords@coords[i,1], 
         y0 = ibt.from.coords@coords[i,2], 
         x1 = ibt.to.coords@coords[i,1],
         y1 = ibt.to.coords@coords[i,2], 
         length = 0.07,
         angle = 40,
         lwd = ibt.df$lwd[i],
         col = ibt.df$Col[i])
}
dev.off()

