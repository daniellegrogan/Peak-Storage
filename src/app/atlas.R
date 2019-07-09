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
