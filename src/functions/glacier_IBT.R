# glacier_IBT.R
# Function to quantify the amount of glacier runoff moved by inter-basin transfers
# Project: NASA HiMAT
# Danielle S Grogan
# Last updated 2019-08-06

library(raster)
library(rgdal)
library(rgeos)

############################################################################################################
glacier_IBT = function(wbm.path,     # path to wbm output
                       ibt.data,     # interbasin transfer database. Must include IDs and Lat/Long
                       shape,        # basin shapefile
                       out.path){
  
  ### test
  wbm.path = "/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/Peak_Storage/ERA_hist"
  shape = basin.shape
  ibt.data = read.delim("/net/nfs/zero/data3/WBM_TrANS/spreadsheets/InterBasinWaterTransferDatabase_NoFuture.csv",
                         skip = 7, header=T, sep="\t")
  ####
  
  # Load the simulation IBT output
  ibt = read.delim(list.files(file.path(wbm.path, "diversion"), full.names=T), header=T, sep="\t")
  
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
  
  
  # plot
  plot(coastline,  xlim = xl, ylim = yl, border='grey70', lwd=1)
  plot(basin.shape, lwd=0.8, add=T)
  for(i in 1:length(ibt.to.coords)){
    arrows(x0 = ibt.from.coords@coords[i,1], 
           y0 = ibt.from.coords@coords[i,2], 
           x1 = ibt.to.coords@coords[i,1],
           y1 = ibt.to.coords@coords[i,2], 
           length = 0.07,
           col = 'darkblue')
  }
  plot(ibt.from.coords, pch = 21, cex=0.5, bg='turquoise3', add=T)
  plot(ibt.to.coords,   pch = 24, cex=0.3, bg='sienna1', add=T)


  # Idenfity which basins are associated with each IBT
  ibt.from.basins = ibt.from.coords[shape,] 
  test = over(ibt.from.coords, shape, fn = NULL)
  
  # loop through each interbasin transfer in the simulation
  for(i in 1:ncol(ibt)-1){
    
  }
}



