# plot_glacier_TS.R  
# Function plot and saves to file glacier runoff, export and storage

# Project: NASA HiMAT
# Danielle S Grogan
# last updated: 2019-07-03

##################################################################################################
plot_glacier_TS = function(plot.dir,  # directory to save the plots
                           gl_roff,   # glacier runoff time series
                           gl_exp,    # glacier export time series
                           gl_stor,   # glacier storage time series
                           years,     # years 
                           plot.nm){  # name for plot (character string)
  
  f1 = file.path(strsplit(plot.dir, "/")[[1]][1])
  if(!file.exists(f1)){
    dir.create(f1)
  }
  if(!file.exists(plot.dir)){
    dir.create(plot.dir)
  }
  
  ### plotting parameters
  roff_col = 'black'
  exp_col  = 'orange'
  stor_col = 'darkgreen'
  
  roff_lty = 1
  exp_lty  = 4
  stor_lty = 2
  
  png(file.path(plot.dir, plot.nm), 
      width = 800, height=600, res=100)
  par(mar=c(7, 5, 1, 2), xpd = TRUE)
  plot(years, 
       gl_roff, 
       ylim=c(min(min(gl_stor), min(gl_exp), min(gl_roff)), 
              max(max(gl_stor), max(gl_exp), max(gl_roff))),
       type='l',
       lwd=1.5,
       col = roff_col,
       lty = roff_lty,
       ylab = expression("Volume" ~km^3 ~ year^-1),
       xlab = "Year",
       bty='n')
  lines(years, 
        gl_exp, 
        lwd=1.5,
        col = exp_col,
        lty = exp_lty)
  lines(years, 
        gl_stor, 
        lwd=1.5,
        col = stor_col,
        lty = stor_lty)
  legend("bottom",
         inset = c(0,-0.3),
         horiz = T,
         #title = "Glacier:",
         title.adj = 0.15,
         legend = c("Runoff", "Export", "Storage"),
         col=c(roff_col, exp_col, stor_col),
         lwd=rep(1.5,3),
         lty=c(roff_lty, exp_lty, stor_lty),
         bty='n')
  dev.off()
}
##################################################################################################
