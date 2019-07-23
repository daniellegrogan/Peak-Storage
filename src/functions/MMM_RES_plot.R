# MMM_RES_plot.R
# RES := Runoff, Export, Storage

# Plots results from glacier analysis
# Plots time series of runoff and export, and cumulative storage, on same plot
# Shows each GCM and the multi-model mean (differs from glacier_RES_plots.R, which only shows one GCM simulation)

# Project: NASA HiMAT
# Danielle S Grogan
# last updated: 2019-07-23

MMM_RES_plot =  function(gl_runoff,   # glacier runoff time series (km3/yr), one model per row
                         storage_c,   # cumulative glacier storage time series (km3), one model per row
                         gl_exp,      # glacier export time series (km3/yr), one model per row
                         plot.dir,    # directory to write the plot
                         plot.nm){    # name of plot
  
  # Calculate multi-model means
  gl_runoff.mmm = colMeans(gl_runoff)
  gl_storage.mmm = colMeans(storage_c)  # cumulative
  gl_exp.mmm = colMeans(gl_exp)
  
  ### plotting parameters
  roff_col = 'black'
  exp_col  = 'orange'
  stor_col = 'darkgreen'
  
  roff_lty = 1
  exp_lty  = 4
  stor_lty = 2
  
  png(file.path(plot.dir, plot.nm),
      width = 800, height=600, res=100)
  for(i in 1:nrow(gl_runoff)){
    if(i == 1){
      par(mar=c(7, 5, 2, 5), xpd = TRUE)
      plot(years, 
           gl_runoff[i,], 
           ylim=c(0, max(max(gl_runoff), max(gl_exp))),
           type='l',
           lwd=0.8,
           col = adjustcolor(roff_col, alpha=0.5),
           lty = roff_lty,
           ylab = expression("Volume" ~(km^3 ~ year^-1)),
           xlab = "Year",
           bty='n')
      
      lines(years, 
            gl_exp[i,], 
            lwd=0.8,
            col = adjustcolor(exp_col, alpha=0.7),
            lty = exp_lty)
    }else{
      lines(years, 
            gl_runoff[i,], 
            lwd=0.8,
            col = adjustcolor(roff_col, alpha=0.5),
            lty = roff_lty)
      
      lines(years, 
            gl_exp[i,], 
            lwd=0.8,
            col = adjustcolor(exp_col, alpha=0.7),
            lty = exp_lty)
    }
  }
  # plot Multi-model means
  lines(years,
        gl_runoff.mmm,
        lwd = 2,
        col = roff_col,
        lty = roff_lty)
  lines(years,
        gl_exp.mmm,
        lwd = 2,
        col = exp_col,
        lty = exp_lty)
  
  for(i in 1:nrow(gl_runoff)){
    par(new = T)
    if(i == 1){
      plot(years, 
           storage_c[i,],
           ylim=c(0, ceiling(max(storage_c))),
           type='l',
           lwd=0.8,
           col = adjustcolor(stor_col, alpha=0.5),
           lty = stor_lty,
           axes = F,
           xlab = NA,
           ylab = NA)
      axis(side = 4, col = stor_col, col.ticks = stor_col, col.axis = stor_col)
      mtext(side = 4, line = 3, expression("Cumulative Storage Volume" ~(km^3)), col='darkgreen')
    }else{
      lines(years,
            storage_c[i,],
            lwd=0.8,
            col = adjustcolor(stor_col, alpha=0.5),
            lty = stor_lty)
    }
  }
  # plot Multi-model mean
  lines(years,
        gl_storage.mmm,
        lwd = 2,
        col = stor_col,
        lty = stor_lty)
  
  legend("bottom",
         inset = c(0,-0.3),
         horiz = T,
         #title = "Glacier:",
         title.adj = 0.15,
         legend = c("Runoff", "Export", "Cumulative Storage"),
         col=c(roff_col, exp_col, stor_col),
         lwd=rep(1.5,3),
         lty=c(roff_lty, exp_lty, stor_lty),
         bty='n')
  dev.off()
}    

