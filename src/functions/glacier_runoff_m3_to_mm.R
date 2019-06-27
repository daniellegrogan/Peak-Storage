# glacier_runoff_m3_to_mm.R
# unit conversion: glacier runoff in m3/time to mm/time

############################################################################################################
glacier_runoff_m3_to_mm = function(glacier.runoff, out.path = NA, out.nm, overwrite=F){
  km2_to_m2 = 1e6
  m_to_mm = 1e3
  area.glacier.grid = km2_to_m2*raster::area(glacier.runoff) # unit: m2
  
  if(!is.na(out.path)){
    glacier.runoff.mm = overlay(glacier.runoff, 
                                area.glacier.grid,
                                fun = function(x,y){return(m_to_mm*(x/y))},
                                filename = file.path(out.path, out.nm),
                                overwrite = overwrite) # unit: mm/yr
    
  }else{
    glacier.runoff.mm = overlay(glacier.runoff, 
                                area.glacier.grid,
                                fun = function(x,y){return(m_to_mm*(x/y))}) # unit: mm/yr
  }
  return = glacier.runoff.mm
}
############################################################################################################