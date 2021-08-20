# https://csaybar.github.io/blog/2018/12/05/resample/

#' Resampling a Raster* object via GDAL
#' 
#' @param r Raster* object to be resampled
#' @param r_base Raster* object with parameters that r should be resampled to.
#' @param method Character. GDAL resampling_method
#' ("near"|"bilinear"|"cubic"|"cubicspline"|
#'  "lanczos"|"average"|"mode"|"max"|"min"|
#'  "med"|"q1"|"q3")
#'
#' @export
gdal_resample <- function(r, r_base, method = 'bilinear') {
  
  #Geometry attributes
  t1 <- c(xmin(r_base), ymin(r_base), 
          xmax(r_base), ymax(r_base))
  res <- res(r_base)
  
  #Temporal files
  tmp_outname <- sprintf('%s.tif', tempfile())
  tmp_inname <- sprintf('%s.tif', tempfile())
  writeRaster(r, tmp_inname)
  
  #GDAL time!
  gdalwarp(tmp_inname, tmp_outname, 
           tr = res, te = t1, r = method)
  resample_raster = raster(tmp_outname)
  
  return(resample_raster)
}