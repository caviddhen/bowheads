#' @title writeMosaic
#' @description Writes mosaics for both seasons
#' @return returns list of stacked
#' @param season name to give raster files "Summer" or "Winter"
#' @param region "Hudson Bay" or "Eastern Arctic"
#' @param write writes stacks and sums as well as mosaic
#' @param extend TRUE extends to make a mosaic of full extent of EA and HB
#' @author David Chen
#' @importFrom  raster raster extent rasterize mask resample crop
#' @export

writeMosaic <- function(season="Summer", write=TRUE, extend=TRUE){

  ## names for writing for rasters
  if (season == "Summer") {
    seas <- "S"} else {
      seas <- "W" }

path <- ("data/PREDICTION_II/ts_input/mosaic/")

ea <- raster(paste0(path, "ts_output_ea_", seas, "_stack.tif"))
hb <- raster(paste0(path, "ts_output_hb_", seas, "_stack.tif"))

### WIthout extend the  resample makes the HB data only the tiny bit that falls into the extent of EA
### meaning mosaic is only EA extent
if (extend==TRUE){
  ea <- extend(ea, extent(hb), value=NA)
  hb <- extend(hb, extent(ea), value=NA)
}

hb <- resample(hb, ea, method="bilinear")
mosaic_ea <- mosaic(ea, hb, fun=mean)
mosaic_ea <- mosaic_ea/mosaic_ea

writeRaster(mosaic_ea, filename=paste0(path,"mosaic_", tolower(season)), format="GTiff", overwrite=TRUE)
cat(paste0(season, "mosaic raster written"))


mosaic_poly<- rasterToPolygons(mosaic_ea, dissolve=T)
raster::shapefile(mosaic_poly, filename=paste0(path,"mosaic_poly_", tolower(season)), overwrite=TRUE)

cat(paste0(season, "mosaic shp file written"))


}
