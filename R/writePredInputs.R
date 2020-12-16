#' @title writePredInputs
#' @description Writes stacks and sums all attributes for a given region and season
#' @return returns list of stacked
#' @param season name to give raster files "Summer" or "Winter"
#' @param region "Hudson Bay" or "Eastern Arctic"
#' @param write writes stacks and sums as well as mosaic
#' @author David Chen
#' @importFrom  raster raster extent rasterize mask resample crop
#' @export

writePredInputs <- function(region="Eastern Arctic", season="Summer", write=TRUE){

   ## names for writing for rasters
  if (region == "Eastern Arctic") {
    reg <- "EA"} else {
      reg <- "HB" }

  ## names for writing for rasters
  if (season == "Summer") {
    seas <- "S"} else {
      seas <- "W" }

stacked_sums <- stackSums(region="Eastern Arctic", season="Summer")
sum_sums <- sum(stacked_sums)

path <- ("data/PREDICTION_II/ts_input/mosaic/")

if(write==TRUE){

writeRaster(stacked_sums, filename=paste0(path, "ts_output_", tolower(reg), "_", tolower(seas),"_stack"),format="GTiff", bylayer=FALSE, overwrite = TRUE)

writeRaster(sum_sums, filename=paste0(path, "ts_output_", tolower(reg), "_", tolower(seas),"_sum"),format="GTiff", overwrite = TRUE)
}
}


