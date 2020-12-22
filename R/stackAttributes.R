#' @title stackAttributes
#' @description stacks and sums all attributes for a given region and season
#' @return returns list of stacked
#' @param season name to give raster files "Summer" or "Winter"
#' @param region "Hudson Bay" or "Eastern Arctic"
#' @param output "years" for list of raster stacks by year or "sums" for sum/average for each year
#' @author David Chen
#' @importFrom  raster raster extent rasterize mask resample crop
#' @export

stackAttributes <- function(region="Eastern Arctic", season="Summer", year = 2006, write=TRUE){

  ## names for later writing for rasters
  if (region == "Eastern Arctic") {
    reg <- "EA"} else {
      reg <- "HB" }

   ### CURRENTLY ONLY UNTIL 2014 FOR SST

  path <- paste0("data/BASELINE/", region, "/", season, "_", reg, "_Output/")

  ct <- stack(paste0(path,"ct/stack_ct_", tolower(season),"_", tolower(reg),year, ".tif"))
  sa <- stack(paste0(path,"sa/stack_sa_", tolower(season),"_", tolower(reg),year, ".tif"))
  fa <- stack(paste0(path,"fa/stack_fa_", tolower(season),"_", tolower(reg),year, ".tif"))
  sst <- stack(paste0(path,"sst/stack_sst_", tolower(season),"_", tolower(reg),year, ".tif"))

  all <- stack(matchExtents(list(ct,sa, fa, sst)))

  ### THIS IS NOW DIVIDE BY nlayers in the stack, to give a potential maximum of 1...

  sum_all <- sum(all)/length(names(all))

  if (write==TRUE){
  writeRaster(all, filename=paste0(path, "output_ct_sa_fa_sst/", "stack_ct_sa_fa_sst_", tolower(season), "_", tolower(reg), year),format="GTiff", bylayer=FALSE, overwrite = TRUE)

  writeRaster(sum_all, filename=paste0(path, "output_ct_sa_fa_sst/", "sum_ct_sa_fa_sst_",  tolower(season), "_", tolower(reg), year),format="GTiff", overwrite = TRUE)
  }
  cat(paste0("All attributes stacked and summed and written to file for year ", year))

  return(sum_all)

}


