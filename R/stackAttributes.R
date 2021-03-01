#' @title stackAttributes
#' @description stacks normalizes and sums all attributes for a given region and season
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

  # by dividing by themselves we get normalized ones where suitable =1 and not =0
  ct <- stack(paste0(path,"ct/sum_ct_", tolower(season),"_", tolower(reg),year, ".tif"))
  ct_norm <- ct/ct
  sa <- stack(paste0(path,"sa/sum_sa_", tolower(season),"_", tolower(reg),year, ".tif"))
  sa_norm <- sa/sa
  fa <- stack(paste0(path,"fa/sum_fa_", tolower(season),"_", tolower(reg),year, ".tif"))
  fa_norm <- fa/fa
  sst <- stack(paste0(path,"sst/sum_sst_", tolower(season),"_", tolower(reg),year, ".tif"))
  sst_norm <- sst/sst

  all <- stack(matchExtents(list(ct_norm,sa_norm, fa_norm, sst_norm)))

  ### THIS IS NOW DIVIDE BY nlayers in the stack, to give a potential maximum of 1...maybe does not give the correct percentage... ####
  sum_all <- sum(all)/length(names(all))

  if (write==TRUE){
    writeRaster(ct_norm, filename=paste0(path, "ct/", "norm_ct_", tolower(season), "_", tolower(reg), year),format="GTiff", bylayer=FALSE, overwrite = TRUE)
    writeRaster(sa_norm, filename=paste0(path, "sa/", "norm_sa_", tolower(season), "_", tolower(reg), year),format="GTiff", bylayer=FALSE, overwrite = TRUE)
    writeRaster(fa_norm, filename=paste0(path, "fa/", "norm_fa", tolower(season), "_", tolower(reg), year),format="GTiff", bylayer=FALSE, overwrite = TRUE)
    writeRaster(sst_norm, filename=paste0(path, "sst/", "norm_sst", tolower(season), "_", tolower(reg), year),format="GTiff", bylayer=FALSE, overwrite = TRUE)



   writeRaster(all, filename=paste0(path, "output_ct_sa_fa_sst/", "stack_ct_sa_fa_sst_", tolower(season), "_", tolower(reg), year),format="GTiff", bylayer=FALSE, overwrite = TRUE)
  writeRaster(sum_all, filename=paste0(path, "output_ct_sa_fa_sst/", "sum_ct_sa_fa_sst_",  tolower(season), "_", tolower(reg), year),format="GTiff", overwrite = TRUE)
  }
  cat(paste0("All attributes stacked and summed and written to file for year ", year, " ", region, " ", season))

  return(sum_all)

}

