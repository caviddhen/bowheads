#' @title stackAttributes
#' @description stacks and sums all attributes for a given region and season
#' @return returns list of stacked
#' @param season name to give raster files "Summer" or "Winter"
#' @param region "Hudson Bay" or "Eastern Arctic"
#' @param output "years" for list of raster stacks by year or "sums" for sum/average for each year
#' @author David Chen
#' @importFrom  raster raster extent rasterize mask resample crop
#' @export

stackAttributes <- function(region="Eastern Arctic", season="Summer", att_name="CT"){

  ## names for later writing for rasters
  if (region == "Eastern Arctic") {
    reg <- "EA"} else {
      reg <- "HB" }

   ### CURRENTLY ONLY UNTIL 2014 FOR SST

  files <- list.files("data/BASELINE/Hudson Bay/Winter_HB_Output/ct", recursive = TRUE)

  years <- as.character(c(1:length(files)+2005))
  years_list <- sapply(years,function(x) NULL)

  years_sums <- paste0(c(1:length(b)+2005), "_sums")
  sums_list <- sapply(years_sums,function(x) NULL)

for (i in 1:length(years)) {
 ct <- paste0("data/BASELINE/", region, "/", season, "_", reg, "_Output", "/ct/",
                "stack_ct_", tolower(season), "_", tolower(reg), (i+2005),".tif")
 sa <-  paste0("data/BASELINE/", region, "/", season, "_", reg, "_Output", "/sa/",
                    "stack_sa_", tolower(season), "_", tolower(reg), (i+2005),".tif")
 fa <-  paste0("data/BASELINE/", region, "/", season, "_", reg, "_Output", "/fa/",
               "stack_fa_", tolower(season), "_", tolower(reg), (i+2005),".tif")
 sst <-  paste0("data/BASELINE/", region, "/", season, "_", reg, "_Output", "/sst/",
               "stack_sst_", tolower(season), "_", tolower(reg), (i+2005),".tif")

 years_list[[i]] <- stack(ct,sa,fa,sst)
 sums_list[[i]] <- sum(years_list[[i]])

 ## RENAME (in 2 spots) IF INCLUDING ALL 4
writeRaster(years_list[[i]], filename = paste0("data/BASELINE/", region, "/", season, "_", reg, "_Output", "/output_ct_sa_fa/",
                                               "stack_ct_sa_fa_", tolower(season), "_", tolower(reg), (i+2005),".tif"),
                            format="GTiff", overwrite=TRUE)
writeRaster(sums_list[[i]], filename = paste0("data/BASELINE/", region, "/", season, "_", reg, "_Output", "/output_ct_sa_fa/",
                                               "stack_ct_sa_fa_", tolower(season), "_", tolower(reg), (i+2005),".tif"),
            format="GTiff", overwrite=TRUE)

cat(paste0("Year ", i+2005, "all attributes stacked and summed and written to file"))

}
}

