#' @title readCIS
#' @description reads in Ice data
#' @return returns rasters
#' @param season name to give raster files "Summer" or "Winter"
#' @param region "Hudson Bay" or "Eastern Arctic"
#' @author David Chen
#' @importFrom  raster raster extent rasterize
#' @importFrom ncdf4 nc_open ncvar_get
#' @importFrom sp CRS spTransform
#' @importFrom rgdal readOGR
#' @export

readCIS <- function(region="Hudson Bay", season="Summer"){

filepath <- file.path("data", "BASELINE", region, season)

#datasource <- "C:/PIK/BASELINE/data/Hudson Bay/Summer/"

years <- list.dirs(filepath, recursive = FALSE)
years_list <- sapply(years,function(x) NULL)

for (year in years){
  files <- list.files(year, recursive = FALSE, pattern = "\\.shp$", full.names = TRUE)

    # read original shapefile
  ice_data <- lapply(files, readOGR, verbose=FALSE)

years_list[[year]] <- ice_data
cat(year, "done!")
}
return(years_list)
}
