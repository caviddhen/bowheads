#' @title readCIS
#' @description reads in Ice data
#' @return returns rasters
#' @param season name to give raster files "Summer" or "Winter"
#' @param region "Hudson Bay" or "Eastern Arctic"
#' @param binary TRUE or FALSE if TRUE this converts using binary_func into binary raster stack. TRUE returns list of stacks by variable
#' @param write the raster to file as both stack and sum
#' @author David Chen
#' @importFrom  raster raster extent rasterize
#' @importFrom ncdf4 nc_open ncvar_get
#' @importFrom rgdal readOGR
#' @export

readCIS <- function(region="Hudson Bay", season="Summer", year=2006, binary = TRUE, write=TRUE){

     ## names for later writing for rasters
     if (region == "Eastern Arctic") {
          reg <- "EA"} else {
          reg <- "HB" }

filepath <- file.path("data", "BASELINE", region, season)
folder <- list.files(filepath, recursive = FALSE, pattern=as.character(year), full.names=TRUE)

files <- list.files(folder, recursive = FALSE, pattern = "\\.shp$", full.names = TRUE)

    # read original shapefile
data <- lapply(files, readOGR, verbose=FALSE)

cat("Done reading CIS shp file for ", year, " ", region, " ", season, "!")

att_list <- paste0(tolower(c("CT", "SA", "FA")), "_", as.character(year)) ## list of attributes to store
att_list <- sapply(att_list,function(x) NULL)


if (binary ==TRUE){

      for (att in c("CT", "SA", "FA")){

      bin <- lapply(data, binary_func, att_name = att, season = season)
      bin <- matchExtents(bin) # match tiny differences in extents
      bin <- stack(bin)

      sum <- sum(bin)/length(names(bin))

       if (write==TRUE){
            writeRaster(bin, filename= paste0("data/BASELINE/", region, "/", season, "_", reg, "_Output/", tolower(att),
                                                  "/stack_", tolower(att), "_", tolower(season), "_", tolower(reg),
                                                   year),
                        bylayer=FALSE,format="GTiff", overwrite=TRUE)

            writeRaster(sum, filename= paste0("data/BASELINE/", region, "/", season, "_", reg, "_Output/", tolower(att),
                                        "/sum_", tolower(att), "_", tolower(season), "_", tolower(reg),
                                       year),
                        format="GTiff", overwrite=TRUE)
  }

  att_list[[paste0(tolower(att),"_", year)]] <- bin
}

  cat("Done transforming CIS file into 3 binary raster stacks for CT SA FA ", year, " ", region, " ", season, "!")
 if (write==TRUE){ cat("Written to file!")}

return(att_list)

}


else { ## if binary is false only read the OGR data shape file
  if (write==TRUE){ # no writing to file if no binary
    stop("Binary must be TRUE to write data")}
  else{
    return(data)
  }

}
}
