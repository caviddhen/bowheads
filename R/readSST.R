#' @title readSST
#' @description reads in SST data, matches to canadian ice service arctic regional sea ice chart extent, takes 7-day average
#' @return saves a GTiff file and returns raster of suitable area (1) or not suitable (0)
#' @param binary convert to binary
#' @param season season to analyse
#' @param write write raster to file
#' @param year to process
#' @author David Chen
#' @importFrom  raster raster extent rasterize subset
#' @importFrom ncdf4 nc_open ncvar_get

#' @export

readSST <- function(season="Summer", region="Eastern Arctic", year, binary=TRUE, write=TRUE){

  ## names for later writing for rasters
  if (region == "Eastern Arctic") {
    reg <- "EA"} else {
      reg <- "HB" }

  ###map of canada
  #data(wrld_simpl)
  #SPDF <- subset(wrld_simpl, NAME=="Canada")
  folder <- "CMEMS"
  file <- list.files(folder, pattern=as.character(year))


  ## Read in dummy mask: use CIS shape file of 2006 for year and season for cropping extent
  dummy_file <- list.files(file.path("data/BASELINE", region, season), pattern = "\\.shp$", full.names=T, recursive=T)[1]
  dummy <- readOGR(dummy_file, verbose=F)
  r <- raster(ext=extent(dummy), res=0.05)
  dummy$CT <- as.factor(dummy$CT)
  dummy <- rasterize(dummy,r, field=dummy$CT)

## Read each year and subset to season and crop to region selected

   a <- nc_open(file.path(folder,file)) #nc_open just for the names

  data <- brick(file.path(folder,file), varname=names(a$var)) #make big brick with every day

  ##subset to season
  if (season=="Summer"){

  data <- raster::subset(data, which(substr(getZ(data),6,7)%in%c("07","08","09","10","11") |
                              substr(getZ(data),6,10) %in% paste0("06-",c(27:30)) |
                              substr(getZ(data),6,10) %in% paste0("12-",c(1:27))))

  }
  else if (season=="Winter"){

    data <- raster::subset(data, which(substr(getZ(data),6,10) %in% paste0("12-",c(28:31)) |
                       substr(getZ(data),6,7)%in%c("01", "02") |
                       substr(getZ(data),6,10) %in% paste0("03",c(1:15))))
  }

  ## take 7 day averages. Data are exactly 25 weeks in summer and 9 weeks in winter
  indices <- rep(1:floor(length(names(data))/7), each=7)
  if(year %in% seq(2008,2012, by=4)){ # except for leap years! where simple solution just to make a 8 day week at the end
    indices <- c(indices, indices[length(indices)])}

  data <-stackApply(data, indices, fun = mean)

  ## convert from Kelvin to C
  data <- data - 273

  #make New raster cropped exactly to CIS region resolution and extent
  data= resample(data, dummy, "bilinear")
    ex = extent(data)
    z = crop(dummy, ex)
    data = mask(data, z)

    cat(paste0("read SST ncdf file for year ", year, "!" ))

if (binary==TRUE){
  data <- binary_func(data, att_name = "SST", season=season)
  data_sum <- sum(data)/length(names(data))

}

if(write==TRUE){
  writeRaster(data, filename= paste0("data/BASELINE/", region, "/", season, "_", reg,
                                     "_Output/sst/stack_sst_", tolower(season), "_", tolower(reg),
                                    year),
              bylayer=FALSE,format="GTiff", overwrite=TRUE)

  writeRaster(data_sum, filename= paste0("data/BASELINE/", region, "/", season, "_", reg,
                                         "_Output/sst/sum_sst_", tolower(season), "_", tolower(reg),
                                    year),
               format="GTiff", overwrite=TRUE)

  cat("Stacks and sums written to file!")
}
  return(data)

}


