#' @title readSST
#' @description reads in SST data, matches to canadian ice service arctic regional sea ice chart extent, takes 7-day average
#' @return saves a GTiff file and returns raster of suitable area (1) or not suitable (0)
#' @param data data source to convert
#' @param raster_name name to give raster files
#' @param att_name name of ice attribute to analyse
#' @param season season to analyse
#' @author David Chen
#' @importFrom  raster raster extent rasterize subset
#' @importFrom ncdf4 nc_open ncvar_get

#' @export

readSST <- function(season="Summer", region="Eastern Arctic"){

  ###map of canada
  #data(wrld_simpl)
  #SPDF <- subset(wrld_simpl, NAME=="Canada")
  folder <- "CMEMS"
  files <- list.files(folder)

#make years list to put each year's output
  years <- as.character(c(1:length(files)+2005)) ## starting year 2006
  years_list <- sapply(years,function(x) NULL)


  ## Read in dummy mask: use CIS shape file of 2006 for year and season for cropping extent
  dummy_file <- list.files(file.path("data/BASELINE/", region, season), pattern = "\\.shp$", full.names=T, recursive=T)[1]
  dummy <- readOGR(dummy_file, verbose=F)
  r <- raster(ext=extent(dummy), res=0.05)
  dummy <- rasterize(dummy,r, field=dummy$CT)



## Read each year and subset to season and crop to region selected
for (i in 1:length(files)){

   a <- nc_open(file.path(folder,files[i])) #nc_open just for the names

  t <- brick(file.path(folder,files[i]), varname=names(a$var)) #make big brick with every day

  ##subset to season
  if (season=="Summer"){

  t <- raster::subset(t, which(substr(getZ(t),6,7)%in%c("07","08","09","10","11") |
                              substr(getZ(t),6,10) %in% paste0("06-",c(27:30)) |
                              substr(getZ(t),6,10) %in% paste0("12-",c(1:27))))

  }
  else if (season=="Winter"){

  t <- raster::subset(t, which(substr(getZ(t),6,10) %in% paste0("12-",c(28:31)) |
                       substr(getZ(t),6,7)%in%c("01", "02") |
                       substr(getZ(t),6,10) %in% paste0("03",c(1:15))))
  }

  ## take 7 day averages. Data are exactly 25 weeks in summer and 9 weeks in winter
  indices <- rep(1:floor(length(names(t))/7), each=7)
  if(i %in% seq(3,18, by=4)){ # except for leap years! where simple solution just to make a 8 day week at the end
    indices <- c(indices, 9)}

  t2 <-stackApply(t, indices, fun = mean)

  ## convert from Kelvin to C
  t <- t - 273

  #make New raster cropped exactly to CIS region resolution and extent
    cropped= resample(t, dummy, "bilinear")
    ex = extent(t)
    z = crop(dummy, ex)
    cropped = mask(cropped, z)

    years_list[[i]] <- cropped

    cat(paste0("read SST ncdf file for year ", (i+2005), "!" ))

    }

  return(years_list)

}


