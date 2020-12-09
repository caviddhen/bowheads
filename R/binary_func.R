#' @title binary_func
#' @description returns binary output from .shp files, based on bowhead habitat variables and conditions
#' @return saves a GTiff file and returns raster of suitable area (1) or not suitable (0)
#' @param data data source to convert
#' @param att_name name of ice attribute to analyse
#' @param season season to analyse Summer or Winter
#' @author David Chen
#' @importFrom  raster raster extent rasterize
#' @importFrom ncdf4 nc_open ncvar_get
#' @importFrom sp CRS spTransform
#' @export

binary_func <- function(data, season, att_name){

  if (att_name %in% c("CT", "FA","SA")){

    # reproject
    des <- CRS("+proj=longlat +datum=WGS84")
    data <- spTransform(data, des)


    if (att_name == "CT") {
      data$CT <- as.numeric.factor(data$CT)  # transform factor to numeric
      data <- data[,(names(data) %in% att_name)] # select specific attribute
      r<- raster(ext=extent(data))
      res(r) <- 0.05# create raster
      data_rasterized <- rasterize(data, r, field = data$CT)

      if(season=="Summer") {
        data_rasterized@data@values[data_rasterized@data@values < 67] <- 0
        data_rasterized@data@values[data_rasterized@data@values >= 67] <- 1
        data_rasterized@data@values[data_rasterized@data@values == 99] <- 0
      }
      else if (season=="Winter"){
        data_rasterized@data@values[data_rasterized@data@values == 40] <- 1
        data_rasterized@data@values[data_rasterized@data@values == 50] <- 1
        data_rasterized@data@values[data_rasterized@data@values == 60] <- 1
        data_rasterized@data@values[data_rasterized@data@values == 67] <- 1
        data_rasterized@data@values[data_rasterized@data@values == 34] <- 1
        data_rasterized@data@values[data_rasterized@data@values == 35] <- 1
        data_rasterized@data@values[data_rasterized@data@values == 45] <- 1
        data_rasterized@data@values[data_rasterized@data@values == 46] <- 1
        data_rasterized@data@values[data_rasterized@data@values == 56] <- 1
        data_rasterized@data@values[data_rasterized@data@values < 1] <- 0
        data_rasterized@data@values[data_rasterized@data@values > 1] <- 0
      }

    }

    else if (att_name == "SA") {
      data$SA <- as.numeric.factor(data$SA)
      data <- data[,(names(data) %in% att_name)]
      r <- raster(ncol = 1000, nrow = 1000)
      extent(r) <- extent(data)
      data_rasterized <- rasterize(data, r, field = data$SA)

       if (season == "Summer") {
         data_rasterized@data@values[data_rasterized@data@values == 91] <- 1
         data_rasterized@data@values[data_rasterized@data@values == 55] <- 1
         data_rasterized@data@values[data_rasterized@data@values == 93] <- 1
         data_rasterized@data@values[data_rasterized@data@values < 1] <- 0
         data_rasterized@data@values[data_rasterized@data@values > 1] <- 0
       }
       else if (season == "Winter") {
         data_rasterized@data@values[data_rasterized@data@values == 80] <- 1
         data_rasterized@data@values[data_rasterized@data@values == 81] <- 1
         data_rasterized@data@values[data_rasterized@data@values == 82] <- 1
         data_rasterized@data@values[data_rasterized@data@values == 83] <- 1
         data_rasterized@data@values[data_rasterized@data@values == 84] <- 1
         data_rasterized@data@values[data_rasterized@data@values == 85] <- 1
         data_rasterized@data@values[data_rasterized@data@values == 87] <- 1
         data_rasterized@data@values[data_rasterized@data@values == 88] <- 1
         data_rasterized@data@values[data_rasterized@data@values == 89] <- 1
         data_rasterized@data@values[data_rasterized@data@values < 1] <- 0
         data_rasterized@data@values[data_rasterized@data@values > 1] <- 0
       }
      }

    else if (att_name == "FA") {
      data$FA <- as.numeric.factor(data$FA)
      data <- data[,(names(data) %in% att_name)]
      r <- raster(ncol = 1000, nrow = 1000)
      extent(r) <- extent(data)
      data_rasterized <- rasterize(data, r, field = data$FA)

      if  (season == "Summer") {
        data_rasterized@data@values[data_rasterized@data@values == 22] <- 1
        data_rasterized@data@values[data_rasterized@data@values == 1] <- 1
        data_rasterized@data@values[data_rasterized@data@values == 2] <- 1
        data_rasterized@data@values[data_rasterized@data@values == 3] <- 1
        data_rasterized@data@values[data_rasterized@data@values == 4] <- 1
        data_rasterized@data@values[data_rasterized@data@values == 5] <- 1
        data_rasterized@data@values[data_rasterized@data@values < 1] <- 0
        data_rasterized@data@values[data_rasterized@data@values > 1] <- 0
      }
        else if (season=="Winter") {
          data_rasterized@data@values[data_rasterized@data@values == 2] <- 1
          data_rasterized@data@values[data_rasterized@data@values == 3] <- 1
          data_rasterized@data@values[data_rasterized@data@values == 4] <- 1
          data_rasterized@data@values[data_rasterized@data@values < 1] <- 0
          data_rasterized@data@values[data_rasterized@data@values > 1] <- 0
        }

      } }

  if (att_name =="SST"){

    ### conditions for SST range

    data[] =  ifelse(data[]>2,0, ifelse(data[] < -0.5,0,1))
  data_rasterized <- data

  }
  stack(data_rasterized)
  return(data_rasterized)
}


