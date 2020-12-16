#' @title matchExtents
#' @description ### Helper function to check and fix slight extent alignment issues, when stacking lists
#' @return list
#' @param data list of rasters
#' @author David M Chen
#' @importFrom raster extent crop resample mask
#' @export

matchExtents <- function(data){

zz <- data[[1]][[1]] #zz is basis extent that all other layers in list will match

  for(i in 2:length(data)) {

    e <- extent(zz)
    r <-data[[i]]
    rc <- crop(r, e)
    if(sum(as.matrix(extent(rc))!=as.matrix(e)) == 4){
      rc <- mask(rc, zz)
    }else{
      rc <- resample(rc,zz)
      rc<- mask(rc, zz)
    }

    data[[i]] <- rc
  }
return(data)
}


