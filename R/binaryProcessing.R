#' @title binaryProcessing
#' @description transforms source files from read-in formats into binary outputs
#' @return returns list of rasters
#' @param season name to give raster files "Summer" or "Winter"
#' @param att_name name of ice attribute to analyse CT FA or SA
#' @param region "Hudson Bay" or "Eastern Arctic"
#' @author David Chen
#' @importFrom  raster extent rasterize mask resample crop writeRaster
#' @export


binaryProcessing <- function(att_name="CT", region="Eastern Arctic", season="Summer"){

        ## names for later writing for rasters
        if (region == "Eastern Arctic") {
                reg <- "EA"} else {
                reg <- "HB" }

### CT SA and FA
if (att_name %in% c("CT", "SA","FA")){

        b <- readCIS(region = region, season=season)
        b <- rapply(b, binary_func, season=season, att_name=att_name, how="list")

        ### Helper function to check and fix slight extent alignment issues

        zz <- b[[1]][[1]] #zz is basis extent that all other layers in list will match

                for (i in 1:length(b)){

                for(j in 1:length(b[[i]])) {

                        e <- extent(zz)
                        r <-b[[i]][[j]]
                        rc <- crop(r, e)
                   if(sum(as.matrix(extent(rc))!=as.matrix(e)) == 4){
                        rc <- mask(rc, zz)
                   }else{
                        rc <- resample(rc,zz)
                        rc<- mask(rc, zz)
                        }

                    b[[i]][[j]] <- rc
                }
                }

        ##make output lists for stacks and summed stacks
        years <- as.character(c(1:length(b)+2005))
        years_list <- sapply(years,function(x) NULL)

        years_sums <- paste0(c(1:length(b)+2005), "_sums")
        sums_list <- sapply(years_sums,function(x) NULL)

        ##### do the stack

         for(i in 1:length(b)){
        years_list[[i]] <- stack(b[[i]])
        }
        }


else if (att_name=="SST"){ #### SST is already in stacks so just read and transform to binary

               b <- readSST(region=region, season=season)
               years_list <- lapply(b, binary_func, season=season, att_name="SST")
}

### Write yearly stack and summed


                years_sums <- paste0(c(1:length(b)+2005), "_sums")
                sums_list <- sapply(years_sums,function(x) NULL)

                for (i in 1:length(sums_list)){
                sums_list[[i]] <- sum(years_list[[i]])/length(names(years_list[[i]]))
                }


  for (i in 1:length(years_list)){
                writeRaster(years_list[[i]], filename= paste0("data/BASELINE/", region, "/", season, "_", reg, "_Output/", tolower(att_name),
                                                     "/stack_", tolower(att_name), "_", tolower(season), "_", tolower(reg),
                                                     i+2005),
                            bylayer=FALSE,format="GTiff", overwrite=TRUE)

          cat(paste0("stack_", tolower(att_name), "_", tolower(season), "_", tolower(reg),
                     i+2005, " written to file!"))
  }



                for (i in 1:length(sums_list)){
                        writeRaster(sums_list[[i]], filename= paste0("data/BASELINE/", region, "/", season, "_", reg, "_Output/", tolower(att_name),
                                                                  "/sum_", tolower(att_name), "_", tolower(season), "_", tolower(reg),
                                                                  i+2005),
                                    bylayer=FALSE,format="GTiff", overwrite=TRUE)
                        cat(paste0("sum", "_", tolower(att_name), "_", tolower(season), "_", tolower(reg),
                                   i+2005, " written to file!"))
                }


}
