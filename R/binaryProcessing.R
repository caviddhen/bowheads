#' @title binaryProcessing
#' @description transforms source files from read-in formats into binary outputs
#' @return returns list of rasters
#' @param season name to give raster files "Summer" or "Winter"
#' @param att_name name of ice attribute to analyse CT FA or SA
#' @param region "Hudson Bay" or "Eastern Arctic"
#' @param output "years" for list of raster stacks by year or "sums" for sum/average for each year
#' @author David Chen
#' @importFrom  raster raster extent rasterize mask resample crop
#' @export


binaryProcessing <- function(att_name="CT", region="Eastern Arctic", season="Summer", output="sums"){

if (att_name %in% c("CT", "SA","FA")){
a <- readCIS(region = region, season=season)
b <- rapply(a, binary_func, season=season, att_name=att_name, how="list")

### Helper function to check and fix slight extent alignment issues

        t <- b[[1]][[1]] #t is basis extent that all other layers in list will match
                for (i in 1:length(b)){
                for(j in 1:length(b[[i]])) {
                        e <- extent(t)
                        r <-b[[i]][[j]]
                        rc <- crop(r, e)
                   if(sum(as.matrix(extent(rc))!=as.matrix(e)) == 0){
                        rc <- mask(rc, t)
                   }else{
                        rc <- resample(rc,t)
                        rc<- mask(rc, t)
                        }

                    b[[i]][[j]] <- rc
        }}

##make output lists for stacks and summed stacks

years <- as.character(c(2006:2014))
years_list <- sapply(years,function(x) NULL)

years_sums <- paste0(c(2006:2014), "_sums")
sums_list <- sapply(years_sums,function(x) NULL)

# do the stack

for(i in 1:length(b)){
years_list[[i]] <- stack(b[[i]])
}
}

#### SST is already in stacks
 if (att_name=="SST"){
                a <- readSST(region=region, season=season)
                years_list <- lapply(a, binary_func, season=season, att_name="SST")
        }

if (output=="years"){

        return(years_list)
        }
else if (output=="sums"){
        for (i in length(b))
        sums_list[[i]] <- sum(years_list[[i]])/length(names(years_list[[i]]))

}



}
