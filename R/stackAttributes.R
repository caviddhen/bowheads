#' @title stackAttributes
#' @description stacks and sums all attributes for a given region and season
#' @return returns list of stacked
#' @param season name to give raster files "Summer" or "Winter"
#' @param region "Hudson Bay" or "Eastern Arctic"
#' @param output "years" for list of raster stacks by year or "sums" for sum/average for each year
#' @author David Chen
#' @importFrom  raster raster extent rasterize mask resample crop
#' @export

stackAttributes <- function(region="Eastern Arctic", season="Summer", output="stack"){

ct <-  binaryProcessing(att_name="CT", region=region, season=season, output=output)
sa <-  binaryProcessing(att_name="SA", region=region, season=season, output=output)
fa <-  binaryProcessing(att_name="FA", region=region, season=season, output=output)
sst <-  binaryProcessing(att_name="SST", region=region, season=season, output=output)

for (i in length(ct)){


  }
test <- stack(ct[[1]],sa[[1]],fa[[1]],sst[[1]])

}
