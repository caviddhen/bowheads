#' @title stackSums
#' @description  simple helper function to stack the summed all attribute outputs
#' @return factors as numeric
#' @param season name to give raster files "Summer" or "Winter"
#' @param region "Hudson Bay" or "Eastern Arctic"
#' @param attribute attribute to use to write prediction ct sa sst or all
#' @author David C
#' @export

stackSums <- function(region, season, attribute){

  if (region == "Eastern Arctic") {
    reg <- "EA"} else {
      reg <- "HB" }

  if (attribute %in% c("ct", "sa", "fa","sst")){
    attribute = attribute
    pattern = "norm_"} else {
    attribute="output_ct_sa_fa_sst"
    pattern="sum_"
  }

  path <- paste0("data/BASELINE/",region, "/", season, "_", reg,"_Output/", attribute)
  files <- list.files(path, full.names = TRUE, pattern = pattern)

  stacked <- stack(matchExtents(lapply(files, stack)))

  }
