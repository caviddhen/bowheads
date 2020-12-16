#' @title stackSums
#' @description  simple helper function to stack the summed all attribute outputs
#' @return factors as numeric
#' @param season name to give raster files "Summer" or "Winter"
#' @param region "Hudson Bay" or "Eastern Arctic"
#' @author David C
#' @export

stackSums <- function(region, season) {

  if (region == "Eastern Arctic") {
    reg <- "EA"} else {
      reg <- "HB" }

  path <- paste0("data/BASELINE/",region, "/", season, "_", reg,"_Output/output_ct_sa_fa_sst/")
  files <- list.files(path, full.names = TRUE, pattern = "sum_")

  stacked <- stack(matchExtents(lapply(files, stack)))

  }
