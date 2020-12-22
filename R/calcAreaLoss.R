#' @title calcAreaLoss
#' @description  Calculates loss in area in sq.km from current habitat to climate change scenario
#' @return returns scalar
#' @param year "2050", or "2100
#' @param rcp "RCP45" or "RCP85"
#' @param season "Summer" or "Winter"
#' @author David Chen
#' @importFrom  raster area sum
#' @export


calcAreaLoss <- function(year, rcp="RCP45", season="Summer"){

  ## names for reading of rasters
  if (season == "Summer") {
    seas <- "s"} else {
      seas <- "w" }

mosaic_path <- "data/PREDICTION_II/ts_input/mosaic/"
current <- raster(paste0(mosaic_path, "mosaic_",season, ".tif"))

current_area <- tapply(area(current), current[], sum)

future_path <- "data/PREDICTION_II/suit_hab_future_env_sep/"

future_temp <- raster(paste0(future_path, "suit_hab_", substr(rcp,4,5), "_", year, "_temp_", seas,".tif"))
future_thick <- raster(paste0(future_path, "suit_hab_", substr(rcp,4,5), "_", year, "_thick_", seas,".tif"))
future_both  <- raster(paste0(future_path, "suit_hab_", substr(rcp,4,5), "_", year, "_both_", seas,".tif"))


future_area_temp <- sum(tapply(area(future_temp), future_temp[], sum))
future_area_thick <- sum(tapply(area(future_thick), future_thick[], sum))
future_area_both <- sum(tapply(area(future_both), future_both[], sum))


temp_diff <- current_area - future_area_temp
temp_perc <- future_area_temp/current_area * 100

thick_diff <- current_area - future_area_thick
thick_perc <- future_area_thick/current_area * 100


both_diff <- current_area - future_area_both
both_perc <- future_area_both/current_area * 100

df <- data.frame(row.names = c(paste0(c("SST_", "Ice thickness_", "Both_"), rcp, "_", year)))
df[1,1] <- temp_diff
df[1,2] <- temp_perc
df[2,1] <- thick_diff
df[2,2] <- thick_perc
df[3,1] <- both_diff
df[3,2] <- both_perc

colnames(df) <- c("km2", "percentage of current habitat")


print(paste0("future habitat under ", rcp, " in year ", year, " for ", season, " will be ", round(temp_diff,0), " km2 less based on future SST."))
print(paste0("Future Habitat is ",round(temp_perc,2), "% of current habitat based on SST"))

print(paste0("future habitat under ", rcp, " in year ", year, " for ", season, " will be ", round(thick_diff,0), " km2 less based on future Ice Thickness"))
print(paste0("Future Habitat is ",round(thick_perc,2), "% of current habitat based on ice thickness."))


print(paste0("future habitat under ", rcp, " in year ", year, " for ", season, " will be ", round(both_diff,0), " km2 less based on future both SST and Ice Thickness"))
print(paste0("Future Habitat is ",round(both_perc,2), "% of current habitat based on both SST and ice thickness."))

return(df)

}

