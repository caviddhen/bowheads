#' @title predictFutureHabitat
#' @description  reads and writes rasters cropped future and masked present, Predict suitable habitat under climate change
#' @return returns raster
#' @param year "Present", "2050", or "2100
#' @param rcp "RCP45" or "RCP85" ONLY relevant if not year not "Present"
#' @author David Chen
#' @importFrom  raster raster stack mask crop resample
#' @export

predictFutureHabitat <- function(year, rcp, season="Summer"){

  ## names for writing for rasters
  if (season == "Summer") {
    seas <- "s"} else {
      seas <- "w" }

pred_path <- "data/PREDICTION_II/suit_hab_future_env_sep/"
mask_path <- "data/PREDICTION_II/mask/"
crop_path <- "data/PREDICTION_II/crop_future_env_sep/"

futuretemp_cropped <- raster(paste0(crop_path,"futuretemp_",substr(rcp, 4,5),"_", year,"_cropped.tif"))
presenttemp_masked <- raster(paste0(mask_path,"presenttemp_", tolower(season), "_masked.tif"))

suit_hab_temp <- futuretemp_cropped<maxValue(presenttemp_masked)
suit_hab_temp <- suit_hab_temp*futuretemp_cropped
suit_hab_temp <- mask(futuretemp_cropped, suit_hab_temp, maskvalue=0)
suit_hab_temp <- crop(suit_hab_temp, presenttemp_masked)
suit_hab_temp <- mask(suit_hab_temp, presenttemp_masked, maskvalue=0)
#plot(suit_hab_temp)

futurethick_cropped <- raster(paste0(crop_path,"futurethick_",substr(rcp, 4,5),"_", year,"_cropped.tif"))
presentthick_masked <- raster(paste0(mask_path,"presentthick_", tolower(season), "_masked.tif"))

suit_hab_thick <- futurethick_cropped<maxValue(presentthick_masked)
suit_hab_thick <- suit_hab_thick*futurethick_cropped
suit_hab_thick <- mask(futurethick_cropped, suit_hab_thick, maskvalue=0)
suit_hab_thick <- crop(suit_hab_thick, presentthick_masked)
suit_hab_thick <- mask(suit_hab_thick, presentthick_masked)
#plot(suit_hab_thick)


#make raster of both as masks overlayed
temp_m <- suit_hab_temp/suit_hab_temp
thick_m <- suit_hab_thick/suit_hab_thick

suit_hb_both <- temp_m + thick_m

writeRaster(suit_hab_temp, filename = paste0(pred_path,"suit_hab_", substr(rcp, 4,5),"_", year, "_temp_", seas), format = "GTiff", overwrite = TRUE)
writeRaster(suit_hab_thick, filename = paste0(pred_path,"suit_hab_", substr(rcp, 4,5),"_", year, "_thick_", seas), format = "GTiff", overwrite = TRUE)
writeRaster(suit_hb_both, filename= paste0(pred_path,"suit_hab_", substr(rcp, 4,5),"_", year, "_both_", seas), format = "GTiff", overwrite = TRUE)

## Mask already has somewhat larger extent,,, from ArcGIS processing??? Compar suit_hab_temp and thick to tt and ttt below
# massk <- raster("D:/PREDICTION_II/mask/presenttemp_summer_masked.tif")
# plot(massk)
#
# plot(futuretemp_cropped)
# ttt <- raster("D:/PREDICTION_II/crop_future_env_sep/futuretemp_45_100_cropped.tif")
#
# tt <- raster("D:/PREDICTION_II/suit_hab_future_env_sep/suit_hab_45_100_temp.tif")
#
# ttt <- raster("D:/PREDICTION_II/suit_hab_future_env_sep/suit_hab_45_100_thick.tif")
#

}
