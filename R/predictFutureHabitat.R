#' @title predictFutureHabitat
#' @description  reads and writes rasters cropped future and masked present, Predict suitable habitat under climate change
#' @return returns raster
#' @param year "Present", "2050", or "2100
#' @param rcp "RCP45" or "RCP85" ONLY relevant if not year not "Present"
#' @param attribute st ca fa sst all
#' @author David Chen
#' @importFrom  raster raster stack mask crop resample
#' @export

predictFutureHabitat <- function(year, rcp, season="Summer", attribute){

  ## names for writing for rasters
  if (season == "Summer") {
    seas <- "s"} else {
      seas <- "w" }

pred_path <- "data/PREDICTION_II/suit_hab_future_env_sep/"
mask_path <- "data/PREDICTION_II/mask/"
crop_path <- "data/PREDICTION_II/crop_future_env_sep/"

## SST
futuretemp_cropped <- raster(paste0(crop_path,"futuretemp_",substr(rcp, 4,5),"_", year,"_cropped.tif"))
presenttemp_masked <- raster(paste0(mask_path,"present", "_", attribute, "_temp_", tolower(season), "_masked.tif"))

suit_hab_temp_max <- futuretemp_cropped<maxValue(presenttemp_masked)
suit_hab_temp_min <- futuretemp_cropped>minValue(presenttemp_masked)

suit_hab_temp <- (suit_hab_temp_max+suit_hab_temp_min)
suit_hab_temp[which(suit_hab_temp[]==1|suit_hab_temp[]==0)] <-0
suit_hab_temp <- suit_hab_temp/suit_hab_temp

suit_hab_temp <- suit_hab_temp*futuretemp_cropped

suit_hab_temp <- mask(futuretemp_cropped, suit_hab_temp, maskvalue=0)


### ICE THICKNESS
futurethick_cropped <- raster(paste0(crop_path,"futurethick_",substr(rcp, 4,5),"_", year,"_cropped.tif"))
presentthick_masked <- raster(paste0(mask_path,"present", "_", attribute, "_thick_", tolower(season), "_masked.tif"))

suit_hab_thick_max <- futurethick_cropped<maxValue(presentthick_masked)
suit_hab_thick_min <- futuretemp_cropped>minValue(presenttemp_masked)

suit_hab_thick <- (suit_hab_thick_max+suit_hab_thick_min)
suit_hab_thick[which(suit_hab_thick[]==1|suit_hab_thick[]==0)] <-0
suit_hab_thick <- suit_hab_thick/suit_hab_thick

suit_hab_thick <- suit_hab_thick*futurethick_cropped

suit_hab_thick <- mask(futurethick_cropped, suit_hab_thick)


#plot(suit_hab_thick)


#make raster of both as masks overlayed
temp_m <- suit_hab_temp/suit_hab_temp
thick_m <- suit_hab_thick/suit_hab_thick

suit_hab_both <- temp_m + thick_m

writeRaster(suit_hab_temp, filename = paste0(pred_path,"suit_hab_", substr(rcp, 4,5),"_", year, "_", attribute, "_temp_", seas), format = "GTiff", overwrite = TRUE)
writeRaster(suit_hab_thick, filename = paste0(pred_path,"suit_hab_", substr(rcp, 4,5),"_", year, "_", attribute,"_thick_", seas), format = "GTiff", overwrite = TRUE)
writeRaster(suit_hab_both, filename= paste0(pred_path,"suit_hab_", substr(rcp, 4,5),"_", year,"_", attribute, "_both_", seas), format = "GTiff", overwrite = TRUE)


}
