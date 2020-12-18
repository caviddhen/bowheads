#' @title readBioOracle
#' @description reads and writes rasters cropped future and masked present
#' @return returns raster
#' @param year "Present", "2050", or "2100
#' @param rcp "RCP45" or "RCP85" ONLY relevant if not year not "Present"
#' @author David Chen
#' @importFrom  raster raster stack mask crop resample
#' @export

calcBioOracle <- function(year, rcp=NULL, season="Summer"){

  if (year == 2050 || year == 2100) {
    year_m <- paste0(year,"AOGCM")
    rcp <- paste0(".",rcp)
  } else if (year== "Present") {
               year_m <- year
               rcp <- NULL
               } else { stop("Not a valid bio-oracle time. Options are \"Present\", 2050, or 2100")}

  path <- ("data/PREDICTION/bio-oracle_input/")

  temp <- raster(paste0(path,year_m, rcp, ".Surface.Temperature.Mean.tif"))
  thick <- raster(paste0(path, year_m, rcp, ".Surface.Ice.thickness.Mean.tif"))

  # crop to mosaic

  mosaic_path <- "data/PREDICTION_II/ts_input/mosaic/"
  mosaic_r <- stack(paste0(mosaic_path,"mosaic", "_", tolower(season), ".tif"))

  temp_cropped <- crop(temp, mosaic_r)
  thick_cropped <- crop(thick, mosaic_r)

  if (year == 2050 || year == 2100) {
    crop_path <- "data/PREDICTION_II/crop_future_env_sep/"
  } else {
  crop_path <- "data/PREDICTION_II/crop/" }

  writeRaster(temp_cropped, filename = paste0(crop_path,"futuretemp_",substr(rcp, 5,6),"_", year,"_cropped"), format = "GTiff", overwrite = TRUE)
  writeRaster(thick_cropped, filename = paste0(crop_path,"futurethick_",substr(rcp, 5,6),"_", year,"_cropped"), format = "GTiff", overwrite = TRUE)

  if(year=="Present"){
  #### Mask

  mask_path <- "data/PREDICTION_II/mask/"

  # need to reproject mosaic to 1/12 degree resolution of BioOracle data
  mosaic_r <- resample(mosaic_r, temp_cropped,  method="bilinear")

  temp_masked <- mask(temp_cropped, mosaic_r)
  thick_masked <- mask(thick_cropped, mosaic_r)

  writeRaster(temp_masked, filename = paste0(mask_path, tolower(year),"temp_",tolower(season),"_masked"), format = "GTiff", overwrite = TRUE)
  writeRaster(thick_masked, filename = paste0(mask_path, tolower(year),"thick_",tolower(season),"_masked"), format = "GTiff", overwrite = TRUE)
  }
}
