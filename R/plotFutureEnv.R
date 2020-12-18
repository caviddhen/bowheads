#' @title plotFutureEnv
#' @description makes future environment plots
#' @return returns ggplot
#' @param year "Present", "2050", or "2100
#' @param rcp "RCP45" or "RCP85"
#' @param season "Summer" or "Winter
#' @param write saves plots to file
#' @author David Chen
#' @importFrom  ggplot2 ggplot
#' @importFrom  raster as.data.frame
#' @importFrom rnaturalearth ne_countries
#' @importFrom sf st_crop
#' @export

plotFutureEnv <- function(year, rcp="RCP45", season="Summer", write=TRUE){

  ## names for writing for rasters
  if (season == "Summer") {
    seas <- "s"} else {
      seas <- "w" }

  # Base Maps ------------------------------------------------------------

  path <- ("data/PREDICTION_II/ts_input/mosaic/")
  mosaic_summer <- readOGR(paste0(path, "mosaic_poly_summer.shp"))

  base_map <- ggplot() +
    geom_polygon(data=mosaic_summer, aes(x = long, y = lat, group = group), fill = "#E1C9F5")

  ## Suit Hab rasters maps on top -----------------------

  path <- "data/PREDICTION_II/suit_hab_future_env_sep/"

  temp <- raster(paste0(path, "suit_hab_", substr(rcp,4,5), "_", year, "_temp_", seas,".tif"))
  thick <- raster(paste0(path, "suit_hab_", substr(rcp,4,5), "_", year, "_thick_", seas,".tif"))
  both <-  raster(paste0(path, "suit_hab_", substr(rcp,4,5), "_", year, "_both_", seas,".tif"))


  temp_df <- raster::as.data.frame(temp, xy=TRUE, na.rm=TRUE)
  thick_df <-raster::as.data.frame(thick, xy=TRUE, na.rm=TRUE)
  both_df <-  raster::as.data.frame(both, xy=TRUE, na.rm=TRUE)

if (year==2050){
  year_range <- "2040-2050"} else{
    year_range <- "2090-2100"
  }

# create plots ## WHERE DOES X2100AOGCM.RCP45.Surface.Temperature.Mean come from??
# summer
# future environment based on the 4.5 concentration scenario for 2050

  world <- ne_countries(scale = "medium", returnclass = "sf")

suit_hab_temp_map <- base_map +
  geom_sf(data=world, fill = "antiquewhite1") +
  coord_sf(xlim = c(-115, -50), ylim = c(60,83), expand = FALSE)+
  geom_tile(data = temp_df, aes(x=x, y=y, fill = temp_df[,3])) +
  scale_fill_gradient(low = "green", high = "red", breaks=c(-2,0,2,4), limits=c(-2, 4)) +
  labs(fill = "SST (C)",
       x = "lon",
       y = "lat",
       title = "", subtitle = paste0(rcp, ": ", year_range, season))

print(suit_hab_temp_map)

suit_hab_thick_map <- base_map +
  geom_sf(data=world, fill = "antiquewhite1") +
  coord_sf(xlim = c(-115, -50), ylim = c(60,83), expand = FALSE)+
  geom_tile(data = thick_df, aes(x=x, y=y, fill = thick_df[,3])) +
  scale_fill_gradient(low = "green", high = "red", breaks=c(0,2,4,8), limits=c(0, 7)) +
  labs(fill = "Ice Thickness (m)",
       x = "lon",
       y = "lat",
       title = "", subtitle = paste0(rcp, ": ", year_range, season))

print(suit_hab_thick_map)



suit_hab_both_map <- base_map +
  geom_sf(data=world, fill = "antiquewhite1") +
  coord_sf(xlim = c(-115, -50), ylim = c(60,83), expand = FALSE)+
  geom_tile(data = both_df, aes(x=x, y=y, fill = both_df[,3])) +
  labs(fill = "Suitable Future Habitat",
       x = "lon",
       y = "lat",
       title = "", subtitle = paste0(rcp, ": ", year_range, season))

print(suit_hab_both_map)

if (write==TRUE){
plot_path <- "data/PREDICTION_II/plots/"
ggsave(plot=suit_hab_temp_map,filename = paste0(plot_path,"suit_hab_", substr(rcp,4,5), "_", year, "_Env_temp_", "basemap.png"))
ggsave(plot=suit_hab_thick_map, filename =paste0(plot_path,"suit_hab_", substr(rcp,4,5), "_", year, "_Env_thick_", "basemap.png"))
ggsave(plot=suit_hab_both_map, filename =paste0(plot_path,"suit_hab_", substr(rcp,4,5), "_", year, "_Env_both_", "basemap.png"))

}
}

