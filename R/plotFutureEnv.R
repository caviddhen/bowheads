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
  mosaic_winter <- readOGR(paste0(path, "mosaic_poly_winter.shp"))

  if (season == "Summer"){
    base_map <- ggplot() +
      geom_polygon(data=mosaic_summer, aes(x = long, y = lat, group = group), fill = "#E1C9F5")
  }else {
    base_map <- ggplot() +
      geom_polygon(data=mosaic_winter, aes(x = long, y = lat, group = group), fill = "#E1C9F5")
  }

  ## Suit Hab rasters -----------------------

  path <- "data/PREDICTION_II/suit_hab_future_env_sep/"

  temp <- raster(paste0(path, "suit_hab_", substr(rcp,4,5), "_", year, "_temp_", seas,".tif"))
  thick <- raster(paste0(path, "suit_hab_", substr(rcp,4,5), "_", year, "_thick_", seas,".tif"))
  both <-  raster(paste0(path, "suit_hab_", substr(rcp,4,5), "_", year, "_both_", seas,".tif"))

  temp_df <- raster::as.data.frame(temp, xy=TRUE, na.rm=TRUE)
  thick_df <-raster::as.data.frame(thick, xy=TRUE, na.rm=TRUE)
  both_df <-  raster::as.data.frame(both, xy=TRUE, na.rm=TRUE)

  both_df[,3] <- "SUITABLE"

if (year==2050){
  year_range <- "2040-2050"} else{
    year_range <- "2090-2100"
  }

# create plots


  world <- ne_countries(scale = "medium", returnclass = "sf")

suit_hab_temp_map <- base_map +
  geom_sf(data=world, fill = "antiquewhite1") +
  coord_sf(xlim = c(-115, -50), ylim = c(50,83), expand = FALSE)+
  scale_y_continuous(breaks = c(50, 60, 70, 80)) +
  scale_x_continuous(breaks = c(-50, -70, -90, -110)) +
  geom_tile(data = temp_df, aes(x=x, y=y, fill = temp_df[,3])) +
  scale_fill_gradient(low = "blue", high = "red", breaks=c(-2,0,2,4), limits=c(-2, 5)) +
  labs(fill = "SST (°C)",
       x = "lon",
       y = "lat",
       title = "", subtitle = paste0(rcp, ": ", year_range, ", ", season)) +
  theme_grey(base_size = 9) +
  theme(legend.key.size = unit(0.8,"line"))

print(suit_hab_temp_map)

suit_hab_thick_map <- base_map +
  geom_sf(data=world, fill = "antiquewhite1") +
  coord_sf(xlim = c(-115, -50), ylim = c(50,83), expand = FALSE)+
  scale_y_continuous(breaks = c(50, 60, 70, 80)) +
  scale_x_continuous(breaks = c(-50, -70, -90, -110)) +
  geom_tile(data = thick_df, aes(x=x, y=y, fill = thick_df[,3])) +
  scale_fill_gradient(low = "blue", high = "orange", breaks=c(0,2,4,6,8), limits=c(0, 8)) +
  labs(fill = "Ice Thickness (m)",
       x = "lon",
       y = "lat",
       title = "", subtitle = paste0(rcp, ": ", year_range, ", ", season)) +
  theme_grey(base_size = 9) +
  theme(legend.key.size = unit(0.8,"line"))

print(suit_hab_thick_map)

suit_hab_both_map <- base_map +
  geom_sf(data=world, fill = "antiquewhite1") +
  coord_sf(xlim = c(-115, -50), ylim = c(50,83), expand = FALSE)+
  scale_y_continuous(breaks = c(50, 60, 70, 80)) +
  scale_x_continuous(breaks = c(-50, -70, -90, -110)) +
  geom_tile(data = both_df, aes(x=x, y=y, fill = both_df[,3])) +
  labs(fill = "",
       x = "lon",
       y = "lat",
       title = "", subtitle = paste0(rcp, ": ", year_range, ", ", season)) +
  theme_grey(base_size = 9) +
  theme(legend.key.size = unit(0.8,"line"))

print(suit_hab_both_map)

if (write==TRUE){
plot_path <- "data/PREDICTION_II/plots/"
ggsave(plot=suit_hab_temp_map,filename = paste0(plot_path,season,"_suit_hab_", substr(rcp,4,5), "_", year, "_Env_temp_", "basemap.png"))
ggsave(plot=suit_hab_thick_map, filename =paste0(plot_path,season,"_suit_hab_", substr(rcp,4,5), "_", year, "_Env_thick_", "basemap.png"))
ggsave(plot=suit_hab_both_map, filename =paste0(plot_path,season,"_suit_hab_", substr(rcp,4,5), "_", year, "_Env_both_", "basemap.png"))

}
}
