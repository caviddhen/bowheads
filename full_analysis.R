
### Read in the CIS and SST data, process them to binary,
    # and then stack all 4 of them into one raster
regions <- c("Eastern Arctic", "Hudson Bay")
seasons <- c("Summer", "Winter")
years <- c(2006:2014)

for (reg in regions){
  for ( seas in seasons){
    for(year in years){

readCIS(region = reg, season = seas, year = year, binary=TRUE, write=TRUE)
removeTmpFiles(h=0) #remove files in temporary memory after each round

readSST(region = reg, season = seas, year= year, binary=TRUE, write=TRUE)
removeTmpFiles(h=0) #remove files in temporary memory after each round

stackAttributes(region=reg, season=seas, year=year, write=TRUE)
removeTmpFiles(h=0)
          }
  }
}

##### TS Analysis ## Do the time series for variables graphs this not needed if you want to just do the final maps
for (region in regions){
  for (season in seasons){

tsAnalysis(region=region, season=season)
}}


## Write prediction inputs ##### This is the stacked sums from the CIS and SST analysis to make the mosaic

for (region in regions){
  for (season in seasons){

writePredInputs(region=region, season=season, write=TRUE)
    removeTmpFiles(h=0)
  }
  }

## Make mosaic from pred Inputs

for (season in seasons){
  writeMosaic(season = season, write=TRUE, extend = TRUE)
}

### Process all BioOracle data, read them in, crop, mask the present

rcps <- c("RCP45", "RCP85")

for (season in seasons){

  calcBioOracle(year="Present", season=season)

      for (year in c(2050, 2100)){

        for (rcp in rcps){

            calcBioOracle(year=year, rcp=rcp, season=season) }

}
}


### DO SUITABLE HABITAT FUTURE PREDICTION RASTERS

for (season in seasons){

  for (year in c(2050,2100)){

        for (rcp in rcps){

        predictFutureHabitat(year=year, rcp =rcp, season=season)
   }}}
#
# this85 <- raster("C:/PIK/bowhead/data/PREDICTION_II/suit_hab_future_env_sep/suit_hab_85_2100_temp.tif")
# orig85<- raster("D:/PREDICTION_II/suit_hab_future_env_sep/suit_hab_85_100_temp.tif")
#
# this45 <- raster("C:/PIK/bowhead/data/PREDICTION_II/suit_hab_future_env_sep/suit_hab_45_2050_temp.tif")
# orig45 <- raster("D:/PREDICTION_II/suit_hab_future_env_sep/suit_hab_45_50_temp.tif")

#### ### Get range of suitable temperatures and Ice Thicknesses ####

files_list <- list.files("data/PREDICTION_II/mask/", full.names = TRUE, pattern=".tif$")

files <- lapply(files_list,raster)
names(files) <- list.files("data/PREDICTION_II/mask/", pattern=".tif$")

lapply(files, maxValue)
lapply(files, minValue)

### WRITE PLOTS OF FUTUTRE CHANGE AGAINST CURRENT SUITABLE HABITAT

for (season in seasons){
  for (year in c(2050,2100)){
    for (rcp in rcps){

plotFutureEnv(year=year, rcp=rcp, season=season, write=TRUE)
    }}}

### Calculate Habitat difference for future scenarios in area and percentage

area_loss <- data.frame()

for (season in seasons){
  for (year in c(2050,2100)){
    for (rcp in rcps){

loss<- calcAreaLoss(year=year, rcp=rcp, season=season)

  area_loss <- rbind(area_loss, loss)
    }}}


### save excel of habitat loss

write.csv(area_loss, file=paste0("data/PREDICTION_II/future_habitat_loss.csv"))





