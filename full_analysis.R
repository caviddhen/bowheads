library(rgdal)
library(raster)
library(ncdf4)
library(bowheads)
library(rgeos)
#setwd("C:/PIK/bowhead") #David's wd
setwd("C:/Users/silja/iCloudDrive/DAVID/bowhead") #Siljas wd


### Read in the CIS and SST data, process them to binary,
    # and then stack all 4 of them into one raster, this does the aggregation
  #for the weeks in a given year
regions <- c("Eastern Arctic", "Hudson Bay")
seasons <- c("Summer", "Winter")
years <- c(2006:2016)

#years <- c(2007:2008)
#regions <- c("Hudson Bay")
#seasons <- c("Summer", "Winter")

#attributes to include as separate potential habitat maps
attributes <- c("ct", "sa", "fa", "sst", "all")

for (reg in regions){
  for ( seas in seasons){
    for(year in years){

readCIS(region = reg, season = seas, year = year, binary=TRUE, write=TRUE)
removeTmpFiles(h=0) #remove files in temporary memory after each round

readSST(region = reg, season = seas, year= year, binary=TRUE, write=TRUE)
removeTmpFiles(h=0) #remove files in temporary memory after each round
    }
  }
  }



#separate reading and stacking, this does the aggregation for the attributes
 for (reg in regions){
    for ( seas in seasons){
      for(year in years){

stackAttributes(region=reg, season=seas, year=year, write=TRUE)
removeTmpFiles(h=0)
          }
  }
}

##### TS Analysis - not well tested ## Do the time series for variables graphs this not needed if you want to just do the final maps
# for (region in regions){
#   for (season in seasons){
#
# tsAnalysis(region=region, season=season)
# }}


## Write prediction inputs ##### This is
# this makes the potential suitable habitat rasters
#aggregation of all years happens here

for (region in regions){
  for (season in seasons){
    for (attribute in attributes){

writePredInputs(region=region, season=season, attribute=attribute, write=TRUE)
    removeTmpFiles(h=0)

      }
  }
}
## Make mosaic from pred Inputs

for (season in seasons){
  for (attribute in attributes){
  writeMosaic(season = season, attribute = attribute, write=TRUE, extend = TRUE)
}
}

## 22.02.2021 CURRENTLY new attribute separated analysis goes until here

### Process all BioOracle data, read them in, crop, mask the present

rcps <- c("RCP26", "RCP45", "RCP85")

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
