### See https://github.com/baeolophus/ou-grassland-bird-survey for manuscript_files 
#Here you can go through the steps that Claire Curry used to run models using cloud computing.

#Ensemble model code for Cerrado birds 

### Set source directory to the folder this file came from within RStudio
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #this will work on any computer bc it is not to a specific filepath on MM's computer

#libraries needed:
library(microbenchmark)
library(raster)

## Bring in point data
setwd("ACCDC_eBird_data_csv") #set the working directory to where the .csv files are
complete.dataset.for.sdm<-read.csv("studyarea_ebird2015_2020.csv", header=TRUE)

##make it spatial (this is needed to do the spatial subset). Note whether lat/long values are in decimal degrees or other unit.
#eliminate missing values first.
complete.dataset.for.sdm.na <- complete.dataset.for.sdm %>%
  dplyr::filter(!is.na(LONGITUDE) & !is.na(LATITUDE))

#make it spatial
coordinates(complete.dataset.for.sdm.na)<-c("LONGITUDE", "LATITUDE")

#specify projection
proj4string(complete.dataset.for.sdm.na)<-CRS(as.character("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))#change this if should for Cerrado region

#Then convert to utm if not already
complete.dataset.for.sdm.na.utm <- spTransform(complete.dataset.for.sdm.na,
                                               CRS("+proj=utm +zone=14 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
#confirm UTM
proj4string(complete.dataset.for.sdm.na.utm)

#View the records
plot(complete.dataset.for.sdm.na.utm)

#Write to file
write.csv(as.data.frame(complete.dataset.for.sdm.na.utm),
          "completedatasetforsdm_naomit_utm.csv")

#Responses
#Bring in whole response data set (with NA lat/long already removed) as a spatial object including presence/absence.
#Is already in utm
complete.dataset.for.sdm <- read.csv(file = "completedatasetforsdm_naomit_utm.csv")


#Get training data 'sample size'.
samplesize <- as.data.frame(complete.dataset.for.sdm.na.utm) %>% group_by(SAMPLING_EVENT_IDENTIFIER) %>%
  distinct(SAMPLING_EVENT_IDENTIFIER, .keep_all = TRUE)

#length of sample size is number of checklists
length(samplesize) #47

## Bring in data layers
#create temporary raster files on large drive because they can occupy many GB
rasterOptions()$tmpdir
rasterOptions(tmpdir=paste0(getwd(),
                            "/rastertemp"))

### Bring in/remember predictor data. This will be a stacked raster file created in 0b_Preparing_Environmental_Data.R
# the predictor data is called 'for_stack' in the Global Environment

## Do ensemble model:
#parameters for random forest models and support set sizes.
#random forest parameters
ntree <- 500
importance <- FALSE
## support set dimensions/samples if looking at varying the regional scale
# radius.small <- 60000 #small radius in meters. =6,000 = 60 km = 120 x 120 km boxes #200 points
# radius.medium <- 100000 #med radius in meters. =100,000 = 100 km = 200 x 200 km boxes #75 points
# radius.large <- 225000 #large radius in meters. =250,000 = 250 km = 500 x 500 km boxes #25 points
# numberofpoints.small <- 100
# numberofpoints.medium <- 37
# numberofpoints.large <- 12

library(randomForest)

### See https://github.com/baeolophus/ou-grassland-bird-survey for manuscript_files 
#Here you can go through the steps that Claire Curry used to run models using cloud computing.