#0b_preparing current (2015-2020) environmental data layers 
rm(list=ls()) #clear the R Global Environment

## We are looking at all records 2015-2020 (present). 

### Set source directory to the folder this file came from within RStudio
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #this will work on any computer bc it is not to a specific filepath on MM's computer

##################################################################
###/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\###
##################################################################
#The following code was developed from that on this website: https://dominicroye.github.io/en/2018/access-to-climate-reanalysis-data-from-r/
#install the RNCEP, lubridate and tidyverse packages
if(!require("RNCEP")) install.packages("RNCEP")
if(!require("lubridate")) install.packages("lubridate")
if(!require("tidyverse")) install.packages("tidyverse")
if(!require("sf")) install.packages("sf")

#load the packages
library(RNCEP)
library(lubridate) #for date and time manipulation
library(tidyverse) #for data manipulation and visualization
library(RColorBrewer) #for color schemes
library(sf) #to import a spatial object and to work with geom_sf in ggplot2
##################################################################
###/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\###
##################################################################

### Get the data layers for the current time period (2015-2020)
## Environmental variables that are relevant to Cerrado birds include: 
#elevation (DEM), 
#rainfall (average, total, anomalous); 
#temperature (average, high, low)

## For today's workshop, we'll get temperature and precipitation data from reanalysis 1 (NCEP data); 
#Another idea/ previous thought: BIOCLIM for temp and rainfall? If use BIOCLIM, must consider whether this matches dates of species occurrence.

##################################################################
###/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\###
##################################################################
#DOWNLOAD NCEP DATA FOLLOWING WEBSITE:https://dominicroye.github.io/en/2018/access-to-climate-reanalysis-data-from-r/
#define the necessary arguments
month_range <- c(1,12)     #period of months
year_range <- c(2015,2020) #period of years; this is the period for the current time period

lat_range <- c(-59.487,12.62908)      #latitude range of South America (this will be clipped to the SouthAmerica shapefile later)
lon_range <- c(-109.4604,-26.23419)     #longitude range of South America (this will be clipped to the SouthAmerica shapefile later)

start_time<-Sys.time() #this will keep track of what time it was when you start the next line of code

data <- NCEP.gather("air",    #name of the variable
                    850, #pressure level 1000hPa is the surface pressure exerted in the Cerrado (1mb=1hPa) as seen here: https://www.worldweatheronline.com/cerrado-weather/parana/br.aspx
                    month_range,year_range,
                    lat_range,lon_range,
                    return.units = TRUE,
                    reanalysis2=TRUE)#the reanalysis2 argument allows us to download both version I and version II, being by default FALSE. So setting this to TRUE means we are only accessing reanalysis I. 

end_time<-Sys.time() #this will keep track of what time it was when the last function ended

end_time - start_time #this will tell you how long the data<-NCEP.gather() function took to execute
#Time difference of 3.485134 mins (consistently ~3 mins)

##BONUS! The NCEP.gather() function also includes a 'Total Progress' pop-up bar.

## [1] Units of variable 'air' are degK

#dimensions                    
dim(data) #Look at the dimensions of the air data
## [1]   32   23 8768

#we find lon, lat and time with dimnames()
#date and time
date_time <- dimnames(data)[[3]]
date_time <- ymd_h(date_time)
head(date_time)

## [1] "2015-01-01 00:00:00 UTC" "2015-01-01 06:00:00 UTC" "2015-01-01 12:00:00 UTC" "2015-01-01 18:00:00 UTC"
## [5] "2015-01-02 00:00:00 UTC" "2015-01-02 06:00:00 UTC"

#longitude and latitude
lat <- dimnames(data)[[1]]
lon <- dimnames(data)[[2]]

#The atomic vector created for latitude and longitude are "characters" instead of "integers".
#The NCEP data seems to download longitude as 360 degrees instead of 0:180; 0:-180 which is what we need to match the shapefile.
typeof(lon) # "character"

lon <- as.integer(lon)
lat <- as.integer(lat)

typeof(lon) # "integer"
x <- lon -180
lon <- -180 + x
  
head(lon);head(lat)

## [1] -110 -108 -105 -103 -100  -98
## [1] "15"   "12.5" "10"   "7.5"  "5"    "2.5" 

##Create a yearly average
#create our grouping variable
group <- year(date_time) 

#estimate the average temperature by year 
data_year <- aperm(
  apply(
    data, #our air data
    c(1,2), #apply to each time series 1:row, 2:column a the mean( ) function
    by, #group by
    group, #years
    function(x)ifelse(all(is.na(x)),NA,mean(x))),
  c(2,3,1)) #reorder to get an array like the original

dim(data_year) #1000haPa temperature per year 2015 to 2020
## [1] 31 35  6

##Visualize the air data
#first we create all the combinations of lon-lat
lonlat <- expand.grid(lon=lon,lat=lat)

#as lonlat was a row/column name, it is character, that's why we convert it into numeric
lonlat <- apply(lonlat,2,as.numeric) #this line might not be needed

  
#lon and lat are not in the order as we expect
#row=lon; column=lat
data_year <- aperm(data_year,c(2,1,3))

#subtract 273.15K to convert K to ºC.
df <- data.frame(lonlat,
                 Ta01=as.vector(data_year[,,1])-273.15,
                 Ta06=as.vector(data_year[,,6])-273.15) #What does Ta01 and Ta07 mean? = the example was Jan-July so it's the first and seventh month; I changed Ta07 to [,,6] because there are 6 years

#convert the wide table into a long one
df <- gather(df,year,Ta,Ta01:Ta06)%>%
  mutate(year=factor(year,unique(year),c("2015","2020")))

#import the continent limits
limit <- st_read("SouthAmerica.shp")

## Reading layer `SouthAmerica' from data source `C:\Users\maggi\OneDrive\Documents\GitHub\Cerrado_spatial\SouthAmerica.shp' using driver `ESRI Shapefile'
## Simple feature collection with 12002 features and 7 fields
## Geometry type: MULTIPOLYGON
## Dimension:     XY
## Bounding box:  xmin: -109.4604 ymin: -59.48714 xmax: -26.23419 ymax: 12.62908
## Geodetic CRS:  WGS 84

#color scheme
colbr <- brewer.pal(11,"RdBu")

ggplot(df)+
  geom_tile(aes(lon,lat,fill=Ta))+ #temperature data
  geom_sf(data=limit,fill=NA,size=.5)+ #limits 
  scale_fill_gradientn(colours=rev(colbr))+
  coord_sf(ylim=c(-59.487,12.62908),xlim=c(-109.4604,-26.23419))+
  scale_x_continuous(breaks=seq(-60,16,10),expand=c(0,0))+
  scale_y_continuous(breaks=seq(-85,-30,5),expand=c(0,0))+
  labs(x="",y="",fill="Ta 1000hPa (ºC)")+
  facet_grid(year~.)+ #plot panels by year
  theme_bw() 
##################################################################
###/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\###
##################################################################
#NCEP doesn't have precipitation data

##################################################################
###/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\###
##################################################################


### Clip the data layers to the SouthAmerica area #studyarea.extent<-extent(-63,-40,-2,-27)

### Stack the data layers (this is the way they are organized for processing in model)

