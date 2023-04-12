#0a_Preparing eBird and Cerrado database point data
rm(list=ls())
library(ff)
library(ffbase)
library(dplyr)
library(readr)

#modified from: https://github.com/baeolophus/ou-grassland-bird-survey/blob/master/manuscript_files/0_preparation_ebird_data_import.R

### Set source directory to the folder this file came from within RStudio
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #this will work on any computer bc it is not to a specific filepath on MM's computer

## Prepare the eBird data for the current distribution model
#download data via request from ebird.org (Nadinni did)
#Record the full citation for the eBird data here: 

years<-c(2015:2020) #we chose 2015 because this is the first year that eBird data is considered reliable and robust

setwd("ACCDC_eBird_data_csv") #set the working directory to where the .csv files are
getwd() #Check that this is the correct file path on your machine

df<-list.files(pattern="*.csv", full.names=TRUE) %>%
  lapply(read_csv) %>%
  bind_rows #collect all of the .csv files for each species

#look at the list of files to make sure you're not getting anything extra
View(df) #look at the list of files to make sure you're not getting anything extra

class(df$`OBSERVATION DATE`) #returns "character"

### Need to make date into 3 separate columns or at least just have the year
df$`OBSERVATION DATE`<-as.Date(df$`OBSERVATION DATE`, '%m/%d/%Y')
class(df$`OBSERVATION DATE`) #returns "Date"

library(lubridate)
library(data.table)

df_1<-print(df[df$`OBSERVATION DATE` >"2015-01-01" & df$`OBSERVATION DATE`<"2020-12-31"]) #subset the years 2015-2020
View(df_1)

#studyarea.extent<-extent(-63,-40,
#                         -2,-27) 
studyarea.ebird<-subset(df_1,
                          LATITUDE>=-27 & LATITUDE<=-12 &
                            LONGITUDE >=-59 & LONGITUDE <=-43) #this is for a square around the Cerrado in Brazil

write.csv(studyarea.ebird,
               file="studyarea_ebird2015_2020.csv") #this should save a single .csv file with all the eBird data we will use from with the area and years we told it to subset

## If you have a historical dataset, you can prepare that occurence data in the same format for a historical model (e.g., 1985-2015*) 
#*Note: the end date will be identified by the last year you have minimum sample sizes (n=3 for MaxEnt) for point data for any species.

#The point data must have the species ID, latitude, longitude, and day/month/year with the same column headings so that we can use the same code to analyze both datasets