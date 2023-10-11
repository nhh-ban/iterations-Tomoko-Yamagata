#data_transformations.r

#Purpose of this file:
#Create a function "transform_metadata_to_df"
# which complete the transformation of stations_metadata 
# to a data frame that looks similar to the example rows below:

# A tibble: 4,444 x 6

#id            name                          latestData            lat   lon
# <chr>         <chr>                         <dttm>              <dbl> <dbl>
#1 97411V72313   Myrsund                       2022-05-08 00:00:00  63.4 10.2 
#2 20036V605081  PILESTREDET                   2009-11-22 00:00:00  59.9 10.7 
#3 01492V971789  GRENSEN ENDE                  2015-11-12 00:00:00  59.0 11.5 
#4 54013V2352341 ØRGENVIKAKRYSSET RAMPE NORDG. 2022-05-08 00:00:00  60.3  9.68
#5 15322V971307  STOREBAUG                     2022-05-08 00:00:00  59.4 10.7 


# load packages
library(jsonlite)
library(DescTools)
library(tidyverse)
library(magrittr)
library(rlang)
library(lubridate)
library(anytime)


#Create a function to the followig:
transform_metadata_to_df<-
  function(stations_metadata){


# Create the data frame from dtm 
# by converting each column to tibble, then bind rows by the column names
df_stattions_metadata <-
  stations_metadata[[1]] %>% 
  map(as_tibble) %>% 
  list_rbind()

# Convert the latestDate into char, and update the timezone  
df_stattions_metadata <-
  df_stattions_metadata %>% 
  mutate(
    latestData = map_chr(latestData,
                         1,
                         .default =NA_character_)
  ) %>% 
  # at this point, the timezone of latestData is already "UTC" (tz(df_stattions_metadata$latestData))
  mutate(
    latestData = as_datetime(latestData)
    ) 

# Create 2 columns "lat"& "lon" from "location" column
df_stattions_metadata <-
  df_stattions_metadata %>% 
  # let the location column to be 2 levels
  mutate(location = map(location, unlist)) %>% 
  mutate(
    lat = map_dbl(location, "latLon.lat"),
    lon = map_dbl(location, "latLon.lon")
  )  %>% 
  # unselect the older "location" column
  select(-location) 
  
  }

