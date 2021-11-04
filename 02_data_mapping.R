## Meta-information ##########----------
## Author: Hardika Dayalani (dayalani@rand.org)
## Creation: 2019-06-18 for Access to Substance Abuse Clinics
## Description: Plotting Substance Abuse Clinics in California in 2017  

## Environment Setup ##########----------
remove( list= objects() )
options( stringsAsFactors= FALSE)

## Load libraries 
library(sf)
library(tmap)
library(leaflet)

## Prepare data ##########----------

## Load boundaries
load("CA_tract_df.Rdata")

## Load Clinic Data
df <- read.csv("Substance Abuse Geocoded Data/data_1990_2018.csv")

## Subsetting to 2017 data
## Since the NHTS data is available for 2017, the geododed data was subsetted to 2017 for consistency.
df_2017 <- df[df$year == 2017,]

cat(length(which(df_2017$x == 0 & df_2017$y == 0)), "observations are missing corrdinates") ## 80

## Eliminating missing values
df_2017 <- df_2017[-which(df_2017$x == 0 & df_2017$y == 0),]

# Subsetting to CA 
df_2017 <- df_2017[df_2017$state == "CA",]

## Creating spatial object
CA_clinics_2017 <- st_as_sf(df_2017, coords = c("x", "y"), crs = st_crs(CA_tract_df))

## Static Plot ##########----------
tm_shape(CA_tract_df)+
  tm_polygons(col="URBANICITY", border.col="White", palette = "Dark2")+
  tm_shape(CA_clinics_2017)+
  tm_dots(size = 0.1, col = "red")

## Leaflet Plot ##########----------

## color palette

clinic_map <- leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addMarkers(data = CA_clinics_2017,
             clusterOptions = markerClusterOptions()) %>%
  addPolygons(data = CA_tract_df, weight = 1, fillOpacity = 0)

clinic_map

save(CA_clinics_2017, file = "CA_clinics_2017.Rdata")
