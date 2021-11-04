## Meta-information ##########----------
## Author: Hardika Dayalani (dayalani@rand.org)
## Creation: 2019-06-18 for Access to Substance Abuse Clinics
## Description: Creating measures of access to Substance Abuse Clinics   

## Environment Setup ##########----------
remove( list= objects() )
options( stringsAsFactors= FALSE)

## Load libraries 
library(sf)
library(dplyr)
library(tmap)

## Prepare data ##########----------

## Load boundaries
load("CA_tract_df.Rdata")

## Load Clinic Data
load("CA_clinics_2017.Rdata")

## Fix Projections
CA_tract_df <- st_transform(CA_tract_df, crs = st_crs("+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))
CA_clinics_2017 <- st_transform(CA_clinics_2017, crs = st_crs("+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))


## Count clinics in a tracts ##########----------

## Add tract data to every clinic
clinic_in_tract <- st_join(CA_clinics_2017, CA_tract_df, join = st_within)

## aggregate the number of clinics by tract
clinic_tract_count <- count(as_tibble(clinic_in_tract), GEOID)
names(clinic_tract_count)[2] <- "clinic_count"

## Add clinic count to tract data
CA_tract_df <- left_join(CA_tract_df, clinic_tract_count)
CA_tract_df$clinic_count[is.na(CA_tract_df$clinic_count)] <- 0

## Count clinics within 2 miles of tract centriod ##########----------

## Create a function to count clinics
BufferCount <- function(tract, pts, dist = 1000){
  centroid <- st_centroid(tract)
  buffer <- st_buffer(centroid, dist = dist)
  clinic_in_buffer <-st_intersection(pts, buffer, sparse = F)
  return(nrow(clinic_in_buffer))
}

CA_tract_df$clinic_buffer_count_2mile <- sapply(CA_tract_df, FUN = BufferCount, pts = CA_clinics_2017, dist = 2*1609.34)

for (i in 1:nrow(CA_tract_df)) {
  CA_tract_df$clinic_buffer_count_2mile[i] <- BufferCount(CA_tract_df[i, ], pts = CA_clinics_2017, dist = 2*1609.34)
}

summary(CA_tract_df$clinic_buffer_count_2mile)