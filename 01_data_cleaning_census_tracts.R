## Meta-information ##########----------
## Author: Hardika Dayalani (dayalani@rand.org)
## Creation: 2019-06-18 for Access to Substance Abuse Clinics
## Description: Download and wrangle Census Tract boundaries  

## Environment Setup ##########----------
remove( list= objects() )
options( stringsAsFactors= FALSE)

## Load libraries 
library(tigris)
library(sf)

## Census Tract Data from tigris ##########----------

## Downloading shapfiles for all tracts in California
CA_tract_df <- tracts(state = 'CA', cb = T)

## Adding County Names to attributes Dataframe
data("fips_codes")
head(fips_codes)

fips_codes <- fips_codes[fips_codes$state == "CA",]
CA_tract_df$COUNTY_NAME <- fips_codes[match(CA_tract_df$COUNTYFP, fips_codes$county_code), "county"]
## Subset data
CA_tract_df <- CA_tract_df[,c("STATEFP", "COUNTYFP", "TRACTCE", "AFFGEOID", "GEOID", "NAME", "COUNTY_NAME")]

## Extracting Centroid
CA_centriods <- st_centroid(CA_tract_df) 

## Determining Urbanicity ##########----------

## downloading Shapfiles for all urban areas
## including urbanized areas (densely developed areas with a population of at least 50,000) and urban clusters (population between 2,500 and 50,000)
urban <- urban_areas()

## extract state codes
urban$state <- sapply(urban$NAME10, FUN = function(x) unlist(strsplit(x, split = ", "))[2]) 

## subset urban areas to CA
urban <- urban[grepl("CA", urban$state), ]

## determine if the tract centroid is in an urban area
temp <- st_join(CA_centriods, urban, join = st_intersects)

## Assign urbanicity
CA_tract_df$URBANICITY <- "Rural"

## Urbanized Areas
i <- match(CA_tract_df$GEOID, temp$GEOID[temp$UATYP10 == "U"])
CA_tract_df$URBANICITY[i] <- "Urbanized Area"

## Urban Cluster
i <- match(CA_tract_df$GEOID, temp$GEOID[temp$UATYP10 == "C"])
CA_tract_df$URBANICITY[i] <- "Urban Cluster"

save(CA_tract_df, file = "CA_tract_df.Rdata")