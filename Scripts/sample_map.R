library(tidyverse)
library(sp)
library(sf)
library(readxl)
library(tidycensus)
library(tigris)

#load data
#Brownfields Properties
me_properties <- read_csv("Data/property_locations.csv")#load property CSV
st_as_sf(me_properties,coords = c("longitude","latitude"))->me_properties#convert properties tibble to a spatial points dataframe

#ME counties
me_counties <- counties(state="ME")#download Maine's county shapefile from the Census 

#make coordinate system of Maine BF properties match that of the Census shapefile; required to map both objects
st_crs(me_properties)<-st_crs(me_counties)


#create a map
ggplot(me_counties)+
  geom_sf()+
  geom_sf(data=me_properties)
