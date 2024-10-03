#Spatial Data Lab

install.packages("sf")
install.packages("leaflet")
install.packages("geosphere")

library(sf)
library(leaflet)
library(geosphere)
library(tidyverse)

city_sf <- st_read("https://raw.githubusercontent.com/worldbank/dime-r-training/refs/heads/main/DataWork/DataSets/Final/city.geojson")
glimpse(city_sf)
names(city_sf) #doesn't have area 

st_area(city_sf) #also gives the unit of area (R auto converts area in decimal degree to metre)

st_crs(city_sf)

ggplot() + geom_sf(data = city_sf)

langata <- city_sf %>%
    filter(NAME_2 == "Langata")

roads_sf <- st_read("https://raw.githubusercontent.com/worldbank/dime-r-training/refs/heads/main/DataWork/DataSets/Final/roads.geojson")
glimpse(roads_sf)

st_crs(roads_sf)

ggplot() + geom_sf(data = roads_sf)

st_length(roads_sf) #gets length of each line 


schools_df <-
    read_csv("https://raw.githubusercontent.com/worldbank/dime-r-training/refs/heads/main/DataWork/DataSets/Final/schools.csv")

glimpse(schools_df)

#convert csv to spatial data object
schools_sf <- st_as_sf(schools_df, 
                       coords = c("longitude", "latitude"),
                       crs = 4326)

#plotting the spatial object of schools
ggplot() + geom_sf(data = schools_sf)

#plotting the dataframe of schools 
ggplot() + geom_point(data = schools_df,
                      aes (x = longitude, y = latitude))

#plotting a leaflet map
leaflet() %>% addTiles() %>% addPolygons(data = city_sf,
                                         popup = ~NAME_2) %>%
    addCircles(data = schools_sf,
               popup = ~name,
               color = "black") %>%
    addPolylines(data = roads_sf,
                 color = "orange",
                 weight = 2)









