library(tidyverse)
library(ggplot2)
library(sf)
library(tmap)
library(tmaptools)
library(leaflet)
library(dplyr)
library("maps")
library("tools")
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library("ggspatial")
library("ggrepel")
library(readxl)

#Industry and Health Services Map Variables
#business <- read_excel("cleaned_business.xlsx")
health_care <- read_excel("health_page.xlsx")
shp <- st_read("cb_2018_us_county_500k.shp")
merge_business <- merge(business, shp, by = "GEOID")
industry <- st_as_sf(merge_business)
world <- ne_countries(scale = "medium", returnclass = "sf")
counties <- st_as_sf(map("county", plot = FALSE, fill = TRUE))

#Health Services Map
ggplot(data = world) + 
  geom_sf() +
  geom_sf(data = counties)+
  geom_point(data = health_care, aes(x = Longitude, y = Latitude), size = 2, shape = 23, fill = "darkred") +
  coord_sf(xlim = c(-78.7, -78.25), ylim = c(38.4, 38.85), expand = FALSE) + ggtitle("Page County Health Services")


#Drug Arrest Map variables
arrests2 <- read_excel("code/cleaned_arrests.xlsx")
merged_data <- merge(arrests2, shp, by = "GEOID")
drug_arrests <- st_as_sf(merged_data)

#Drug Arrests for the State of Virginia 2018
ggplot(drug_arrests) + 
  geom_sf(aes(fill = Arrests)) +
  scale_fill_gradient(low = "#5681F7", high = "#132B43") + ggtitle("2018 Drug Arrests By County")

#Per Capita Drug Arrests for Virginia 2018
ggplot(drug_arrests) + 
  geom_sf(aes(fill = Per1000)) +
  scale_fill_gradient(low = "#5681F7", high = "#132B43") + ggtitle("2018 Drug Arrests Per Capita By County")
