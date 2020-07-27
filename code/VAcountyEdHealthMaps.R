setwd("C:/Users/Admin/Documents/DSPG/Page/Page_County_V2")

library(dplyr)
library(tidyverse)
library(ggmap)
library(leaflet)
library(rgdal)
library(sf)
library(rgeos)
library(geosphere)
library(pracma)
library(tidycensus)

`%notin%` <- Negate(`%in%`)

businessVA_data <- read.csv("C:/Users/Admin/Documents/DSPG/Page/Virginia_Business_Data.csv", header=T,stringsAsFactors=F)
centerPop_county <- read.csv("C:/Users/Admin/Documents/DSPG/Page/Page_County_v2/data/CenPop2010_Mean_CO51.txt", header=T,stringsAsFactors=F)
map <- st_read("C:/Users/Admin/Documents/DSPG/Page/Page_County_v2/data/VirginiaShapeFiles/countylevel/SDE_USDC_CENSUS_VA_COUNTY.shp",
                stringsAsFactors = FALSE)


healthVA_data <- businessVA_data %>%
  filter(X2.Digit.Title == "Health Care and Social Assistance") %>%
  filter(X3.Digit.Title %notin% c("Nursing and Residential Care Facilities")) %>%
  filter(X4.Digit.Title %notin% c("Offices of Dentists", "Home Health Care Services", "Child Day Care Services",
                                  "Continuing Care Retirement Communities and Assisted Living Facilities for the Elderly",
                                  "Individual and Family Services", "Other Residential Care Facilities",
                                  "Outpatient Care Centers", "Offices of Other Health Practitioners")) %>%
  filter(X5.Digit.Title %notin% c("Offices of Optometrists", "Offices of Chiropractors", "Community Food Services",
                                  "Community Housing Services", "Emergency and Other Relief Services", 
                                  "Offices of Physical, Occupational and Speech Therapists, and Audiologists",
                                  "Nursing Care Facilities (Skilled Nursing Facilities)", "Medical and Diagnostic Laboratories")) %>%
  filter(X6.Digit.Title %notin% c("Offices of Podiatrists", "Blood and Organ Banks",
                                  "Specialty (except Psychiatric and Substance Abuse) Hospitals"))
    
unique(healthVA_data$X5.Digit.Title)


educationVA_data <- businessVA_data%>%
  filter(X2.Digit.Title == "Educational Services") %>%
  filter(X4.Digit.Title %notin% c("Other Schools and Instruction", "Elementary and Secondary Schools"))

unique(educationVA_data$X4.Digit.Title)


colnames(centerPop_county)[6] <- "CENTER.LATITUDE"
colnames(centerPop_county)[7] <- "CENTER.LONGITUDE"

# Remove found duplicate entry
centerPop_county <- centerPop_county %>%
  filter(COUNTYFP != 515)




# Helper function: calculates distance in meters given 2 sets of coordinates (long, lat)
# longitude first, then latitude
calculate_distance <- function(long1, lat1, long2, lat2){
  ans <- distm(c(long1, lat1), c(long2, lat2), fun=distHaversine)
  return(ans)
}

# Pass in location points dataset
# radius defaults are 15 miles (in meters)
# uses county Population center
loc_within_radius <- function(loc_data1, loc_data2, radius_1 = 24140.2){
  start.time <- Sys.time()
  centerData_copy <- centerPop_county
  
  centerData_copy$ED_COUNT_15mile <- NA
  centerData_copy$ED_INVERSE_DIST_15mile <- NA
  centerData_copy$HEALTH_COUNT_15mile <- NA
  centerData_copy$HEALTH_INVERSE_DIST_15mile <- NA
  
  for(i in 1:nrow(centerData_copy)){
    count_1 <- 0
    inverse_dist_1 <- 0
    count_2 <- 0
    inverse_dist_2 <- 0
    centerData_copy$ED_COUNT_15mile[i] <- 0
    centerData_copy$ED_INVERSE_DIST_15mile[i] <- 0
    centerData_copy$HEALTH_COUNT_15mile[i] <- 0
    centerData_copy$HEALTH_INVERSE_DIST_15mile[i] <- 0
    
    for(j in 1:nrow(loc_data1)){
      distance1 <- calculate_distance(centerData_copy$CENTER.LONGITUDE[i], centerData_copy$CENTER.LATITUDE[i],
                                     loc_data1$Longitude[j], loc_data1$Latitude[j])
      
      if(distance1 <= radius_1){
        count_1 <- count_1 + 1
        inverse_dist_1 <- (1/distance1) + inverse_dist_1
      }
      
    }
    centerData_copy$ED_COUNT_15mile[i] <- count_1
    centerData_copy$ED_INVERSE_DIST_15mile[i] <- inverse_dist_1 / centerData_copy$POPULATION[i]
    
    for(k in 1:nrow(loc_data2)){
      distance2 <- calculate_distance(centerData_copy$CENTER.LONGITUDE[i], centerData_copy$CENTER.LATITUDE[i],
                                     loc_data2$Longitude[k], loc_data2$Latitude[k])
      
      if(distance2 <= radius_1){
        count_2 <- count_2 + 1
        inverse_dist_2 <- (1/distance2) + inverse_dist_2
      }
      
    }
    centerData_copy$HEALTH_COUNT_15mile[i] <- count_2
    centerData_copy$HEALTH_INVERSE_DIST_15mile[i] <- inverse_dist_2 / centerData_copy$POPULATION[i]
    
  }
  stop.time <- Sys.time()
  print(loop_time <- stop.time - start.time)
  return(centerData_copy)
}

output <- loc_within_radius(educationVA_data, healthVA_data)

map$COUNTYFP <- as.numeric(map$COUNTYFP)

# VA only
CenterPopCounty_map <- inner_join(output, map, by="COUNTYFP")

# Check for values that did not join
anti_join(output, map, by="COUNTYFP")

ggplot(CenterPopCounty_map) + 
  geom_sf(aes(fill= HEALTH_INVERSE_DIST_15mile, geometry=geometry)) +
  # geom_sf(data=va_sf, fill="transparent", color="black", size=.5) +
  # geom_sf(data=loudoun_outline, fill="transparent", color="red", size=.75) +
  # ylim(-38.4,-39.3) + xlim(-78.1, -77) +
  # scale_fill_gradientn(colours=c("red", "yellow", "green", "blue") ,name="Count", trans="log",  
  #                     breaks = c(0, 1, 3, 5, 10, 30, 100), labels=c(0, 1, 3, 5, 10, 30, 100),
  #                     na.value="grey") +
  theme_bw() +theme(legend.title = element_blank()) +
  scale_fill_gradientn(colours=c("red", "yellow", "green", "blue") ,name="Count", trans="log",  
                       breaks = c(0, 0.0000001, 0.000001, 0.00001, 0.0001), labels=c(0, 0.0000001, 0.000001, 0.00001, 0.0001),
                       na.value="grey") +
  ggtitle("Inverse Distance for Health Facilities within 15 Miles of County Center")
  # scale_fill_viridis_c() + scale_color_viridis_c()
  
  
ggplot(CenterPopCounty_map) + 
  geom_sf(aes(fill= ED_INVERSE_DIST_15mile, geometry=geometry)) +
  # geom_sf(data=va_sf, fill="transparent", color="black", size=.5) +
  # geom_sf(data=loudoun_outline, fill="transparent", color="red", size=.75) +
  # ylim(-38.4,-39.3) + xlim(-78.1, -77) +
  # scale_fill_gradientn(colours=c("red", "yellow", "green", "blue") ,name="Count", trans="log",  
  #                     breaks = c(0, 1, 3, 5, 10, 30, 100), labels=c(0, 1, 3, 5, 10, 30, 100),
  #                     na.value="grey") +
  theme_bw() +theme(legend.title = element_blank()) +
  scale_fill_gradientn(colours=c("red", "yellow", "green", "blue") ,name="Count", trans="log",  
                       breaks = c(0, 0.0000001, 0.000001, 0.00001, 0.0001), labels=c(0, 0.0000001, 0.000001, 0.00001, 0.0001),
                       na.value="grey") +
  ggtitle("Inverse Distance for Education Facilities within 15 Miles of County Center")
# scale_fill_viridis_c() + scale_color_viridis_c()


