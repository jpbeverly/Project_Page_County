
setwd("E:/Grad/intern/DSPG/Projects/Page_County/Variable_Plot/")
library(tidyverse)
library(tidycensus)
library(sf)
library(osmdata)
library(leaflet)
library(sp)
library(purrr)
library(mapview)
library(osrm)
library(rmapzen)
library(rgdal)
library(ggplot2)
library(scales)
library(readr)


VAcounty_data_master <- read_csv("VAcounty_data_master.csv")




counties=c("Page",
         "Rockingham",
         "Greene",
         "Madison",
         "Rappahannock",
         "Warren",
         "Shenandoah",
         "Harrisonburg")

page_data <- VAcounty_data_master %>% filter( COUNAME %in% counties)


#mycols <- c("#9A5EA6", "#E5C473", "#B98B50", "#61276D", "#2E368F","#D8C5E0", "#0000FF", "#FF0000", "#FFFF00","#00FF00")

mycols <- c("#9A5EA6", "#E5C473", "#B98B50", "#61276D", "#55DDE0","#D8C5E0", "#0000FF", "#FF0000", "#FFFF00","#00FF00")



# Without Insurance Under 6 years

ggplot(page_data, aes(x=COUNAME, y=round(tot_insurance_under_6yrs_without_E*100/POPULATION, 2), fill=COUNAME)) + 
  geom_bar(stat="identity") +
  xlab("County Name")+
  ylab("% Without Insurance Under 6 years")+
  theme_minimal()


# Without Insurance between 6 to 18 years

ggplot(page_data, aes(x=COUNAME, y=round(tot_insurance_6to18yrs_without_E*100/POPULATION, 2), fill=COUNAME)) + 
  geom_bar(stat="identity") +
  xlab("County Name")+
  ylab("% Without Insurance between 6 to 18 years")+
  theme_minimal()


# Without Insurance above 75 years

ggplot(page_data, aes(x=COUNAME, y=round(tot_insurance_75yrs_and_up_without_E*100/POPULATION, 2), fill=COUNAME)) + 
  geom_bar(stat="identity") +
  xlab("County Name")+
  ylab("% Without Insurance above 75 years")+
  theme_minimal()


# Without Insurance between 19 to 25 years

ggplot(page_data, aes(x=COUNAME, y=round(tot_insurance_19to25yrs_without_E*100/POPULATION, 2), fill=COUNAME)) + 
  geom_bar(stat="identity") +
  xlab("County Name")+
  ylab("% Without Insurance between 19 to 25 years")+
  theme_minimal()


# Without Insurance between 26 to 34 years

ggplot(page_data, aes(x=COUNAME, y=round(tot_insurance_26to34yrs_without_E*100/POPULATION, 2), fill=COUNAME)) + 
  geom_bar(stat="identity") +
  xlab("County Name")+
  ylab("% Without Insurance between 26 to 34 years")+
  theme_minimal()


# Without vehicle total

ggplot(page_data, aes(x=COUNAME, y=tot_vehicle_without_E, fill=COUNAME)) + 
  geom_bar(stat="identity") +
  xlab("County Name")+
  ylab("Total Without Vehicle")+
  theme_minimal()
