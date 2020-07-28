# setwd("~/Project_Page_County")

# Want 2 maps 1 of population and 1 with income using 2018 acs data

library(tidycensus)
library(tidyverse)
library (stringr)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(sf)
library(scales)

myACSkey <- "2580514e97d888fe585f59b4e328fce92342fe8f"

#show available variables for ACS survey
acs5 <- load_variables(2018, "acs5", cache=T)

# B19013_001 - MedianIncome
# B01003_001 - Total Population


#FUNCTIONS:

# 1. "acs_tables" calls "get_acs" (from tidycensus) on a vector of table names. It returns a dataframe of 
# all the tables bound together.  The function requires a vector of table names, 
# a census API key, and a geographical unit.  The user can add other parameters as well.

acs_tables<-function(tables,key,geography,...){
  acs_data<-NULL
  for(i in 1:length(tables)){
    data<-get_acs(geography = geography,
                  table = tables[i],
                  key = key,
                  show_call = T,
                  cache_table=T,
                  ...
    )
    acs_data<-rbind(acs_data,data.frame(data))
  }
  return(acs_data)
}

# 2. "acs_wide" cleans the data returned from a census API call.  More specifically, 
# it separates the variable column into separate variables, and it separates "NAME" into 
# different columns with pre-defined column names (NAME_col_names). The function also
# drops the "margin of error" column.

acs_wide<-function(data,NAME_col_names){
  data%>%
    select (-moe)%>%
    pivot_wider(names_from = variable,values_from=estimate)%>%
    separate(NAME, into=NAME_col_names, sep = ", ")
}


#3. acs_years retrieves individual variables (or a list of variables) across a series of years.
acs_years<-function(years,key,geography,...){
  acs_data<-NULL
  for(i in 1:length(years)){
    acs<-get_acs(geography = geography,
                 #variables = vars,
                 key = key,
                 year=years[i],
                 output = "wide",
                 show_call = T,
                 geometry = F,
                 ...)
    acs_data<-(rbind(acs_data,data.frame(acs)))
  }
  acs_data<-cbind(acs_data,year=rep((years),each=length(unique(acs_data$GEOID))))
  return(acs_data)
}


#4. "acs_years_tables" uses two previously defined functions (acs_tables and acs_wide) to return multiple 
# variable tables across multiple years in one single tibble.  A couple of notes: the way that 
# get_acs handles variables before 2013 varies, so this function only works for 2013 and after.
# For variable tables before 2013, use acs_tables to pull individual sets of tables.  Also, I have 
# not included "geometry" in the function.  If the user includes geometry, he/she may need 
# to modify the call to acs_wide.


acs_years_tables<-function(tables,years,key,geography,NAME_col_names,...){
  acs_data<-NULL
  for (j in 1:length(years)){
    acs<-acs_tables(tables=tables,year=years[j],key=key,geography = geography,...)
    year<-rep(years[j],times=length(acs$GEOID))
    acs_years2<-cbind(year,data.frame(acs))
    acs_data<-(rbind(acs_data,acs_years2))
  }
  acs_data<-acs_wide(acs_data,NAME_col_names = NAME_col_names)
  return(acs_data)
}


#NATIONAL AND Page DATA

# Variables
# B19013_001 - Median Income
# B01003_001 - Total Population

tables<-c("B01003","B19013")
years<-c(2018)
colnames=c("Census_tract","County","State")

# Pull ACS data for Page only
acs_Page<-acs_years_tables(tables=tables,
                             years=years,
                             key=myACSkey,
                             geography="tract",
                             state="VA",
                             county="Page",
                             NAME_col_names = colnames)
# Rename Variable Columns
acs_Page <- acs_Page %>%
  rename(
    Median_Income = B19013_001,
    Total_Population = B01003_001
  )

# Surrounding Counties
# Page, Rockingham, Greene, Madison, Rappahannock, Warren, Shenandoah, Harrisonbrug
acs_Page_area<-acs_years_tables(tables=tables,
                           years=years,
                           key= myACSkey,
                           geography="tract",
                           state="VA",
                           county=c("Page county",
                                    "Rockingham county",
                                    "Greene county",
                                    "Madison county",
                                    "Rappahannock county",
                                    "Warren county", 
                                    "Shenandoah county",
                                    "HARRISONBURG"),
                           NAME_col_names = colnames)

# Rename Variable Columns
acs_Page_area <- acs_Page_area %>%
  rename(
    Median_Income = B19013_001,
    Total_Population = B01003_001
  )

# Mapping

# Read in Virginia shape file
# mapVA  <- st_read("~data/VirginiaShapeFiles/tl_2019_51_tract.shp", stringsAsFactors = FALSE)
mapVA  <- st_read("C:/Users/Admin/Documents/DSPG/Page/Project_Page_County/data/VirginiaShapeFiles/tl_2019_51_tract.shp",
                  stringsAsFactors = FALSE)

map_and_data <- inner_join(mapVA, acs_Page_area, by = "GEOID")


# Read in other County Data
college_university_data <- read.csv("C:/Users/Admin/Documents/DSPG/Page/Project_Page_County/data/Colleges_and_Universities.csv")
small_business_data <- read.csv("C:/Users/Admin/Documents/DSPG/Page/Project_Page_County/data/Small_Business_Development_Centers.csv")
workforce_dev_center_data <- read.csv("C:/Users/Admin/Documents/DSPG/Page/Project_Page_County/data/Workforce_Development_Centers.csv")
virginia_business_data <- read.csv("C:/Users/Admin/Documents/DSPG/Page/Project_Page_County/data/Virginia_Business_Data.csv")

college_university_data_filtered <- college_university_data %>%
  filter(STATE ==  "VA") %>%
  filter(COUNTY %in% c("PAGE",
                     "ROCKINGHAM",
                     "GREENE",
                     "MADISON ",
                     "RAPPAHANNOCK",
                     "WARREN", 
                     "SHENANDOAH",
                     "HARRISONBURG"))

colnames(small_business_data)[1] <- "X"
small_business_data <- small_business_data %>%
  filter(LOC %in% c("Harrisonburg", "Page", "Rockingham", "Greene", 
                    "Madison", "Rappahannock", "Warren", "Shenandoah"))

colnames(workforce_dev_center_data)[1] <- "X"
workforce_dev_center_data <- workforce_dev_center_data %>%
  filter(Zip %in% c("22851", "22650", "22827", "22849", "22835", # Page
                    "22801", "22802", "22807", # Harrisonburg
                    "22626", "22842", "22810", "22845", "22824",
                    "22660", "22641", "22657", "22644", "22847",
                    "22652", "22664", # Shenandoah
                    "22968", "22935", "22965", "22968", "22935", 
                    "22973", # Greene
                    "22727" # Part of Madison
                    ))
  


virginia_services_data <- virginia_business_data 
ambulance_services_data <- virginia_services_data %>%
  filter(virginia_services_data$County.Ind.City %in% c("Harrisonburg City", "Warren",
                                       "Rockingham", "Shenandoah", "Page",
                                       "Greene", "Rappahannock", "Madison"))

ambulance_services_data <- ambulance_services_data %>%
  filter(ambulance_services_data$X6.Digit.Title == "Ambulance Services")

correctional_institutions <- virginia_services_data %>%
  filter(virginia_services_data$X6.Digit.Title == "	Correctional Institutions")


farms_data <- virginia_services_data %>%
  filter(virginia_services_data$Business.Description == "Farms")

# Filtering Virginia Business Data (Large DataSet)
virginia_business_data_filtered <- virginia_business_data %>%
  filter(X2.Digit.Title =="Health Care and Social Assistance") %>%
  filter(virginia_business_data_filtered$County.Ind.City %in% c("Harrisonburg City", "Warren",
                                                                "Rockingham", "Shenandoah", "Page",
                                                                "Greene", "Rappahannock", "Madison"))

  # unique(virginia_business_data_filtered$Business.Description)

  virginia_business_data_filtered <- virginia_business_data_filtered %>%
    filter(Business.Description %in% c("Crisis Intervention Service", "Hospitals",
                                       "Preventive Medicine", "Wellness Programs",
                                       "Physicians & Surgeons-Emergency Service",
                                       "Health Care Facilities", "Emergency Medical & Surgical Service",
                                       "Social Workers-Clinical", "Rehabilitation Services",
                                       "Social Service & Welfare Organizations",
                                       "Counselors", "Counseling Services",
                                       "Counselors-Licensed Professional", "Medical Centers",
                                       "Mental Health Services", "Emergency Minor Medical Facilities/Svcs",
                                       "Ambulance Service", "Integrated Medicine",
                                       "Clinics", "Health Services", "Nurses & Nurses' Registires",
                                       "Nurses-Practitioners"))



virginia_business_data_filtered <- subset(virginia_business_data_filtered, 
                                          X3.Digit.Title != "Ambulatory Health Care Services")
virginia_business_data_filtered <- subset(virginia_business_data_filtered, 
                                          X3.Digit.Title != "Nursing and Residential Care Facilities")
virginia_business_data_filtered <- subset(virginia_business_data_filtered, 
                                          X4.Digit.Title != "Individual and Family Services")
virginia_business_data_filtered <- subset(virginia_business_data_filtered, 
                                          X4.Digit.Title != "Vocational Rehabilitation Services")

unique(virginia_business_data_filtered$X4.Digit.Title)
unique(virginia_business_data_filtered$County.Ind.City)



# Get VA County Outlines
va_sf<-get_acs(geography = "county",
               state="VA",
               county=c("Page county",
                        "Rockingham county",
                        "Greene county",
                        "Madison county",
                        "Rappahannock county",
                        "Warren county", 
                        "Shenandoah county",
                        "HARRISONBURG"),
               variables = "B19058_002",
               survey = "acs5",
               key = myACSkey,
               year=2018,
               output = "wide",
               show_call = T,
               geometry = T,
               keep_geo_vars = T)%>%
  select(COUNTYFP,geometry)

# Get Page County outline
page_outline<-get_acs(geography = "county",
                   state="VA",
                   county=c("Page county"),
                   variables = "B19058_002",
                   survey = "acs5",
                   key = myACSkey,
                   year=2018,
                   output = "wide",
                   show_call = T,
                   geometry = T,
                   keep_geo_vars = T)%>%
  select(COUNTYFP,geometry)



# Population Map
ggplot(map_and_data) +
  geom_sf(aes(fill=Total_Population)) +
  geom_sf(data=va_sf, fill="transparent", color="black", size=.5) +
  geom_sf(data=page_outline, fill="transparent", color="red", size=1) +
  scale_fill_viridis_c(labels = comma) + labs(fill = "") +
  
  geom_point(data=virginia_business_data_filtered, aes(x=Longitude,
                                                       y=Latitude, colour="Hospitals & Clinics"), size=1.5) +
  geom_point(data=college_university_data_filtered, aes(y=LATITUDE,
                                                        x=LONGITUDE, colour = "Colleges & Universities"), size=1.5) +
  geom_point(data=small_business_data, aes(x=X, y=Y, 
                                           colour = "Small Business"), size=1.5)+
  geom_point(data=workforce_dev_center_data, aes(x=X, y=Y, 
                                                 colour = "Workforce Development"), size=1.5)+
  scale_colour_manual("" ,values= c("Colleges & Universities" = "black", 
                                    "Small Business" = "red", 
                                    "Workforce Development" = "Orange",
                                    "Hospitals & Clinics" = "purple")) + theme_bw() +
  labs(subtitle = "Total Population by Census Tract") +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank()) 


# tiff("test2.tiff", units="in", width=10, height=5, res=300)

# dev.off()


# Median Income Map
ggplot(map_and_data) +
  geom_sf(aes(fill=Median_Income)) +
  geom_sf(data=va_sf, fill="transparent", color="black", size=.5) +
  geom_sf(data=page_outline, fill="transparent", color="red", size=1) +
  scale_fill_viridis_c(labels = comma) + labs(fill = "") +
  
  geom_point(data=virginia_business_data_filtered, aes(x=Longitude,
                                                       y=Latitude, colour="Hospitals & Clinics"), size=1.5) +
  geom_point(data=college_university_data_filtered, aes(y=LATITUDE,
                                                        x=LONGITUDE, colour = "Colleges & Universities"), size=1.5) +
  geom_point(data=small_business_data, aes(x=X, y=Y, 
                                           colour = "Small Business"), size=1.5)+
  geom_point(data=workforce_dev_center_data, aes(x=X, y=Y, 
                                                 colour = "Workforce Dev"), size=1.5)+
  scale_colour_manual("" ,values= c("Colleges & Universities" = "black", 
                                    "Small Business" = "red", 
                                    "Workforce Dev" = "White",
                                    "Hospitals & Clinics" = "purple")) + theme_bw() +
  labs(title= "Page County", subtitle = "Median Income by Census Tract") +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank()) 

  
  


# Page County Only

# map_and_data_Page <- inner_join(mapVA, acs_Page, by = "GEOID")
# 
#  ggplot(map_and_data_Page) +
#   geom_sf(aes(fill=Total_Population)) +
#    geom_sf(data=va_sf, fill="transparent", color="black", size=.5)
# # scale_fill_viridis_c() + scale_color_viridis_c()

  