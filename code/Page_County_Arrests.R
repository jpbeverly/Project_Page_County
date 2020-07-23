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
library(readxl)

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
mapVA  <- st_read("tl_2019_51_tract.shp",
                  stringsAsFactors = FALSE)

#map_and_data <- inner_join(mapVA, acs_Page_area, by = "GEOID")
arrests2 <- read_excel("cleaned_arrests.xlsx")

shp <- st_read("cb_2018_us_county_500k.shp")
merged_data <- merge(arrests2, shp, by = "GEOID")

drug_arrests <- st_as_sf(merged_data)
page_arrests <- st_join(drug_arrests, mapVA, by = "GEOID")

# Read in other County Data


#college_university_data <- read.csv("Colleges_and_Universities.csv")
#small_business_data <- read.csv("Small_Business_Development_Centers.csv")
#workforce_dev_center_data <- read.csv("Workforce_Development_Centers.csv")

Arrests_filtered <- subset(page_arrests, GEOID.x == 51139| GEOID.x == 51660| GEOID.x == 51165| GEOID.x == 51079| GEOID.x == 51157|GEOID.x == 51187|GEOID.x == 51171 |GEOID.x == 51113) 
  #filter(GEOID.x == "51139", "51660", "51165", "51079", "51113", "51157", "51187", "51171")

newdata <- subset(mydata, age >= 20 | age < 10,

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



# c("Harrisonburg", "Page", "Rockingham", "Greene",
# "Madison", "Rappahannock", "Warren", "Shenandoah"))


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



# Arrests Map
ggplot(Arrests_filtered) +
  geom_sf(aes(fill=Per1000)) +
  geom_sf(data=va_sf, fill="transparent", color="black", size=.5) +
  geom_sf(data=page_outline, fill="transparent", color="red", size=1) +
  theme(legend.title = element_blank())+ 
  scale_fill_gradient(low = "#5681F7", high = "#132B43") + theme_bw() + ggtitle("Page County 2018 Drug Arrests")
  


#geom_point(data=college_university_data_filtered, y=college_university_data_filtered$LATITUDE,
             #x=college_university_data_filtered$LONGITUDE, colour = "red", size=2) +
  #scale_fill_continuous() + theme_bw() +
  #geom_point(data=small_business_data, x=small_business_data$X, y=small_business_data$Y, 
             #colour = "orange", size=2)+
  #geom_point(data=workforce_dev_center_data, x=workforce_dev_center_data$X, y=workforce_dev_center_data$Y, 
             #colour = "pink", size=2)+
  #scale_fill_viridis_c() + scale_color_viridis_c()


# Median Income Map


ggplot(map_and_data) +
  geom_sf(aes(fill=Median_Income)) +
  geom_sf(data=va_sf, fill="transparent", color="black", size=.5) +
  geom_sf(data=page_outline, fill="transparent", color="red", size=1) +
  theme(legend.title = element_blank()) +
  geom_point(data=college_university_data_filtered, y=college_university_data_filtered$LATITUDE,
             x=college_university_data_filtered$LONGITUDE, colour = "red", size=2) +
  scale_fill_continuous() + theme_bw() +
  geom_point(data=small_business_data, x=small_business_data$X, y=small_business_data$Y, 
             colour = "orange", size=2)+
  geom_point(data=workforce_dev_center_data, x=workforce_dev_center_data$X, y=workforce_dev_center_data$Y, 
             colour = "pink", size=2)+
  scale_fill_viridis_c() + scale_color_viridis_c()




# Page County Only
map_and_data_Page <- inner_join(mapVA, acs_Page, by = "GEOID")

ggplot(drug_arrests) +
  geom_sf(aes(fill=Per1000)) +
  geom_sf(data=va_sf, fill="transparent", color="black", size=.5)
# # scale_fill_viridis_c() + scale_color_viridis_c()

