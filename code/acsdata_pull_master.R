# ---
# title: "Page ACS Virginia Counties Data
# author: "Talib Grant"
# date: "July 24, 2020"
# ---

library(tidycensus)
library(tidyverse)
library (stringr)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(sf)
library(scales)
library(readxl)


myACSkey <- ""

#show available variables for ACS survey
acs5 <- load_variables(2010, "acs", cache=T)

# Candidate variable tables for describing poverty, educational attainment, race distribution, and housing type
# B17020 
#(001-009, 
#POVERTY STATUS IN THE PAST 12 MONTHS BY AGE, 
#Universe: Population For Whom Poverty Status Is Determined), 
# B15003 
#(001-025, 
#EDUCATIONAL ATTAINMENT FOR THE POPULATION 25 YEARS AND OVER, 
#Universe: Population 25 Years And Over), 
#B02001, B03003, B02009
#(Race/Demographics)
#B11001
#(Housing Type)


# Create a vector of variables to pull

vars<-c(
  tot_pov_sts_12mths_age_B17020_ = "B17020_001", 
  tot_income_12mths_blw_pov_lvl_B17020_ = "B17020_002",
  tot_edu_attain_25yrs_and_up_B15003_= "B15003_001",
  tot_edu_attain_25yrs_and_up_HS_diploma_B15003_ = "B15003_017",
  tot_edu_attain_25yrs_and_up_GED_B15003_ = "B15003_018", 
  tot_edu_attain_25yrs_and_up_somecollege_freshman_B15003_ = "B15003_019", 
  tot_edu_attain_25yrs_and_up_somecollege_abovefreshman_B15003_ = "B15003_020", 
  tot_edu_attain_25yrs_and_up_assoc_B15003_= "B15003_021",
  tot_edu_attain_25yrs_and_up_bach_B15003_= "B15003_022", 
  tot_edu_attain_25yrs_and_up_masters_B15003_ = "B15003_023", 
  tot_edu_attain_25yrs_and_up_prof_degree_B15003_ = "B15003_024", 
  tot_edu_attain_25yrs_and_up_doctorate_B15003_ = "B15003_025",
  tot_race_known_ = "B02001_001",
  tot_race_black_african_american_alone_combination_ = "B02009_001",
  tot_hisp_latino_origin_known_ = "B03003_001",
  tot_hisp_latino_origin_ = "B03003_003")
 



# Pull ACS data for Page only
acs_VA_CO <- get_acs(variables = vars, 
                     key = myACSkey,
                     geography = "county",
                     state = "VA",
                     survey = "acs5",
                     output = "wide",
                     show_call = T,
                     geometry = T,
                     keep_geo_vars = T,
                     year = 2018)



acs_VA_CO$COUNTYFP <- as.numeric(acs_VA_CO$COUNTYFP)

# Create measures based on ACS variables

acs_VA_CO$per_black <- 
  round((acs_VA_CO$tot_race_black_african_american_alone_combination_E / acs_VA_CO$tot_race_known_E*100),2)
acs_VA_CO$per_hisp <-
  round((acs_VA_CO$tot_hisp_latino_origin_E / acs_VA_CO$tot_hisp_latino_origin_known_E*100),2)
acs_VA_CO$per_poverty <-
  round((acs_VA_CO$tot_income_12mths_blw_pov_lvl_B17020_E / acs_VA_CO$tot_pov_sts_12mths_age_B17020_E*100),2)
acs_VA_CO$per_hs_diploma <-
  round((acs_VA_CO$tot_edu_attain_25yrs_and_up_HS_diploma_B15003_E/acs_VA_CO$tot_edu_attain_25yrs_and_up_B15003_E*100),2)
acs_VA_CO$perw_adv_degree <- 
  round(((acs_VA_CO$tot_edu_attain_25yrs_and_up_assoc_B15003_E + 
            acs_VA_CO$tot_edu_attain_25yrs_and_up_bach_B15003_E +
            acs_VA_CO$tot_edu_attain_25yrs_and_up_masters_B15003_E +
            acs_VA_CO$tot_edu_attain_25yrs_and_up_prof_degree_B15003_E +
            acs_VA_CO$tot_edu_attain_25yrs_and_up_doctorate_B15003_E)/ acs_VA_CO$tot_edu_attain_25yrs_and_up_B15003_E*100),2)
  

VAcounty_data_merged <- inner_join(output, acs_VA_CO, by="COUNTYFP")

# write.csv(VAcounty_data_merged, 
#           "C:/Users/Admin/Documents/DSPG/Page/Page_County_V2/data\\VAcounty_data_master.csv",
#           row.names = FALSE)
