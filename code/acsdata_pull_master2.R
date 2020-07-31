# ---
# title: "Page ACS Virginia Counties Data
# author: "Talib Grant" 
#Edited: Josh Beverly
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
library(readr)


myACSkey <- "9fb41fe9979018809a049069659efa1df8810000"

#show available variables for ACS survey
acs5 <- load_variables(2018, "acs5", cache=T)
view(acs5)

acs5_subject <- load_variables(2018, "acs5/subject", cache=T)
View(acs5_subject)

acs5_profile<- load_variables(2018, "acs5/profile", cache=T)
View(acs5_profile)

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
  tot_hisp_latino_origin_ = "B03003_003",
   tot_speak_spanish = "B06007_008",
   tot_speak_other_language = "B06007_005",
   tot_language = "B06007_001",
  tot_6_17_single_parent = "B05009_031",
  tot_6_single_parent = "B05009_013",
  tot_single_parent = "B05009_001",
  pov_12month_income = "B17001_002",
  tot_12month_income= "B17001_001",
  tot_male_17 = "B01001_006",
  tot_male_14 = "B01001_005",
  tot_male_9 = "B01001_004",
  tot_male_5 = "B01001_003",
  tot_pop = "B01001_001",
  tot_female_5 = "B01001_027",
  tot_female_9 = "B01001_028",
  tot_female_14 = "B01001_029",
  tot_female_17 = "B01001_030",
  tot_male_65 = "B01001_020",
  tot_male_67 = "B01001_021",
  tot_male_70 = "B01001_022",
  tot_male_75 = "B01001_023",
  tot_male_80 = "B01001_024",
  tot_male_85 = "B01001_025",
  tot_female_65 = "B01001_044",
  tot_female_67 = "B01001_045",
  tot_female_70 = "B01001_046",
  tot_female_75 = "B01001_047",
  tot_female_80 = "B01001_048",
  tot_female_85 = "B01001_049",
  tot_disability = "B18101_001",
  tot_dis_pop = "B01003_001",
  tot_group_quarters18 = "B09001_010",
  tot_group_quarters = "B26001_001"
  
 
)


# Pull ACS data for VA counties
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
  round(((acs_VA_CO$tot_race_black_african_american_alone_combination_E) / acs_VA_CO$tot_race_known_E)*100,2)
acs_VA_CO$per_hisp <-
  round(((acs_VA_CO$tot_hisp_latino_origin_E) / acs_VA_CO$tot_hisp_latino_origin_known_E)*100,2)
acs_VA_CO$per_poverty <-
  round(((acs_VA_CO$tot_income_12mths_blw_pov_lvl_B17020_E) / acs_VA_CO$tot_pov_sts_12mths_age_B17020_E)*100,2)
acs_VA_CO$per_hs_diploma <-
  round(((acs_VA_CO$tot_edu_attain_25yrs_and_up_HS_diploma_B15003_E)/acs_VA_CO$tot_edu_attain_25yrs_and_up_B15003_E)*100,2)
acs_VA_CO$perw_adv_degree <- 
  round((((acs_VA_CO$tot_edu_attain_25yrs_and_up_assoc_B15003_E + 
            acs_VA_CO$tot_edu_attain_25yrs_and_up_bach_B15003_E +
            acs_VA_CO$tot_edu_attain_25yrs_and_up_masters_B15003_E +
            acs_VA_CO$tot_edu_attain_25yrs_and_up_prof_degree_B15003_E +
            acs_VA_CO$tot_edu_attain_25yrs_and_up_doctorate_B15003_E)/ acs_VA_CO$tot_edu_attain_25yrs_and_up_B15003_E)*100),2)
acs_VA_CO$language <- round((((acs_VA_CO$tot_speak_spanishE + acs_VA_CO$tot_speak_other_languageE)/acs_VA_CO$tot_languageE)*100),2)
  acs_VA_CO$per_poverty2 <- round((acs_VA_CO$pov_12month_incomeE/acs_VA_CO$tot_12month_incomeE)*100,2)
  acs_VA_CO$per_Age17 <- round(((acs_VA_CO$tot_male_5E + acs_VA_CO$tot_male_9E + acs_VA_CO$tot_male_14E + acs_VA_CO$tot_male_17E + acs_VA_CO$tot_female_5E +acs_VA_CO$tot_female_9E +acs_VA_CO$tot_female_14E + acs_VA_CO$tot_female_17E)/acs_VA_CO$tot_popE)*100, 2)
  acs_VA_CO$per_Age65 <- round(((acs_VA_CO$tot_male_65E +acs_VA_CO$tot_male_67E + acs_VA_CO$tot_male_70E +acs_VA_CO$tot_male_75E + acs_VA_CO$tot_male_80E +acs_VA_CO$tot_male_85E +
                                   acs_VA_CO$tot_female_65E + acs_VA_CO$tot_female_67E + acs_VA_CO$tot_female_70E + acs_VA_CO$tot_female_75E + acs_VA_CO$tot_female_80E +acs_VA_CO$tot_female_85E)/acs_VA_CO$tot_popE)*100, 2)
  acs_VA_CO$per_disability <- round ((acs_VA_CO$tot_disabilityE/acs_VA_CO$tot_dis_popE)*100,2)
  acs_VA_CO$per_group_quarters <- round((acs_VA_CO$tot_group_quarters18E/acs_VA_CO$tot_group_quartersE)*100,2)
  acs_VA_CO$per_sing_par <- round(((acs_VA_CO$tot_6_17_single_parentE + acs_VA_CO$tot_6_single_parentE)/acs_VA_CO$tot_single_parentE)*100,2)

#VAcounty_data_merged2 <- inner_join(output, acs_VA_CO, by="COUNTYFP")

  view(acs_VA_CO)
indicator_data <- acs_VA_CO[,c(1:10,110:121)]  
write.csv(indicator_data, 
          "indicator_data.csv")

data <- read_csv("indicator_data.csv")
