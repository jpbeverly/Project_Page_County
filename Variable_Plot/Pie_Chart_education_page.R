
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

per_education_attainment <- page_data %>% select( COUNTYFP, COUNAME, POPULATION, CENTER.LATITUDE, CENTER.LONGITUDE)

per_education_attainment$per_HS_diplima = 
  round((page_data$tot_edu_attain_25yrs_and_up_HS_diploma_B15003_E/ page_data$tot_edu_attain_25yrs_and_up_B15003_E)*100,2)

per_education_attainment$per_GED = 
  round((page_data$tot_edu_attain_25yrs_and_up_GED_B15003_E/ page_data$tot_edu_attain_25yrs_and_up_B15003_E)*100,2)

per_education_attainment$per_college_freashman = 
  round((page_data$tot_edu_attain_25yrs_and_up_somecollege_freshman_B15003_E/ page_data$tot_edu_attain_25yrs_and_up_B15003_E)*100,2)

per_education_attainment$per_above_clg_freashman = 
  round((page_data$tot_edu_attain_25yrs_and_up_somecollege_abovefreshman_B15003_E/ page_data$tot_edu_attain_25yrs_and_up_B15003_E)*100,2)

per_education_attainment$per_assoc = 
  round((page_data$tot_edu_attain_25yrs_and_up_assoc_B15003_E/ page_data$tot_edu_attain_25yrs_and_up_B15003_E)*100,2)

per_education_attainment$per_bach = 
  round((page_data$tot_edu_attain_25yrs_and_up_bach_B15003_E/ page_data$tot_edu_attain_25yrs_and_up_B15003_E)*100,2)

per_education_attainment$per_masters = 
  round((page_data$tot_edu_attain_25yrs_and_up_masters_B15003_E/ page_data$tot_edu_attain_25yrs_and_up_B15003_E)*100,2)

per_education_attainment$per_prof_degree = 
  round((page_data$tot_edu_attain_25yrs_and_up_prof_degree_B15003_E/ page_data$tot_edu_attain_25yrs_and_up_B15003_E)*100,2)

per_education_attainment$per_doctorate = 
  round((page_data$tot_edu_attain_25yrs_and_up_doctorate_B15003_E/ page_data$tot_edu_attain_25yrs_and_up_B15003_E)*100,2)


per_education_attainment$per_education_all <- per_education_attainment$per_HS_diplima+
  per_education_attainment$per_GED + per_education_attainment$per_college_freashman+ 
  per_education_attainment$per_above_clg_freashman + per_education_attainment$per_assoc +
  per_education_attainment$per_bach + per_education_attainment$per_masters + 
  per_education_attainment$per_prof_degree + per_education_attainment$per_doctorate

per_education_attainment$per_less_HS_Diploma <- 100 - per_education_attainment$per_education_all


#mycols <- c("#9A5EA6", "#E5C473", "#B98B50", "#61276D", "#2E368F","#D8C5E0", "#0000FF", "#FF0000", "#FFFF00","#00FF00")

mycols <- c("#9A5EA6", "#E5C473", "#B98B50", "#61276D", "#55DDE0","#D8C5E0", "#0000FF", "#FF0000", "#FFFF00","#00FF00")


labels <- c("Doctorate", "Professional Degree", "Masters Degree", "Associate Degree",
            "College Freasemen", "GE Degree", "Bachelors Degree", "Above College Freashmen",
            "Less then HS Deploma","HS Diploma")

test <- per_education_attainment %>% filter( COUNAME == "Page") %>%  select (- c( COUNTYFP, POPULATION, CENTER.LATITUDE, CENTER.LONGITUDE, per_education_all)) %>%
  pivot_longer(cols = -c(COUNAME)) 
test <- test[order(test$value),]
test$label <- paste(test$value, "%" , labels)
ggplot(test, aes(x="", y=value, fill=fct_inorder(label))) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y",direction = -1)+
  scale_fill_manual(values = mycols)+
  labs(x = NULL, y = NULL, fill = NULL, title = "Education Attainment Percentage in Page County")+
  theme_classic()


