#######################################################################
# see page 89 secion 6.1 of ``Constructing_composite_indicators.pdf''
#Other Resources 
#Hartmann, K., Krois, J., Waske, B. (2018): E-Learning Project SOGA: Statistics and Geospatial Data Analysis. Department of Earth Sciences, Freie Universitaet Berlin.
#
#Johnson, R.A and Wichern, D.W, Applied Multivariate Statistical Analysis (6th ed). Pearson, 2007. Ch 9.
#######################################################################

## The following code is from
## https://www.geo.fu-berlin.de/en/v/soga/Geodata-analysis/factor-analysis/A-simple-example-of-FA/index.html#:~:text=In%20the%20R%20software%20factor,specified%20by%20the%20argument%20factors%20.
#install.packages("umx")
library(umx)
#install.packages("psych")
library(psych)
#install.packages("GPArotation")
library(GPArotation)
#install.packages("sem")
library(sem)
#install.packages("plm")
library(plm)
library(readr)
library(dplyr)
library(sf)
library(ggplot2)
library (stringr)
library(tidyverse)
library(sf)
library(scales)
library(readxl)
library(ggmap)
library(leaflet)
library(rgdal)
library(rgeos)
library(geosphere)
library(pracma)
library(tidycensus)
library(acs)
library(radiant.multivariate)

data <- read_csv("data/VAcounty_data_master.csv")
#incarceration <- read_xlsx("code/VA_Incarceration_Rates.xlsx")
#incarc2015 <- subset(incarceration, year == 2015)
#prison_rates <- incarc2015[,c("fips","total_prison_pop_rate")]
#jail_rates <- incarc2015[,c("fips", "total_jail_pop_rate")]

#str(prison_rates)
#data2 <- inner_join(data,prison_rates, by = "fips")
#data_final <- inner_join(data2, jail_rates, by = "fips") 
#data <- data_final[,-c(172,173)]
  
vul <- data[,c(9,10,156:160,162,167,170)]
vul2 <- data[,c(9,10,155:164,166:167,170:171)]

view(vul)
#Standarsize variables
vul_scale <- scale(vul)
vul_scale2<- scale(vul2)

#matrix form
mymat <- data.matrix(vul_scale)
mymat3 <- data.matrix(vul2)

## factor analysis with rotation type varimax
## the number of factors should be defined by some theoretical model/pre-assumptions
vul.fa <- factanal(mymat, factors = 5, n.obs = 133, rotation = 'varimax', lower = 0.1)
vul.fa


#Correlation and Cov matrices
cormat1 <- cor(mymat)
covmat1 <- cov(mymat)

#chronbach's Alpha (Group Predictors)
#pov <- data[, 155:171]
mymat2 <- data.matrix(vul)
omega(vul2, check.keys = TRUE)
omega(vul,nfactors = 5,n.obs =133, check.keys = TRUE)


#Check Residual Matrix
round(cor(mymat) - (vul.fa$loadings %*% t(vul.fa$loadings) + diag(vul.fa$uniquenesses)), 3)

#Calculate Prop_var
Prop_var <- data.matrix(colSums(vul.fa$loadings^2)/nrow(vul.fa$loadings^2))

## calculate the composite index 
Comp_index <- mymat %*% vul.fa$loadings %*% Prop_var
## loadings are the coefficients of linear combination of two underlying factors
## Pro_var is the proportion of variance explained by the factor
str(Comp_index)

max(Comp_index)
min(Comp_index)


data$zeros1 <- rep(51000, 133)
data$fips <- data$zeros1+data$COUNTYFP

Index <- data.frame(data$fips,Comp_index)
names(Index)[names(Index) == "data.fips"] <- "GEOID"
shp <- st_read("code/cb_2018_us_county_500k.shp")
merged <- merge(Index, shp, by = "GEOID")
Composite <- st_as_sf(merged)

#write.csv(Index, file = "Composite_Index.csv")



