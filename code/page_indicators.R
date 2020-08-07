#######################################################################
# see page 89 secion 6.1 of ``Constructing_composite_indicators.pdf''
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
pov <- data[, 155:171]
mymat2 <- data.matrix(vul)
alpha(pov)
omega(pov, check.keys = TRUE)
omega(vul,nfactors = 5,n.obs =133, check.keys = TRUE)
alpha(mymat, check.keys = TRUE)

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

write.csv(Index, file = "Composite_Index.csv")

# mypalette2 <- colorQuantile(palette="OrRd", Composite$Comp_index,n=9)
# data2 <- read_csv("Composite_Index")
# 
# library(ggmap)
# register_google(key = "9fb41fe9979018809a049069659efa1df8810000")
# 
# va_basemap <- get_map(location=c(lon = -77.5, lat = 38.8), zoom=6, scale = "auto", source = 'stamen')
# 
# ggmap(va_basemap) +
#   geom_sf(data=Composite ,aes(fill= mypalette2(Comp_index), geometry=geometry), inherit.aes = FALSE) +
#   geom_sf(data=va_sf, fill="transparent", color="black", size=.5, inherit.aes = FALSE) +
#   geom_sf(data=page_outline, fill="transparent", color="orange", size=.75, inherit.aes = FALSE) +
#   scale_fill_brewer(name = "Vulnerability Composite Index", palette = "OrRd", labels=c("High","","","","", "","","","Low")) +
#   coord_sf(crs = st_crs(4326)) +
#   theme(axis.line=element_blank(),axis.text.x=element_blank(),
#         axis.text.y=element_blank(),axis.ticks=element_blank(),
#         axis.title.x=element_blank(),
#         axis.title.y=element_blank(),legend.position="right",
#         panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
#         panel.grid.minor=element_blank(),plot.background=element_blank()) +
#   ggtitle("Vulnerability Index by County in Virginia") +
#   ylim(-36.5,-39.5) + xlim(-84, -75) 
# 
# ggplot(Composite) + 
#   geom_sf(data = Composite,aes(fill= mypalette2(Comp_index), geometry=geometry), inherit.aes = FALSE) +
#   geom_sf(data=va_sf, fill="transparent", color="black", size=.5, inherit.aes = FALSE) +
#   geom_sf(data=page_outline, fill="transparent", color="orange", size=.75, inherit.aes = FALSE) +
#   scale_fill_brewer(name = "Vulnerability Composite Index", palette = "OrRd", labels=c("High","","","","", "","","","Low")) +
#   coord_sf(crs = st_crs(4326)) +
#   theme(axis.line=element_blank(),axis.text.x=element_blank(),
#         axis.text.y=element_blank(),axis.ticks=element_blank(),
#         axis.title.x=element_blank(),
#         axis.title.y=element_blank(),legend.position="right",
#         panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
#         panel.grid.minor=element_blank(),plot.background=element_blank()) +
#   ggtitle("Vulnerability Index by County in Virginia") +
#   ylim(-36.5,-39.5) + xlim(-84, -75) 
#Compute the fraction of the variable's total variance explained by the factor -> communality
#var_exp <- apply(vul.fa$loadings^2,1,sum)
#var_exp_mat <- data.matrix(var_exp)

#Get the residual matrix
#Lambda <- vul.fa$loadings
#Psi <- diag(vul.fa$uniquenesses)
#S <- vul.fa$correlation
#Sigma <- Lambda %*% t(Lambda) + Psi

#round(S-Sigma, 6)



