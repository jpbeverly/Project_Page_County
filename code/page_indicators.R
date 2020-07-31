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
install.packages("sem")
library(sem)
install.packages("plm")
library(plm)

data <- read_csv("VAcounty_data_master.csv")
vulnerable <- data[,89:98]
#vulnerable <- vulnerable[,-1]
#vulnerable <- vulnerable[,-'geometry']
#vulnerable <- as.numeric(vulnerable)
vulnerable2 <- scale(vulnerable) ## standardize the data

## vulnerable is a data set of x columns
#str(vulnerable)

#Convert to matrices
mymat <- data.matrix(vulnerable)
mymat2 <- data.matrix(vulnerable2)

#Correlation and Cov matrices
cormat1 <- cor(mymat)
covmat1 <- cov(mymat)
cormat2 <- cor(mymat2)
covmat1 <- cov(mymat2)

#chronbach's Alpha (Group Predictors)
omega(mymat, check.keys = TRUE)
omega(mymat2, check.keys = TRUE)

## factor analysis with rotation type varimax
## the number of factors should be defined by some theoretical model/pre-assumptions
vul.fa <- factanal(mymat, factors = 5, n.obs = 20, rotation = 'varimax')
vul.fa2 <- factanal(mymat2, factors = 5, n.obs = 133, rotation = 'varimax')

vul.fa
vul.fa2#Doesnt change anything

#Check Residual Matrix
round(cor(mymat) - (vul.fa$loadings %*% t(vul.fa$loadings) + diag(vul.fa$uniquenesses)), 3)

#Calculate Prop_var
Prop_var <- data.matrix(colSums(vul.fa$loadings^2)/nrow(vul.fa$loadings^2))

## calculate the composite index 
Comp_index <- mymat %*% vul.fa$loadings %*% Prop_var
## loadings are the coefficients of linear combination of two underlying factors
## Pro_var is the proportion of variance explained by the factor

max(Comp_index)
min(Comp_index)


#Compute the fraction of the variable's total variance explained by the factor -> communality
#var_exp <- apply(vul.fa$loadings^2,1,sum)
#var_exp_mat <- data.matrix(var_exp)

#Get the residual matrix
#Lambda <- vul.fa$loadings
#Psi <- diag(vul.fa$uniquenesses)
#S <- vul.fa$correlation
#Sigma <- Lambda %*% t(Lambda) + Psi

#round(S-Sigma, 6)



