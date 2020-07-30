#######################################################################
# see page 89 secion 6.1 of ``Constructing_composite_indicators.pdf''
#######################################################################

## The following code is from
## https://www.geo.fu-berlin.de/en/v/soga/Geodata-analysis/factor-analysis/A-simple-example-of-FA/index.html#:~:text=In%20the%20R%20software%20factor,specified%20by%20the%20argument%20factors%20.
#install.packages("umx")
library(umx)


data <- read.csv("VAcounty_data_master.csv")
vulnerable <- data[,]
## vulnerable is a data set of x columns
str(vulnerable)

#check consistency for chronbach's alpha
mymat <- data.matrix(vulnerable)
reliability(cov(mymat))

vulnerable <- scale(vulnerable) ## standardize the data

## factor analysis with rotation type varimax
## the number of factors should be defined by some theoretical model/pre-assumptions
vul.fa <- factanal(vulnerable, factors = 2, rotation = 'varimax')

vul.fa

#Compute the fraction of the variable's total variance explained by the factor -> communality
var_exp <- apply(vul.fa$loadings^2,1,sum)

#Get the residual matrix
Lambda <- vul.fa$loadings
Psi <- diag(vul.fa$uniquenesses)
S <- vul.fa$correlation
Sigma <- Lambda %*% t(Lambda) + Psi

round(S-Sigma, 6)


## calculate the composite index 
Comp_index <- vulnerable %*% vul.fa$loadings %*% var_exp
## loadings are the coefficients of linear combination of two underlying factors
## var_exp is the proportion of variance explained by the factor
## this step transforms the design matrix from 5d to 2d

## the next step doesn't make sense to me
SS_loadings <- colSums(food.fa$loadings^2)/sum(colSums(food.fa$loadings^2))
Comp_index <- as.matrix(food) %*% food.fa$loadings %*% SS_loadings