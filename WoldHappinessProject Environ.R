# Import Packages & Dataset
library("dplyr")
library("car")
library("caret")
library("gvlma")
library("predictmeans") 
library("e1071")
library("rcompanion")
library(c("tidyr", "devtools"))
library("gmodels")
library("tidyr")
library("gmodels")
library("caret")
library("predictmeans")
library("ggplot2")
install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")
library("corrplot")

head(EnvironHappiness_File)
head(HealthHappiness_File)

# Data Wrangle: recoded column names
colnames(EnvironHappiness_File)

EnvironHappiness_File$HappinessScore <- EnvironHappiness_File$"Happiness Score"
EnvironHappiness_File$HappinessRank <- EnvironHappiness_File$"Happiness Rank"  
EnvironHappiness_File$EconomyGDP <- EnvironHappiness_File$"Economy (GDP per Capita)"
EnvironHappiness_File$HealthLifeExpectancy <- EnvironHappiness_File$"Health (Life Expectancy)"
EnvironHappiness_File$TrustGovernmentCorruption <- EnvironHappiness_File$"Trust (Government Corruption)"
EnvironHappiness_File$DystopiaResidual <- EnvironHappiness_File$"Dystopia Residual"


colnames(EnvironHappiness_File)

#---------------------
#Assumptions for Multiple Linear Regression
#1 There is a linear relationship between x and y.
#2 Homoscedasticity: The error term is normally distributed.
#3 Homogenity of variance: the variance of the error terms is constant for all values of x.
#4 The x's are fixed and measured without error. (In other words, the x's can be considered as known constants.)
#5 Multicollinearity: the observations are independent.
#6 Lack of outliers

#---------------------

# Scatter Plot
## Is Linearity
scatter.smooth(x=EnvironHappiness_File$EconomyGDP, y=EnvironHappiness_File$HappinessScore, main="Happiness Score by ...")
scatter.smooth(x=EnvironHappiness_File$Family, y=EnvironHappiness_File$HappinessScore, main="Happiness Score by ...")
scatter.smooth(x=EnvironHappiness_File$HealthLifeExpectancy, y=EnvironHappiness_File$HappinessScore, main="Happiness Score by ...")
scatter.smooth(x=EnvironHappiness_File$Family, y=EnvironHappiness_File$HappinessScore, main="Happiness Score by ...")
scatter.smooth(x=EnvironHappiness_File$HealthLifeExpectancy, y=EnvironHappiness_File$HappinessScore, main="Happiness Score by ...")


### is nonlinear
scatter.smooth(x=EnvironHappiness_File$Freedom, y=EnvironHappiness_File$HappinessScore, main="Happiness Score by ...")
scatter.smooth(x=EnvironHappiness_File$TrustGovernmentCorruption, y=EnvironHappiness_File$HappinessScore, main="Happiness Score by ...")
### is nonlinear try qudratice
scatter.smooth(x=EnvironHappiness_File$Generosity, y=EnvironHappiness_File$HappinessScore, main="Happiness Score by ...")
### is nonlinear try split generosity half  is vs not, can use quartiles
scatter.smooth(x=EnvironHappiness_File$DystopiaResidual, y=EnvironHappiness_File$HappinessScore, main="Happiness Score by ...")
### is nonlinear qua
scatter.smooth(x=EnvironHappiness_File$DrinkingWater, y=EnvironHappiness_File$HappinessScore, main="Happiness Score by ...")
### is linear????  (note another variable in play)
scatter.smooth(x=EnvironHappiness_File$SanitizationServices, y=EnvironHappiness_File$HappinessScore, main="Happiness Score by ...")
### is linear????
scatter.smooth(x=EnvironHappiness_File$Doctors, y=EnvironHappiness_File$HappinessScore, main="Happiness Score by ...")
### is nonlinear
scatter.smooth(x=EnvironHappiness_File$NurseMidwife, y=EnvironHappiness_File$HappinessScore, main="Happiness Score by ...")
### is nonlinear
scatter.smooth(x=EnvironHappiness_File$CleanFuelTech, y=EnvironHappiness_File$HappinessScore, main="Happiness Score by ...")
### is linear try (qudrative) 

#---------------------

## Homoscedasticity
d <- ggplot(EnvironHappiness_File, aes(x = EconomyGDP))
d + geom_histogram()

lmMod <- lm(EconomyGDP~HappinessScore, data=EnvironHappiness_File)
lmtest::bptest(lmMod)

#homogeneity
gvlma(lmMod)

lmMod <- lm(EconomyGDP~HappinessScore, data=EnvironHappiness_File)
lmtest::bptest(lmMod)

#homogeneity
gvlma(lmMod)


#---------------------
#Testing using Quadratic Modeling Plot

quadPlot <- ggplot(EnvironHappiness_File, aes(x = CleanFuelTech, y=HappinessScore)) + geom_point() + stat_smooth(method = "lm", formula = y ~x + I(x^2), size =1)
print(quadPlot)

scatter.smooth(x=EnvironHappiness_File$CleanFuelTech, y=EnvironHappiness_File$HappinessScore, main="Happiness Score by ...")
### is linear try (qudrative) 


#---------------------
#2 Homoscedasticity: The error term is normally distributed.


