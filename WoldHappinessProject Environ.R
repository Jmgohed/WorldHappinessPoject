# Import Packages & Dataset
library("dplyr")
library("car")
library("caret")
library("gvlma")
library("predictmeans") 
library("e1071")

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

# Test Assumptions
## Test for Linearity
scatter.smooth(x=EnvironHappiness_File$EconomyGDP, y=EnvironHappiness_File$HappinessScore, main="Happiness Score by ...")
### is linear
scatter.smooth(x=EnvironHappiness_File$Family, y=EnvironHappiness_File$HappinessScore, main="Happiness Score by ...")
### is linear
scatter.smooth(x=EnvironHappiness_File$HealthLifeExpectancy, y=EnvironHappiness_File$HappinessScore, main="Happiness Score by ...")
### is linear
scatter.smooth(x=EnvironHappiness_File$Freedom, y=EnvironHappiness_File$HappinessScore, main="Happiness Score by ...")
### is nonlinear
scatter.smooth(x=EnvironHappiness_File$TrustGovernmentCorruption, y=EnvironHappiness_File$HappinessScore, main="Happiness Score by ...")
### is nonlinear
scatter.smooth(x=EnvironHappiness_File$Generosity, y=EnvironHappiness_File$HappinessScore, main="Happiness Score by ...")
### is nonlinear
scatter.smooth(x=EnvironHappiness_File$DystopiaResidual, y=EnvironHappiness_File$HappinessScore, main="Happiness Score by ...")
### is nonlinear
scatter.smooth(x=EnvironHappiness_File$DrinkingWater, y=EnvironHappiness_File$HappinessScore, main="Happiness Score by ...")
### is linear????
scatter.smooth(x=EnvironHappiness_File$SanitizationServices, y=EnvironHappiness_File$HappinessScore, main="Happiness Score by ...")
### is linear????
scatter.smooth(x=EnvironHappiness_File$Doctors, y=EnvironHappiness_File$HappinessScore, main="Happiness Score by ...")
### is nonlinear
scatter.smooth(x=EnvironHappiness_File$NurseMidwife, y=EnvironHappiness_File$HappinessScore, main="Happiness Score by ...")
### is nonlinear
scatter.smooth(x=EnvironHappiness_File$CleanFuelTech, y=EnvironHappiness_File$HappinessScore, main="Happiness Score by ...")
### is linear


#---------------------



