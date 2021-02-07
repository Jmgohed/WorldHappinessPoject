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
scatter.smooth(x=EnvironHappiness_File$DrinkingWater, y=EnvironHappiness_File$HappinessScore, main="Happiness Score by ...")
### is linear????  (note another variable in play)
scatter.smooth(x=EnvironHappiness_File$SanitizationServices, y=EnvironHappiness_File$HappinessScore, main="Happiness Score by ...")
### is linear????
scatter.smooth(x=EnvironHappiness_File$CleanFuelTech, y=EnvironHappiness_File$HappinessScore, main="Happiness Score by ...")
### is linear try (qudrative) 


### is nonlinear
scatter.smooth(x=EnvironHappiness_File$Freedom, y=EnvironHappiness_File$HappinessScore, main="Happiness Score by ...")
scatter.smooth(x=EnvironHappiness_File$TrustGovernmentCorruption, y=EnvironHappiness_File$HappinessScore, main="Happiness Score by ...")
### is nonlinear try qudratice
scatter.smooth(x=EnvironHappiness_File$Generosity, y=EnvironHappiness_File$HappinessScore, main="Happiness Score by ...")
### is nonlinear try split generosity half  is vs not, can use quartiles
scatter.smooth(x=EnvironHappiness_File$DystopiaResidual, y=EnvironHappiness_File$HappinessScore, main="Happiness Score by ...")
### is nonlinear qua
scatter.smooth(x=EnvironHappiness_File$Doctors, y=EnvironHappiness_File$HappinessScore, main="Happiness Score by ...")
### is nonlinear
scatter.smooth(x=EnvironHappiness_File$NurseMidwife, y=EnvironHappiness_File$HappinessScore, main="Happiness Score by ...")
### is nonlinear

###~ Using 3 linear variables: EconomyGDP, DrinkingWater, CleanFuelTech ~###

#---------------------
#2 Homoscedasticity

lmModALL <- lm((HappinessScore
                ~EconomyGDP + 
                  DrinkingWater + 
                  CleanFuelTech), 
               data=EnvironHappiness_File)
lmtest::bptest(lmModALL)
# it Pass because not significant 

lmMod1 <- lm((HappinessScore~EconomyGDP),data=EnvironHappiness_File)
lmtest::bptest(lmMod1)
# it Pass because not significant 

lmMod2 <- lm((HappinessScore~DrinkingWater),data=EnvironHappiness_File)
lmtest::bptest(lmMod2)
# it Pass because not significant 

lmMod3 <- lm((HappinessScore~CleanFuelTech),data=EnvironHappiness_File)
lmtest::bptest(lmMod3)
# it Failed because is significant 


#---------------------
#3 Homogenity of variance: the variance of the error terms is constant for all values of x.
gvlma(lmModALL)
## All assumptions acceptable
gvlma(lmMod1)
gvlma(lmMod2)
gvlma(lmMod3)
                 

#4 The x's are fixed and measured without error. (In other words, the x's can be considered as known constants.)
## Assumption Met

#5 Multicollinearity: the observations are independent.
EnvironHappiness_File_quant1 <- EnvironHappiness_File[, c(4,16,24,27)]

chart.Correlation(EnvironHappiness_File_quant1, histogram=FALSE, method="pearson")

corr_matrix <- cor(EnvironHappiness_File_quant1)

corr_matrix

#---------------------
#6 Lack of outliers

CookD(lmModALL, group=NULL, plot=TRUE, idn=3, newwd=TRUE)
summary(influence.measures(lmModALL))
## Outlines on rows for x: 54, 60, 69

# test for leverage
lev = hat(model.matrix(lmModALL))
plot(lev)

#
EnvironHappiness_File[lev>.2,]

## Test outliers in y space
car::outlierTest(lmModALL)
 
#test for influential values, or outliers in both x and y space, there are two metrics to look at: DFFITS and DFBETAS
summary(influence.measures(lmModALL))
# ALL Good (not over 1)

EnvironHappiness_File2 = EnvironHappiness_File[-c(54,60,69),]

lmModAll2 <- lm(HappinessScore ~  EconomyGDP + 
                  DrinkingWater + 
                  CleanFuelTech, data = EnvironHappiness_File2)
summary(influence.measures(lmModAll2))
# check dfb.1 and dffit is nothing is over 1


#---------------------
summary(lmModAll2)
### EconomyGDP*** 
### DrinkingWater
### CleanFuelTech 


#---------------------
# Non Linear modeling (family, lifeex, freedom, doctor)

## Exponential Model
exModFamily <- lm(log(EnvironHappiness_File$HappinessScore)~EnvironHappiness_File$Family)
summary(exModFamily)
### p value is shows Significant. Est. 1.06 to 60%

exModFreedom <- lm(log(EnvironHappiness_File$HappinessScore)~EnvironHappiness_File$Freedom)
summary(exModFreedom)


exModDoctors <- lm(log(EnvironHappiness_File$HappinessScore)~EnvironHappiness_File$Doctors)
summary(exModDoctors)


exModHLE <- lm(log(EnvironHappiness_File$HappinessScore)~EnvironHappiness_File$HealthLifeExpectancy)
summary(exModHLE)














#---------------------
#Testing using Quadratic Modeling Plot 

scatter.smooth(x=EnvironHappiness_File$CleanFuelTech, y=EnvironHappiness_File$HappinessScore, main="Happiness Score by ...")
### is linear try (qudrative) 

quadPlot <- ggplot(EnvironHappiness_File, aes(x = CleanFuelTech, y=HappinessScore)) + geom_point() + stat_smooth(method = "lm", formula = y ~x + I(x^2), size =1)
print(quadPlot)



#---------------------

chart.Correlation(EnvironHappiness_File, histogram=FALSE, method="pearson")

### Histogram
HappinessScoreHistogram <- ggplot(EnvironHappiness_File, aes(x = HappinessScore))
HappinessScoreHistogram + geom_histogram()
