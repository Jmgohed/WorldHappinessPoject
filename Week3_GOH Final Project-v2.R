#Week 3 Goh Final Project
#Health and Happiness

library("readxl")
HealthHappiness = read_excel("C:/Users/krist/Documents/2020 Bethel Tech/Final Project/Data Wrangling/HealthHappiness_File.xlsx",sheet = "Sheet1") 
head(HealthHappiness)

library("car")
library("rcompanion")
library(c("tidyr", "devtools"))
library("gmodels")
library("dplyr")
library("tidyr")
library("gmodels")
library("caret")
library("gvlma")
library("predictmeans")
library("e1071")
library("ggplot2")
install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")
unique(HealthHappiness$Country)
install.packages("corrplot")
library("corrplot")

summary(HealthHappiness$`Happiness Score`)


## subset for correlation matrix
HealthHappiness2 <- HealthHappiness[,c('Dim1', 'Location', 'Region', 'Country', 'Happiness Score', 'InfantMortality', 'LifeExpectancy', 'MortalityRatePoisoning', 'SuicideRates', 'AlcoholSubstanceAbuse', 'Tobacco', 'DiseaseDeath')]

head(HealthHappiness2)

HealthHappiness3 <- filter(HealthHappiness2, Dim1 == "Both sexes")
head(HealthHappiness3)
View(HealthHappiness3)

HealthHappiness3$HappinessScore <- HealthHappiness3$'Happiness Score'


#Assumptions for Multiple Linear Regression
#1 There is a linear relationship between x and y.
#2 Homoscedasticity: The error term is normally distributed.
#3 Homogenity of variance: the variance of the error terms is constant for all values of x.
#4 The x's are fixed and measured without error. (In other words, the x's can be considered as known constants.)
#5 Multicollinearity: the observations are independent.
#6 Lack of outliers


# 1 Linear Relationship
#scatter plot
scatter.smooth(x=HealthHappiness3$LifeExpectancy, y=HealthHappiness3$HappinessScore, main="Happiness Score by Life Expectancy")
#somewhat linear
scatter.smooth(x=HealthHappiness3$MortalityRatePoisoning, y=HealthHappiness3$HappinessScore, main="Happiness Score by Mortality Rate Poisoning")
scatter.smooth(x=HealthHappiness3$SuicideRates, y=HealthHappiness3$HappinessScore, main="Happiness Score by Suicide Rates")
scatter.smooth(x=HealthHappiness3$AlcoholSubstanceAbuse, y=HealthHappiness3$HappinessScore, main="Happiness Score by Alcohol Substance Abuse")
scatter.smooth(x=HealthHappiness3$Tobacco, y=HealthHappiness3$HappinessScore, main="Happiness Score by Tobacco Use")
scatter.smooth(x=HealthHappiness3$DiseaseDeath, y=HealthHappiness3$HappinessScore, main="Happiness Score by Chronic Disease Death")

scatter.smooth(x=HealthHappiness3$DiseaseDeath, y=HealthHappiness3$LifeExpectancy, main="H")
scatter.smooth(x=HealthHappiness3$MortalityRatePoisoning, y=HealthHappiness3$LifeExpectancy, main="H")
scatter.smooth(x=HealthHappiness3$SuicideRates, y=HealthHappiness3$LifeExpectancy, main="H")
scatter.smooth(x=HealthHappiness3$Tobacco, y=HealthHappiness3$LifeExpectancy, main="H")


#homoscedasity

lmMod <- lm(HappinessScore~LifeExpectancy, data=HealthHappiness3)
lmtest::bptest(lmMod)

#homogeneity
gvlma(lmMod)


####### WorldBankData and Happiness

library("readxl")
WorldBank = read_excel("C:/Users/krist/Documents/2020 Bethel Tech/Final Project/Possible Data Sets/WorldBankData.xlsx",sheet = "Sheet4") 
View(WorldBank)

WorldBank_quant1 <- WorldBank[, c(3,22,23,24,25,26,27,28,29,30,31,32,33,34)]
WorldBank_quant2 <- WorldBank[, c(3,22,23,24,25,26,27,28,29,30,31,32,33,34)]

chart.Correlation(WorldBank_quant1, histogram=FALSE, method="pearson")
chart.Correlation(WorldBank_quant2, histogram=FALSE, method="pearson")

corr_matrix <- cor(WorldBank_quant1)
corr_matrix

#scatter plot

scatter.smooth(x=WorldBank$AdolescentSchool, y=WorldBank$HappinessScore, main="Happiness Score by ...")
scatter.smooth(x=WorldBank$ArmedForces, y=WorldBank$HappinessScore, main="Happiness Score by ...")
scatter.smooth(x=WorldBank$ArmedForces2, y=WorldBank$HappinessScore, main="Happiness Score by ...")
scatter.smooth(x=WorldBank$BasicDrinkWater, y=WorldBank$HappinessScore, main="Happiness Score by ...")
scatter.smooth(x=WorldBank$BasicSanitization, y=WorldBank$HappinessScore, main="Happiness Score by ...")
#somewhat linear
scatter.smooth(x=WorldBank$BussinessScore, y=WorldBank$HappinessScore, main="Happiness Score by ...")
#somewhat linear
scatter.smooth(x=WorldBank$Cellular, y=WorldBank$HappinessScore, main="Happiness Score by ...")
#somewhat linear
scatter.smooth(x=WorldBank$Electricity, y=WorldBank$HappinessScore, main="Happiness Score by ...")
scatter.smooth(x=WorldBank$FertilityRate, y=WorldBank$HappinessScore, main="Happiness Score by ...")
#somewhat linear
scatter.smooth(x=WorldBank$FoodInsecurity, y=WorldBank$HappinessScore, main="Happiness Score by ...")
#somewhat linear
scatter.smooth(x=WorldBank$GDPGrowth, y=WorldBank$HappinessScore, main="Happiness Score by ...")
scatter.smooth(x=WorldBank$HealthExpGDP, y=WorldBank$HappinessScore, main="Happiness Score by ...")
#somewhat linear
scatter.smooth(x=WorldBank$HealthExpGov, y=WorldBank$HappinessScore, main="Happiness Score by ...")
#somewhat linear
scatter.smooth(x=WorldBank$LegalRightsIndex, y=WorldBank$HappinessScore, main="Happiness Score by ...")
scatter.smooth(x=WorldBank$Manufactoring, y=WorldBank$HappinessScore, main="Happiness Score by ...")
scatter.smooth(x=WorldBank$NationalIncome, y=WorldBank$HappinessScore, main="Happiness Score by ...")
scatter.smooth(x=WorldBank$Pop1564, y=WorldBank$HappinessScore, main="Happiness Score by ...")
scatter.smooth(x=WorldBank$Pop65, y=WorldBank$HappinessScore, main="Happiness Score by ...")
scatter.smooth(x=WorldBank$PopFem, y=WorldBank$HappinessScore, main="Happiness Score by ...")
scatter.smooth(x=WorldBank$PopMale, y=WorldBank$HappinessScore, main="Happiness Score by ...")
scatter.smooth(x=WorldBank$PrimarySchool, y=WorldBank$HappinessScore, main="Happiness Score by ...")
scatter.smooth(x=WorldBank$RenewableEnergy, y=WorldBank$HappinessScore, main="Happiness Score by ...")
#somewhat linear
scatter.smooth(x=WorldBank$RuralPop, y=WorldBank$HappinessScore, main="Happiness Score by ...")
#somewhat linear
scatter.smooth(x=WorldBank$SecEduc, y=WorldBank$HappinessScore, main="Happiness Score by ...")
#somewhat linear
scatter.smooth(x=WorldBank$SevereFoodInsecurity, y=WorldBank$HappinessScore, main="Happiness Score by ...")
scatter.smooth(x=WorldBank$TradeGDP, y=WorldBank$HappinessScore, main="Happiness Score by ...")
scatter.smooth(x=WorldBank$TradeinServices, y=WorldBank$HappinessScore, main="Happiness Score by ...")
scatter.smooth(x=WorldBank$Undernourishment, y=WorldBank$HappinessScore, main="Happiness Score by ...")
scatter.smooth(x=WorldBank$Unemployment, y=WorldBank$HappinessScore, main="Happiness Score by ...")
scatter.smooth(x=WorldBank$UrbanGrowth, y=WorldBank$HappinessScore, main="Happiness Score by ...")
scatter.smooth(x=WorldBank$UrbanPop, y=WorldBank$HappinessScore, main="Happiness Score by ...")
#somewhat linear

#Variables that were linear
#BasicSanitization, BussinessScore, Cellular, FertilityRate, FoodInsecurity, HealthExpGDP, HealthExpGov, RenewableEnergy, RuralPop, SecEduc, UrbanPop

#Testing for homoscedasity

#BasicSanitization
lmMod1 <- lm(HappinessScore~BasicSanitization, data=WorldBank)
lmtest::bptest(lmMod1)
#does not pass

WorldBank2 <- na.omit(WorldBank)

lmMod_edit <- caret::BoxCoxTrans(WorldBank2$HappinessScore)
print(lmMod_edit)

View(WorldBank2)

WorldBank2 <- cbind(WorldBank2, dist_newM=predict(lmMod_edit, WorldBank2$HappinessScore))
lmMod_edit <- lm(dist_newM~BasicSanitization, data=WorldBank2)
lmtest::bptest(lmMod_edit)
#now passes

#BussinessScore
lmMod2 <- lm(HappinessScore~BussinessScore, data=WorldBank)
lmtest::bptest(lmMod2)
#passes

#Cellular
lmMod3 <- lm(HappinessScore~Cellular, data=WorldBank)
lmtest::bptest(lmMod3)
#passes

#FertilityRate
lmMod4 <- lm(HappinessScore~FertilityRate, data=WorldBank)
lmtest::bptest(lmMod4)
#does not pass

lmMod_edit2 <- lm(dist_newM~FertilityRate, data=WorldBank2)
lmtest::bptest(lmMod_edit2)
#now passes

#FoodInsecurity
lmMod5 <- lm(HappinessScore~FoodInsecurity, data=WorldBank)
lmtest::bptest(lmMod5)
#passes

#HealthExpGDP
lmMod6 <- lm(HappinessScore~HealthExpGDP, data=WorldBank)
lmtest::bptest(lmMod6)
#passes

#HealthExpGov
lmMod7 <- lm(HappinessScore~HealthExpGov, data=WorldBank)
lmtest::bptest(lmMod7)
#passes

#RenewableEnergy
lmMod8 <- lm(HappinessScore~RenewableEnergy, data=WorldBank)
lmtest::bptest(lmMod8)
#passes

#RuralPop
lmMod9 <- lm(HappinessScore~RuralPop, data=WorldBank)
lmtest::bptest(lmMod9)
#passes

#SecEduc
lmMod10 <- lm(HappinessScore~SecEduc, data=WorldBank)
lmtest::bptest(lmMod10)
#passes

#UrbanPop
lmMod11 <- lm(HappinessScore~UrbanPop, data=WorldBank)
lmtest::bptest(lmMod11)
#passes


## Testing for homogeneity
gvlma(lmMod_edit)
gvlma(lmMod2)
gvlma(lmMod3)
#Link Function violated.
gvlma(lmMod4)
gvlma(lmMod_edit2)
gvlma(lmMod5)
#Global Stat, Kurtosis, and Link Function is violated.
gvlma(lmMod6)
gvlma(lmMod7)
gvlma(lmMod8)
#Skewness violated
gvlma(lmMod9)
gvlma(lmMod10)
gvlma(lmMod11)

#outliers

CookD(lmMod_edit, group=NULL, plot=TRUE, idn=3, newwd=TRUE)
summary(influence.measures(lmMod_edit))


#Running the multiple linear regression
model1 <- lm(HappinessScore ~BasicSanitization + BussinessScore +Cellular + FoodInsecurity + HealthExpGDP + HealthExpGov + RenewableEnergy + RuralPop + SecEduc + UrbanPop, data = WorldBank)
summary(model1)
#HealthExpGov and RuralPop are significant


model2 <- lm(HappinessScore ~BasicSanitization + FertilityRate, data = WorldBank2)
summary(model2)
#BasicSanitization and FertilityRate are significant

##

ljoin = merge(x = HealthHappiness3, y = WorldBank, by = "Country", all.x = TRUE)
head(ljoin)

model3 <- lm(LifeExpectancy ~BasicSanitization + BussinessScore +Cellular + FoodInsecurity + HealthExpGDP + HealthExpGov + RenewableEnergy + RuralPop + SecEduc + UrbanPop, data = ljoin)
summary(model3)
#BasicSanitization, FoodInsecurity, and HealthExpGov are significant
