#FINAL PROJECT
library(readr)
Real_estate_valuation_data_set <- read_csv("Downloads/Real estate valuation data set.csv")
Real_estate_valuation_data_set
Price <- Real_estate_valuation_data_set$`Y house price of unit area`
Age<- Real_estate_valuation_data_set$`X2 house age`
DistancetoMRT <- Real_estate_valuation_data_set$`X3 distance to the nearest MRT station`
NumberofStores <- Real_estate_valuation_data_set$`X4 number of convenience stores`
Latitude <- Real_estate_valuation_data_set$`X5 latitude`
Longitude <- Real_estate_valuation_data_set$`X6 longitude`


plot(DistancetoMRT, Price, xlab = "Distance to MRT(meters)", ylab = "House Price per Unit of Area", main = "Price vs Distance to MRT", col = 'blue')
fit1 = lm(Price~DistancetoMRT)
abline(fit1, col = 'red')
fitted = fit1$fitted.values
plot(resid ~ fitted, xlab = "Fitted Values", ylab = "Residuals", main = "Residuals Vs Fits Plot")
abline(h = 0, lty = 2, col = 'red')
yhat <- fitted(fit1)
e <- Price - yhat 
qqnorm(e)
qqline(e, col = 'red')


lnprice = log(Price)
lndist = log(DistancetoMRT)
plot(lndist, lnprice, xlab =  "Log of Distance to MRT(meters)", ylab = "Log of House Price per Unit of Area", main = "Log of Price vs Log of Distance to MRT", col = 'blue')
fit2 = lm(lnprice~lndist)
abline(fit2, col = 'red')
fitted2 = fit2$fitted.values
plot(resid ~ fitted2, xlab = "Fitted Values", ylab = "Residuals", main = "Residuals Vs Fits Plot")
abline(h = 0, lty = 2, col = 'red')
yhat <- fitted(fit2)
e <- lnprice - yhat 
qqnorm(e)
qqline(e, col = 'red')
summary(fit1)
summary(fit2)
anova(fit2)

#SECOND QUESTION 
pairs(~Price+Age+DistancetoMRT+NumberofStores, col = "green")

library(leaps)
mod.0 = lm(Price~1)
mod.full =  lm(Y~Age+DistancetoMRT+NumberofStores)
m.forward <-step(mod.0, scope = list(lower = mod.0, upper = mod.full),
                 direction = "forward") #FORWARD Selection

m.backward <-step(mod.full, scope = list(lower = mod.0, upper = mod.full),
                  direction = "backward") #Backward Selection 
m.forward
m.backward
stepwise <- step(mod.0, scope = list(lower = mod.0, upper = mod.full),
                 direction = "both") #HYBRID Selection
stepwise



mod.reg = regsubsets(cbind(Age, DistancetoMRT,NumberofStores),
                     Price, data = Real_estate_valuation_data_set )
summary.reg <- summary(mod.reg)
names(summary.reg)
summary.reg$rsq
summary.reg$cp
summary.reg$bic
summary.reg$adjr2

fit10 <- lm(Price~Age+DistancetoMRT+NumberofStores)
summary(fit10) #Global F-test

#predicted price per unit for newest house on market
dataset = data.frame(Age = min(Age), DistancetoMRT = mean(DistancetoMRT), NumberofStores = mean(NumberofStores))
test <- predict.lm(fit, dataset, interval = "confidence")
dataset2 = data.frame(Age = max(Age), DistancetoMRT = mean(DistancetoMRT), NumberofStores = mean(NumberofStores))
test2 <- predict.lm(fit, dataset2, interval = "confidence")

#predicted price per unit for houses closest to MRT station
dataset = data.frame(Age = mean(Age), DistancetoMRT = min(DistancetoMRT), NumberofStores = mean(NumberofStores))
test3 <- predict.lm(fit, dataset, interval = "confidence")
dataset = data.frame(Age = mean(Age), DistancetoMRT = max(DistancetoMRT), NumberofStores = mean(NumberofStores))
test4 <- predict.lm(fit, dataset, interval = "confidence")

#predicted price per unit for houses with the most convenience stores next to them
dataset = data.frame(Age = mean(Age), DistancetoMRT = mean(DistancetoMRT), NumberofStores = max(NumberofStores))
test5 <- predict.lm(fit, dataset, interval = "confidence")
dataset = data.frame(Age = mean(Age), DistancetoMRT = mean(DistancetoMRT), NumberofStores = min(NumberofStores))
test6 <- predict.lm(fit, dataset, interval = "confidence")

dataframe <- rbind(test[1],test2[1],test3[1],test4[1],test5[1],test6[1])
rownames(dataframe) <- c("newest house", "oldest house", "closest to MRT", "farthest from MRT", "most stores", "least stores")
colnames(dataframe) <- "Price per unit"
dataframe




