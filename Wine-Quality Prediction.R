# Read the data file
wine <- read.csv("wine.csv")
str(wine)
summary(wine)

# Linear Regression  Model(one variable)
mod1 <- lm(Price ~ AGST, data=wine)
summary(model1)

# Sum of Squared Errors
mod1$residuals
SSE <- sum(mod1$residuals^2)
SSE

# Linear Regression model (two variables)
mod2 <- lm(Price ~ AGST + HarvestRain, data=wine)
summary(mod2)

# Sum of Squared Errors
SSE <- sum(mod2$residuals^2)
SSE

# Linear Regression model (all variables)
mod3 <- lm(Price ~ AGST + HarvestRain + WinterRain + Age + FrancePop, data=wine)
summary(mod3)

# Sum of Squared Errors
SSE <- sum(mod3$residuals^2)
SSE


# Remove Francepop variable
model4 <- lm(Price ~ AGST + HarvestRain + WinterRain + Age, data=wine)
summary(model4)


# Correlations
cor(wine$WinterRain, wine$Price)
cor(wine$Age, wine$FrancePop)
cor(wine)

# Remove Age and FrancePop variables
model5 <- lm(Price ~ AGST + HarvestRain + WinterRain, data=wine)
summary(model5)


# Read in test set
wineTest <- read.csv("wine_test.csv")
str(wineTest)

# Make test set predictions
predictTest <- predict(model4, newdata=wineTest)
predictTest

# Compute R-squared
SSE <- sum((wineTest$Price - predictTest)^2)
SST <- sum((wineTest$Price - mean(wine$Price))^2)
1 - SSE/SST
