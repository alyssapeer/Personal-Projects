
install.packages("ggthemes")
install.packages("corrplot")

library(ggthemes)
library(dplyr)
library(ggplot2)
library(ggthemes)
theme_wsj()
library(lubridate)
library(tidyr)
library(corrplot)
library(lmtest)

################################################################################
# Input and Clean Data
################################################################################

# read in data
renewables <- readxl::read_xlsx('Renewables.xlsx')
glimpse(renewables)

# store date as date 
renewables$Month <- as_date(renewables$Month)

###############################################################################
# Exploratory Analysis Visuals
###############################################################################

# graph the production data
production <- renewables %>%
  select(Month, 'Wood Energy Production', 'Biofuels Production', 'Total Biomass Energy Production') %>%
  gather(key = "variable", value = "value", -Month)
head(production, 3)

productionplot <- ggplot(production, aes(x = Month, y = value)) + 
  geom_line(aes(color = variable), size = 1) +
  theme_wsj()

print(productionplot + ggtitle("Monthly Wood, Biofuel, and Total \nBiomass Energy Production Since 1984"))

# graph consumption data
consumption <- renewables %>%
  select(Month, "Hydroelectric Power Consumption", "Geothermal Energy Consumption", "Solar Energy Consumption", "Wind Energy Consumption", "Total Biomass Energy Consumption") %>%
  gather(key = "variable", value = "value", -Month)
head(consumption, 3)

consumptionplot <- ggplot(consumption, aes(x = Month, y = value)) + 
  geom_line(aes(color = variable), size = 1) +
  theme_wsj()

print(consumptionplot + ggtitle("Monthly Hydroelectric, Geothermal, \nSolar, Wind, and Total Biomass Energy \nConsumption Since 1984"))

# graph Total Production and Total Consumption
total <- renewables %>%
  select(Month, "Total Renewable Energy Production", "Total Renewable Energy Consumption") %>%
  gather(key = "variable", value = "value", -Month)
head(total, 3)

totalplot <- ggplot(total, aes(x = Month, y = value)) + 
  geom_line(aes(color = variable), size = 1) +
  theme_wsj()

print(totalplot + ggtitle("Total Monthly Energy Production and Consumption \nSince 1984"))


###############################################################################
# Check and Visualize Correlations
###############################################################################

# shorten column names
df <- renewables 

colnames(df) <- c("Month",	"Wood.Prod",	"Biofuel.Prod",	"Total.Biomass.Prod",	"Total.Prod",	"Hydro.Cons",	"Geothermal.Cons",	"Solar.Cons",	"Wind.Cons",	"Wood.Cons",	"Waste.Cons",	"Biofuel.Cons",	"Total.Biomass.Cons",	"Total.Cons")

# correlation matrix
correl <- df %>%
  select(-Month)
correlationMatrix <- cor(correl)

# visualize correlation matrix
corrplot(correlationMatrix, tl.col="black", tl.srt=45)

###############################################################################
# Multiple Linear Regression
###############################################################################

# run regular regression with total production as response and Consumption vars as predictors
lm.model <- lm(Total.Prod~Geothermal.Cons+Solar.Cons+Total.Biomass.Cons+Waste.Cons, data = df)
summary(lm.model)

install.packages("car")
library(car)
vif(lm.model)

# plot fitted values on residuals from model to check error assumptions
plot(lm.model$fitted.values,lm.model$residuals)

# check error assumptions on qq-plot
qqnorm(residuals(lm.model),ylab="Residuals",main="")
qqline(residuals(lm.model))


###############################################################################
# Transform model to try to accommodate heteroskedasticity
###############################################################################

# plot fitted values on residuals from model to check error assumptions
plot(lm.model$fitted.values,lm.model$residuals)

# model plus relationship between hydro/geothermal and solar/wind
lm.model <- lm(log(Total.Prod)~Geothermal.Cons+Solar.Cons+Total.Biomass.Cons+Waste.Cons, data = df)
summary(lm.model)

# plot fitted values on residuals from model to check error assumptions
plot(lm.model$fitted.values,lm.model$residuals) # there is a slight pattern in the error values
hist(residuals(lm.model)) # approximately normal

# refit the model without highly correlated predictors and interpret results
lm.model <- lm(Total.Prod~Solar.Cons+Total.Biomass.Cons, data = df)
summary(lm.model)

# plot fitted values on residuals from model to check error assumptions
plot(lm.model$fitted.values,lm.model$residuals) # there still appears to be a pattern in the errors, I suspect this to be due to the fact that this data was captured in successive months
hist(residuals(lm.model)) #approximately normal but large variance
###############################################################################
# Testing for Autocorrelation
###############################################################################

# fit a model that estimates the correlation of successive months
cor(residuals(lm.model)[-1],residuals(lm.model)[-length(residuals(lm.model))])

# Correlation among values of successive months is approximately 0.792. This is very high.


# check for autocorrelation
dwtest(Total.Prod~Solar.Cons+Total.Biomass.Cons, data = df)

#  With a DW statistics of 0.41 (much smaller than 2) and a p-value that is less than 0.05 we reject the null hypothesis that the errors are not correlated.

###############################################################################
# Generalized Least Squares Method
###############################################################################

# install nlme package
install.packages("nlme")
library(nlme)

# add id column
df <- tibble::rowid_to_column(df, "id")

# run model with gls fitting function to fit auto-regressive form
lm.model <- gls(Total.Prod~Solar.Cons+Total.Biomass.Cons, correlation=corAR1(form = ~id),method = "ML", data = df)


# check confidence interval
intervals(lm.model)
hist(residuals(lm.model))

plot(predict(lm.model),                                # Draw plot using Base R
     df$Total.Prod,
     xlab = "Predicted Values",
     ylab = "Observed Values")
abline(a = 0,                                        # Add straight line
       b = 1,
       col = "red",
       lwd = 2)


###############################################################################
# Prediction
###############################################################################

nrow(df)*0.8

# split data into 80% train and 20% test, not randomly, we want to maintain the data's structure (time)
data.train <- df[1:371, ]
data.test <- df[372:464, ]

# linear model
gls(Total.Prod~Solar.Cons+Total.Biomass.Cons, correlation=corAR1(form = ~id),method="ML", data = df)

# predict on the test set
yhat.test <- predict(lm.model, data.test)

# calculate test MSE
y.test <- data.test$Total.Prod
MSE.test <- mean((y.test - yhat.test)^2)
MSE.test

# calculate root MSE
RMSE.test <- sqrt(MSE.test)
RMSE.test

# calculate normalized root MSE
NRMSE.test <- RMSE.test / mean(y.test)
NRMSE.test

# The linear model gives an 8% error for predicting Total Renewable Energy Production
