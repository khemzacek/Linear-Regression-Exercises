
# Exercise: Least Squares Regression --------------------------------------

##   Use the /states.rds/ data set. Fit a model predicting energy consumed
##   per capita (energy) from the percentage of residents living in
##   metropolitan areas (metro). Be sure to
##   1. Examine/plot the data before fitting the model
##   2. Print and interpret the model `summary'
##   3. `plot' the model to look for deviations from modeling assumptions

##   Select one or more additional predictors to add to your model and
##   repeat steps 1-3. Is this model significantly better than the model
##   with /metro/ as the only predictor?


library(dplyr)
library(ggplot2)

# Read in data
states.data <- readRDS("dataSets/states.rds")

# Examine data
str(states.data) # Look at all variables available
states.info <- data.frame(attributes(states.data)[c("names", "var.labels")])
states.info # Show all variable names and descriptions
metro.energy <- select(states.data, metro, energy)
summary(metro.energy) # Examine subset of variables of interest
# Some observations contain NA

# Examine correlation between variables
plot(metro.energy)
# Correlation doesn't look particularly linear, some very significant positive outliers
cor(metro.energy, use = "complete.obs") # Correlation after removing observations containing NA
# = -0.3397; shows some negative correlation between variables
# Suggests that in general, states with a higher percentage of people in metropolitan
# areas will have lower energy consumption. Makes sense to me - more rural area
# require farther travel, have less shared utilities...
outliers_rmvd <- filter(metro.energy, energy <= 500)
cor(outliers_rmvd, use = "complete.obs")
plot(outliers_rmvd)
# stronger correlation with outliers removed
# still a couple negative outliers at the end of lower metro %
# *most variation in states with low metropolitan population %


# Create and examine model
energy_model <- lm(energy ~ metro, data = states.data) # create model
summary(energy_model)
# large residuals - on the same order as the order of the data
# Intercept very significant, metro coefficient somewhat significant
# Metro coefficient negative (as expected, given we saw negative correlation earlier)
# R-squared = 0.1154 -> bad fit
SSE = sum(energy_model$residuals^2) # sum of squared error
SSE
RMSE = sqrt(SSE/(nrow(states.data)-1)) # root mean squared error
RMSE

plot(energy_model)
# By looking at the residuals vs fitted graph, we see the 4 significant positive outliers
# By looking at the Normal Q-Q plot, we see that the data is not truly normally distributed
# The positive outliers are also obvious on this graph
# Outliers aside, the tails deviate from the line in a noticable way


# What might explain our outliers?
pos_outliers <- filter(states.data, energy > 500)
pos_outliers$state
# Alaska, Louisiana, Texas, Wyoming
# Alaska and Texas have 2 of the largest land areas
# All lean politically conservative
# All lean toward low population density, but this may be too highly correlated to metro

# What might explain variation when metro is low?
low_metro <- filter(states.data, metro < 45)
low_metro$state
ggplot(low_metro, aes(x = state, y = energy)) +
  geom_point()
# cold northern states? (no measure for temps, but maybe region...)

# What else might be significantly correlated with per capita energy consumption?
# since per capita, population might be a factor
# waste, toxic - lifestyle as well as energy to dispose of waste
# miles - more miles traveled = more energy consumed
# income - higher income = more efficient tech? or fancier (more consumptive) stuff?

# What do the numbers tell us?
cor(states.data[3:21], use = "complete.obs")

# highest correlation with energy: green, toxic, house, senate, miles, income, percent, college, density, area, expense, metro
# surprises: waste is negatively correlated; percent more highly correlated than expected
# interesting: income slightly negatively correlated (higher income = more efficiency/less energy?)

# variables with low correlation to metro, high correlation to energy: area, toxic
# area and toxic have relatively low correlation to each other


# Creating a better model
energy_model2 <- lm(energy ~ metro + area + toxic, data = states.data)
summary(energy_model2)
# R2 = 0.6484 -> much better fit
# area and toxic are very significant, metro is not significant

energy_model3 <- lm(energy ~ area + toxic, data = states.data)
summary(energy_model3)
# R2 = 0.6225 -> not bad, but could be better

# What variable(s) higly correlated with energy, low correlation with area, toxic
# green? - yes
# house/senate? - too highly correlated with area and toxic
# miles? - yes, but pretty high correlation with green
# income? - too highly correlated with toxic
# percent? - too high w/ area and toxic
# college? - too high w/ toxic
# density? - too high w/ area
# expense? - too high w/ area and toxic

energy_model4 <- lm(energy ~ area + toxic + green, data = states.data)
summary(energy_model4)
# R2 = 0.7802 -> much better
# area is no longer significant

energy_model5 <- lm(energy ~ toxic + green, data = states.data)
summary(energy_model5)
# R2 = 0.7627 -> slightly lower, but still not bad; all coefficients very significant

# density was eliminated only because of high correlation with area; try adding it?
energy_model6 <- lm(energy ~ toxic + green + density, data = states.data)
summary(energy_model6)
# R2 = 0.7631 -> only slightly higher, density not significant

# does any other variable have high correlation with energy and low correlation with toxic/green?
# not really...

# So, the model with toxic and green seems to be the best
final_energy_model <- energy_model5


# Examine Data of Final Model
final_model_data <- na.omit(select(states.data, energy, green, toxic))
summary(final_model_data) # Examine subset of variables of interest

# Examine correlation between variables
plot(states.data$green, states.data$energy)
plot(states.data$toxic, states.data$energy)
# all contain some outliers, but decent linear correlation

cor(final_model_data, use = "complete.obs")
# all indep variables fairly highly correlated with energy, lower correlation between indep variable

# Check Quality Final Model
SSE = sum(final_energy_model$residuals^2) # sum of squared error
SSE
RMSE = sqrt(SSE/(nrow(final_model_data))) # root mean squared error
RMSE
# much lower error than with metro alone

plot(final_energy_model)
# Normal Q-Q still not perfectly linear, both positive and negative outliers
# Residuals has outliers, but not as severe



# Exercise: Interactions and Factors --------------------------------------

##   Use the states data set.

##   1. Add on to the regression equation that you created in exercise 1 by
##      generating an interaction term and testing the interaction.

##   2. Try adding region to the model. Are there significant differences
##      across the four regions?


## INTERACTIONS

# We said earlier that land area, political activity, or density may explain
# the energy vs. metro model outliers. Let's test these as interaction terms.

# Lets use the energy vs. metro model as a baseline
metro_model <- lm(energy ~ metro, data = states.data)
summary(metro_model)
# R2 = 0.1154

# metro*area
inter_mod1 <- lm(energy ~ metro*area, data = states.data)
summary(inter_mod1)
# R2 = 0.5139 <- much better!

# metro*house
inter_mod2 <- lm(energy ~ metro*house, data = states.data)
summary(inter_mod2)
# R2 = 0.4767 <- much better than baseline, but not as good as area

# metro*senate
inter_mod3 <- lm(energy ~ metro*senate, data = states.data)
summary(inter_mod3)
# R2 = 0.2725 <- better than baseline, but not great

# metro*density
inter_mod4 <- lm(energy ~ metro*density, data = states.data)
summary(inter_mod4)
# R2 = 0.1775 <- not much better than baseline

# variable pairs with low intercorrelation seem to make better interaction terms
# high has low correlation with metro
inter_mod5 <- lm(energy ~ metro*high, data = states.data)
summary(inter_mod5)
# R2 = 0.2003
# however, high has relatively low correlation to energy as well, not good interaction


# House has a relatively high correlation with toxic and green, so we do not
# expect this to be a good addition to the model.
# Metro and area have relatively low correlation with toxic and green, so...
# Try adding metro*area to final_energy_model from previous exercise
energy_model7 <- lm(energy ~ toxic + green + metro*area, data = states.data)
summary(energy_model7)
# R2 = 0.7871 <- better than model without interaction term
# However, the interaction coefficient is not significant
# But, we can see that metro and area are much less significant on their own
# than as an interaction, so we can see the benefit of using interaction terms

# we will use this
final_energy_model2 <- energy_model7
  

# Examine Data of Final Model
final_model2_data <- na.omit(select(states.data, energy, green, toxic, area, metro))
summary(final_model2_data) # Examine subset of variables of interest

cor(final_model2_data, use = "complete.obs")
# all indep variables fairly highly correlated with energy; green/metro
# somewhat high correlation, but overall low correlation between indep variables

# Check Quality Final Model
SSE2 = sum(final_energy_model2$residuals^2) # sum of squared error
SSE2
RMSE2 = sqrt(SSE2/(nrow(final_model2_data))) # root mean squared error
RMSE2
# lower than without interaction term

plot(final_energy_model2)
# Normal Q-Q still has heavy tails, both positive and negative outliers
# Residuals has outliers, but positive outliers not as severe

# What is the negative outlier?
states.data[45, 1]
# Utah


## FACTORS

# Region was another variable of interest identified for describing metro outliers

# Model energy vs. region
str(states.data$region)
states.data$region <- factor(states.data$region) # set region as categorical
factor_mod1 <- lm(energy ~ region, data = states.data)
summary(factor_mod1)
# R2 = 0.1367 <- pretty bad fit
# regionN.East is somewhat significant

# Region contrasts
contrasts(states.data$region)

# Use different regions as reference 
summary(lm(energy ~ C(region, base=1), data=states.data))
summary(lm(energy ~ C(region, base=2), data=states.data))
summary(lm(energy ~ C(region, base=3), data=states.data))
summary(lm(energy ~ C(region, base=4), data=states.data))
# same R2 for all, base = 2 gives 2 slightly significant coefficients

# Change coding scheme
summary(lm(energy ~ C(region, contr.helmert), data=states.data))
summary(lm(energy ~ C(region, contr.sum), data=states.data))
summary(lm(energy ~ C(region, contr.poly), data=states.data))
summary(lm(energy ~ C(region, contr.SAS), data=states.data))
# same R2 for all, contr.sum gives one significant and one barely significant coeff


# Overall, the differences across regions does not seem to be significant alone
# Can we use regions in an interaction term?
factor_mod2 <- lm(energy ~ metro*region, data = states.data)
summary(factor_mod2)
# R2 = 0.3126 <- better than either metro or region on their own
# metro, regionN.East, and metro:regionN.East are significant

# Add to final_energy_model
energy_model8 <- lm(energy ~ toxic + green + metro*region, data = states.data)
summary(energy_model8)
# R2 = 0.7906 <- better than final_energy_model2 (with metro*area term)
# toxic and green are the only significant coefficients

final_energy_model3 <- energy_model8


# Examine Data of Final Model
final_model3_data <- na.omit(select(states.data, energy, green, toxic, region, metro))
summary(final_model2_data) # Examine subset of variables of interest

# Check Quality Final Model
SSE3 = sum(final_energy_model3$residuals^2) # sum of squared error
SSE3
RMSE3 = sqrt(SSE3/(nrow(final_model3_data))) # root mean squared error
RMSE3
# a bit lower than either of the other final models

plot(final_energy_model3)
# Normal Q-Q still has heavy tails, both positive and negative outliers
# Residuals has outliers, but none as severe