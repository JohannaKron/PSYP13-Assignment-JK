### Home Assignment ZK 2019

# Basics

# clears the workspace
rm(list=ls(all=TRUE)) 

# clears graphics
graphics.off() 

# Set Work Directory
setwd("~/R")

# Function Graph
source("GraphPlot.R")
openGraph()

#load packages
require(tidyverse)
require(psych)
require(lm.beta)
require(gridExtra)
require(lmtest)
require(boot)
require(sandwich)
require(car)
require(influence.ME) 	
require(lattice) 
require(lme4) 
require(lmerTest) 
require(lm.beta) 
require(r2glmm)
require(MuMIn)
require(cAIC4)


#####################################

### PART 1

# Load dataset
library(readr)
home_sample_1 <- read_csv("Home assignment/home_sample_1.csv")

## Check and explore the data
# View the data
view(home_sample_1)

# Investigate structure of the data
str(home_sample_1)

# Check for missing data
home_sample_1[!complete.cases(home_sample_1), ]
# Returns no missing data

# Check structure of the data to se if there are any irregularities
home_sample_1 %>% summary()
# Sex & ID - should be factor
# STAI_trait min:3.50, but scale is between 20 and 80
# Household_income -4562. 

# Treat the irregularities/ Clean the data
# Before any changes to dataset, copy dataset
mydata1_cleaned<-home_sample_1

# Convert sex-variable to factor
mydata1_cleaned$sex <- as.factor(mydata1_cleaned$sex)
mydata1_cleaned$ID <- as.factor(mydata1_cleaned$ID)

# STAI_trait is a scale of 20-80, so no value can be outside that range
# Replace out of range value with mean in order to keep the participant
mydata1_cleaned$STAI_trait[mydata1_cleaned$STAI_trait<20]=mean(mydata1_cleaned$STAI_trait)

# Not likely that household income is negative
# Replace irregular value with mean in order to keep the participant
mydata1_cleaned$household_income[mydata1_cleaned$household_income<0]=mean(mydata1_cleaned$household_income)

# Re-check to see if data cleaning worked 
mydata1_cleaned %>% summary()
describe(mydata1_cleaned)

############################

# Using cleaned data frpn dataset 1
mydata1 <- mydata1_cleaned 

## Building Model 1 

# Visualize
mydata1 %>% ggplot() + aes(x = age) + geom_histogram(bins = 30)
mydata1 %>% ggplot() + aes(x = pain) + geom_histogram(bins = 30)

mydata1 %>% ggplot() + aes(x=age, y=pain ) + geom_point()
mydata1 %>% ggplot() + aes(x=sex, y=pain ) + geom_boxplot()

# Model 1 
mod1 <- lm(pain ~ age + sex, data = mydata1)
mod1
summary(mod1)

# Scatterplot, for visual effect 
plot1 = mydata1 %>% ggplot() + aes(x = age, y=pain) + geom_point() + geom_smooth(method = "lm")
plot2 = mydata1 %>% ggplot() + aes(x = sex, y=pain) + geom_point() + geom_smooth(method = "lm")
grid.arrange(plot1, plot2, nrow=1)

# TEST FOR OUTLIERS
# Residual - leverage plot
mod1 %>% plot(which =5)

# Cook's distance
mod1 %>% plot(which =4)


# TEST FOR THE ASSUMPTION OF NORMALITY
# QQ plot
mod1 %>% plot(which =2)

##################

# Remove outliers from dataset
data1_nooutliers = mydata1 %>% slice(-c(28, 88))

# refit model 1 without outliers
mod1_nooutliers = lm(pain ~ age + sex, data= data1_nooutliers)

# Test for normality
# histogram for residuals
residuals_mod1 = enframe(residuals(mod1_nooutliers))
residuals_mod1 %>% ggplot() + aes(x=value) + geom_histogram(bins=30)
describe(residuals(mod1_nooutliers))
# skew and kourtosis not >1 indicating violation of assumption of normality

# QQ-Plot
mod1_nooutliers %>% plot(which =2)

# Test for linearity. Non-significant means that the assumption holds true.
mod1_nooutliers %>% residualPlots()

# Test for Homoscedasticity
mod1_nooutliers %>% plot(which=3)
# NCV test. Non-significant means that the assumption holds true.
mod1_nooutliers %>% ncvTest()
# Breush-Pagan test. Non-significant means that the assumption holds true.
mod1_nooutliers %>% bptest()

# Test for Multicollinearity. If >3, explore and treat it.
mod1_nooutliers %>% vif()

mod1_final <- mod1_nooutliers

###########

# Use cleaned data
mydata12 <- mydata1_cleaned

## Building Model 2
mod2 = lm(pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum + cortisol_saliva + mindfulness, data = mydata12)
mod2
summary(mod2)


## Model diagnostics for model 2

# Scatterplot, for visual effect 
plot1 = mydata12 %>% ggplot() + aes(x = age, y=pain) + geom_point() + geom_smooth(method = "lm")
plot2 = mydata12 %>% ggplot() + aes(x = sex, y=pain) + geom_point() + geom_smooth(method = "lm")
plot3 = mydata12 %>% ggplot() + aes(x = STAI_trait, y=pain) + geom_point() + geom_smooth(method = "lm")
plot4 = mydata12 %>% ggplot() + aes(x = pain_cat, y=pain) + geom_point() + geom_smooth(method = "lm")
plot5 = mydata12 %>% ggplot() + aes(x = cortisol_serum, y=pain) + geom_point() + geom_smooth(method = "lm")
plot6 = mydata12 %>% ggplot() + aes(x = cortisol_saliva, y=pain) + geom_point() + geom_smooth(method = "lm")
plot7 = mydata12 %>% ggplot() + aes(x = mindfulness, y=pain) + geom_point() + geom_smooth(method = "lm")
grid.arrange(plot1, plot2, plot3,plot4, plot5, plot6, plot7, nrow=2)

# TEST FOR OUTLIERS
# Residual - leverage plot
mod2 %>% plot(which =5)

# Cook's distance
mod2 %>% plot(which =4)



# QQ plot
mod2 %>% plot(which =2)

##################

# Remove outliers from dataset
mydata12_nooutliers = mydata12 %>% slice(-c(74, 88))

# refit model 2 without outliers
mod2_nooutliers = lm(pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum + cortisol_saliva + mindfulness, data= mydata12_nooutliers)

#######

# Test for the assumption of normality
# histogram for residuals
residuals_mod2 = enframe(residuals(mod2_nooutliers))
residuals_mod2 %>% ggplot() + aes(x=value) + geom_histogram(bins=30)
describe(residuals(mod2_nooutliers))
# skew and kourtosis not >1 indicating violation of assumption of normality
# QQ plot
mod2_nooutliers %>% plot(which =2)

# Test for linearity. Non-significant means that the assumption holds true.
mod2_nooutliers %>% residualPlots()

# Test for Homoscedasticity
mod2_nooutliers %>% plot(which=3)
# NCV test. Non-significant means that the assumption holds true.
mod2_nooutliers %>% ncvTest()
# Breush-Pagan test. Non-significant means that the assumption holds true.
mod2_nooutliers %>% bptest()

# Test for Multicollinearity. If >3, explore and treat it.
mod2_nooutliers %>% vif()
# Cortisol_serum & Cortisol_saliva has values over 3. Need to explore...

#########

# If Structural multicollinearity: Center the cortisol predictors 
mydata12_test_centered = mydata12_nooutliers %>% mutate(cortisol_saliva_centered = cortisol_saliva - mean(cortisol_saliva), cortisol_serum_centered = cortisol_serum - mean(cortisol_serum)) 
# Build new model 2 
mod2_test_nooutlier_centered= lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum_centered + cortisol_saliva_centered , data= mydata12_test_centered)
mod2_test_nooutlier_centered %>% vif()
# NO DIFFERENCE - structural mutlicollinearity does not seem to be the problem.

#########

# Or due to Data multicollinearity: Look att correlation matrix of the predictors
mydata12_nooutliers %>% select(pain, age, sex, STAI_trait, pain_cat, cortisol_serum, cortisol_saliva, mindfulness) %>% pairs.panels(col= "red", lm= T)
# cortisol_serum and corisol_saliva correlates very high, at 0.87.

# Exclude cortisol_saliva due to high correlation with cortisol_serum, and less theoretical grounds to keep.
mod2_nosaliva = lm(pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum + mindfulness, data= mydata12_nooutliers)

# Re-check no multicollinearity:
mod2_nosaliva %>% vif()

#########################################

# Re-run all checks after excluding outliers and predictor
# Summary 
summary(mod2_nosaliva)

# Test for linearity. Non-significant means that the assumption holds true.
mod2_nosaliva%>% residualPlots()
# After deleting the cortisol_saliva variable, pain_cat seems to violate the linear assumption. 

# Test for Homoscedasticity
mod2_nosaliva %>% plot(which=3)

# NCV test. Non-significant means that the assumption holds true.
mod2_nosaliva %>% ncvTest()

# Breush-Pagan test. Non-significant means that the assumption holds true.
mod2_nosaliva %>% bptest()

# Test for Multicollinearity. If >3, explore and treat it.
mod2_nosaliva %>% vif()


## As noted above, after deleting the cortisol_saliva variable, pain_cat seems to violate the linear assumption. 

# Visualize with scatterplot
data2_nooutliers %>% ggplot() + aes(y = pain, x= pain_cat) + geom_point()

# refit model to higher term
mod2_higher = lm(pain ~ age + sex + STAI_trait + pain_cat + I(pain_cat^2) + cortisol_serum + mindfulness, data= mydata12_nooutliers)

summary(mod2_higher)


# Test for linearity. Non-significant means that the assumption holds true.
mod2_higher %>% residualPlots()

# Test for Homoscedasticity
mod2_higher %>% plot(which=3)

# NCV test. Non-significant means that the assumption holds true.
mod2_higher %>% ncvTest()

# Breush-Pagan test. Non-significant means that the assumption holds true.
mod2_higher %>% bptest()

# Test for Multicollinearity. If >3, explore and treat it.
mod2_higher %>% vif()
# pain_cat and I(pain_cat^2) are high in multicollinerity due to structural multicollinearity. 

# Treat Structural multicollinearity: Center the variables
mydata12_centered = mydata12_nooutliers %>% mutate( pain_cat_centered = pain_cat - mean(pain_cat))
                                                                                                                                             
mod2_higher_centered= lm(pain ~ age + sex + STAI_trait  + pain_cat_centered + I( pain_cat_centered^2) + cortisol_serum + mindfulness, data= mydata12_centered)
mod2_higher_centered %>% vif()

## Re-run again
# Test for linearity. Non-significant means that the assumption holds true.
# Plot
mod2_higher_centered %>% residualPlots()
# Look for skewness and kurtosis
describe(mydata12_centered)

# Test for Homoscedasticity
mod2_higher_centered %>% plot(which=3)

# NCV test. Non-significant means that the assumption holds true.
mod2_higher_centered %>% ncvTest()

# Breush-Pagan test. Non-significant means that the assumption holds true.
mod2_higher_centered %>% bptest()

# Test for Multicollinearity. If >3, explore and treat it.
mod2_higher_centered%>% vif()

# Copy dataset
mod2_final <- mod2_higher_centered

########################################

# The two models
mod1_final
mod2_final

# Model comparison
# Summary 
summary(mod1_final)
summary(mod2_final)

# Summary adjusted R2
summary(mod1_final)$adj.r.squared
summary(mod2_final)$adj.r.squared

# ANOVA
anova(mod1_final, mod2_final)

# AIC
AIC(mod1_final)
AIC(mod2_final)


confint(mod1_final)
confint(mod2_final)

lm.beta(mod1_final)
lm.beta(mod2_final)

# Coefficient table Custom function by ZK
coef_table = function(model){
  require(lm.beta)
  mod_sum = summary(model)
  mod_sum_p_values = as.character(round(mod_sum$coefficients[,4], 3))	
  mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"] = substr(mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"], 2, nchar(mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"]))	
  mod_sum_p_values[mod_sum_p_values == "0"] = "<.001"	
  
  
  mod_sum_table = cbind(as.data.frame(round(cbind(coef(model), confint(model), c(0, lm.beta(model)$standardized.coefficients[c(2:length(model$coefficients))])), 2)), mod_sum_p_values)	
  names(mod_sum_table) = c("b", "95%CI lb", "95%CI ub", "Std.Beta", "p-value")	
  mod_sum_table["(Intercept)","Std.Beta"] = "0"	
  return(mod_sum_table)
}

coef_table(mod1_final)
coef_table(mod2_final)

#############################################
##############################################

### PART 2

# Repeat build of model 2 from assignment 1
mod2_final


###############

## Building model 3 
# From same cleaned and treated data as model 2
mydata12_nooutliers

# Building a model with all available variables
mod3 = lm(pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum + mindfulness + weight + IQ + household_income, data = mydata12_nooutliers)
mod3
summary(mod3)


# Model diagnostics for model 3

# Scatterplot, for visual effect 
plot11 = mydata12_nooutliers %>% ggplot() + aes(x = age, y=pain) + geom_point() + geom_smooth(method = "lm")
plot12 = mydata12_nooutliers %>% ggplot() + aes(x = sex, y=pain) + geom_point() + geom_smooth(method = "lm")
plot13 = mydata12_nooutliers %>% ggplot() + aes(x = STAI_trait, y=pain) + geom_point() + geom_smooth(method = "lm")
plot14 = mydata12_nooutliers %>% ggplot() + aes(x = pain_cat, y=pain) + geom_point() + geom_smooth(method = "lm")
plot15 = mydata12_nooutliers %>% ggplot() + aes(x = cortisol_serum, y=pain) + geom_point() + geom_smooth(method = "lm")
plot16 = mydata12_nooutliers %>% ggplot() + aes(x = mindfulness, y=pain) + geom_point() + geom_smooth(method = "lm")
plot17 = mydata12_nooutliers %>% ggplot() + aes(x = weight, y=pain) + geom_point() + geom_smooth(method = "lm")
plot18 = mydata12_nooutliers %>% ggplot() + aes(x = IQ, y=pain) + geom_point() + geom_smooth(method = "lm")
plot19 = mydata12_nooutliers %>% ggplot() + aes(x = household_income, y=pain) + geom_point() + geom_smooth(method = "lm")
grid.arrange(plot11, plot12, plot13,plot14, plot15, plot16, plot17, plot18, plot19, nrow=3)

# TEST FOR OUTLIERS
# Residual - leverage plot
mod3 %>% plot(which =5)

# Cook's distance
mod3 %>% plot(which =4)


# TEST FOR THE ASSUMPTION OF LINEAR REGRESSION
# QQ plot
mod3 %>% plot(which =2)


# histogram for residuals
residuals_mod3 = enframe(residuals(mod3))
residuals_mod3 %>% ggplot() + aes(x=value) + geom_histogram(bins=30)

describe(residuals(mod3))
# skew and kourtosis not >1 indicating violation of assumption of normality

# Test for linearity. Non-significant means that the assumption holds true.
mod3 %>% residualPlots()
# pain_cat seems to violate the linear assumption. 

# Test for Homoscedasticity
mod3 %>% plot(which=3)
# NCV test. Non-significant means that the assumption holds true.
mod3 %>% ncvTest()
# Breush-Pagan test. Non-significant means that the assumption holds true.
mod3 %>% bptest()

# Test for Multicollinearity. If >3, explore and treat it.
mod3 %>% vif()
# Cortisol_serum & Cortisol_saliva has values over 3. Need to explore...


# As noted above, after deleting the cortisol_saliva variable, pain_cat seems to violate the linear assumption. 

# refit model to higher term
mod3_higher = lm(pain ~ age + sex + STAI_trait + pain_cat + I(pain_cat^2) + cortisol_serum + mindfulness + weight + IQ + household_income, data= mydata12_nooutliers)

summary(mod3_higher)


# Test for linearity. Non-significant means that the assumption holds true.
mod3_higher %>% residualPlots()

# Test for Homoscedasticity
mod3_higher %>% plot(which=3)

# NCV test. Non-significant means that the assumption holds true.
mod3_higher %>% ncvTest()

# Breush-Pagan test. Non-significant means that the assumption holds true.
mod3_higher %>% bptest()

# Test for Multicollinearity. If >3, explore and treat it.
mod3_higher %>% vif()
# pain_cat and I(pain_cat^2) are high in multicollinerity due to structural multicollinearity. 

# Treat Structural multicollinearity: Center the variables
mydata12_centered_mod3 = mydata12_nooutliers %>% mutate( pain_cat_centered = pain_cat - mean(pain_cat))

mod3_higher_centered= lm(pain ~ age + sex + STAI_trait  + pain_cat_centered + I( pain_cat_centered^2) + cortisol_serum + mindfulness + weight + IQ + household_income, data= mydata12_centered_mod3)
mod3_higher_centered %>% vif()

# Re-run again
# Test for linearity. > .05 means that the assumption holds true.
mod3_higher_centered %>% residualPlots()

# Test for Homoscedasticity
# Plot for visualization
mod3_higher_centered %>% plot(which=3)
# NCV test. > .05 means that the assumption holds true.
mod3_higher_centered %>% ncvTest()
# Breush-Pagan test. > .05 means that the assumption holds true.
mod3_higher_centered %>% bptest()

# Test for Multicollinearity. If >3, explore and treat it.
mod3_higher_centered%>% vif()

# Copy dataset
mod3_final <- mod3_higher_centered

mod3_final

##################

## Backwards regression
mod3_back = step(mod3_final, direction = "backward")

# Predictors retained in the end of the backward regression
backward_model = lm (pain ~ age + sex + pain_cat_centered + I(pain_cat_centered^2) + 
                       cortisol_serum + mindfulness, data = mydata12_centered_mod3)


## Compare initial model (model 3) to Backward model
# AIC
AIC(mod3_final) 
AIC(backward_model)

# ANOVA
anova(mod3_final, backward_model)
# No significant difference


## Compare the models Backward & Theory
# Regression model from assignment 1
theory_based_model <- mod2_final

# Summary 
summary(backward_model)
summary(theory_based_model)

# Compare AIC
AIC(backward_model)
AIC(theory_based_model)

# ANOVA
anova(backward_model, theory_based_model)

# Custom function Coefficient table
coef_table(backward_model)
coef_table(theory_based_model)


#####################

# Load dataset 2
library(readr)
home_sample_2 <- read_csv("Home assignment/home_sample_2.csv")

# Check and explore the data
# View the data
view(home_sample_2)

# Investigate structure of the data
str(home_sample_2)

# Check for missing data
home_sample_2[!complete.cases(home_sample_2), ]
# Returns no missing data

# Check structure of the data to se if there are any irregularities
home_sample_2 %>% summary()
# Sex & ID - should be factor

# Treat the irregularities/ Clean the data
# Before any changes to dataset, copy dataset
mydata2_cleaned<-home_sample_2

# Convert sex and ID variable to factor
mydata2_cleaned$sex <- as.factor(mydata2_cleaned$sex)
mydata2_cleaned$ID <- as.factor(mydata2_cleaned$ID)

# Re-check to see if data cleaning worked 
mydata2_cleaned %>% summary()
describe(mydata2_cleaned)

######################################

# to use the cleaned dataset 2 in this part
mydata2<- mydata2_cleaned

# In order to compare, need to add pain_cat_centered
mydata2_pain= mydata2 %>% mutate( pain_cat_centered = pain_cat - mean(pain_cat))

# Make predictions dataset 2
predict_pain_backward = predict(backward_model, newdata = mydata2_pain)
predict_pain_theory = predict(theory_based_model, newdata = mydata2_pain)

# Calculate the sum of squared residuals on dataset 2	
RSS_theory = sum((mydata2_pain[,"pain"] - predict_pain_theory)^2)	
RSS_back = sum((mydata2_pain[,"pain"] - predict_pain_backward)^2)	
RSS_theory
# 237.35
RSS_back	
# 236,20

# TSS
data2_mean <- lm(pain ~ 1, data = mydata2_pain)
TSS_data2 = sum((mydata2_pain[,"pain"] - predict(data2_mean))^2)
TSS_data2
# 361.29

# Sum of absolut difference
#Forumla: 1 - (RSS/TSS)
1 - (RSS_theory/TSS_data2)
# 0.34 (0.343)
1 - (RSS_back/TSS_data2)
# 0.35 (0.346)

######################################################
######################################################

## PART 3

# Load dataset
library(readr)
home_sample_3 <- read_csv("Home assignment/home_sample_3.csv")

# Check and explore the data
# View the data
view(home_sample_3)

# Investigate structure of the data
str(home_sample_3)

# Check for missing data
home_sample_3[!complete.cases(home_sample_3), ]
# Returns no missing data

# Check structure of the data to se if there are any irregularities
home_sample_3 %>% summary()
# ID, Sex, Hospital needs to be converted to factor 
# Mindfulness is on a scale of 1-6, value cannot be over 6. (One participant has 6.05)

# Treat the irregularities/ Clean the data
# Before any changes to dataset, copy dataset
mydata3_cleaned<-home_sample_3

# Convert sex, ID and hospital-variable to factor
mydata3_cleaned$sex <- as.factor(mydata3_cleaned$sex)
mydata3_cleaned$ID <- as.factor(mydata3_cleaned$ID)
mydata3_cleaned$hospital <- as.factor(mydata3_cleaned$hospital)

# Mindfulness is a scale of 1-6, so no value can be outside that range
# Replace out of range value with mean in order to keep the participant
mydata3_cleaned$mindfulness[mydata3_cleaned$mindfulness>6]=mean(mydata3_cleaned$mindfulness)

# Re-check to see if data cleaning worked 
mydata3_cleaned %>% summary()
# one "Female", because of different characters is in a separate factor than the other "female". Needs to be converted.

# Convert Female into female
mydata3_cleaned<- mydata3_cleaned %>% mutate(sex = droplevels(replace(sex, sex == "Female", "female")))

# Re-check again to see if data cleaning worked 
mydata3_cleaned %>% summary()
describe(mydata3_cleaned)

##################################	

# Use cleaned data from dataset 3
mydata3 <- mydata3_cleaned

# Build the linear model with random intercept from dataset 3
mod4_int_test = lmer(pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum + cortisol_saliva + mindfulness + (1|hospital), data = mydata3)	

mydata32 = mydata3 %>% 	
  mutate(resid = residuals(mod4_int_test))	


# Model diagnostics for linear mixed models	
# Influential outliers	
influence_observation1 = influence(mod4_int_test, obs = T)$alt.fixed 
influence_group1 = influence(mod4_int_test, group = "hospital")$alt.fixed	

data_plot_influence1 = as_tibble(influence_group1) %>% 	
  gather(colnames(influence_group1), value = coefficient, key = predictor)	

data_plot_influence1 %>% 	
  ggplot() +	
  aes(x = 1, y = coefficient, group = predictor) +	
  geom_violin() +	
  facet_wrap( ~ predictor, scales = "free")	


# ## Normality
qqmath(mod4_int_test, id=0.05)	

qqmath(ranef(mod4_int_test))	

# Check for Linearity	
plot(mod4_int_test, arg = "pearson")	

# Scatterplot of the residuals and the fixed predictors separately.
mydata32 %>% 	
  ggplot() +	
  aes(x = age, y = resid) +	
  geom_point()	

mydata32 %>% 	
  ggplot() +	
  aes(x = sex, y = resid) +	
  geom_point()	

mydata32 %>% 	
  ggplot() +	
  aes(x = STAI_trait, y = resid) +	
  geom_point()	

mydata32 %>% 	
  ggplot() +	
  aes(x = pain_cat, y = resid) +	
  geom_point()	

mydata32 %>% 	
  ggplot() +	
  aes(x = cortisol_serum, y = resid) +	
  geom_point()

mydata32 %>% 	
  ggplot() +	
  aes(x = cortisol_saliva, y = resid) +	
  geom_point()

mydata32 %>% 	
  ggplot() +	
  aes(x = mindfulness, y = resid) +	
  geom_point()


# Check for Homoscedasticity	
# Check the complete model F-test p-value. If < .05, heteroscedasticity on the cluster level might be a problem.
homosced_mod1 = lm(resid^2 ~ hospital, data = mydata32)	
summary(homosced_mod1)	
# p-value: .129


# Cyclone plot
# caluclate interquartile range within each cluster	
IQR_of_residuals_by_participant1 = sapply(split(mydata32, f = mydata32$hospital), function(x) IQR(x$resid))	
# rank ordering them	
rank1 = rank(IQR_of_residuals_by_participant1)	
# adding rank to the dataframe containing the residuals	
mydata32$rank1 = rep(rank1, each = length(c("pain")))	
# creating a vector of participant IDs ordered based on the rank, this will be used as labels	
IDforplot1 = unique(mydata32$hospital[order(mydata32$rank1)])	

# create the plot	
ggplot(mydata32, aes(y = resid, x = factor(rank1), labels = hospital))+	
  geom_boxplot()+	
  scale_x_discrete(labels=IDforplot1)+	
  coord_flip()	


# Check for Multicollinearity	
pairs.panels(mydata32[,c("pain", "age", "sex", "STAI_trait", "pain_cat", "cortisol_serum", "cortisol_saliva", "mindfulness")], col = "red", lm = T)	
# cortisol_serum & cortisol_saliva high correlation at 0.89

###################################################

# Refit model Intercept, remove cortisol_saliva
mod4_int_nosaliva = lmer(pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum + mindfulness + (1|hospital), data = mydata32)	

mydata33 = mydata3 %>% 	
  mutate(resid = residuals(mod4_int_nosaliva))	


# Re-run Model diagnostics for new model	
#Influential outliers	
influence_observation2 = influence(mod4_int_nosaliva, obs = T)$alt.fixed 
influence_group2 = influence(mod4_int_nosaliva, group = "hospital")$alt.fixed	

data_plot_inflience2 = as_tibble(influence_group2) %>% 	
  gather(colnames(influence_group2), value = coefficient, key = predictor)	

data_plot_inflience2 %>% 	
  ggplot() +	
  aes(x = 1, y = coefficient, group = predictor) +	
  geom_violin() +	
  facet_wrap( ~ predictor, scales = "free")	


# ## Normality
qqmath(mod4_int_nosaliva, id=0.05)	

qqmath(ranef(mod4_int_nosaliva))	


# Check for Linearity	
plot(mod4_int_nosaliva, arg = "pearson")	

# Scatterplot of the residuals and the fixed predictors separately.
  mydata33 %>% 	
  ggplot() +	
  aes(x = age, y = resid) +	
  geom_point()	

mydata33 %>% 	
  ggplot() +	
  aes(x = sex, y = resid) +	
  geom_point()	

mydata33 %>% 	
  ggplot() +	
  aes(x = STAI_trait, y = resid) +	
  geom_point()	

mydata33 %>% 	
  ggplot() +	
  aes(x = pain_cat, y = resid) +	
  geom_point()	

mydata33 %>% 	
  ggplot() +	
  aes(x = cortisol_serum, y = resid) +	
  geom_point()

mydata33 %>% 	
  ggplot() +	
  aes(x = mindfulness, y = resid) +	
  geom_point()


# Check for Homoscedasticity	

# Check the complete model F-test p-value. If < .05, heteroscedasticity on the cluster level might be a problem.
homosced_mod2 = lm(resid^2 ~ hospital, data = mydata33)	
summary(homosced_mod2)	
# Now p-value: 0.412


#Cyclone plot
# caluclate interquartile range within each cluster	
IQR_of_residuals_by_participant2 = sapply(split(mydata33, f = mydata33$hospital), function(x) IQR(x$resid))	
# rank ordering them	
rank2 = rank(IQR_of_residuals_by_participant2)	
# adding rank to the dataframe containing the residuals	
mydata33$rank2 = rep(rank2, each = length(c("pain")))	
# creating a vector of participant IDs ordered based on the rank, this will be used as labels	
IDforplot2 = unique(mydata33$hospital[order(mydata33$rank2)])	

# create the plot	
ggplot(mydata33, aes(y = resid, x = factor(rank2), labels = hospital))+	
  geom_boxplot()+	
  scale_x_discrete(labels=IDforplot2)+	
  coord_flip()	


# Check for Multicollinearity	
pairs.panels(mydata33[,c("pain", "age", "sex", "STAI_trait", "pain_cat", "cortisol_serum", "mindfulness")], col = "red", lm = T)	


# Final model intercept
mod4_int <- mod4_int_nosaliva

mod4_int


## Custom function to extract standardized beta coefficients from linear mixed models.	
stdCoef.merMod <- function(object) {	
  sdy <- sd(getME(object,"y"))	
  sdx <- apply(getME(object,"X"), 2, sd)	
  sc <- fixef(object)*sdx/sdy	
  se.fixef <- coef(summary(object))[,"Std. Error"]	
  se <- se.fixef*sdx/sdy	
  return(data.frame(stdcoef=sc, stdse=se))	
}	

# b (Estimate) and p-value
summary(mod4_int)
# CI 
confint(mod4_int)
# Std. Beta
stdCoef.merMod(mod4_int)

# Marginal R squared
r2beta(mod4_int)

# Marginal and conditional R squared
r.squaredGLMM(mod4_int)

#################################

# Apply regression equation to dataset 4

# Load dataset 4 
library(readr)
home_sample_4 <- read_csv("Home assignment/home_sample_4.csv")

# Check and explore the data
# View the data
view(home_sample_4)

# Investigate structure of the data
str(home_sample_4)

# Check for missing data
home_sample_4[!complete.cases(home_sample_4), ]
# Returns no missing data

# Check structure of the data to se if there are any irregularities
home_sample_4 %>% summary()
# ID, Sex, Hospital needs to be converted to factor 
# Two household_income has a negative number.

# Treat the irregularities/ Clean the data
# Before any changes to dataset, copy dataset
mydata4_cleaned<-home_sample_4

# Convert sex, ID and hospital-variable to factor
mydata4_cleaned$sex <- as.factor(mydata4_cleaned$sex)
mydata4_cleaned$ID <- as.factor(mydata4_cleaned$ID)
mydata4_cleaned$hospital <- as.factor(mydata4_cleaned$hospital)

# Household income can most likely not be negative (balance or expenses can, but no the actual income)
# Replace out of range value with mean in order to keep the two participants
mydata4_cleaned$household_income[mydata4_cleaned$household_income<0]=mean(mydata4_cleaned$household_income)

# Re-check to see if data cleaning worked 
mydata4_cleaned %>% summary()


#########################################

# to use the cleaned dataset 4 in this part
mydata4<- mydata4_cleaned

# model to use
mod4_int

# Make predictions dataset 4
predict_pain_mod4 = predict(mod4_int, newdata = mydata4, allow.new.levels = T)

# Calculate the sum of squared residuals on dataset 2	
RSS_mod4_int = sum((mydata4[,"pain"] - predict_pain_mod4)^2)	
RSS_mod4_int 
# 322.59

# TSS
data4_mean <- lm(pain ~ 1, data = mydata4)
TSS_mod4_int = sum((mydata4[,"pain"] - predict(data4_mean))^2)
TSS_mod4_int

# Forumla: 1 - (RSS/TSS)
1 - (RSS_mod4_int/TSS_mod4_int)
# 0.31

########################################

# Dataset is checked from before and untouched by the other models
mydata34 <- mydata3_cleaned

# Exploring the data & Visualizing
# Plot with one line
mydata34 %>% ggplot() + aes(y=pain, x=cortisol_serum) + geom_point(aes(color = hospital), size = 4) + geom_smooth(method = "lm", se = F)


# Scatterplot with both intercept and slope of the regression line
slope_plot = mydata34 %>% 		
  ggplot() +		
  aes(y = pain, x = cortisol_serum, color = hospital) +		
  geom_point(size = 4) +		
  geom_smooth(method = "lm", se = F, fullrange=TRUE) +		
  xlim(-1, 10) +		
  geom_hline(yintercept=0)+		
  geom_vline(xintercept=0)		
slope_plot		

# To compare with first plot, zoom in by changing the xlim
slope_plot_compare = mydata34 %>% 		
  ggplot() +		
  aes(y = pain, x = cortisol_serum, color = hospital) +		
  geom_point(size = 4) +		
  geom_smooth(method = "lm", se = F, fullrange=TRUE) +		
  xlim(2, 8) +		
  geom_hline(yintercept=0)+		
  geom_vline(xintercept=0)
slope_plot_compare


## Build new linear model on the most influential predictor from mod3_int: Cortisol_serum
mod5_rnd_slope = lmer(pain ~ cortisol_serum + (cortisol_serum|hospital), data = mydata34)		


# Fixed model to compare with 
mod5_fixed = lm(pain ~ cortisol_serum, data = mydata34)		
# Random intercept to compare with
mod5_rnd_int = lmer(pain ~ cortisol_serum + (1|hospital), data = mydata34)


# RSS
sum(residuals(mod5_fixed)^2)		
sum(residuals(mod5_rnd_int)^2)		
sum(residuals(mod5_rnd_slope)^2)	


# Visualize for each hospital separately
# Saving predictions of the model into a variable
mydata34_pred = mydata34 %>% 		
  mutate(pred_int = predict(mod5_rnd_int), pred_slope = predict(mod5_rnd_slope)) 		

# Regression line of the random intercept model	
mydata34_pred  %>% 		
  ggplot() +		
  aes(y = pain, x = cortisol_serum, group = hospital)+		
  geom_point(aes(color = hospital), size = 4) +		
  geom_line(color='red', aes(y=pred_int, x= cortisol_serum ))+		
  facet_wrap( ~ hospital, ncol = 2)

# Regression line of the random slope model	
mydata34_pred %>% 		
  ggplot() +		
  aes(y = pain, x = cortisol_serum, group = hospital)+		
  geom_point(aes(color = hospital), size = 4) +		
  geom_line(color='red', aes(y=pred_slope, x=cortisol_serum))+		
  facet_wrap( ~ hospital, ncol = 2)	

# AIC for fixed, to compare
AIC(mod5_fixed)
# cAIC
cAIC(mod5_rnd_int)$caic		
# 684.41 - smaller by more than 2: Intercept a slightly better fit.
cAIC(mod5_rnd_slope)$caic	
# 688.81

# ANOVA
anova(mod5_rnd_int, mod5_rnd_slope)
# No significant difference

