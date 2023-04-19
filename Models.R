## modeling file
setwd("/Users/meggie/Documents/23 Spring/482/capstone2023")
source("EDA.R")
source("AIC-leaps.R")
library(leaps)

# simple linear regression
# removed X (identifier), state (already accounted for in longitude and latitude, numbers have no prediction value)
# and fire_class_size because it is associated with dependent variables
# full model
linmod = lm(fire_size ~ . - X - state - fire_size_class 
            - wstation_wban - wstation_byear - wstation_eyear 
            - discovery_month - disc_pre_year - disc_pre_month
            - fire_mag - Vegetation, data = wf)
summary(linmod)

# R squared: 0.2684

# BIC of full model
AIC(linmod,k=log(length(wf$fire_size)))

# predictors df
X = wf[, c(4,5,10,16:32)]
y = wf[, 2]

leaps_ic = leaps.AIC(X, y)
# see distribution of BIC values
boxplot(leaps_ic[[2]])
# get minimum BIC value
nvar_minBIC = which(leaps_ic[[2]] == min(leaps_ic[[2]]))

leaps_output = leaps(X, y, nbest =1)
var_names = colnames(X)
var_mask = leaps_output$which[nvar_minBIC, ]
model_vars = var_names[var_mask]
model_vars

# reduced model chosen by BIC
red_linmod = lm(fire_size ~ latitude + longitude + dstation_m 
                + Temp_pre_30 + Wind_pre_7 + Wind_cont + Hum_pre_7 
                + Hum_cont + remoteness, data = wf)
summary(red_linmod)

# R squared is 0.2683

## see if there is a significant diff between full and reduced models
anova(linmod, red_linmod)
