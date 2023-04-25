## modeling file
setwd("/Users/meggie/Documents/23 Spring/482/capstone2023")
source("EDA.R")
source("AIC-leaps.R")
library(leaps)

# simple linear regression
# removed X (identifier), state (already accounted for in longitude and latitude, numbers have no prediction value)
# and fire_class_size because it is associated with dependent variables

## Looking at model MSE

ntrain = round(.6 * nrow(wf))
ntest = nrow(wf) - ntrain

set.seed(482)
train_id = sort(sample(1:nrow(wf), ntrain)) 

wf_train = wf[train_id, ]
wf_test = wf[-train_id, ]

# Full Model
fulltrain_linmod = lm(fire_size ~ . - X - state - fire_size_class 
            - wstation_wban - wstation_byear - wstation_eyear 
            - discovery_month - disc_pre_year - disc_pre_month
            - fire_mag - Vegetation, data = wf_train)
fulltrain_summ = summary(fulltrain_linmod)

# train mse
mean(fulltrain_summ$residuals^2)

# test mse
full_preds = predict(fulltrain_linmod, newdata = wf_test)
mean((wf_test$fire_size - full_preds)^2)

fulltrain_summ$r.squared

# make reduced model
# predictors df
X = wf_train[, c(4,5,10,16:32)]
y = wf_train[, 2]

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

# Reduced Model
redtrain_linmod = lm(fire_size ~ latitude + longitude + dstation_m 
                     + Temp_pre_30 + Wind_pre_7 + Wind_cont + Hum_pre_30 
                     + Hum_cont + remoteness, data = wf_train)
redtrain_summ = summary(redtrain_linmod)

# reduced train mse
mean(redtrain_summ$residuals^2)

# reduced test mse
red_preds = predict(redtrain_linmod, newdata = wf_test)
mean((wf_test$fire_size - red_preds)^2)

redtrain_summ$r.squared

