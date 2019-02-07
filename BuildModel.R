# rm(list = ls())
# Load packages and set some things up #
require(doParallel)
set.seed(1987)
cl <- makePSOCKcluster(detectCores()-2)
registerDoParallel(cl)

require(tidyverse)
require(VIM)
require(corrplot)
require(mice)
require(caret)
library(moments)
library(MASS)



I feel you jumped into this before truly considering the data, just want to do the fun bits huh?


load("tmp/dat.train.R")

  # Dave's exploratory data analysis
  # Lets first look at Y variable.

density_y <- density(y.train)
hist(y.train, plot = TRUE)
plot(density_y)

# Heavily right skewed. Looks like it may be log normally distributed. Try log transform?
hist(log(y.train), plot = TRUE)
density_logy <- density(log(y.train))
plot(density_logy)

# ?ks.test
# Ok looks much better. Lets test for normality. P-Value will tell us the probability that this data comes from a normal distribution.

shapiro.test(log(y.train))
#gay, this test is gay

# Ok so not normal...

# Lets look at skewness and kurtosis stats:

skewness(log(y.train))
kurtosis(log(y.train))
# nice.

# Skewness seems like less of a problem than kurtosis.
# skew should be 0 kurt should be 3. We can look at box-cox transform or ignore because its at
# least approx normal and review residuals or regression.

# Lets see what a basic linear model from stepwise regression looks like:

# library(leaps)

dat.train$y <- y.train

#yeah, defult is na.fail
#you didnt clean your data, then you throw it out below!
length(complete.cases(dat.train))
dim(dat.train)

# # Something wrong with one or more variables. Lets find it.
#
# univariate_lm <- lapply(dat.train, function(x){
#   lm(y.train ~ x)
# })
# univariate_lm

# They all seem to run fine as univariate regressions...
# Except Utilities only has one observation witha  different value. Lets remove it and see if it works.

# if you jsut did your data pre-processing...

# Probably not enough degrees of freedom for the estimation.
?lm
all_regression <- lm(y ~ ., data = dat.train, na.action = na.fail)

# Hooray! Ok lets use Akaike stepwise regression front + back

# ?stepAIC
test.step <- stepAIC(all_regression, direction = "both", na.fail = TRUE)

# so you can do this with bootstrapping, more robust but still not a great method
require(bootStepAIC)
boot.stepAIC(all_regression, dat.train, B = 100, alpha = 0.05, direction = "both",  k = 2, verbose = T)


summary(test.step)
par(mfrow = c(2, 2))
plot(test.step)

# Look like we have two major outliers that are impacting the model. 524 and 826.
View(dat.train[c(208, 180), ])

# Lets look at the transformed data
all.data.log <- cbind(y = log(y.train), dat.train)
# all.data.log.nona <- all.data.log[ ,!unlist(lapply(all.data.log, function(x){any(is.na(x))}))]
all.data.log.nona <- lm(y ~ ., data = all.data.log)
test.step.log.nona <- stepAIC(all.data.log.nona, direction = "both")
summary(test.step.log.nona)
plot(test.step.log.nona)

# Outliers dont change much. except 1325 is replaced by 463 in plot 1. 692 is replaced by 463 in plot 2 and 3 and 11893 is replaced by better fitting 89 in plot 4.
View(all.data.nona[c(7, 91, 192), ])

# surprisingly none of the outliers are the one house without all public utilities...

# before we think about removing the outliers, lets try address over-fitting using a cross-validation model

# Also need to try cross validation models or LASSO regression from glmnet package.










# caret implementation of below
set.seed(1987)
trnCtrl.lmAIC <- trainControl(
  method = "cv", ## CV #methods = "boot", "cv", "LOOCV", "LGOCV", "repeatedcv", "timeslice", "none" and "oob"
  number = 10, ## 10 folds
  # repeats = 10,## repeated ten times
  verbose = F,
  allowParallel = T
  # search = "random"
)


quickMet <- function(mod = NULL){
  print(mod)
  densityplot(mod, pch = "|")
  # fit.BTlin$finalModel
  v.imp <- varImp(fit.BTlin)
  print(v.imp)
  plot(v.imp)
}


# mod 1 quickie
fit.lm <- train(y ~ .,
                data = dat.train[1:30,],
                method = "lm",
                trControl = trnCtrl.lmAIC,
                metric = "RMSE"
)
quickMet(fit.lm)

# mod 2 quickie
fit.lmAIC <- train(y ~ .,
                   data = dat.train[1:30,],
                   method = "glmStepAIC",
                   trControl = trnCtrl.lmAIC,
                   verbose = T
)
quickMet(fit.lmAIC)

# Now see how easy a rf for example would be to adapt here to an ensembles of GLMs
fit.rGLM <- train(y ~ .,
                  data = dat.train[1:30,],
                  method = "randomGLM",
                  trControl = trnCtrl.lmAIC,
                  verbose = T
)
quickMet(fit.rGLM)

# or a grad boosted tree
fit.BTlin <- train(y ~ .,
                   data = dat.train,
                   method = "xgbLinear",
                   trControl = trnCtrl.lmAIC,
                   verbose = T,
                   tuneLength = 2
)
quickMet(fit.BTlin)

obs more tuning and settings required and such but you get the idea





wont work as havent manipulated dat.val as we did dat.train
# predict(fit.rGLM, dat.val)



# install.packages("DAAG")
library(DAAG)
?cv.lm
str(test.step.log.nona)
cv.lm(data = all.data.log.nona, form.lm = y ~ .)
str(all.data.log.nona[, -8])
# Is this because the condition2 factor levels names are the same as condition1?
levels(all.data.log.nona$Condition1)
levels(all.data.log.nona$Condition2)
# But this can't be a problem because lots of factors have the same name...
View(data.frame(all.data.log.nona$Condition1, all.data.log.nona$Condition2))

# try remove cond 2
cv.lm(data = all.data.log.nona[, -c(8, 13)], form.lm = y ~ .)
# Now same problem with roofstyle?

cross_validate_3 <- cv.lm(data = all.data.log.nona[, -c(8, 13, 20, 21, 22, 23, 25, 31, 32, 46, 60, 65, 69, 74, 77)], form.lm = y ~ ., m = 3)
str(cross_validate_3)
which(colnames(all.data.log.nona) == "GarageCond")
# Ok so if i remove all factors with a dominant level it works. Now lets try fix this by replacing those factors with corresponding dummy variables.



# Solution
# https://stackoverflow.com/questions/28274040/cvlm-with-categorical-variables-factor-has-new-levels
# This is the typical problem of having different levels in the factor variables between the folds in the cross validation. The algorithm creates dummy variables for the training set but the test set has different levels to the training set and thus the error. The solution is to create the dummy variables on your own and then use the CVlm function:



#   Solution
#
# dummy_LW <- model.matrix(~LW, data=df)[,-1]    #dummy for LW
# dummy_CWT <- model.matrix(~CWT, data=df)[,-1]  #dummies for CWT
# df <- Filter(is.numeric,df)                    #exclude LW and CWT from original dataset
# df <- cbind(df,dummy_LW,dummy_CWT)             #add the dummies instead
# Then run the model as you did (make sure you add the new variable names):
#
#   model<-  lm(formula = o3 ~ LagO3 + Z + RH + ST + TC + Tmx + dummy_LW +
#                 CWTC + CWTE + CWTN + CWTNE + CWTNW + CWTS +
#                 CWTSW + CWTU + CWTW,
#               data = df, na.action = na.exclude)
# cvlm.mod <- CVlm(na.omit(data),model,m=10)
# Unfortunately, I cannot test the above as your code has too few rows to work (only 6 rows are not enough) but the above will work.
#
# A few words about model.matrix:
#
#   It creates dummy variables for categorical data. By default is leaves one level out as the reference level (as it should), because you will have a correlation of 1 between dummies otherwise. [,-1] in the above code just removes the intercept which is an unneeded column of 1s.


# everything below I just took from some of my other scripts, havent changed anything yet








##########################
## Exploratory Analysis ##
# require(caret)
# these are supposed to be ugly and quick

# feat plot
featurePlot(x = dat.train[,3:4],
            y = y.train,
            # plot = "pairs",
            # plot = "ellipse",
            # plot = "density",
            # plot = "scatter",
            auto.key = list(columns = 3)
)

# Can't get this to work with all the plots? only the first plot works when run alone? But I have no idea how to read this...


# Identifying Correlated Predictors
# require(corrplot)
dat.train.mat <- as.matrix(dat.train)
dat.train.cor <- cor(dat.train.mat)
corrplot.mixed(dat.train.cor, lower.col = "black", number.cex = .7)


# Zero- and Near Zero-Variance Predictors
nzv <- nearZeroVar(dat.train)

# Linear Dependencies
# QR decomposition of a matrix to enumerate sets of linear combinations (if they exist)
comboInfo <- findLinearCombos(dat.train)





##########################





##################
## Make Dummies ##

##################




## Feature Engineering ##
dat.train %>% mutate(YrOld = YrSold - YearBuilt,
                     YrOldReno = YrSold - YearRemodAdd,
                     YrGar = YrSold - GarageYrBlt) -> dat.train

#########################


################
## preProcess ##
preProcValues <- preProcess(dat, method = c("center", "scale")) #, "knnImpute"
dat.train <- predict(preProcValues, dat.train)
# testTransformed <- predict(preProcValues, test)

lets use caret for this

################




########################
## Trianing the Model ##
lets use caret for this
########################
## Trianing the Model ##
set.seed(1987)
trnCtrl.rf <- trainControl(
  method = "repeatedcv", ## CV #methods = "boot", "cv", "LOOCV", "LGOCV", "repeatedcv", "timeslice", "none" and "oob"
  number = 10, ## 10 folds
  repeats = 10,## repeated ten times
  verbose = F,
  allowParallel = T
  # search = "random"
)


# Alternate Tuning Grids
grid.rf <- expand.grid(mtry = c(2, 3, 4, 5, 6),
                       splitrule = c("gini", "extratrees"),
                       min.node.size = c(1, 3, 5))
nrow(grid.rf)

# gradient boosting machine
fit.rf <- train(y.train ~ .,
                data = dat.train,
                method = "rf",
                trControl = trnCtrl.rf,
                verbose = FALSE,
                metric = "RMSE"
                # tuneLength = 2
                # tuneGrid = grid.rf
)
fit.rf
plot(fit.rf)
densityplot(fit.rf, pch = "|")
fit.rf$finalModel


# Variable Importance
fit.rf.Imp <- varImp(fit.rf$finalModel, scale = FALSE)
fit.rf.Imp
plot(fit.rf.Imp)




########################

#################
## EVALUATIONS ##
lets use caret for this

# Measures for Predicted Classes
youll need to transform the test set same as you did the train

predict(fit.rf, newdata = dat.train)
(mod.probs=predict(fit.rf, newdata = dat.train, type = "prob"))


# make some plots

#################