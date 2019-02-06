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

##################
## read in data ##
# from Kaggle https://www.kaggle.com/c/house-prices-advanced-regression-techniques
dat.train <- read.csv("data/train.csv")
dat.test <- read.csv("data/test.csv")
str(dat.train)
str(dat.test)

# figure how to read this in
# system("cat data/data_description.txt")
# how we read this file in nice? idk what format it is..

# Is this what you mean? I think the best we can do is read as csv and the format wide if we want?
data_description <- read.csv("D:/Research/R learning/Kaggle/data/data_description.txt", stringsAsFactors = FALSE)


# head(read.csv("data/sample_submission.csv"))
colnames(dat.train)[!colnames(dat.train)%in%colnames(dat.test)]
y.train <- dat.train$SalePrice
dat.train <- dat.train[colnames(dat.train)!="SalePrice"]
table(colnames(dat.train)%in%colnames(dat.test))
##################

################
## Misingness ##

# require(tidyverse)
missing <- dat.train %>%
  select_if(function(x) any(is.na(x)))

# require(VIM)
aggr(missing, numbers = TRUE, prop = c(TRUE, FALSE))

(to_rem <- dat.train %>%
    select_if(function(x) sum(is.na(x))>nrow(dat.train)*0.2) %>%
    summarise_each(funs(sum(is.na(.))))
)

# Alley
# system("grep -A 5 Alley data/data_description.txt")
# looks like the missing ally means none, so lets change it to "none" for now


# Lets see all
# system("grep NA data/data_description.txt")

# Instead of being super fancy:
View(data_description[[1]][grep("NA", data_description[[1]])])


# so all missing that are factor are none
# missing <- dat.train %>%
#   select_if(function(x) any(is.na(x)))%>%
#   select_if(function(x) is.factor(x))

missing <- as.data.frame(
  dat.train %>%
    select_if(function(x) {any(is.na(x)) & is.factor(x)}) %>%
    apply(., 2, function(y){
      recode_factor(y, .missing = "None")})
)

dat.train <- dat.train[!colnames(dat.train)%in%colnames(missing)]

#think its in the same orde, bas code you can clean :)
dat.train <- bind_cols(dat.train, data.frame(missing))


# these are truly missing?
colnames(dat.train %>%
           select_if(function(x) any(is.na(x))))

# Lets check

apply(dat.train %>%
        select_if(function(x) {any(is.na(x))})
      , 2, min, na.rm = TRUE)

# Lot Frontage not a panhandle?

# No idea what masonry veneer area is but it looks like truly missing because min value is zero

# Garage Year Built is NA when there is no Garage but its not missing data
identical(
  dat.train$GarageYrBlt[dat.train$GarageYrBlt == "NA"] == "NA",
  dat.train$GarageYrBlt[dat.train$GarageType == "None"] == "NA"
)

# ok so same conclusion as below, your method is better but choice of variable is worse :)

# system("grep -A 2 -B 2 GarageYrBlt data/data_description.txt")
table(is.na(dat.train$GarageYrBlt), dat.train$GarageCars==0)
#so this isnt there as they is no garage

# system("grep LotFrontage data/data_description.txt")
# table(lotFront = is.na(dat.train$LotFrontage), lotArea = dat.train$LotArea>0)
# ah, one truley missing! Lets impute this later
# Nope, lot area doesn't need frontage


# system("grep Mas data/data_description.txt")
table(is.na(dat.train$MasVnrArea), dat.train$MasVnrType)


# But why only 8 out of 864 have missing values? surely these should be zero if masonry veneer type is "none"?

View(dat.train$MasVnrArea[dat.train$MasVnrType == "None"])

# Ok very confusing. All zero except for the NAs and like 5 weird outliers...

aggr(dat.train %>%
       select_if(function(x) any(is.na(x))),
     numbers = TRUE, prop = c(TRUE, FALSE))

# Can we delete the imputations?
# Also should we make NA LotFrontage 0?
# What should we do about Mason Veneer Area? Surely if type is none then area should be none? Or does that mean there is allocated space for mason veneer that's not used? like an untiled bathroom that needs tiles?

# # so we will impute LotFrontage
# # GarageYrBlt and MasVnrArea must be dealt with but not imputed
#
# #something like this - thinking just using the Lot data for the lot imputation?
# # require(mice)
# methods(mice)
# tempData <- mice(dat.train[grepl("Lot", colnames(dat.train))], m=5, maxit=50, meth='cart', seed=1987)
# summary(tempData)
# densityplot(tempData)
# stripplot(tempData, pch = 20, cex = 1.2)
#
# completedData <- complete(tempData, 2)
# table(is.na(completedData))
# dim(completedData)
#
# dat.train$LotFrontage <- completedData$LotFrontage
#
#
# aggr(dat.train %>%
#        select_if(function(x) any(is.na(x))),
#      numbers = TRUE, prop = c(TRUE, FALSE))
#
# dat.train <- dat.train[-c(1)]

################

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

# Ok so not normal...

# Lets look at skewness and kurtosis stats:

skewness(log(y.train))
kurtosis(log(y.train))

# Skewness seems like less of a problem than kurtosis. skew should be 0 kurt should be 3. We can look at box-cox transform or ignore because its at least approx normal and review residuals or regression.

# Lets see what a basic linear model from stepwise regression looks like:

# library(leaps)

all.data <- cbind(y = y.train, dat.train[ ,-1])

lm(y ~ ., data = all.data)

# Something wrong with one or more variables. Lets find it.

univariate_lm <- lapply(dat.train[, -1], function(x){
  lm(y.train ~ x)
})
univariate_lm

# They all seem to run fine as univariate regressions...
# Except Utilities only has one observation witha  different value. Lets remove it and see if it works. Probably not enough degrees of freedom for the estimation.
?lm
all_regression <- lm(y ~ ., data = all.data[, -9], na.action = na.omit)

# Hooray! Ok lets use Akaike stepwise regression front + back

# ?stepAIC
test.step <- stepAIC(all_regression, direction = "both")

# Error message relates to stepAIC notes : "The model fitting must apply the models to the same dataset. This may be a problem if there are missing values and an na.action other than na.fail is used (as is the default in R). We suggest you remove the missing values first."

test.step <- stepAIC(all_regression, direction = "both", na.fail = TRUE)

# Ok I'm just going to exclude the variables with NA from the regression for now

all.data.nona <- all.data[ ,!unlist(lapply(all.data, function(x){any(is.na(x))}))]

all_regression.nona <- lm(y ~ ., data = all.data.nona)

test.step.nona <- stepAIC(all_regression.nona, direction = "both")

summary(test.step.nona)
par(mfrow = c(2, 2))
plot(test.step.nona)

# Look like we have two major outliers that are impacting the model. 524 and 826.
View(all.data.nona[c(524, 826), ])

# Lets look at the transformed data
all.data.log <- cbind(y = log(y.train), dat.train[ ,-1])
all.data.log.nona <- all.data.log[ ,!unlist(lapply(all.data.log, function(x){any(is.na(x))}))]
all_regression.log.nona <- lm(y ~ ., data = all.data.log.nona)
test.step.log.nona <- stepAIC(all_regression.log.nona, direction = "both")
summary(test.step.log.nona)
plot(test.step.log.nona)

# Outliers dont change much. except 1325 is replaced by 463 in plot 1. 692 is replaced by 463 in plot 2 and 3 and 11893 is replaced by better fitting 89 in plot 4.
View(all.data.nona[c(463, 524, 826), ])

# surprisingly none of the outliers are the one house without all public utilities...

# before we think about removing the outliers, lets try address over-fitting using a cross-validation model

# Also need to try cross validation models or LASSO regression from glmnet package.

# install.packages("DAAG")
library(DAAG)
?cv.lm
str(test.step.log.nona)
cv.lm(data = all.data.log.nona, form.lm = y ~ MSZoning + LotArea + Street + LandContour +
        LotConfig + LandSlope + Neighborhood + Condition1 + Condition2 +
        BldgType + OverallQual + OverallCond + YearBuilt + YearRemodAdd +
        RoofStyle + RoofMatl + Exterior1st + ExterCond + Foundation +
        BsmtFinSF1 + BsmtFinSF2 + BsmtUnfSF + Heating + HeatingQC +
        CentralAir + X1stFlrSF + X2ndFlrSF + LowQualFinSF + BsmtFullBath +
        FullBath + HalfBath + KitchenAbvGr + KitchenQual + TotRmsAbvGrd +
        Functional + Fireplaces + GarageCars + GarageArea + WoodDeckSF +
        EnclosedPorch + X3SsnPorch + ScreenPorch + PoolArea + SaleType +
        SaleCondition + BsmtQual + BsmtCond + BsmtExposure + GarageQual +
        GarageCond + PoolQC)



# everything below I jsut took from some of my other scripts, havent changed anything yet








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