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


##################
## read in data ##
# from Kaggle https://www.kaggle.com/c/house-prices-advanced-regression-techniques
dat.train <- read.csv("data/train.csv")
dat.test <- read.csv("data/test.csv")
str(dat.train)
str(dat.test)

# figure how to read this in
system("cat data/data_description.txt")
# how we read this file in nice? idk what format it is..


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
system("grep -A 5 Alley data/data_description.txt")
# looks like the missing ally means none, so lets change it to "none" for now

# Lets see all
system("grep NA data/data_description.txt")

# so all missing that are factor are none
missing <- dat.train %>%
  select_if(function(x) any(is.na(x)))%>%
  select_if(function(x) is.factor(x))

dat.train <- dat.train[!colnames(dat.train)%in%colnames(missing)]

missing[] <- lapply(missing, as.character)
missing[is.na(missing)] <- "None"
missing[] <- lapply(missing, as.factor)

#think its in the same orde, bas code you can clean :)
dat.train <- bind_cols(dat.train, data.frame(missing))


# these are trule missing?
colnames(dat.train %>%
       select_if(function(x) any(is.na(x))))

system("grep -A 2 -B 2 GarageYrBlt data/data_description.txt")
table(is.na(dat.train$GarageYrBlt), dat.train$GarageCars==0)
#so this isnt there as they is no garage

system("grep LotFrontage data/data_description.txt")
table(lotFront = is.na(dat.train$LotFrontage), lotArea = dat.train$LotArea>0)
# ah, one truley missing! Lets impute this later

system("grep Mas data/data_description.txt")
table(is.na(dat.train$MasVnrArea), dat.train$MasVnrType)

aggr(dat.train %>%
       select_if(function(x) any(is.na(x))),
     numbers = TRUE, prop = c(TRUE, FALSE))


# so we will impute LotFrontage
# GarageYrBlt and MasVnrArea must be dealt with but not imputed

#something like this - thinking just using the Lot data for the lot imputation?
# require(mice)
methods(mice)
tempData <- mice(dat.train[grepl("Lot", colnames(dat.train))], m=5, maxit=50, meth='cart', seed=1987)
summary(tempData)
densityplot(tempData)
stripplot(tempData, pch = 20, cex = 1.2)

completedData <- complete(tempData, 2)
table(is.na(completedData))
dim(completedData)

dat.train$LotFrontage <- completedData$LotFrontage


aggr(dat.train %>%
       select_if(function(x) any(is.na(x))),
     numbers = TRUE, prop = c(TRUE, FALSE))

dat.train <- dat.train[-c(1)]

################








everything below I jsut took from some of my other scripts, havent changed anything yet








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




#########################
## Feature Engineering ##

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