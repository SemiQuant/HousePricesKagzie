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


# if we going to use some data for holdout - didnt realize the forst time that test was the predict data
# we can also only do everything to the test data and then use the cv only for final models eval
# can actually split into 3 if we want, training testing and validation sets
set.seed(1987)
keep <- createDataPartition(y = dat.train$SalePrice, p = 0.80, list = FALSE, times = 1)
dat.val <- dat.train[-keep,]
dat.train <- dat.train[-keep,]

# So anything we do to test set, we have to store to apply to the val set. Like scaling, imputaions etc.


# figure how to read this in
# system("cat data/data_description.txt")
# how we read this file in nice? idk what format it is..

# Is this what you mean? I think the best we can do is read as csv and the format wide if we want?
# We already in the folder so dont use full paths as they wont work accross computers...
# aslo no, ugly :p
data_description <- read.csv("data/data_description.txt", stringsAsFactors = FALSE)
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

# drop poolQC as none in train set
table(dat.train$PoolArea, dat.train$PoolQC, useNA = "a")
dat.train <- dat.train[!colnames(dat.train)=="PoolQC"]


# Alley
# system("grep -A 5 Alley data/data_description.txt")
# looks like the missing ally means none, so lets change it to "none" for now


# Lets see all
# system("grep NA data/data_description.txt")

# Instead of being super fancy:
# ok, but calling view when you keep re-runing code is annoying :p
# View(data_description[[1]][grep("NA", data_description[[1]])])

missing <- as.data.frame(
  dat.train %>%
    select_if(function(x) {any(is.na(x)) & is.factor(x)}) %>%
    apply(., 2, function(y){
      recode_factor(y, .missing = "None")})
)
# i dont like convertinf to matrix then back to data frame. strange things can happen. Would rather have more or uglier code and get round this.
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

# system("grep Mas data/data_description.txt")
table(is.na(dat.train$MasVnrArea), dat.train$MasVnrType)


# But why only 8 out of 864 have missing values? surely these should be zero if masonry veneer type is "none"?
# View(dat.train$MasVnrArea[dat.train$MasVnrType == "None"])
table(dat.train$MasVnrArea[dat.train$MasVnrType == "None"])

# Ok very confusing. All zero except for the NAs and like 5 weird outliers...

aggr(dat.train %>%
       select_if(function(x) any(is.na(x))),
     numbers = TRUE, prop = c(TRUE, FALSE))


# aggr(dat.train)

# ok, so still na's, we shoudl fix later - please note, this is jsut so i can look at the rest of the code, this is not what i want to do

# dat.train$LotFrontage[is.na(dat.train$LotFrontage)] <- 0
dat.train$LotFrontage <- replace_na(dat.train$LotFrontage, 0)
dat.train$MasVnrArea <- replace_na(dat.train$MasVnrArea, 0)


# I dont really know how to deal with this (also some models allow na's so dont "have too")
dat.train$GarageYrBlt <- replace_na(dat.train$MasVnrArea, median(dat.train$GarageYrBlt))
table(dat.train$GarageType, dat.train$GarageYrBlt, useNA = "a")
table(dat.train$YearBuilt, dat.train$GarageYrBlt, useNA = "a")
plot(dat.train$YearBuilt, dat.train$GarageYrBlt)
plot(dat.train$YearRemodAdd, dat.train$GarageYrBlt)
table(dat.train$YearRemodAdd == dat.train$YearBuilt, useNA = "a")
hmm <- ifelse(dat.train$YearRemodAdd != dat.train$YearBuilt,
              dat.train$YearRemodAdd,
              dat.train$YearBuilt)
plot(hmm, dat.train$GarageYrBlt)
points(x = ifelse(is.na(dat.train$GarageYrBlt), dat.train$YearBuilt, NA), y = rep(1930, nrow(dat.train)) ,
       col = "red", pch = 23)
# jsut make it year built? or 0?
# make seperate variable for if garage remodeled?
dat.train$GarageYrBlt <- replace_na(dat.train$GarageYrBlt, 0)




##################
## Make Dummies ##
check_inf_na <- function(dat.in){
  dat.in %>%
    select_if(function(x) any(is.infinite(x))) %>%
    summarise_all(funs(sum(is.infinite(.))))
  dat.in %>%
    select_if(function(x) any(is.na(x))) %>%
    summarise_all(funs(sum(is.na(.))))
}
check_inf_na(dat.train)

# remove id
rownames(dat.train) <- dat.train$Id
dat.train <- dat.train[-c(1)]


require(recipes)
ml.train.rec1 <- recipe( ~ ., data = dat.train) %>%
  # add an interaction term
  step_interact( ~ OverallQual:YearBuilt) %>%
  # make all factors dummies, can make better later (i say that a lot)
  step_dummy(all_predictors(), -all_numeric()) %>%   # this makes 1 hot encoded variabels
  #scale and center
  step_center(all_numeric()) %>%
  step_scale(all_numeric())

train_rec1 <- prep(ml.train.rec1, training = dat.train)

dat.train.trans <- bake(train_rec1, new_data = dat.train)


check_inf_na(dat.train.trans)
# hmm, should have checked data before this.. hence the

dat.train[colnames(dat.train)%in%names(check_inf_na(dat.train.trans))]


dat.train <- dat.train.trans[!colnames(dat.train.trans)%in%names(check_inf_na(dat.train.trans))]

# APPLY TO CV AND TRAIN
# dat.val <- bake(train_rec1, new_data = dat.val)
# dat.test <- bake(train_rec1, new_data = dat.test)


##################


#########################
## Feature Engineering ##
dat.train <- dat.train %>% mutate(YrOld = YrSold - YearBuilt,
                                  YrOldReno = YrSold - YearRemodAdd,
                                  YrGar = YrSold - GarageYrBlt)

dat.train$remod <- as.factor(ifelse(dat.train$YearRemodAdd!=dat.train$YearBuilt, 1, 0))
dat.train$remod.gar <-  as.factor(ifelse(dat.train$YearRemodAdd < dat.train$GarageYrBlt, 1, 0))
#########################


##########################
## Exploratory Analysis ##
# require(caret)
# these are supposed to be ugly and quick

# feat plot
featurePlot(x = dat.train[,1:4],
            y = y.train,
            # plot = "pairs",
            # plot = "ellipse",
            # plot = "density",
            # plot = "scatter",
            auto.key = list(columns = 3)
)


# Identifying Correlated Predictors
require(corrplot)
dat.train.mat <- data.matrix(dat.train)
dat.train.cor <- cor(dat.train.mat)
# corrplot.mixed(dat.train.cor, lower.col = "black", number.cex = .7)


# Zero- and Near Zero-Variance Predictors - will have to refine this
nzv <- nearZeroVar(dat.train)

dat.train <- dat.train[!colnames(dat.train)%in%colnames(dat.train)[nzv]]

# why dat.train[!nzv] not work??

# Linear Dependencies
# QR decomposition of a matrix to enumerate sets of linear combinations (if they exist)
comboInfo <- findLinearCombos(dat.train)
comboInfo

# dat.train[!comboInfo$remove]
dat.train <- dat.train[!colnames(dat.train)%in%colnames(comboInfo$remove)[nzv]]


##########################

dat.train$y <- y.train
save(dat.train, file = "tmp/dat.train.R")



######################################
## apply transformations to val set ##
dat.val <- dat.val[!colnames(dat.val)=="PoolQC"]
missing.val <- as.data.frame(
  dat.val %>%
    select_if(colnames(dat.val)%in%colnames(missing)) %>%
    apply(., 2, function(y){
      recode_factor(y, .missing = "None")})
)
dat.val <- dat.val[!colnames(dat.val)%in%colnames(missing.val)]
dat.val <- bind_cols(dat.val, data.frame(missing))
dat.val$LotFrontage <- replace_na(dat.val$LotFrontage, 0)
dat.val$MasVnrArea <- replace_na(dat.val$MasVnrArea, 0)
dat.val$GarageYrBlt <- replace_na(dat.val$GarageYrBlt, 0)

table(is.na(missing.val))
# thank the lord, or this woudl get more complicated
# what we would have to do is use our training data to predict, as you cant use the test data (like we do with bake below, so that scales using the scaling learnt form the train data for example)
# not too hard but we'll leave it for next exercise

dat.val.trans <- bake(train_rec1, new_data = dat.val)
dat.val <- dat.val %>% mutate(YrOld = YrSold - YearBuilt,
                              YrOldReno = YrSold - YearRemodAdd,
                              YrGar = YrSold - GarageYrBlt)
dat.val$remod <- as.factor(ifelse(dat.val$YearRemodAdd!=dat.val$YearBuilt, 1, 0))
dat.val$remod.gar <-  as.factor(ifelse(dat.val$YearRemodAdd < dat.val$GarageYrBlt, 1, 0))


dat.val <- dat.val[colnames(dat.val)%in%colnames(dat.train)]
table(colnames(dat.val)%in%colnames(dat.train))

save(dat.val, file = "tmp/dat.val.R")
######################################