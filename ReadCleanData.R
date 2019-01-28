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

require(tidyverse)
missing <- dat.train %>%
  select_if(function(x) any(is.na(x)))

require(VIM)
aggr(missing, numbers = TRUE, prop = c(TRUE, FALSE))

(to_rem <- dat.train %>%
    select_if(function(x) sum(is.na(x))>nrow(dat.train)*0.2) %>%
    summarise_each(funs(sum(is.na(.))))
)

# Alley
system("grep -A 5 Alley data/data_description.txt")
# looks like the missing ally means none, so lets change it to "none" for now
class(dat.train$Alley)
dat.train$Alley <- as.character(dat.train$Alley); dat.train$Alley[is.na(dat.train$Alley)] <- "none"

# FireplaceQu
system("grep -A 8 FireplaceQu data/data_description.txt")
# looks like the missing ally means none, so lets change it to "none" for now
class(dat.train$FireplaceQu)
unique(dat.train$FireplaceQu)
dat.train$FireplaceQu <- as.character(dat.train$FireplaceQu); dat.train$FireplaceQu[is.na(dat.train$FireplaceQu)] <- "none"
table(dat.train$FireplaceQu)
# will get to reordering and making thing nice later on, lets do NAs for now

# PoolQC
system("grep -A 8 FireplaceQu data/data_description.txt")

# Fence
system("grep -A 8 FireplaceQu data/data_description.txt")

# MiscFeature
system("grep -A 8 FireplaceQu data/data_description.txt")


# Lets chuck greater than 20% misingness (we probably going to use techniques that cant handel missing data easy)

################
