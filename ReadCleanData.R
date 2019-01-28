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

# Lets see all
system("grep NA data/data_description.txt")

# so all missing that are factor are none
missing <- dat.train %>%
  select_if(function(x) any(is.na(x)))%>%
  select_if(function(x) is.factor(x))

dat.train <- dat.train[!colnames(dat.train)%in%colnames(missing)]

missing[] <- lapply(missing, as.character)
missing[is.na(missing)] <- "none"
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
# where is this ?? dat.train$MasVnrType


aggr(dat.train %>%
       select_if(function(x) any(is.na(x))),
     numbers = TRUE, prop = c(TRUE, FALSE))



# so we will impute LotFrontage and MasVnrArea?
# GarageYrBlt must be dealt with





################
