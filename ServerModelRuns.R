require(doParallel)
set.seed(1987)
cl <- makePSOCKcluster(6)
registerDoParallel(cl)

require(caret)


# setwd("/home/lmbjas002/HousePricesKagzie")
load("tmp/dat.train.R")



trnCtrl.xgbLin<- trainControl(
  method = "repeatedcv", ## CV #methods = "boot", "cv", "LOOCV", "LGOCV", "repeatedcv", "timeslice", "none" and "oob"
  number = 10, ## 10 folds
  repeats = 10, ## repeated ten times
  verbose = F,
  allowParallel = T
)


fit.xgbLin<- train(y ~ .,
                   data = dat.train,
                   method = "xgbLinear",
                   trControl = trnCtrl.xgbLin,
                   verbose = T,
                   tuneLength = 10,
                   metric="RMSE"
)

save(fit.xgbLin, file = "tmp/fit.xgbLin.R")
print("done A")
# then once we identify some good variables we can do a grid search for nice paramaters (I must write an edit to caret wrapper for grid search
                                                                                       # as variying only one paramater is inificient)

grid.xgbLin= expand.grid(
  max_depth = c(6, 12, 13),
  gamma = c(0, 1, 2, 3),
  subsample = c(0.5, 1),
  colsample_bytree = c(0.3, 0.4, 0.5),
  min_child_weight = c(0.5, 1, 1.5)
)

fit.xgbLinGrid <- train(y ~ .,
                   data = dat.train,
                   method = "xgbLinear",
                   trControl = trnCtrl.xgbLin,
                   verbose = F,
                   metric="RMSE",
                   tuneGrid = grid.xgbLin
)

save(fit.xgbLinGrid, file = "tmp/fit.xgbLinGrid.R")




# # slurm run code
#
# #!/bin/sh
# # The line below indicates which accounting group to log your job against
# #SBATCH --account=liiu
# #SBATCH --partition=ada
# #SBATCH --nodes=1 --ntasks=6
# #SBATCH --time=12:00:00
# #SBATCH --job-name="MLxgBoostT1"
# #SBATCH --mail-user=jason.limberis@uct.ac.za
#
# module load 'software/R-3.5.1'
# source /home/lmbjas002/.local/bin/bot_functions.sh
#
# cd "/home/lmbjas002/HousePricesKagzie"
# Rscript "ServerModelRuns.RServerRuns.R" | netcat seashells.io 1337 | tee >$(read Msg; sndMSG "$Msg" > /dev/null)
