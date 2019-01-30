
#####################################################################################################

# only Original data

library(randomForest)
library(caret)

## remove all history
rm(list=ls())

## read csv file
d_score <- read.csv("cluster_origin.csv")

## create folds , 10-fold Cross Validation
fld <- createFolds(d_score$result, k=10)

temp_loss <- c() # temp loss for each fold
final_loss <- c() # final loss

for( i in 1:10 ) {
  temp_loss <- c()
  for( j in 1:10 ) {
    
    ds.train <- d_score[-fld[[j]], ]
    ds.test <- d_score[fld[[j]], ]
    
    d_score.rf <- randomForest(result ~ ., data=ds.train, ntree = 20000)
    
    d_score.pred <- predict(d_score.rf, ds.test)
    d_score.pred <- round(d_score.pred, 0)
    
    ## compare with real answer
    compare <- cbind(d_score.pred, ds.test[, "result"], abs(d_score.pred-ds.test[,"result"]))
    
    ## calculate RMSE
    lm_loss_avg <- sqrt( sum( ( d_score.pred-ds.test[, "result"] )^2 ) / nrow(compare) )
    temp_loss[j] <- lm_loss_avg
    
  }
  final_loss[i] <- mean(temp_loss)
}
final_loss
mean(final_loss)

#####################################################################################################

# only vir data

## remove all history
rm(list=ls())

library(randomForest)
library(caret)

## read csv file
d_score <- read.csv("driving_score_virtual_rep.csv")

## create folds , 10-fold Cross Validation
fld <- createFolds(d_score$result, k=10)

temp_loss <- c() # temp loss for each fold
final_loss <- c() # mean of temp_loss for each fold

for( i in 1:10 ) {
  temp_loss <- c()
  for( j in 1:10 ) {
    
    ds.train <- d_score[-fld[[j]], ]
    ds.test <- d_score[fld[[j]], ]
    
    d_score.rf <- randomForest(result ~ ., data=ds.train, ntree = 20000)
    
    d_score.pred <- predict(d_score.rf, ds.test)
    d_score.pred <- round(d_score.pred, 0)
    
    ## compare with real answer
    compare <- cbind(d_score.pred, ds.test[, "result"], abs(d_score.pred-ds.test[,"result"]))
    
    ## calculate RMSE
    RMSE <- sqrt( sum( ( d_score.pred-ds.test[, "result"] )^2 ) / nrow(compare) )
    temp_loss[j] <- RMSE
    
  }
  final_loss[i] <- mean(temp_loss)
}
final_loss
mean(final_loss)

#####################################################################################################