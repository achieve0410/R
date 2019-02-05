
#####################################################################################################

# only Original data

library(e1071)
library(caret)

## remove all history
rm(list=ls())

## read csv file
d_score <- read.csv("cluster_origin_4.csv")

## create folds , 10-fold Cross Validation
fld <- createFolds(d_score$result, k=10)

temp_loss <- c() # loss for each fold

for( i in 1:10 ) {
    
    x_train <- d_score[-fld[[i]], -4]
    y_train <- d_score[-fld[[i]], 4]
    x_test <- d_score[fld[[i]], -4]
    y_test <- d_score[fld[[i]], 4]
    
    # training with train data
    model <- svm(x_train, y_train, type = "nu-regression")
    
    summary(model)
    
    # test with test data
    pred <- predict(model, x_test)
    pred <- round(pred, 0)
    
    ## compare with real answer
    compare <- cbind(pred, y_test, abs(pred-y_test))
    
    ## calculate RMSE
    lm_loss_avg <- sqrt( sum( ( pred-y_test )^2 )  / nrow(compare) )
    temp_loss[i] <- lm_loss_avg
    
}
temp_loss
mean(temp_loss)

#####################################################################################################

# only vir data

library(e1071)
library(caret)

## remove all history
rm(list=ls())

## read csv file
d_score <- read.csv("cluster_new.csv")

head(d_score)

temp_loss <- c() # loss for each fold
final_loss <- c() # mean of temp_loss for each fold

for( i in 1:10 ) {
  
  ## create folds , 10-fold Cross Validation
  fld <- createFolds(d_score$result, k=10)
  
  temp_loss <- c()
  
  for( j in 1:10 ) {
    
    x_train <- d_score[-fld[[j]], -4]
    y_train <- d_score[-fld[[j]], 4]
    x_test <- d_score[fld[[j]], -4]
    y_test <- d_score[fld[[j]], 4]
    
    # training with train data
    model <- svm(x_train, y_train, type = "nu-regression")
    
    summary(model)
    
    # test with test data
    pred <- predict(model, x_test)
    pred <- round(pred, 0)
    
    ## compare with real answer
    compare <- cbind(pred, y_test, abs(pred-y_test))
    
    ## calculate RMSE
    RMSE <- sqrt( sum( ( pred-y_test )^2 )  / nrow(compare) )
    temp_loss[j] <- RMSE
    
  }
  final_loss[i] <- mean(temp_loss)
}

final_loss
mean(final_loss)

#####################################################################################################