## SVM

#####################################################################################################

# using virtual data - vir&Tmap

library(e1071)

## remove all history
rm(list=ls())

## read csv file for model
m_score <- read.csv("driving_score_180ea.csv")
m_score <- m_score[, c(2,3,4)]

## read csv file for data
d_score <- read.csv("Tmap_data_4col.csv")
d_score <- d_score[, c(2,3,4)]

x_train <- m_score[, -3]
y_train <- m_score[, 3]
x_test <- d_score[, -3]
y_test <- d_score[, 3]

# training with train data
model <- svm(x_train, y_train, type = "nu-regression")
summary(model)

# test with test data
pred <- predict(model, x_test)
pred <- round(pred, 0)

## compare with real answer
compare <- cbind(pred, y_test, abs(pred-y_test))
colnames(compare) <- c("pred", "answer", "loss")
compare

## calculate RMSE
RMSE <- sqrt( sum( ( pred-y_test )^2 )  / nrow(compare) )
RMSE

# show table
table(pred, y_test)

#####################################################################################################

# using virtual data - vir only

library(e1071)

## remove all history
rm(list=ls())

## read csv file for model
d_score <- read.csv("driving_score_180ea.csv")
d_score <- d_score[, c(1,2,4)]

## prepare data
set.seed(12354)
idx <- c( sample(1:nrow(d_score), nrow(d_score)*.8) ) # Train : Test = 8 : 2

x_train <- d_score[idx, -3]
y_train <- d_score[idx, 3]
x_test <- d_score[-idx, -3]
y_test <- d_score[-idx, 3]

# training with train data
model <- svm(x_train, y_train, type = "nu-regression")

summary(model)

# test with test data
pred <- predict(model, x_test)
pred <- round(pred, 0)

## compare with real answer
compare <- cbind(pred, y_test, abs(pred-y_test))
colnames(compare) <- c("pred", "answer", "loss")
compare

## calculate RMSE
RMSE <- sqrt( sum( ( pred-y_test )^2 )  / nrow(compare) )
RMSE

# show table
table(pred, y_test)

#####################################################################################################

# only Original data

library(e1071)
library(caret)

## remove all history
rm(list=ls())

## read csv file for model
d_score <- read.csv("driving_score_180ea.csv")
d_score <- d_score[, c(2,3,4)]

temp_loss <- c() # loss for each fold
  
## create folds , 10-fold Cross Validation
fld <- createFolds(d_score$result, k=10)
  
temp_loss <- c()
  
for( j in 1:10 ) {
    
  x_train <- d_score[-fld[[j]], -3]
  y_train <- d_score[-fld[[j]], 3]
  x_test <- d_score[fld[[j]], -3]
  y_test <- d_score[fld[[j]], 3]
    
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

temp_loss
mean(temp_loss)

#####################################################################################################
