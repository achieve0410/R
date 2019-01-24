## RF

#####################################################################################################

## change data set - vir&Tmap

library(randomForest)

## remove all history
rm(list=ls())

## read csv file for model
m_score <- read.csv("driving_score_180ea.csv")
m_score <- m_score[, c(2,3,4)]

## read csv file for data
d_score <- read.csv("Tmap_data_4col.csv")
d_score <- d_score[, c(2,3,4)]

ds.train <- m_score[,]
ds.test <- d_score[,]

d_score.rf <- randomForest(result ~ ., data=ds.train, ntree = 20000)

d_score.pred <- predict(d_score.rf, ds.test)
d_score.pred <- round(d_score.pred, 0)

## compare with real answer
compare <- cbind(d_score.pred, ds.test[, "result"], abs(d_score.pred-ds.test[,"result"]))
colnames(compare) <- c("pred", "answer", "loss")
compare

## calculate RMSE
RMSE <- sqrt( sum( ( d_score.pred-ds.test[, "result"] )^2 )  / nrow(compare) )
RMSE

# show table
table(predicted = d_score.pred, observed = ds.test[ , "result"])

#####################################################################################################

## change data set - vir only

library(randomForest)

## remove all history
rm(list=ls())

## read csv file
d_score <- read.csv("driving_score_180ea.csv")
d_score <- d_score[, c(2,3,4)]

## prepare data
set.seed(12354)
idx <- c( sample(1:nrow(d_score), nrow(d_score)*.8) ) # Train : Test = 8 : 2

ds.train <- d_score[idx,]
ds.test <- d_score[-idx,]

d_score.rf <- randomForest(result ~ ., data=ds.train, ntree = 20000)

d_score.pred <- predict(d_score.rf, ds.test)
d_score.pred <- round(d_score.pred, 0)

## compare with real answer
compare <- cbind(d_score.pred, ds.test[, "result"], abs(d_score.pred-ds.test[,"result"]))
colnames(compare) <- c("pred", "answer", "loss")
compare

## calculate RMSE
RMSE <- sqrt( sum( ( d_score.pred-ds.test[, "result"] )^2 )  / nrow(compare) )
RMSE

# show table
table(predicted = d_score.pred, observed = ds.test[ , "result"])

#####################################################################################################

# only vir data

## remove all history
rm(list=ls())

library(randomForest)
library(caret)

## read csv file
d_score <- read.csv("driving_score_180ea.csv")
d_score <- d_score[, c(2,3,4)]

## create folds , 10-fold Cross Validation
fld <- createFolds(d_score$result, k=10)

temp_loss <- c() # temp loss for each fold

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

temp_loss
mean(temp_loss)

#####################################################################################################
