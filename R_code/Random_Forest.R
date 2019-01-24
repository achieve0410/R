
#####################################################################################################

library(randomForest)

## remove all history
rm(list=ls())

## read csv file
d_score <- read.csv("driving_score.csv")

## prepare data
set.seed(12354)
idx <- c( sample(1:nrow(d_score), nrow(d_score)*.8) ) # Train : Test = 8 : 2

ds.train <- d_score[idx, ]
ds.test <- d_score[-idx, ]

d_score.rf <- randomForest(result ~ ., data=ds.train, ntree = 1000)

d_score.pred <- predict(d_score.rf, ds.test)
d_score.pred <- round(d_score.pred, 0)

## compare with real answer
compare <- cbind(d_score.pred, ds.test[, "result"], abs(d_score.pred-ds.test[,"result"]))
colnames(compare) <- c("pred", "answer", "loss")
compare

## calculate mean absolute loss
lm_loss_avg <- ( sum( abs( d_score.pred-ds.test[, "result"] ) ) ) / nrow(compare)
lm_loss_avg

# show table
table(predicted = d_score.pred, observed = ds.test[ , "result"])

#####################################################################################################

## change data set - mix data set & model

library(randomForest)

## remove all history
rm(list=ls())

## read csv file for model
m_score <- read.csv("driving_score.csv")

## read csv file for data
d_score <- read.csv("Tmap_data2.csv")

ds.train <- m_score[,]
ds.test <- d_score[,]

d_score.rf <- randomForest(result ~ ., data=ds.train, ntree = 1000)

d_score.pred <- predict(d_score.rf, ds.test)
d_score.pred <- round(d_score.pred, 0)

## compare with real answer
compare <- cbind(d_score.pred, ds.test[, "result"], abs(d_score.pred-ds.test[,"result"]))
colnames(compare) <- c("pred", "answer", "loss")
compare

## calculate mean absolute loss
lm_loss_avg <- ( sum( abs( d_score.pred-ds.test[, "result"] ) ) ) / nrow(compare)
lm_loss_avg

# show table
table(predicted = d_score.pred, observed = ds.test[ , "result"])

#####################################################################################################









## change data set - vir&Tmap

library(randomForest)

## remove all history
rm(list=ls())

## read csv file for model
m_score <- read.csv("driving_score_180ea.csv")

## read csv file for data
d_score <- read.csv("Tmap_data_4col.csv")

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

## change data set - vir&Original

library(randomForest)

## remove all history
rm(list=ls())

## read csv file for model
m_score <- read.csv("driving_score_180ea.csv")

## read csv file for data
d_score <- read.csv("driving_score_virtual.csv")

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
