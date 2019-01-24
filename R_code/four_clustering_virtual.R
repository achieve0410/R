
##############################################################################################################################

## model = origin // data = tmap & virtual // goal = data's result prediction

## DSI

rm(list=ls())

virtual <- read.csv("cluster_virtual.csv")                         # 500,  8

#unique(tmap[, c(5:8)])
#unique(virtual[, c(5:8)])

## regression
mod1 <- lm(result ~., data = virtual)

## show model's summary
summary(mod1)

## setting data
answer <- virtual[, "result"]
compl <- virtual[, "compliance"]
accel <- virtual[, "acceleration"]
decel <- virtual[, "deceleration"]
clust3 <- virtual[, "cluster3"]
clust2 <- virtual[, "cluster_cd"]
clust1 <- virtual[, "cluster_ca"]
clust0 <- virtual[, "cluster_ad"]

## create model
prediction <- compl * coef(mod1)[2] + accel * coef(mod1)[3] + decel * coef(mod1)[4] + clust3 * coef(mod1)[5] +
  clust2 * coef(mod1)[6] + clust1 * coef(mod1)[7] + clust0 * coef(mod1)[8] + coef(mod1)[1]
prediction <- round(prediction, 0)

## compare with real answer
compare <- cbind(prediction, answer, abs(prediction-answer))
compare <- cbind(compl, accel, decel, clust3, clust2, clust1, clust0, compare)
colnames(compare) <- c("compl", "accel", "decel", "clust3", "clust2", "clust1", "clust0", "pred", "answer", "loss")
compare

## calculate RMSE
RMSE <- sqrt( sum((prediction-answer)^2)/nrow(compare) )
RMSE

##############################################################################################################################

## model = origin // data = tmap & virtual // goal = data's result prediction

## RF

library(randomForest)
library(caret)

## remove all history
rm(list=ls())

## read csv file
d_score <- read.csv("cluster_virtual.csv")

## create folds , 10-fold Cross Validation
fld <- createFolds(d_score$result, k=10)

temp_loss <- c() # temp loss for each fold
final_loss <- c() # final loss

for( i in 1:1 ) {
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


##############################################################################################################################

## model = origin // data = tmap & virtual // goal = data's result prediction

## SVM

library(e1071)
library(caret)

## remove all history
rm(list=ls())

## read csv file
d_score <- read.csv("cluster_virtual.csv")

temp_loss <- c() # loss for each fold
final_loss <- c()

for( i in 1:100 ) {
  
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
    lm_loss_avg <- sqrt( sum( ( pred-y_test )^2 )  / nrow(compare) )
    temp_loss[j] <- lm_loss_avg
    
  }
  
  final_loss[i] <- mean(temp_loss)
}
final_loss
mean(final_loss)



##############################################################################################################################

## model = origin // data = tmap & virtual // goal = data's result prediction

## MLP

rm(list=ls())

origin <- read.csv("cluster_origin.csv")                         # 182,  8












