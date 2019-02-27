
##############################################################################################################################

## SVM

rm(list=ls())

library(e1071)

origin <- read.csv("cluster_fil11.csv")                         # 120,   8
new <- read.csv("cluster_new_fil11.csv")                        # 12,    8

x_train <- origin[, -8]
y_train <- origin[, 8]
x_test <- new[, -8]
y_test <- new[, 8]

# training with train data
model <- svm(x_train, y_train, type = "nu-regression")
summary(model)

# test with test data
pred <- predict(model, x_test)
pred <- round(pred, 1)

## compare with real answer
compare <- cbind(x_test[,c(1:7)], pred, y_test, abs(pred-y_test))
colnames(compare) <- c("compl", "accel", "decel", "clust_ca", "clust_cd", "clust_ad", 
                       "clust_cad", "pred", "answer", "loss")
compare

## calculate RMSE
RMSE <- sqrt( sum( ( pred-y_test )^2 )  / nrow(compare) )
RMSE

##############################################################################################################################
