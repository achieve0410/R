

## model = origin // data = tmap & virtual // goal = data's result prediction

## SVM

rm(list=ls())

library(e1071)

origin <- read.csv("cluster_origin.csv")                         # 182,  8
tmap <- read.csv("cluster_tmap.csv")                             # 15,   8
virtual <- read.csv("cluster_virtual.csv")                       # 1248, 8
new <- read.csv("cluster_new.csv")

x_train <- origin[, -4]
y_train <- origin[, 4]
x_test <- new[, -4]
y_test <- new[, 4]

# training with train data
model <- svm(x_train, y_train, type = "nu-regression")
summary(model)

# test with test data
pred <- predict(model, x_test)
pred <- round(pred, 0)

## compare with real answer
compare <- cbind(pred, y_test, abs(pred-y_test))s
colnames(compare) <- c("pred", "answer", "loss")
compare

## calculate RMSE
RMSE <- sqrt( sum( ( pred-y_test )^2 )  / nrow(compare) )
RMSE


## Is this right? ##
## I think it'r right ##