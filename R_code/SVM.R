
#####################################################################################################

# no divide data set

library(e1071)

## remove all history
rm(list=ls())

## read csv file
d_score <- read.csv("driving_score.csv")

# training with train data
model <- svm(d_score[ , -4], d_score[ , 4])

summary(model)

# test with train data
pred <- predict(model, d_score[ , -4])
pred <- round(pred, 0)

## compare with real answer
compare <- cbind(pred, d_score[, 4], abs(pred-d_score[ , 4]))
colnames(compare) <- c("pred", "answer", "loss")
compare

## calculate mean absolute loss
lm_loss_avg <- ( sum( abs( pred-d_score[ , 4] ) ) ) / nrow(compare)
lm_loss_avg

## 50 : 7.84 , 180 : 6.693 ##

# show table
table(pred, d_score[, 4])

#####################################################################################################

# divide data set

library(e1071)

## remove all history
rm(list=ls())

## read csv file
d_score <- read.csv("driving_score.csv")

## prepare data
set.seed(12354)
idx <- c( sample(1:nrow(d_score), nrow(d_score)*.8) ) # Train : Test = 8 : 2

x_train <- d_score[idx, -4]
y_train <- d_score[idx, 4]
x_test <- d_score[-idx, -4]
y_test <- d_score[-idx, 4]

# training with train data
model <- svm(x_train, y_train)

summary(model)

# test with test data
pred <- predict(model, x_test)
pred <- round(pred, 0)

## compare with real answer
compare <- cbind(pred, y_test, abs(pred-y_test))
colnames(compare) <- c("pred", "answer", "loss")
compare

## calculate mean absolute loss
lm_loss_avg <- ( sum( abs( pred-y_test ) ) ) / nrow(compare)
lm_loss_avg

## 50 : 16.2 , 180 : 8.702 ##

# show table
table(pred, y_test)

#####################################################################################################

# no divide data set - change data set

library(e1071)

## remove all history
rm(list=ls())

## read csv file
d_score <- read.csv("Tmap_data.csv")

# training with train data
model <- svm(d_score[ , -9], d_score[ , 9])

summary(model)

# test with train data
pred <- predict(model, d_score[ , -9])
pred <- round(pred, 0)

## compare with real answer
compare <- cbind(pred, d_score[, 9], abs(pred-d_score[ , 9]))
colnames(compare) <- c("pred", "answer", "loss")
compare

## calculate mean absolute loss
lm_loss_avg <- ( sum( abs( pred-d_score[ , 9] ) ) ) / nrow(compare)
lm_loss_avg

## 4.866 ##

# show table
table(pred, d_score[, 9])

#####################################################################################################

# divide data set - change data set

library(e1071)

## remove all history
rm(list=ls())

## read csv file
d_score <- read.csv("Tmap_data.csv")

## prepare data
set.seed(12354)
idx <- c( sample(1:nrow(d_score), nrow(d_score)*.8) ) # Train : Test = 8 : 2

x_train <- d_score[idx, -9]
y_train <- d_score[idx, 9]
x_test <- d_score[-idx, -9]
y_test <- d_score[-idx, 9]

# training with train data
model <- svm(x_train, y_train)

summary(model)

# test with test data
pred <- predict(model, x_test)
pred <- round(pred, 0)

## compare with real answer
compare <- cbind(pred, y_test, abs(pred-y_test))
colnames(compare) <- c("pred", "answer", "loss")
compare

## calculate mean absolute loss
lm_loss_avg <- ( sum( abs( pred-y_test ) ) ) / nrow(compare)
lm_loss_avg

## 10.333 ##

# show table
table(pred, y_test)

#####################################################################################################

# mix data set & model

library(e1071)

## remove all history
rm(list=ls())

## read csv file for model
m_score <- read.csv("driving_score.csv")

## read csv file for data
d_score <- read.csv("Tmap_data2.csv")

x_train <- m_score[, -4]
y_train <- m_score[, 4]
x_test <- d_score[, -4]
y_test <- d_score[, 4]

# training with train data
model <- svm(x_train, y_train)

summary(model)

# test with test data
pred <- predict(model, x_test)
pred <- round(pred, 0)

## compare with real answer
compare <- cbind(pred, y_test, abs(pred-y_test))
colnames(compare) <- c("pred", "answer", "loss")
compare

## calculate mean absolute loss
lm_loss_avg <- ( sum( abs( pred-y_test ) ) ) / nrow(compare)
lm_loss_avg

# show table
table(pred, y_test)

#####################################################################################################



















# using virtual data - vir&Tmap

library(e1071)

## remove all history
rm(list=ls())

## read csv file for model
m_score <- read.csv("driving_score_180ea.csv")

## read csv file for data
d_score <- read.csv("Tmap_data_4col.csv")

x_train <- m_score[, -4]
y_train <- m_score[, 4]
x_test <- d_score[, -4]
y_test <- d_score[, 4]

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

## prepare data
set.seed(12354)
idx <- c( sample(1:nrow(d_score), nrow(d_score)*.8) ) # Train : Test = 8 : 2

x_train <- d_score[idx, -4]
y_train <- d_score[idx, 4]
x_test <- d_score[-idx, -4]
y_test <- d_score[-idx, 4]

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

# using virtual data - vir&Original

library(e1071)

## remove all history
rm(list=ls())

## read csv file for model
m_score <- read.csv("driving_score_180ea.csv")

## read csv file for data
d_score <- read.csv("driving_score_virtual.csv")

x_train <- m_score[, -4]
y_train <- m_score[, 4]
x_test <- d_score[, -4]
y_test <- d_score[, 4]

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
