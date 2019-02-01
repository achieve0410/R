
#######################################################################################
?install_keras
rm(list=ls())
library(keras)
library(MASS)

use_session_with_seed(1, disable_parallel_cpu = FALSE)

## read csv file
d_score <- read.csv("driving_score.csv")

## prepare data
set.seed(12354)
idx <- c( sample(1:nrow(d_score), nrow(d_score)*.8) ) # Train : Test = 8 : 2

x_train <- d_score[idx, -4]
y_train <- d_score[idx, 4]
x_test <- d_score[-idx, -4]
y_test <- d_score[-idx, 4]

# scale to [0,1]
x_train <- as.matrix(apply(x_train, 2,
                           function(x) (x-min(x))/(max(x) - min(x))))
x_test <- as.matrix(apply(x_test, 2,
                          function(x) (x-min(x))/(max(x) - min(x))))

# create model
model = keras_model_sequential()
model %>%
  layer_dense(input_shape = ncol(x_train), units = 64, activation = "relu") %>%
  layer_dense(units = 64, activation = "relu") %>%
  layer_dense(units = 1)

summary(model)

# add a loss function and optimizer
model %>%
  compile(
    loss = "mse",
    optimizer = "adam",
    metrics = list("mean_absolute_error")
  )

fit = model %>%
  fit(
    x = x_train,
    y = y_train,
    epochs = 2000
  )

# Training and evaluation
model %>% evaluate(x_test, y_test,verbose = 0)

# print predicted value
pred = model %>% predict(x_test)
result <- cbind(y_test, pred, pred-y_test)
colnames(result) <- c("Answer", "pred", "loss")
result

#######################################################################################

## change data set

rm(list=ls())
library(keras)
library(MASS)

use_session_with_seed(1, disable_parallel_cpu = FALSE)

## read csv file
d_score <- read.csv("Tmap_data.csv")

## prepare data
set.seed(12354)
idx <- c( sample(1:nrow(d_score), nrow(d_score)*.8) ) # Train : Test = 8 : 2

x_train <- as.matrix(d_score[idx, -9])
y_train <- as.matrix(d_score[idx, 9])
x_test <- as.matrix(d_score[-idx, -9])
y_test <- as.matrix(d_score[-idx, 9])

# create model
model = keras_model_sequential()
model %>%
  layer_dense(input_shape = ncol(x_train), units = 64, activation = "relu") %>%
  layer_dense(units = 64, activation = "relu") %>%
  layer_dense(units = 1)

summary(model)

# add a loss function and optimizer
model %>%
  compile(
    loss = "mse",
    optimizer = "adam",
    metrics = list("mean_absolute_error")
  )

fit = model %>%
  fit(
    x = x_train,
    y = y_train,
    epochs = 10000
  )

# Training and evaluation
model %>% evaluate(x_test, y_test, verbose = 0)

# print predicted value
pred = model %>% predict(x_test)
result <- cbind(y_test, pred, pred-y_test)
colnames(result) <- c("Answer", "pred", "loss")
result
#######################################################################################

## mix data set & model

rm(list=ls())
library(keras)
library(MASS)

use_session_with_seed(1, disable_parallel_cpu = FALSE)

## read csv file for model
m_score <- read.csv("driving_score.csv")

## read csv file for data
d_score <- read.csv("Tmap_data2.csv")

x_train <- as.matrix(m_score[, -4])
y_train <- as.matrix(m_score[, 4])
x_test <- as.matrix(d_score[, -4])
y_test <- as.matrix(d_score[, 4])

# create model
model = keras_model_sequential()
model %>%
  layer_dense(input_shape = ncol(x_train), units = 64, activation = "relu") %>%
  layer_dense(units = 64, activation = "relu") %>%
  layer_dense(units = 1)

summary(model)

# add a loss function and optimizer
model %>%
  compile(
    loss = "mse",
    optimizer = "adam",
    metrics = list("mean_absolute_error")
  )

fit = model %>%
  fit(
    x = x_train,
    y = y_train,
    epochs = 2000
  )

# Training and evaluation
model %>% evaluate(x_test, y_test, verbose = 0)

# print predicted value
pred = model %>% predict(x_test)
result <- cbind(y_test, pred, pred-y_test)
colnames(result) <- c("Answer", "pred", "loss")
result

#######################################################################################

## using virtual data set

rm(list=ls())
library(keras)
library(MASS)

use_session_with_seed(1, disable_parallel_cpu = FALSE)

## read csv file for model
m_score <- read.csv("epoch5000_filter5_plus_origin.csv")

## read csv file for data
d_score <- read.csv("Virtual_data.csv")

x_train <- as.matrix(m_score[, -4])
y_train <- as.matrix(m_score[, 4])
x_test <- as.matrix(d_score[, -4])
y_test <- as.matrix(d_score[, 4])

# create model
model = keras_model_sequential()
model %>%
  layer_dense(input_shape = ncol(x_train), units = 128, activation = "relu") %>%
  layer_dense(units = 64, activation = "relu") %>%
  layer_dense(units = 1)

summary(model)

# add a loss function and optimizer
model %>%
  compile(
    loss = "mse",
    optimizer = "adam",
    metrics = list("mean_absolute_error")
  )

fit = model %>%
  fit(
    x = x_train,
    y = y_train,
    batch_size = 128,
    epochs = 5000
  )

# Training and evaluation
model %>% evaluate(x_test, y_test, verbose = 0)

# print predicted value
pred = model %>% predict(x_test)
result <- cbind(y_test, pred, pred-y_test)
result
result <- cbind(x_test, result)
dim(x_test)

colnames(result) <- c("compliance","acceleration","deceleration", "Answer", "pred", "loss")
result

write.csv(result, file="epoch5000_filter5_plus_origin_result.csv", row.names = FALSE)

#######################################################################################

















## using virtual data - vir&Tmap

rm(list=ls())
library(keras)
library(MASS)

use_session_with_seed(1, disable_parallel_cpu = FALSE)

## read csv file for model
m_score <- read.csv("driving_score_180ea.csv")

## read csv file for data
d_score <- read.csv("Tmap_data_4col.csv")

x_train <- as.matrix(m_score[, -4])
y_train <- as.matrix(m_score[, 4])
x_test <- as.matrix(d_score[, -4])
y_test <- as.matrix(d_score[, 4])

# create model
model = keras_model_sequential()
model %>%
  layer_dense(input_shape = ncol(x_train), units = 128, activation = "relu") %>%
  layer_dropout(rate = 0.05) %>%
  layer_dense(units = 128, activation = "relu") %>%
  layer_dropout(rate = 0.05) %>%
  layer_dense(units = 64, activation = "relu") %>%
  layer_dropout(rate = 0.05) %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dropout(rate = 0.05) %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 1)

summary(model)

# add a loss function and optimizer
model %>%
  compile(
    loss = "logcosh",
    optimizer = "Nadam",
    metrics = list("mean_absolute_error")
  )

fit = model %>%
  fit(
    x = x_train,
    y = y_train,
    batch_size = 128,
    epochs = 5000
  )

# Training and evaluation
model %>% evaluate(x_test, y_test, verbose = 0)

# print predicted value
pred = model %>% predict(x_test)
result <- cbind(y_test, pred, pred-y_test)
colnames(result) <- c("result", "pred", "loss")
result

## calculate RMSE
RMSE <- sqrt( sum( ( pred-y_test )^2 )  / nrow(result) )
RMSE

#######################################################################################

## using virtual data - vir&original

rm(list=ls())
library(keras)
library(MASS)

use_session_with_seed(1, disable_parallel_cpu = FALSE)

## read csv file for model
m_score <- read.csv("cluster_origin_10.csv")

## read csv file for data
d_score <- read.csv("cluster_tmap.csv")

x_train <- as.matrix(m_score[, -4])
y_train <- as.matrix(m_score[, 4])
x_test <- as.matrix(d_score[, -4])
y_test <- as.matrix(d_score[, 4])

# create model
model = keras_model_sequential()
model %>%
  layer_dense(input_shape = ncol(x_train), units = 128, activation = "relu") %>%
  layer_dropout(rate = 0.1) %>%
  layer_dense(units = 128, activation = "relu") %>%
  layer_dropout(rate = 0.1) %>%
  layer_dense(units = 64, activation = "relu") %>%
  layer_dropout(rate = 0.1) %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dropout(rate = 0.1) %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 1)

summary(model)

# add a loss function and optimizer
model %>%
  compile(
    loss = "logcosh",
    optimizer = "Nadam",
    metrics = list("mean_absolute_error")
  )

fit = model %>%
  fit(
    x = x_train,
    y = y_train,
    batch_size = 256,
    epochs = 3000
  )

# Training and evaluation
model %>% evaluate(x_test, y_test, verbose = 1)

# print predicted value
pred = model %>% predict(x_test)
result <- cbind(x_test[,], pred, y_test, pred-y_test)
colnames(result) <- c("compl","accel","decel","clust_cr","clust_ar","clust_dr","clust_car","clust_cdr","clust_adr","pred", "answer", "loss")
result

## calculate RMSE
RMSE <- sqrt( sum( ( pred-y_test )^2 )  / nrow(result) )
RMSE

#######################################################################################


## using virtual data - vir only

rm(list=ls())
library(keras)
library(MASS)
library(caret)

use_session_with_seed(1, disable_parallel_cpu = FALSE)

## read csv file
d_score <- read.csv("driving_score_180ea.csv")

## prepare data
set.seed(12354)
idx <- c( sample(1:nrow(d_score), nrow(d_score)*.8) ) # Train : Test = 8 : 2

x_train <- as.matrix(d_score[idx, -4])
y_train <- as.matrix(d_score[idx, 4])
x_test <- as.matrix(d_score[-idx, -4])
y_test <- as.matrix(d_score[-idx, 4])

# create model
model = keras_model_sequential()
model %>%
  layer_dense(input_shape = ncol(x_train), units = 128, activation = "relu") %>%
  layer_dropout(rate = 0.05) %>%
  layer_dense(units = 128, activation = "relu") %>%
  layer_dropout(rate = 0.05) %>%
  layer_dense(units = 64, activation = "relu") %>%
  layer_dropout(rate = 0.05) %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dropout(rate = 0.05) %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 1)

summary(model)

# add a loss function and optimizer
model %>%
  compile(
    loss = "logcosh",
    optimizer = "Nadam",
    metrics = list("mean_absolute_error")
  )

fit = model %>%
  fit(
    x = x_train,
    y = y_train,
    batch_size = 128,
    epochs = 5000
  )

# Training and evaluation
model %>% evaluate(x_test, y_test, verbose = 0)

# print predicted value
pred = model %>% predict(x_test)
result <- cbind(y_test, pred, pred-y_test)
colnames(result) <- c("result", "pred", "loss")
result

## calculate RMSE
RMSE <- sqrt( sum( ( pred-y_test )^2 )  / nrow(result) )
RMSE

#######################################################################################
