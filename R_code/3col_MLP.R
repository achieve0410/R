## MLP

#######################################################################################

## using virtual data - vir&Tmap

rm(list=ls())
library(keras)
library(MASS)

use_session_with_seed(1, disable_parallel_cpu = FALSE)

## read csv file for model
m_score <- read.csv("driving_score_180ea.csv")
m_score <- m_score[, c(2,3,4)]

## read csv file for data
d_score <- read.csv("Tmap_data_4col.csv")
d_score <- d_score[, c(2,3,4)]

x_train <- as.matrix(m_score[, -3])
y_train <- as.matrix(m_score[, 3])
x_test <- as.matrix(d_score[, -3])
y_test <- as.matrix(d_score[, 3])

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

#######################################################################################

# only Original data

rm(list=ls())
library(keras)
library(MASS)
library(caret)

use_session_with_seed(1, disable_parallel_cpu = FALSE)

## read csv file for model
d_score <- read.csv("driving_score_180ea.csv")
d_score <- d_score[, c(2,3,4)]

## create folds , 10-fold Cross Validation
fld <- createFolds(d_score$result, k=10)

temp_loss <- c() # loss for each fold

for(i in 1:10) {
  x_train <- as.matrix(d_score[-fld[[i]], -3])
  y_train <- as.matrix(d_score[-fld[[i]], 3])
  x_test <- as.matrix(d_score[fld[[i]], -3])
  y_test <- as.matrix(d_score[fld[[i]], 3])
  
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
  
  compare <- cbind(y_test, pred, pred-y_test)
  
  ## calculate RMSE
  lm_loss_avg <- sqrt( sum( ( pred-y_test )^2 )  / nrow(compare) )
  temp_loss[i] <- lm_loss_avg
}
temp_loss
mean(temp_loss)

#######################################################################################
