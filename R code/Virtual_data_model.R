
## using virtual data set

rm(list=ls())
library(keras)
library(MASS)

use_session_with_seed(1, disable_parallel_cpu = FALSE)

## original data
m_score <- read.csv("driving_score.csv")

## virtual data
d_score <- read.csv("epoch5000_filter5_data.csv")
d_score <- d_score[, c(1:4)]

## making origin+virtual data set
d_score <- rbind(m_score, d_score)

write.csv(d_score, "epoch5000_filter5_plus_origin.csv", row.names = FALSE)

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


########################################################################################################


## using virtual data set

rm(list=ls())
library(keras)
library(MASS)

use_session_with_seed(1, disable_parallel_cpu = FALSE)

## read csv file for model
m_score <- read.csv("driving_score.csv")

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

result <- cbind(x_test, result)
colnames(result) <- c("compliance","acceleration","deceleration", "result", "pred", "loss")
result

write.csv(result, file="epoch5000_multiLayer_result.csv", row.names = FALSE)

#######################################################################################
