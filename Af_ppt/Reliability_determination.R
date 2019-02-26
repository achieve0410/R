#################################################################################################

## Determinate virtual data's reliability

rm(list=ls())
library(keras)
library(MASS)

use_session_with_seed(1, disable_parallel_cpu = FALSE)

## read csv file for model
m_score <- read.csv("driving_score_180ea.csv")

## read csv file for data
d_score <- read.csv("tmp_data.csv")

dim(m_score)
dim(d_score)

x_train <- as.matrix(m_score[, -4])
y_train <- as.matrix(m_score[, 4])
x_test <- as.matrix(d_score[, -4])
y_test <- as.matrix(d_score[, 4])

# create model
model = keras_model_sequential()
model %>%
  layer_dense(input_shape = ncol(x_train), units = 128, activation = "relu") %>%
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
    loss = "mse",
    optimizer = "adam",
    metrics = list("mean_absolute_error")
  )

fit = model %>%
  fit(
    x = x_train,
    y = y_train,
    batch_size = 128,
    epochs = 300
  )

# Training and evaluation
model %>% evaluate(x_test, y_test, verbose = 1)

# print predicted value
pred = model %>% predict(x_test)
result <- cbind(x_test[,], pred, y_test, pred-y_test)
colnames(result) <- c("compl","accel","decel","pred", "answer", "loss")
result

## calculate RMSE
RMSE <- sqrt( sum( ( pred-y_test )^2 )  / nrow(result) )
RMSE

write.csv(result, "virtual_data.csv")

#################################################################################################