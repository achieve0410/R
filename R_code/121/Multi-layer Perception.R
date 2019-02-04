
#######################################################################################

## using virtual data - vir&original

rm(list=ls())
library(keras)
library(MASS)

use_session_with_seed(1, disable_parallel_cpu = FALSE)

## read csv file for model
m_score <- read.csv("origin_data_121.csv")

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
    optimizer = "adam",
    metrics = list("mean_absolute_error")
  )

fit = model %>%
  fit(
    x = x_train,
    y = y_train,
    batch_size = 64,
    epochs = 3000
  )

# Training and evaluation
model %>% evaluate(x_test, y_test, verbose = 1)

# print predicted value
pred = model %>% predict(x_test)
result <- cbind(x_test[,], pred, y_test, pred-y_test)
colnames(result) <- c("compl","accel","decel","pred", "answer", "loss")
result

write.csv(result, "121_mlpdata.csv")

## calculate RMSE
RMSE <- sqrt( sum( ( pred-y_test )^2 )  / nrow(result) )
RMSE

#######################################################################################
