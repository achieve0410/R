## DS

#####################################################################################################

## using virtual data - vir&Tmap

## remove all history
rm(list=ls())

## read csv file for model
m_score <- read.csv("driving_score_180ea.csv")
m_score <- m_score[, c(2,3,4)]

## read csv file for data
d_score <- read.csv("Tmap_data_4col.csv")
d_score <- d_score[ , c(2,3,4)]

## regression
mod1 <- lm(result ~., data = m_score)

## show model's summary
summary(mod1)

## setting data
answer <- d_score[, "result"]
#compl <- d_score[, "compliance"]
accel <- d_score[, "acceleration"]
decel <- d_score[, "deceleration"]

## create model
#prediction <- compl * coef(mod1)[2] + accel * coef(mod1)[3] + decel * coef(mod1)[4] + coef(mod1)[1]
prediction <- accel * coef(mod1)[2] + decel * coef(mod1)[3] + coef(mod1)[1]
prediction <- round(prediction, 0)

## compare with real answer
compare <- cbind(prediction, answer, abs(prediction-answer))
colnames(compare) <- c("pred", "answer", "loss")
compare

## calculate RMSE
RMSE <- sqrt( sum( (prediction-answer)^2 ) / nrow(compare) )
RMSE

#####################################################################################################

## using virtual data - vir only

## remove all history
rm(list=ls())

## read csv file
d_score <- read.csv("driving_score_180ea.csv")
d_score <- d_score[, c(2,3,4)]

## regression
mod1 <- lm(result ~., data = d_score)

## show model's summary
summary(mod1)

## setting data
answer <- d_score[, "result"]
#compl <- d_score[, "compliance"]
accel <- d_score[, "acceleration"]
decel <- d_score[, "deceleration"]

## create model
#prediction <- compl * coef(mod1)[2] + accel * coef(mod1)[3] + decel * coef(mod1)[4] + coef(mod1)[1]
prediction <- accel * coef(mod1)[2] + decel * coef(mod1)[3] + coef(mod1)[1]
prediction <- round(prediction, 0)

## compare with real answer
compare <- cbind(prediction, answer, abs(prediction-answer))
colnames(compare) <- c("pred", "answer", "loss")
compare

## calculate RMSE
RMSE <- sqrt( sum( (prediction-answer)^2 ) / nrow(compare) )
RMSE

#####################################################################################################

