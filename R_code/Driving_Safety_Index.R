
#####################################################################################################

## no optimizing variables ; compliance, acceleration, deceleration

## remove all history
rm(list=ls())

## read csv file
d_score <- read.csv("driving_score_180ea.csv")

## regression
mod1 <- lm(result ~., data = d_score)

## show model's summary
summary(mod1)

## setting data
answer <- d_score[, "result"]
compl <- d_score[, "compliance"]
accel <- d_score[, "acceleration"]
decel <- d_score[, "deceleration"]

## create model
prediction <- compl * coef(mod1)[2] + accel * coef(mod1)[3] + decel * coef(mod1)[4] + coef(mod1)[1]
prediction <- round(prediction, 0)

## compare with real answer
compare <- cbind(prediction, answer, abs(prediction-answer))
colnames(compare) <- c("pred", "answer", "loss")
compare

## calculate RMSE
RMSE <- sqrt( sum( (prediction-answer)^2 ) / nrow(compare) )
RMSE

## 50: 10.62 , 180 : 9.532 ##

#####################################################################################################

## optimizing variables ; compliance, deceleration

library(MASS)

## remove all history
rm(list=ls())

## read csv file
d_score <- read.csv("driving_score.csv")

## regression
mod1 <- lm(result ~., data = d_score)

## show model's summary
summary(mod1)

## checking the best parameters
step <- stepAIC(mod1, direction = "both")

## create final model
mod2 <- lm(result ~ compliance + deceleration, data = d_score)

## show model's summary
summary(mod2)

## setting data
answer <- d_score[, "result"]
compl <- d_score[, "compliance"]
decel <- d_score[, "deceleration"]

## create model
prediction <- compl * coef(mod2)[2] + decel * coef(mod2)[3] + coef(mod2)[1]
prediction <- round(prediction, 0)


## compare with real answer
compare <- cbind(prediction, answer, abs(prediction-answer))
colnames(compare) <- c("pred", "answer", "loss")
compare

## calculate mean absolute loss
lm_loss_avg <- ( sum( abs( prediction-answer ) ) ) / nrow(compare)
lm_loss_avg

## 50: 10.78 , 180 : 9.576 ##

#####################################################################################################

## change data set

## remove all history
rm(list=ls())

## read csv file
d_score <- read.csv("Tmap_data.csv")

## regression
mod1 <- lm(result ~., data = d_score)

## show model's summary
summary(mod1)

## setting data
answer <- d_score[, "result"]
age <- d_score[, "age"]
gender <- d_score[, "gender"]
career <- d_score[, "career"]
distance <- d_score[, "distance"]
night <- d_score[, "night"]
compl <- d_score[, "compliance"]
accel <- d_score[, "acceleration"]
decel <- d_score[, "deceleration"]

## create model
prediction <- age * coef(mod1)[2] + gender * coef(mod1)[3] + career * coef(mod1)[4] +
  distance * coef(mod1)[5] + night * coef(mod1)[6] + compl * coef(mod1)[7] + 
  accel * coef(mod1)[8] + decel * coef(mod1)[9] + coef(mod1)[1]

prediction <- round(prediction, 0)

## compare with real answer
compare <- cbind(prediction, answer, abs(prediction-answer))
colnames(compare) <- c("pred", "answer", "loss")
compare

## calculate mean absolute loss
lm_loss_avg <- (sum(abs(prediction-answer)))/nrow(compare)
lm_loss_avg

## 5.733 ##

#####################################################################################################

## no optimizing variables - mix data set & model

## remove all history
rm(list=ls())

## read csv file for model
m_score <- read.csv("driving_score_180ea.csv")

## read csv file for data
d_score <- read.csv("Tmap_data_4col.csv")

## regression
mod1 <- lm(result ~., data = m_score)

## show model's summary
summary(mod1)

## setting data
answer <- d_score[, "result"]
compl <- d_score[, "compliance"]
accel <- d_score[, "acceleration"]
decel <- d_score[, "deceleration"]

## create model
prediction <- compl * coef(mod1)[2] + accel * coef(mod1)[3] + decel * coef(mod1)[4] + coef(mod1)[1]
prediction <- round(prediction, 0)

## compare with real answer
compare <- cbind(prediction, answer, abs(prediction-answer))
colnames(compare) <- c("pred", "answer", "loss")
compare

## calculate mean absolute loss
lm_loss_avg <- (sum(abs(prediction-answer)))/nrow(compare)
lm_loss_avg

#####################################################################################################












## using virtual data - vir&Tmap

## remove all history
rm(list=ls())

## read csv file for model
m_score <- read.csv("driving_score_virtual_rep.csv")

## read csv file for data
d_score <- read.csv("Tmap_data_4col.csv")

## regression
mod1 <- lm(result ~., data = m_score)

## show model's summary
summary(mod1)

## setting data
answer <- d_score[, "result"]
compl <- d_score[, "compliance"]
accel <- d_score[, "acceleration"]
decel <- d_score[, "deceleration"]

## create model
prediction <- compl * coef(mod1)[2] + accel * coef(mod1)[3] + decel * coef(mod1)[4] + coef(mod1)[1]
prediction <- round(prediction, 0)

## compare with real answer
compare <- cbind(prediction, answer, abs(prediction-answer))
compare <- cbind(compl, accel, decel, compare)
colnames(compare) <- c("compl", "accel", "decel", "pred", "answer", "loss")
compare

## calculate RMSE
RMSE <- sqrt( sum((prediction-answer)^2)/nrow(compare) )
RMSE

#####################################################################################################

## using virtual data - vir only

## remove all history
rm(list=ls())

## read csv file
d_score <- read.csv("driving_score_virtual_rep.csv")

## regression
mod1 <- lm(result ~., data = d_score)

## show model's summary
summary(mod1)

## setting data
answer <- d_score[, "result"]
compl <- d_score[, "compliance"]
accel <- d_score[, "acceleration"]
decel <- d_score[, "deceleration"]

## create model
prediction <- compl * coef(mod1)[2] + accel * coef(mod1)[3] + decel * coef(mod1)[4] + coef(mod1)[1]
prediction <- round(prediction, 0)

## compare with real answer
compare <- cbind(prediction, answer, abs(prediction-answer))
compare <- cbind(compl, accel, decel, compare)
colnames(compare) <- c("compl", "accel", "decel", "pred", "answer", "loss")
compare

## calculate RMSE
RMSE <- sqrt( sum((prediction-answer)^2)/nrow(compare) )
RMSE

#####################################################################################################

## using virtual data - vir&original

## remove all history
rm(list=ls())

## read csv file for model
m_score <- read.csv("driving_score_cluster.csv")

## read csv file for data
d_score <- read.csv("driving_score_180clust.csv")

## regression
mod1 <- lm(result ~., data = m_score)

## show model's summary
summary(mod1)

## setting data
answer <- d_score[, "result"]
compl <- d_score[, "compliance"]
accel <- d_score[, "acceleration"]
decel <- d_score[, "deceleration"]
clust <- d_score[, "cluster"]

## create model
prediction <- compl * coef(mod1)[2] + accel * coef(mod1)[3] + decel * coef(mod1)[4] + clust * coef(mod1)[5] + coef(mod1)[1]
prediction <- round(prediction, 0)

## compare with real answer
compare <- cbind(prediction, answer, abs(prediction-answer))
compare <- cbind(compl, accel, decel, clust, compare)
colnames(compare) <- c("compl", "accel", "decel", "clust", "pred", "answer", "loss")
compare

## calculate RMSE
RMSE <- sqrt( sum((prediction-answer)^2)/nrow(compare) )
RMSE

#####################################################################################################
