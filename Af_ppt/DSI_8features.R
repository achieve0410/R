
##############################################################################################################################

## DSI

rm(list=ls())

origin <- read.csv("cluster_fil11.csv")                         # 120,   8
new <- read.csv("cluster_new_fil11.csv")                        # 12,    8

## regression
mod1 <- lm(result ~., data = origin)

## show model's summary
summary(mod1)

## setting data
answer <- new[, "result"]
compl <- new[, "compliance"]
accel <- new[, "acceleration"]
decel <- new[, "deceleration"]
clust_ca <- new[, "clust_ca"]
clust_cd <- new[, "clust_cd"]
clust_ad <- new[, "clust_ad"]
clust_cad <- new[, "clust_cad"]

## create model
prediction <- compl * coef(mod1)[2] + accel * coef(mod1)[3] + decel * coef(mod1)[4] + 
  clust_ca * coef(mod1)[5] + clust_cd * coef(mod1)[6] + clust_ad * coef(mod1)[7] + 
  clust_cad * coef(mod1)[8] + coef(mod1)[1]
prediction <- round(prediction, 1)

prediction

## compare with real answer
compare <- cbind(prediction, answer, abs(prediction-answer))
compare <- cbind(new[, c(1:7)], compare)
colnames(compare) <- c("compl", "accel", "decel", "clust_ca", "clust_cd", "clust_ad", "clust_cad", "pred", "result", "loss")
compare

## calculate RMSE
RMSE <- sqrt( sum((prediction-answer)^2)/nrow(compare) )
RMSE

##############################################################################################################################
