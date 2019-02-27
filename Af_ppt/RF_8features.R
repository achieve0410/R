
##############################################################################################################################

## RF

rm(list=ls())

library(randomForest)

origin <- read.csv("cluster_fil11.csv")                         # 120,   8
new <- read.csv("cluster_new_fil11.csv")                        # 12,    8

ds.train <- origin[,]
ds.test <- new[,]

## make RF model
d_score.rf <- randomForest(result ~ ., data=ds.train, ntree = 200)
d_score.rf

d_score.pred <- predict(d_score.rf, ds.test)
d_score.pred <- round(d_score.pred, 1)

## compare with real answer
compare <- cbind(d_score.pred, ds.test[, "result"], abs(d_score.pred-ds.test[,"result"]))
compare <- cbind(ds.test[,c(1:7)], compare)
colnames(compare) <- c("compl", "accel", "decel", "clust_ca", "clust_cd", "clust_ad", 
                       "clust_cad", "pred", "answer", "loss")
compare

## calculate RMSE
RMSE <- sqrt( sum( ( d_score.pred-ds.test[, "result"] )^2 )  / nrow(compare) )
RMSE

##############################################################################################################################
