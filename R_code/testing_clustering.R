


## origin data clustering

rm(list=ls())

m_data <- read.csv("driving_score_180ea.csv")

#n_data <- m_data[, -4]        ## compliance   & acceleration & deceleration
v_data <- m_data[, c(-2, -4)] ## compliance   & deceleration
t_data <- m_data[, c(-3, -4)] ## compliance   & acceleration
#s_data <- m_data[, c(-1, -4)] ## acceleration & deceleration

## compliance   & acceleration & deceleration clustering result
#rst1 <- kmeans(n_data, centers=5, iter.max = 1000, nstart = 1, algorithm = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"))
#rst1

## compliance   & deceleration clustering result
rst2 <- kmeans(v_data, centers=5, iter.max = 1000, nstart = 1, algorithm = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"))
rst2

## compliance   & acceleration clustering result
rst3 <- kmeans(t_data, centers=5, iter.max = 1000, nstart = 1, algorithm = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"))
rst3

## acceleration & deceleration clustering result
#rst4 <- kmeans(s_data, centers=5, iter.max = 1000, nstart = 1, algorithm = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"))
#rst4

#clust1 <- rst1[1]
clust2 <- rst2[1]
clust3 <- rst3[1]
#clust4 <- rst4[1]

#rsult <- cbind(m_data, clust1, clust2, clust3, clust4)
rsult <- cbind(m_data, clust2, clust3)

write.csv(rsult, "cluster_origin.csv")

##############################################################################################################################

## Tmap, virtual, new data classification

rm(list=ls())

require(class)

## Preparing training data set
train_data <- read.csv("cluster_origin.csv")

## delete result
train_data <- train_data[, -4]

## exclude clusters
tr_x <- train_data[, -c(4:5)]

## cluster3
#tr_y3 <- train_data[, 4]

## cluster_cd
tr_y2 <- train_data[, 4]

## cluster_ca
tr_y1 <- train_data[, 5]

## cluster_ad
#tr_y0 <- train_data[, 6]


## Preparing Tmap data set - 15ea, k=3
test1_data <- read.csv("Tmap_data_4col.csv")

## delete result
ts1_x <- test1_data[, -4]

## Preparing Virtual data set - 1248 , k=35
test2_data <- read.csv("driving_score_virtual2.csv")

## delete result
ts2_x <- test2_data[, -4]

## Preparing new data set
test3_data <- read.csv("driving_score_new.csv")

## delete result
ts3_x <- test3_data[, -4]

## Classification Tmap - cluster3
#cluster3 <- knn(train = tr_x, test = ts1_x, cl = tr_y3, k=3, prob=FALSE)
#cluster3

## Classification Tmap - cluster_cd
cluster2 <- knn(train = tr_x, test = ts1_x, cl = tr_y2, k=3, prob=FALSE)
cluster2

## Classification Tmap - cluster_ca
cluster1 <- knn(train = tr_x, test = ts1_x, cl = tr_y1, k=3, prob=FALSE)
cluster1

## Classification Tmap - cluster_ad
#cluster0 <- knn(train = tr_x, test = ts1_x, cl = tr_y0, k=3, prob=FALSE)
#cluster0

#tmap <- cbind(test1_data, cluster3, cluster2, cluster1, cluster0)
tmap <- cbind(test1_data, cluster2, cluster1)

## Classification virtual - cluster3
#cluster3 <- knn(train = tr_x, test = ts2_x, cl = tr_y3, k=15, prob=FALSE)
#cluster3

## Classification virtual - cluster_cd
cluster2 <- knn(train = tr_x, test = ts2_x, cl = tr_y2, k=15, prob=FALSE)
cluster2

## Classification virtual - cluster_ca
cluster1 <- knn(train = tr_x, test = ts2_x, cl = tr_y1, k=15, prob=FALSE)
cluster1

## Classification virtual - cluster_ad
#cluster0 <- knn(train = tr_x, test = ts2_x, cl = tr_y0, k=15, prob=FALSE)
#cluster0

#virtual <- cbind(test2_data, cluster3, cluster2, cluster1, cluster0)
virtual <- cbind(test2_data, cluster2, cluster1)

## Classification new - cluster3
#cluster3 <- knn(train = tr_x, test = ts2_x, cl = tr_y3, k=15, prob=FALSE)
#cluster3

## Classification new - cluster_cd
cluster2 <- knn(train = tr_x, test = ts3_x, cl = tr_y2, k=15, prob=FALSE)
cluster2

## Classification new - cluster_ca
cluster1 <- knn(train = tr_x, test = ts3_x, cl = tr_y1, k=15, prob=FALSE)
cluster1

## Classification new - cluster_ad
#cluster0 <- knn(train = tr_x, test = ts2_x, cl = tr_y0, k=15, prob=FALSE)
#cluster0

#virtual <- cbind(test2_data, cluster3, cluster2, cluster1, cluster0)
new <- cbind(test3_data, cluster2, cluster1)

write.csv(tmap, "cluster_tmap.csv")
write.csv(virtual, "cluster_virtual.csv")
write.csv(new, "cluster_new.csv")

##############################################################################################################################

## model = origin // data = tmap & virtual // goal = data's result prediction

## DSI

rm(list=ls())

origin <- read.csv("cluster_origin.csv")                         # 182,  8
tmap <- read.csv("cluster_tmap.csv")                             # 15,   8
virtual <- read.csv("cluster_virtual.csv")                       # 250, 8
new <- read.csv("cluster_new.csv")

#dim(virtual)
#unique(tmap[, c(5:8)])
#unique(virtual[, c(5:8)])

## regression
mod1 <- lm(result ~., data = origin)

## show model's summary
summary(mod1)

## setting data
answer <- new[, "result"]
compl <- new[, "compliance"]
accel <- new[, "acceleration"]
decel <- new[, "deceleration"]
#clust3 <- origin[, "cluster3"]
clust2 <- new[, "cluster_cd"]
clust1 <- new[, "cluster_ca"]
#clust0 <- origin[, "cluster_ad"]

## create model
#prediction <- compl * coef(mod1)[2] + accel * coef(mod1)[3] + decel * coef(mod1)[4] + clust3 * coef(mod1)[5] +
# clust2 * coef(mod1)[5] + clust1 * coef(mod1)[6] + clust0 * coef(mod1)[7] + coef(mod1)[1]
prediction <- compl * coef(mod1)[2] + accel * coef(mod1)[3] + decel * coef(mod1)[4] +
  clust2 * coef(mod1)[5] + clust1 * coef(mod1)[6] + coef(mod1)[1]
prediction <- round(prediction, 0)

## compare with real answer
compare <- cbind(prediction, answer, abs(prediction-answer))
#compare <- cbind(compl, accel, decel, clust3, clust2, clust1, clust0, compare)
compare <- cbind(compl, accel, decel, clust2, clust1, compare)
#colnames(compare) <- c("compl", "accel", "decel", "clust3", "clust_cd", "clust_ca", "clust_ad", "pred", "answer", "loss")
colnames(compare) <- c("compl", "accel", "decel", "clust_cd", "clust_ca", "pred", "answer", "loss")
compare

## calculate RMSE
RMSE <- sqrt( sum((prediction-answer)^2)/nrow(compare) )
RMSE

##############################################################################################################################

## model = origin // data = tmap & virtual // goal = data's result prediction

## RF

rm(list=ls())

library(randomForest)

origin <- read.csv("cluster_origin.csv")                         # 182,  8
tmap <- read.csv("cluster_tmap.csv")                             # 15,   8
virtual <- read.csv("cluster_virtual.csv")                       # 1248, 8
new <- read.csv("cluster_new.csv")

ds.train <- origin[,]
ds.test <- new[,]

d_score.rf <- randomForest(result ~ ., data=ds.train, ntree = 5000)
d_score.rf

d_score.pred <- predict(d_score.rf, ds.test)
d_score.pred <- round(d_score.pred, 0)

## compare with real answer
compare <- cbind(d_score.pred, ds.test[, "result"], abs(d_score.pred-ds.test[,"result"]))
colnames(compare) <- c("pred", "answer", "loss")
compare

## calculate RMSE
RMSE <- sqrt( sum( ( d_score.pred-ds.test[, "result"] )^2 )  / nrow(compare) )
RMSE

##############################################################################################################################

## model = origin // data = tmap & virtual // goal = data's result prediction

## SVM

rm(list=ls())

library(e1071)

origin <- read.csv("cluster_origin.csv")                         # 182,  8
tmap <- read.csv("cluster_tmap.csv")                             # 15,   8
virtual <- read.csv("cluster_virtual.csv")                       # 1248, 8
new <- read.csv("cluster_new.csv")

x_train <- origin[, -4]
y_train <- origin[, 4]
x_test <- new[, -4]
y_test <- new[, 4]

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

##############################################################################################################################

## model = origin // data = tmap & virtual // goal = data's result prediction

## RF

library(randomForest)
library(caret)

## remove all history
rm(list=ls())

## read csv file
d_score <- read.csv("cluster_origin.csv")

## create folds , 10-fold Cross Validation
fld <- createFolds(d_score$result, k=10)

temp_loss <- c() # temp loss for each fold
final_loss <- c() # final loss

for( i in 1:1 ) {
  temp_loss <- c()
  for( j in 1:10 ) {
    
    ds.train <- d_score[-fld[[j]], ]
    ds.test <- d_score[fld[[j]], ]
    
    d_score.rf <- randomForest(result ~ ., data=ds.train, ntree = 20000)
    
    d_score.pred <- predict(d_score.rf, ds.test)
    d_score.pred <- round(d_score.pred, 0)
    
    ## compare with real answer
    compare <- cbind(d_score.pred, ds.test[, "result"], abs(d_score.pred-ds.test[,"result"]))
    
    ## calculate RMSE
    lm_loss_avg <- sqrt( sum( ( d_score.pred-ds.test[, "result"] )^2 ) / nrow(compare) )
    temp_loss[j] <- lm_loss_avg
    
  }
  final_loss[i] <- mean(temp_loss)
}
final_loss
mean(final_loss)


##############################################################################################################################

## model = origin // data = tmap & virtual // goal = data's result prediction

## SVM

library(e1071)
library(caret)

## remove all history
rm(list=ls())

## read csv file
d_score <- read.csv("cluster_origin.csv")

temp_loss <- c() # loss for each fold
final_loss <- c()

for( i in 1:10 ) {
  
  ## create folds , 10-fold Cross Validation
  fld <- createFolds(d_score$result, k=10)
  
  temp_loss <- c()
  
  for( j in 1:10 ) {
    
    x_train <- d_score[-fld[[j]], -4]
    y_train <- d_score[-fld[[j]], 4]
    x_test <- d_score[fld[[j]], -4]
    y_test <- d_score[fld[[j]], 4]
    
    # training with train data
    model <- svm(x_train, y_train, type = "nu-regression")
    
    summary(model)
    
    # test with test data
    pred <- predict(model, x_test)
    pred <- round(pred, 0)
    
    ## compare with real answer
    compare <- cbind(pred, y_test, abs(pred-y_test))
    
    ## calculate RMSE
    lm_loss_avg <- sqrt( sum( ( pred-y_test )^2 )  / nrow(compare) )
    temp_loss[j] <- lm_loss_avg
    
  }
  
  final_loss[i] <- mean(temp_loss)
}
final_loss
mean(final_loss)

##############################################################################################################################




