


## origin data clustering

rm(list=ls())

m_data <- read.csv("driving_score_180ea.csv")
v_data <- m_data[, -2] ## compliance & deceleration

rst1 <- kmeans(m_data, centers=6, iter.max = 1000, nstart = 2, algorithm = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"))
rst1

rst2 <- kmeans(v_data, centers=6, iter.max = 1000, nstart = 2, algorithm = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"))
rst2

clust1 <- rst1[1]
clust2 <- rst2[1]

rsult <- cbind(m_data, clust1, clust2)

write.csv(rsult, "cluster_origin.csv")

##############################################################################################################################

## Tmap, virtual classification

rm(list=ls())

require(class)

## Preparing training data set
train_data <- read.csv("cluster_origin.csv")

## delete result
train_data <- train_data[, -4]

## exclude cluster2&3
tr_x <- train_data[, c(-4, -5)]

## cluster3
tr_y3 <- train_data[, 4]

## cluster2
tr_y2 <- train_data[, 5]

## Preparing Tmap data set - 15ea, k=3
test1_data <- read.csv("Tmap_data_4col.csv")

## delete result
ts1_x <- test1_data[, -4]

## Preparing Virtual data set - 1248 , k=35
test2_data <- read.csv("driving_score_virtual.csv")

## delete result
ts2_x <- test2_data[, -4]

## Classification Tmap - cluster3
cluster3 <- knn(train = tr_x, test = ts1_x, cl = tr_y3, k=3, prob=FALSE)
cluster3

## Classification Tmap - cluster2
cluster2 <- knn(train = tr_x, test = ts1_x, cl = tr_y2, k=3, prob=FALSE)
cluster2

tmap <- cbind(test1_data, cluster3, cluster2)

## Classification virtual - cluster3
cluster3 <- knn(train = tr_x, test = ts2_x, cl = tr_y3, k=35, prob=FALSE)
cluster3

## Classification virtual - cluster2
cluster2 <- knn(train = tr_x, test = ts2_x, cl = tr_y2, k=35, prob=FALSE)
cluster2

virtual <- cbind(test2_data, cluster3, cluster2)

write.csv(tmap, "cluster_tmap.csv")
write.csv(virtual, "cluster_virtual.csv")

##############################################################################################################################

## model = origin // data = tmap & virtual // goal = data's result prediction

## DSI

rm(list=ls())

origin <- read.csv("cluster_origin.csv")                         # 182,  6
tmap <- read.csv("cluster_tmap.csv")                             # 15,   6
virtual <- read.csv("cluster_virtual.csv")                       # 1248, 6

## regression
mod1 <- lm(result ~., data = origin)

## show model's summary
summary(mod1)

## setting data
answer <- tmap[, "result"]
compl <- tmap[, "compliance"]
accel <- tmap[, "acceleration"]
decel <- tmap[, "deceleration"]
clust3 <- tmap[, "cluster3"]
clust2 <- tmap[, "cluster2"]

## create model
prediction <- compl * coef(mod1)[2] + accel * coef(mod1)[3] + decel * coef(mod1)[4] + clust3 * coef(mod1)[5] +
              clust2 * coef(mod1)[6] + coef(mod1)[1]
prediction <- round(prediction, 0)

## compare with real answer
compare <- cbind(prediction, answer, abs(prediction-answer))
compare <- cbind(compl, accel, decel, clust3, clust2, compare)
colnames(compare) <- c("compl", "accel", "decel", "clust3", "clust2", "pred", "answer", "loss")
compare

## calculate RMSE
RMSE <- sqrt( sum((prediction-answer)^2)/nrow(compare) )
RMSE

##############################################################################################################################

## model = origin // data = tmap & virtual // goal = data's result prediction

## RF

rm(list=ls())

library(randomForest)

origin <- read.csv("cluster_origin.csv")                         # 182,  6
tmap <- read.csv("cluster_tmap.csv")                             # 15,   6
virtual <- read.csv("cluster_virtual.csv")                       # 1248, 6

ds.train <- origin[,]
ds.test <- tmap[,]

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

origin <- read.csv("cluster_origin.csv")                         # 182,  6
tmap <- read.csv("cluster_tmap.csv")                             # 15,   6
virtual <- read.csv("cluster_virtual.csv")                       # 1248, 6

x_train <- origin[, -4]
y_train <- origin[, 4]
x_test <- tmap[, -4]
y_test <- tmap[, 4]

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

## MLP

rm(list=ls())

origin <- read.csv("cluster_origin.csv")                         # 182,  6
tmap <- read.csv("cluster_tmap.csv")                             # 15,   6
virtual <- read.csv("cluster_virtual.csv")                       # 1248, 6












