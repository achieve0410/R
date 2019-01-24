


## origin data clustering

rm(list=ls())

m_data <- read.csv("driving_score_180ea.csv")
v_data <- m_data[, -2] ## compliance   & deceleration
t_data <- m_data[, -3] ## compliance   & acceleration
s_data <- m_data[, -1] ## acceleration & deceleration

rst1 <- kmeans(m_data, centers=5, iter.max = 1000, nstart = 2, algorithm = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"))
rst1

rst2 <- kmeans(v_data, centers=5, iter.max = 1000, nstart = 2, algorithm = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"))
rst2

rst3 <- kmeans(t_data, centers=5, iter.max = 1000, nstart = 2, algorithm = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"))
rst3

rst4 <- kmeans(s_data, centers=5, iter.max = 1000, nstart = 2, algorithm = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"))
rst4


clust1 <- rst1[1]
clust2 <- rst2[1]
clust3 <- rst3[1]
clust4 <- rst4[1]

rsult <- cbind(m_data, clust1, clust2, clust3, clust4)

write.csv(rsult, "cluster_origin.csv")

##############################################################################################################################

## Tmap, virtual classification

rm(list=ls())

require(class)

## Preparing training data set
train_data <- read.csv("cluster_origin.csv")

## delete result
train_data <- train_data[, -4]

## exclude clusters
tr_x <- train_data[, -c(4:7)]

## cluster3
tr_y3 <- train_data[, 4]

## cluster_cd
tr_y2 <- train_data[, 5]

## cluster_ca
tr_y1 <- train_data[, 6]

## cluster_ad
tr_y0 <- train_data[, 7]


## Preparing Tmap data set - 15ea, k=3
test1_data <- read.csv("Tmap_data_4col.csv")

## delete result
ts1_x <- test1_data[, -4]

## Preparing Virtual data set - 1248 , k=35
test2_data <- read.csv("driving_score_virtual2.csv")

## delete result
ts2_x <- test2_data[, -4]

## Classification Tmap - cluster3
cluster3 <- knn(train = tr_x, test = ts1_x, cl = tr_y3, k=3, prob=FALSE)
cluster3

## Classification Tmap - cluster_cd
cluster2 <- knn(train = tr_x, test = ts1_x, cl = tr_y2, k=3, prob=FALSE)
cluster2

## Classification Tmap - cluster_ca
cluster1 <- knn(train = tr_x, test = ts1_x, cl = tr_y1, k=3, prob=FALSE)
cluster1

## Classification Tmap - cluster_ad
cluster0 <- knn(train = tr_x, test = ts1_x, cl = tr_y0, k=3, prob=FALSE)
cluster0

tmap <- cbind(test1_data, cluster3, cluster2, cluster1, cluster0)

## Classification virtual - cluster3
cluster3 <- knn(train = tr_x, test = ts2_x, cl = tr_y3, k=15, prob=FALSE)
cluster3

## Classification virtual - cluster_cd
cluster2 <- knn(train = tr_x, test = ts2_x, cl = tr_y2, k=15, prob=FALSE)
cluster2

## Classification virtual - cluster_ca
cluster1 <- knn(train = tr_x, test = ts2_x, cl = tr_y1, k=15, prob=FALSE)
cluster1

## Classification virtual - cluster_ad
cluster0 <- knn(train = tr_x, test = ts2_x, cl = tr_y0, k=15, prob=FALSE)
cluster0

virtual <- cbind(test2_data, cluster3, cluster2, cluster1, cluster0)

write.csv(tmap, "cluster_tmap.csv")
write.csv(virtual, "cluster_virtual.csv")

##############################################################################################################################

## model = origin // data = tmap & virtual // goal = data's result prediction

## DSI

rm(list=ls())

origin <- read.csv("cluster_origin.csv")                         # 182,  8
tmap <- read.csv("cluster_tmap.csv")                             # 15,   8
virtual <- read.csv("cluster_virtual.csv")                       # 1248, 8

#unique(tmap[, c(5:8)])
#unique(virtual[, c(5:8)])

## regression
mod1 <- lm(result ~., data = virtual)

## show model's summary
summary(mod1)

## setting data
answer <- tmap[, "result"]
compl <- tmap[, "compliance"]
accel <- tmap[, "acceleration"]
decel <- tmap[, "deceleration"]
clust3 <- tmap[, "cluster3"]
clust2 <- tmap[, "cluster_cd"]
clust1 <- tmap[, "cluster_ca"]
clust0 <- tmap[, "cluster_ad"]

## create model
prediction <- compl * coef(mod1)[2] + accel * coef(mod1)[3] + decel * coef(mod1)[4] + clust3 * coef(mod1)[5] +
  clust2 * coef(mod1)[6] + clust1 * coef(mod1)[7] + clust0 * coef(mod1)[8] + coef(mod1)[1]
prediction <- round(prediction, 0)

## compare with real answer
compare <- cbind(prediction, answer, abs(prediction-answer))
compare <- cbind(compl, accel, decel, clust3, clust2, clust1, clust0, compare)
colnames(compare) <- c("compl", "accel", "decel", "clust3", "clust2", "clust1", "clust0", "pred", "answer", "loss")
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

ds.train <- virtual[,]
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

origin <- read.csv("cluster_origin.csv")                         # 182,  8
tmap <- read.csv("cluster_tmap.csv")                             # 15,   8
virtual <- read.csv("cluster_virtual.csv")                       # 1248, 8

x_train <- virtual[, -4]
y_train <- virtual[, 4]
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












