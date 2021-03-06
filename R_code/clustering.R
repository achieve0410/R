


## origin data clustering

rm(list=ls())

v_data <- read.csv("driving_score_180ea.csv")

rst <- kmeans(v_data, centers=5, iter.max = 1000, nstart = 1, algorithm = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"))

clust <- rst[1]

rsult <- cbind(v_data, clust)

write.csv(rsult, "cluster_origin.csv")

##############################################################################################################################

## Tmap, virtual classification

rm(list=ls())

require(class)

## Preparing training data set
train_data <- read.csv("cluster_origin.csv")
train_data <- train_data[, -4]

tr_x <- train_data[, -4]
tr_y <- train_data[, 4]

## Preparing Tmap data set - 15ea, k=3
test1_data <- read.csv("Tmap_data_4col.csv")

ts1_x <- test1_data[, -4]

## Preparing Virtual data set - 1248 , k=35
test2_data <- read.csv("driving_score_virtual.csv")

ts2_x <- test2_data[, -4]

## Classification Tmap
cluster <- knn(train = tr_x, test = ts1_x, cl = tr_y, k=3, prob=FALSE)
cluster

tmap <- cbind(test1_data, cluster)

## Classification virtual
cluster <- knn(train = tr_x, test = ts2_x, cl = tr_y, k=35, prob=FALSE)
cluster

virtual <- cbind(test2_data, cluster)

write.csv(tmap, "cluster_tmap.csv")
write.csv(virtual, "cluster_virtual.csv")

##############################################################################################################################

## model = origin // data = tmap & virtual // goal = data's result prediction

## DSI

rm(list=ls())

origin <- read.csv("cluster_origin.csv")                         # 182,  5
tmap <- read.csv("cluster_tmap.csv")                             # 15,   5
virtual <- read.csv("cluster_virtual.csv")                       # 1248, 5

## regression
mod1 <- lm(result ~., data = virtual)

## show model's summary
summary(mod1)

## setting data
answer <- tmap[, "result"]
compl <- tmap[, "compliance"]
accel <- tmap[, "acceleration"]
decel <- tmap[, "deceleration"]
clust <- tmap[, "cluster"]

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

##############################################################################################################################

## model = origin // data = tmap & virtual // goal = data's result prediction

## RF

rm(list=ls())

library(randomForest)

origin <- read.csv("cluster_origin.csv")                         # 182,  5
tmap <- read.csv("cluster_tmap.csv")                             # 15,   5
virtual <- read.csv("cluster_virtual.csv")                       # 1248, 5

ds.train <- virtual[,]
ds.test <- origin[,]

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

origin <- read.csv("cluster_origin.csv")                         # 182,  5
tmap <- read.csv("cluster_tmap.csv")                             # 15,   5
virtual <- read.csv("cluster_virtual.csv")                       # 1248, 5

x_train <- virtual[, -4]
y_train <- virtual[, 4]
x_test <- origin[, -4]
y_test <- origin[, 4]

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

origin <- read.csv("cluster_origin.csv")                         # 182,  5
tmap <- read.csv("cluster_tmap.csv")                             # 15,   5
virtual <- read.csv("cluster_virtual.csv")                       # 1248, 5












