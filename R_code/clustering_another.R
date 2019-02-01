
## origin data clustering

rm(list=ls())

library("NbClust")

m_data <- read.csv("cluster_origin.csv")

#plot(m_data)
#head(m_data)
#dim(m_data)

v_data <- scale(m_data[, c(1, 2, 4)]) ## result & compliance & acceleration
t_data <- scale(m_data[, c(1, 3, 4)]) ## result & compliance & deceleration
s_data <- scale(m_data[, c(2, 3, 4)]) ## result & acceleration & deceleration


cr_nbclust <- NbClust(v_data, distance = "euclidean",
                      min.nc = 2, max.nc = 20, 
                      method = "kmeans")
ar_nbclust <- NbClust(t_data, distance = "euclidean",
                      min.nc = 2, max.nc = 20, 
                      method = "kmeans")
dr_nbclust <- NbClust(s_data, distance = "euclidean",
                      min.nc = 2, max.nc = 20, 
                      method = "kmeans")

#cr_a <- cr_nbclust$Best.partition
#ar_a <- ar_nbclust$Best.partition
#dr_a <- dr_nbclust$Best.partition

## compliance & result clustering result
#rst1 <- kmeans(n_data, centers=5, iter.max = 1000, nstart = 1, algorithm = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"))
#rst1

## compliance & result clustering result
rst2 <- kmeans(v_data, centers=2, iter.max = 1000, nstart = 2, algorithm = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"))
rst2

## acceleration & result clustering result
rst3 <- kmeans(t_data, centers=2, iter.max = 1000, nstart = 2, algorithm = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"))
rst3

## deceleration & result clustering result
rst4 <- kmeans(s_data, centers=2, iter.max = 1000, nstart = 2, algorithm = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"))
rst4

#clust1 <- rst1[1]
clust2 <- rst2[1]
clust3 <- rst3[1]
clust4 <- rst4[1]

#rsult <- cbind(m_data, clust1, clust2, clust3, clust4)
rsult <- cbind(m_data, clust2, clust3, clust4)

write.csv(rsult, "cluster_origin_10.csv")

origin <- read.csv("cluster_origin_10.csv")

head(origin)
dim(origin)

##############################################################################################################################

## Tmap, virtual, new data classification

rm(list=ls())

require(class)

## Preparing training data set
train_data <- read.csv("cluster_origin_10.csv")

head(train_data)

## for cr cluster
tr_x6 <- train_data[, 1:3]

## for ar cluster
tr_x5 <- train_data[, 1:3]

## for dr cluster
tr_x4 <- train_data[, 1:3]

## for car cluster
tr_x3 <- train_data[, 1:3]

## for cdr cluster
tr_x2 <- train_data[, 1:3]

## for adr cluster
tr_x1 <- train_data[, 1:3]

## cluster_cr
tr_y6 <- train_data[, 5]

## cluster_ar
tr_y5 <- train_data[, 6]

## cluster_dr
tr_y4 <- train_data[, 7]

## cluster_car
tr_y3 <- train_data[, 8]

## cluster_cdr
tr_y2 <- train_data[, 9]

## cluster_adr
tr_y1 <- train_data[, 10]


## Preparing Tmap data set - 15ea, k=3
test1_data <- read.csv("Tmap_data.csv")
ts1 <- test1_data[, -4]

## Preparing Virtual data set - 182 , k=13
test2_data <- read.csv("driving_score_180ea.csv")
ts2 <- test2_data[, -4]

## Preparing new data set - 13 , k=3
test3_data <- read.csv("driving_score_new.csv")
ts3 <- test3_data[, -4]

## Classification Tmap

## Classification Tmap - cluster cr
cluster6 <- knn(train = tr_x6, test = ts1, cl = tr_y6, k=3, prob=TRUE)
cluster6

## Classification Tmap - cluster ar
cluster5 <- knn(train = tr_x5, test = ts1, cl = tr_y5, k=3, prob=TRUE)
cluster5

## Classification Tmap - cluster dr
cluster4 <- knn(train = tr_x4, test = ts1, cl = tr_y4, k=3, prob=TRUE)
cluster4

## Classification Tmap - cluster car
cluster3 <- knn(train = tr_x3, test = ts1, cl = tr_y3, k=3, prob=TRUE)
cluster3

## Classification Tmap - cluster cdr
cluster2 <- knn(train = tr_x2, test = ts1, cl = tr_y2, k=3, prob=TRUE)
cluster2

## Classification Tmap - cluster adr
cluster1 <- knn(train = tr_x1, test = ts1, cl = tr_y1, k=3, prob=TRUE)
cluster1

tmap <- cbind(test1_data, cluster6, cluster5, cluster4, cluster3, cluster2, cluster1)


## Classification Origin

## Classification virtual - cluster cr
cluster6 <- knn(train = tr_x6, test = ts2, cl = tr_y6, k=13, prob=TRUE)
cluster6

## Classification virtual - cluster ar
cluster5 <- knn(train = tr_x5, test = ts2, cl = tr_y5, k=13, prob=TRUE)
cluster5

## Classification virtual - cluster dr
cluster4 <- knn(train = tr_x4, test = ts2, cl = tr_y4, k=13, prob=TRUE)
cluster4

## Classification virtual - cluster car
cluster3 <- knn(train = tr_x3, test = ts2, cl = tr_y3, k=13, prob=TRUE)
cluster3

## Classification virtual - cluster cdr
cluster2 <- knn(train = tr_x2, test = ts2, cl = tr_y2, k=13, prob=TRUE)
cluster2

## Classification virtual - cluster adr
cluster1 <- knn(train = tr_x1, test = ts2, cl = tr_y1, k=13, prob=TRUE)
cluster1

virtual <- cbind(test2_data, cluster6, cluster5, cluster4, cluster3, cluster2, cluster1)


## Classification new

## Classification new - cluster cr
cluster6 <- knn(train = tr_x6, test = ts3, cl = tr_y6, k=3, prob=TRUE)
cluster6

## Classification new - cluster ar
cluster5 <- knn(train = tr_x5, test = ts3, cl = tr_y5, k=3, prob=TRUE)
cluster5

## Classification new - cluster dr
cluster4 <- knn(train = tr_x4, test = ts3, cl = tr_y4, k=3, prob=TRUE)
cluster4

## Classification new - cluster car
cluster3 <- knn(train = tr_x3, test = ts3, cl = tr_y3, k=3, prob=TRUE)
cluster3

## Classification new - cluster cdr
cluster2 <- knn(train = tr_x2, test = ts3, cl = tr_y2, k=3, prob=TRUE)
cluster2

## Classification new - cluster adr
cluster1 <- knn(train = tr_x1, test = ts3, cl = tr_y1, k=3, prob=TRUE)
cluster1

new <- cbind(test3_data, cluster6, cluster5, cluster4, cluster3, cluster2, cluster1)

write.csv(tmap, "cluster_tmap.csv")
write.csv(virtual, "cluster_virtual.csv")
write.csv(new, "cluster_new.csv")

##############################################################################################################################


## model = origin // data = tmap & virtual // goal = data's result prediction

## DSI

rm(list=ls())

origin <- read.csv("cluster_origin_10.csv")                      # 2000,  10
tmap <- read.csv("cluster_tmap.csv")                             # 15,    10
virtual <- read.csv("cluster_virtual.csv")                       # 182,   10
new <- read.csv("cluster_new.csv")                               # 12,    10

## regression
mod1 <- lm(result ~., data = origin)

## show model's summary
summary(mod1)

## setting data
answer <- virtual[, "result"]
compl <- virtual[, "compliance"]
accel <- virtual[, "acceleration"]
decel <- virtual[, "deceleration"]
clust6 <- virtual[, "cluster_cr"]
clust5 <- virtual[, "cluster_ar"]
clust4 <- virtual[, "cluster_dr"]
clust3 <- virtual[, "cluster_car"]
clust2 <- virtual[, "cluster_cdr"]
clust1 <- virtual[, "cluster_adr"]

## create model
#prediction <- compl * coef(mod1)[2] + accel * coef(mod1)[3] + decel * coef(mod1)[4] + clust3 * coef(mod1)[5] +
# clust2 * coef(mod1)[5] + clust1 * coef(mod1)[6] + clust0 * coef(mod1)[7] + coef(mod1)[1]
prediction <- compl * coef(mod1)[2] + accel * coef(mod1)[3] + decel * coef(mod1)[4] +
  clust6 * coef(mod1)[5] + clust5 * coef(mod1)[6] + clust4 * coef(mod1)[7] +
  clust2 * coef(mod1)[9] + clust1 * coef(mod1)[10] + coef(mod1)[1]
prediction <- round(prediction, 1)

## compare with real answer
compare <- cbind(prediction, answer, abs(prediction-answer))
#compare <- cbind(compl, accel, decel, clust3, clust2, clust1, clust0, compare)
compare <- cbind(compl, accel, decel, clust6, clust5, clust4, clust2, clust1, compare)
#colnames(compare) <- c("compl", "accel", "decel", "clust3", "clust_cd", "clust_ca", "clust_ad", "pred", "answer", "loss")
colnames(compare) <- c("compl", "accel", "decel", "clust_cr", "clust_ar", "clust_dr", "clust_cdr", "clust_adr", "pred", "answer", "loss")
compare

## calculate RMSE
RMSE <- sqrt( sum((prediction-answer)^2)/nrow(compare) )
RMSE

##############################################################################################################################

## model = origin // data = tmap & virtual // goal = data's result prediction

## RF

rm(list=ls())

library(randomForest)

origin <- read.csv("cluster_origin_10.csv")                      # 2000, 7
tmap <- read.csv("cluster_tmap.csv")                             # 15,   7
virtual <- read.csv("cluster_virtual.csv")                       # 182,  7
new <- read.csv("cluster_new.csv")                               # 12,   7

ds.train <- origin[,]
ds.test <- new[,]

d_score.rf <- randomForest(result ~ ., data=ds.train, ntree = 5000)
d_score.rf

d_score.pred <- predict(d_score.rf, ds.test)
d_score.pred <- round(d_score.pred, 1)

## compare with real answer
compare <- cbind(d_score.pred, ds.test[, "result"], abs(d_score.pred-ds.test[,"result"]))
compare <- cbind(ds.test[,-4], compare)
colnames(compare) <- c("compl","accel","decel","clust_cr","clust_ar","clust_dr","clust_car","clust_cdr","clust_adr","pred", "answer", "loss")
compare

## calculate RMSE
RMSE <- sqrt( sum( ( d_score.pred-ds.test[, "result"] )^2 )  / nrow(compare) )
RMSE

##############################################################################################################################

## model = origin // data = tmap & virtual // goal = data's result prediction

## SVM

rm(list=ls())

library(e1071)

origin <- read.csv("cluster_origin_10.csv")                      # 182,  8
tmap <- read.csv("cluster_tmap.csv")                             # 15,   8
virtual <- read.csv("cluster_virtual.csv")                       # 1248, 8
new <- read.csv("cluster_new.csv")                               # 12,   8

x_train <- origin[, -4]
y_train <- origin[, 4]
x_test <- virtual[, -4]
y_test <- virtual[, 4]

# training with train data
#model <- svm(x_train, y_train, type = "nu-regression")
model <- svm(x_train, y_train)
summary(model)

# test with test data
pred <- predict(model, x_test)
pred <- round(pred, 1)

## compare with real answer
compare <- cbind(x_test[,], pred, y_test, abs(pred-y_test))
colnames(compare) <- c("compl","accel","decel","clust_cr","clust_ar","clust_dr","clust_car","clust_cdr","clust_adr","pred", "answer", "loss")
compare

## calculate RMSE
RMSE <- sqrt( sum( ( pred-y_test )^2 )  / nrow(compare) )
RMSE

##############################################################################################################################
