
## origin data clustering

rm(list=ls())

library("NbClust")

# temp <- read.csv("driving_score_480ea.csv")
#
# plot(temp)

#m_data <- read.csv("driving_score_virtual_rep.csv")
m_data <- read.csv("driving_score_180ea.csv")
head(m_data)
num <- c()

for( i in 1:nrow(m_data) ) {
  num[ i ] = 0
}

for( i in 1:nrow(m_data) ) {
  num[ m_data[i, 4] ] = num[ m_data[i, 4] ] + 1
}

num

plot(x = m_data$result, y = num, type = "h")

head(m_data)
dim(m_data)

v_data <- scale(m_data[, c(-3, -4)]) ## compliance & acceleration
t_data <- scale(m_data[, c(-2, -4)]) ## compliance & deceleration
s_data <- scale(m_data[, c(-1, -4)]) ## acceleration & deceleration
n_data <- scale(m_data[, -4]) ## compliance & acceleration & deceleration

#k_data <- scale(m_data[, -3]) ## result & compliance & acceleration
#o_data <- scale(m_data[, -2]) ## result & compliance & deceleration
#u_data <- scale(m_data[, -1]) ## result & acceleration & deceleration

ca_nbclust <- NbClust(v_data, distance = "manhattan",
                      min.nc = 2, max.nc = 15, 
                      method = "kmeans")
cd_nbclust <- NbClust(t_data, distance = "manhattan",
                      min.nc = 2, max.nc = 15, 
                      method = "kmeans")
ad_nbclust <- NbClust(s_data, distance = "manhattan",
                      min.nc = 2, max.nc = 15, 
                      method = "kmeans")
cad_nbclust <- NbClust(n_data, distance = "manhattan",
                       min.nc = 2, max.nc = 15, 
                       method = "kmeans")

#car_nbclust <- NbClust(k_data, distance = "euclidean",
#                      min.nc = 2, max.nc = 20, 
#                      method = "kmeans")
#cdr_nbclust <- NbClust(o_data, distance = "euclidean",
#                      min.nc = 2, max.nc = 20, 
#                      method = "kmeans")
#adr_nbclust <- NbClust(u_data, distance = "euclidean",
#                      min.nc = 2, max.nc = 20, 
#                      method = "kmeans")

## compliance & acceleration clustering result
rst2 <- kmeans(v_data, centers=15, iter.max = 1000, nstart = 2, algorithm = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"))


## compliance & deceleration clustering result
rst3 <- kmeans(t_data, centers=2, iter.max = 1000, nstart = 2, algorithm = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"))


## acceleration & deceleration clustering result
rst4 <- kmeans(s_data, centers=2, iter.max = 1000, nstart = 2, algorithm = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"))


## compliance & acceleration & deceleration clustering result
rst5 <- kmeans(n_data, centers=2, iter.max = 1000, nstart = 2, algorithm = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"))


## compliance & result clustering result
#rst5 <- kmeans(k_data, centers=3, iter.max = 1000, nstart = 2, algorithm = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"))


## acceleration & result clustering result
#rst6 <- kmeans(o_data, centers=20, iter.max = 1000, nstart = 2, algorithm = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"))


## deceleration & result clustering result
#rst7 <- kmeans(u_data, centers=7, iter.max = 1000, nstart = 2, algorithm = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"))


clust2 <- rst2[1]
clust3 <- rst3[1]
clust4 <- rst4[1]
clust5 <- rst5[1]
#clust6 <- rst6[1]
#clust7 <- rst7[1]


#rsult <- cbind(m_data, clust2, clust3, clust4, clust5, clust6, clust7)
rsult <- cbind(m_data, clust2, clust3, clust4, clust5)

write.csv(rsult, "cluster_origin_4.csv")

origin <- read.csv("cluster_origin_4.csv")

head(origin)
dim(origin)

##############################################################################################################################

## Tmap, virtual, new data classification

rm(list=ls())

require(class)

## Preparing training data set
train_data <- read.csv("cluster_origin_4.csv")

head(train_data)


## for ca cluster
tr_x6 <- train_data[, -c(4:8)]

## for cd cluster
tr_x5 <- train_data[, -c(4:8)]

## for ad cluster
tr_x4 <- train_data[, -c(4:8)]

## for cad cluster
tr_x3 <- train_data[, -c(4:8)]

## for car cluster
#tr_x3 <- train_data[, -c(4:7)]

## for cdr cluster
#tr_x2 <- train_data[, -c(4:7)]

## for adr cluster
#tr_x1 <- train_data[, -c(4:7)]

## cluster_ca
tr_y6 <- train_data[, 5]

## cluster_cd
tr_y5 <- train_data[, 6]

## cluster_ad
tr_y4 <- train_data[, 7]

## cluster_ad
tr_y3 <- train_data[, 8]


## cluster_car
#tr_y3 <- train_data[, 8]

## cluster_cdr
#tr_y2 <- train_data[, 9]

## cluster_adr
#tr_y1 <- train_data[, 10]


## Preparing Tmap data set - 15ea, k=3
test1_data <- read.csv("Tmap_data.csv")
ts1 <- test1_data[, -4]

## Preparing Virtual data set - 121 , k=11
test2_data <- read.csv("driving_score_121.csv")
ts2 <- test2_data[, -4]

## Preparing new data set - 13 , k=3
test3_data <- read.csv("driving_score_new.csv")
ts3 <- test3_data[, -4]

## Classification Tmap

## Classification Tmap - cluster ca
cluster6 <- knn(train = tr_x6, test = ts1, cl = tr_y6, k=3, prob=TRUE)

## Classification Tmap - cluster cd
cluster5 <- knn(train = tr_x5, test = ts1, cl = tr_y5, k=3, prob=TRUE)

## Classification Tmap - cluster ad
cluster4 <- knn(train = tr_x4, test = ts1, cl = tr_y4, k=3, prob=TRUE)

## Classification Tmap - cluster cad
cluster3 <- knn(train = tr_x3, test = ts1, cl = tr_y3, k=3, prob=TRUE)

## Classification Tmap - cluster cdr
#cluster2 <- knn(train = tr_x2, test = ts1, cl = tr_y2, k=3, prob=TRUE)

## Classification Tmap - cluster adr
#cluster1 <- knn(train = tr_x1, test = ts1, cl = tr_y1, k=3, prob=TRUE)

#tmap <- cbind(test1_data, cluster6, cluster5, cluster4, cluster3, cluster2, cluster1)
tmap <- cbind(test1_data, cluster6, cluster5, cluster4, cluster3)


## Classification Origin

## Classification virtual - cluster ca
cluster6 <- knn(train = tr_x6, test = ts2, cl = tr_y6, k=11, prob=TRUE)

## Classification virtual - cluster cd
cluster5 <- knn(train = tr_x5, test = ts2, cl = tr_y5, k=11, prob=TRUE)

## Classification virtual - cluster ad
cluster4 <- knn(train = tr_x4, test = ts2, cl = tr_y4, k=11, prob=TRUE)

## Classification virtual - cluster cad
cluster3 <- knn(train = tr_x3, test = ts2, cl = tr_y3, k=11, prob=TRUE)

## Classification virtual - cluster cdr
#cluster2 <- knn(train = tr_x2, test = ts2, cl = tr_y2, k=11, prob=TRUE)

## Classification virtual - cluster adr
#cluster1 <- knn(train = tr_x1, test = ts2, cl = tr_y1, k=11, prob=TRUE)

#virtual <- cbind(test2_data, cluster6, cluster5, cluster4, cluster3, cluster2, cluster1)
virtual <- cbind(test2_data, cluster6, cluster5, cluster4, cluster3)


## Classification new

## Classification new - cluster ca
cluster6 <- knn(train = tr_x6, test = ts3, cl = tr_y6, k=3, prob=TRUE)

## Classification new - cluster cd
cluster5 <- knn(train = tr_x5, test = ts3, cl = tr_y5, k=3, prob=TRUE)

## Classification new - cluster ad
cluster4 <- knn(train = tr_x4, test = ts3, cl = tr_y4, k=3, prob=TRUE)

## Classification new - cluster cad
cluster3 <- knn(train = tr_x3, test = ts3, cl = tr_y3, k=3, prob=TRUE)

## Classification new - cluster cdr
#cluster2 <- knn(train = tr_x2, test = ts3, cl = tr_y2, k=3, prob=TRUE)

## Classification new - cluster adr
#cluster1 <- knn(train = tr_x1, test = ts3, cl = tr_y1, k=3, prob=TRUE)

#new <- cbind(test3_data, cluster6, cluster5, cluster4, cluster3, cluster2, cluster1)
new <- cbind(test3_data, cluster6, cluster5, cluster4, cluster3)


write.csv(tmap, "cluster_tmap_4.csv")
write.csv(virtual, "cluster_virtual_4.csv")
write.csv(new, "cluster_new_4.csv")

##############################################################################################################################

## model = origin // data = tmap & virtual // goal = data's result prediction

## DSI

rm(list=ls())

origin <- read.csv("cluster_origin_4.csv")                         # 2000,  7
tmap <- read.csv("cluster_tmap_4.csv")                             # 15,    7
virtual <- read.csv("cluster_virtual_4.csv")                       # 182,   7
new <- read.csv("cluster_new_4.csv")                               # 12,    7

## regression
mod1 <- lm(result ~., data = origin)

## show model's summary
summary(mod1)

## setting data
answer <- origin[, "result"]
compl <- origin[, "compl"]
accel <- origin[, "accel"]
decel <- origin[, "decel"]
clust3 <- origin[, "cluster_ca"]
clust2 <- origin[, "cluster_cd"]
clust1 <- origin[, "cluster_ad"]
clust0 <- origin[, "cluster_cad"]

## create model
#prediction <- compl * coef(mod1)[2] + accel * coef(mod1)[3] + decel * coef(mod1)[4] + clust3 * coef(mod1)[5] +
# clust2 * coef(mod1)[5] + clust1 * coef(mod1)[6] + clust0 * coef(mod1)[7] + coef(mod1)[1]
prediction <- compl * coef(mod1)[2] + accel * coef(mod1)[3] + decel * coef(mod1)[4] +
  clust3 * coef(mod1)[5] + clust2 * coef(mod1)[6] + clust1 * coef(mod1)[7] + clust0 * coef(mod1)[8] + coef(mod1)[1]
prediction <- round(prediction, 1)

## compare with real answer
compare <- cbind(prediction, answer, abs(prediction-answer))
#compare <- cbind(compl, accel, decel, clust3, clust2, clust1, clust0, compare)
compare <- cbind(compl, accel, decel, clust3, clust2, clust1, clust0, compare)
#colnames(compare) <- c("compl", "accel", "decel", "clust3", "clust_cd", "clust_ca", "clust_ad", "pred", "answer", "loss")
colnames(compare) <- c("compl", "accel", "decel", "clust_ca", "clust_cd", "clust_ad", "clust_cad", "pred", "answer", "loss")
compare

## calculate RMSE
RMSE <- sqrt( sum((prediction-answer)^2)/nrow(compare) )
RMSE

##############################################################################################################################

## model = origin // data = tmap & virtual // goal = data's result prediction

## RF

rm(list=ls())

library(randomForest)

origin <- read.csv("cluster_origin_4.csv")                         # 2000,  7
tmap <- read.csv("cluster_tmap_4.csv")                             # 15,    7
virtual <- read.csv("cluster_virtual_4.csv")                       # 182,   7
new <- read.csv("cluster_new_4.csv")                               # 12,    7

ds.train <- origin[,]
ds.test <- tmap[,]

d_score.rf <- randomForest(result ~ ., data=ds.train, ntree = 5000)
d_score.rf

d_score.pred <- predict(d_score.rf, ds.test)
d_score.pred <- round(d_score.pred, 1)

## compare with real answer
compare <- cbind(d_score.pred, ds.test[, "result"], abs(d_score.pred-ds.test[,"result"]))
compare <- cbind(ds.test[,c(1:3,5:8)], compare)
colnames(compare) <- c("compl","accel","decel","clust_ca","clust_cd","clust_ad", "clust_cad", "pred", "answer", "loss")
compare

## calculate RMSE
RMSE <- sqrt( sum( ( d_score.pred-ds.test[, "result"] )^2 )  / nrow(compare) )
RMSE

##############################################################################################################################

## model = origin // data = tmap & virtual // goal = data's result prediction

## SVM

rm(list=ls())

library(e1071)

origin <- read.csv("cluster_origin_4.csv")                         # 2000,  7
tmap <- read.csv("cluster_tmap_4.csv")                             # 15,    7
virtual <- read.csv("cluster_virtual_4.csv")                       # 182,   7
new <- read.csv("cluster_new_4.csv")                               # 12,    7

x_train <- origin[, -4]
y_train <- origin[, 4]
x_test <- new[, -4]
y_test <- new[, 4]

# training with train data
#model <- svm(x_train, y_train, type = "nu-regression")
model <- svm(x_train, y_train)
summary(model)

# test with test data
pred <- predict(model, x_test)
pred <- round(pred, 1)

## compare with real answer
compare <- cbind(x_test[,], pred, y_test, abs(pred-y_test))
colnames(compare) <- c("compl","accel","decel","clust_ca","clust_cd","clust_ad", "clust_cad", "pred", "answer", "loss")
compare

## calculate RMSE
RMSE <- sqrt( sum( ( pred-y_test )^2 )  / nrow(compare) )
RMSE

##############################################################################################################################
