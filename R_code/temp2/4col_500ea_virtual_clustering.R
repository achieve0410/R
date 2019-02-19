
## origin data clustering

rm(list=ls())

library("NbClust")

m_data <- read.csv("driving_score_180ea.csv")

head(m_data)
dim(m_data)

b_data <- scale(m_data[, 4]) ## result
v_data <- scale(m_data[, c(1, 4)]) ## compliance & result
t_data <- scale(m_data[, c(2, 4)]) ## acceleration & result
s_data <- scale(m_data[, c(3, 4)]) ## deceleration & result
n_data <- scale(m_data[, -3]) ## compliance & acceleration & result
f_data <- scale(m_data[, -2]) ## compliance & deceleration & result
r_data <- scale(m_data[, -1]) ## acceleration & deceleration & result
h_data <- scale(m_data[,]) ## compliance & acceleration & deceleration & result


r_nbclust <- NbClust(b_data, distance = "manhattan",
                      min.nc = 2, max.nc = 25, 
                      method = "kmeans")
cr_nbclust <- NbClust(v_data, distance = "manhattan",
                      min.nc = 2, max.nc = 25, 
                      method = "kmeans")
ar_nbclust <- NbClust(t_data, distance = "manhattan",
                      min.nc = 2, max.nc = 25, 
                      method = "kmeans")
dr_nbclust <- NbClust(s_data, distance = "manhattan",
                       min.nc = 2, max.nc = 25, 
                       method = "kmeans")
car_nbclust <- NbClust(n_data, distance = "manhattan",
                     min.nc = 2, max.nc = 25, 
                     method = "kmeans")
cdr_nbclust <- NbClust(f_data, distance = "manhattan",
                     min.nc = 2, max.nc = 25, 
                     method = "kmeans")
adr_nbclust <- NbClust(r_data, distance = "manhattan",
                     min.nc = 2, max.nc = 25, 
                     method = "kmeans")
cadr_nbclust <- NbClust(h_data, distance = "manhattan",
                     min.nc = 2, max.nc = 25, 
                     method = "kmeans")


## result clustering
rst1 <- kmeans(b_data, centers=4, iter.max = 1000, nstart = 2, algorithm = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"))


## compliance & result clustering
rst2 <- kmeans(v_data, centers=24, iter.max = 1000, nstart = 2, algorithm = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"))


## acceleration & result clustering
rst3 <- kmeans(t_data, centers=23, iter.max = 1000, nstart = 2, algorithm = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"))


## deceleration & result clustering
rst4 <- kmeans(s_data, centers=2, iter.max = 1000, nstart = 2, algorithm = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"))


## compliance & acceleration & result clustering
rst5 <- kmeans(n_data, centers=2, iter.max = 1000, nstart = 2, algorithm = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"))


## compliance & deceleration & result clustering
rst6 <- kmeans(f_data, centers=2, iter.max = 1000, nstart = 2, algorithm = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"))


## acceleration & deceleration & result clustering
rst7 <- kmeans(r_data, centers=2, iter.max = 1000, nstart = 2, algorithm = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"))


## compliance & acceleration & deceleration & result clustering
rst8 <- kmeans(h_data, centers=2, iter.max = 1000, nstart = 2, algorithm = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"))


clust1 <- rst1[1]
clust2 <- rst2[1]
clust3 <- rst3[1]
clust4 <- rst4[1]
clust5 <- rst5[1]
clust6 <- rst6[1]
clust7 <- rst7[1]
clust8 <- rst8[1]

rsult <- cbind(m_data, clust1, clust2, clust3, clust4, clust5, clust6, clust7, clust8)

write.csv(rsult, "cluster_origin_1.csv")

origin <- read.csv("cluster_origin_1.csv")

head(origin)
dim(origin)

##############################################################################################################################

## Tmap, virtual, new data classification

rm(list=ls())

require(class)

## Preparing training data set
train_data <- read.csv("cluster_origin_1.csv")

head(train_data)


## for r cluste6r
tr_x8 <- train_data[, -c(4:12)]

## for cr cluster
tr_x7 <- train_data[, -c(4:12)]

## for ar cluster
tr_x6 <- train_data[, -c(4:12)]

## for dr cluster
tr_x5 <- train_data[, -c(4:12)]

## for car cluster
tr_x4 <- train_data[, -c(4:12)]

## for cdr cluster
tr_x3 <- train_data[, -c(4:12)]

## for adr cluster
tr_x2 <- train_data[, -c(4:12)]

## for cadr cluster
tr_x1 <- train_data[, -c(4:12)]



## cluster_r
tr_y8 <- train_data[, 5]

## cluster_cr
tr_y7 <- train_data[, 6]

## cluster_ar
tr_y6 <- train_data[, 7]

## cluster_dr
tr_y5 <- train_data[, 8]

## cluster_car
tr_y4 <- train_data[, 9]

## cluster_cdr
tr_y3 <- train_data[, 10]

## cluster_adr
tr_y2 <- train_data[, 11]

## cluster_cadr
tr_y1 <- train_data[, 12]



## Preparing Tmap data set - 15ea, k=3
test1_data <- read.csv("Tmap_data.csv")
ts1 <- test1_data[, -4]

## Preparing Virtual data set - 121 , k=11
#test2_data <- read.csv("driving_score_121.csv")
#ts2 <- test2_data[, -4]

## Preparing new data set - 13 , k=3
test3_data <- read.csv("driving_score_new.csv")
ts3 <- test3_data[, -4]



## Classification Tmap

## Classification Tmap - cluster ca
cluster8 <- knn(train = tr_x8, test = ts1, cl = tr_y8, k=3, prob=TRUE)

## Classification Tmap - cluster cd
cluster7 <- knn(train = tr_x7, test = ts1, cl = tr_y7, k=3, prob=TRUE)

## Classification Tmap - cluster ad
cluster6 <- knn(train = tr_x6, test = ts1, cl = tr_y6, k=3, prob=TRUE)

## Classification Tmap - cluster cad
cluster5 <- knn(train = tr_x5, test = ts1, cl = tr_y5, k=3, prob=TRUE)

## Classification Tmap - cluster ca
cluster4 <- knn(train = tr_x4, test = ts1, cl = tr_y4, k=3, prob=TRUE)

## Classification Tmap - cluster cd
cluster3 <- knn(train = tr_x3, test = ts1, cl = tr_y3, k=3, prob=TRUE)

## Classification Tmap - cluster ad
cluster2 <- knn(train = tr_x2, test = ts1, cl = tr_y2, k=3, prob=TRUE)

## Classification Tmap - cluster cad
cluster1 <- knn(train = tr_x1, test = ts1, cl = tr_y1, k=3, prob=TRUE)

## Classification Tmap - cluster adr
#cluster1 <- knn(train = tr_x1, test = ts1, cl = tr_y1, k=3, prob=TRUE)

#tmap <- cbind(test1_data, cluster6, cluster5, cluster4, cluster3, cluster2, cluster1)
tmap <- cbind(test1_data, cluster8, cluster7, cluster6, cluster5, cluster4, cluster3, cluster2, cluster1)



## Classification Origin

## Classification virtual - cluster ca
#cluster6 <- knn(train = tr_x6, test = ts2, cl = tr_y6, k=11, prob=TRUE)

## Classification virtual - cluster cd
#cluster5 <- knn(train = tr_x5, test = ts2, cl = tr_y5, k=11, prob=TRUE)

## Classification virtual - cluster ad
#cluster4 <- knn(train = tr_x4, test = ts2, cl = tr_y4, k=11, prob=TRUE)

## Classification virtual - cluster cad
#cluster3 <- knn(train = tr_x3, test = ts2, cl = tr_y3, k=11, prob=TRUE)

## Classification virtual - cluster cdr
#cluster2 <- knn(train = tr_x2, test = ts2, cl = tr_y2, k=11, prob=TRUE)

## Classification virtual - cluster adr
#cluster1 <- knn(train = tr_x1, test = ts2, cl = tr_y1, k=11, prob=TRUE)

#virtual <- cbind(test2_data, cluster6, cluster5, cluster4, cluster3, cluster2, cluster1)
#virtual <- cbind(test2_data, cluster6, cluster5, cluster4, cluster3)



## Classification new

## Classification new - cluster ca
cluster8 <- knn(train = tr_x8, test = ts3, cl = tr_y8, k=3, prob=TRUE)

## Classification new - cluster cd
cluster7 <- knn(train = tr_x7, test = ts3, cl = tr_y7, k=3, prob=TRUE)

## Classification new - cluster ca
cluster6 <- knn(train = tr_x6, test = ts3, cl = tr_y6, k=3, prob=TRUE)

## Classification new - cluster cd
cluster5 <- knn(train = tr_x5, test = ts3, cl = tr_y5, k=3, prob=TRUE)

## Classification new - cluster ad
cluster4 <- knn(train = tr_x4, test = ts3, cl = tr_y4, k=3, prob=TRUE)

## Classification new - cluster cad
cluster3 <- knn(train = tr_x3, test = ts3, cl = tr_y3, k=3, prob=TRUE)

## Classification new - cluster cdr
cluster2 <- knn(train = tr_x2, test = ts3, cl = tr_y2, k=3, prob=TRUE)

## Classification new - cluster cdr
cluster1 <- knn(train = tr_x1, test = ts3, cl = tr_y1, k=3, prob=TRUE)

## Classification new - cluster adr
#cluster1 <- knn(train = tr_x1, test = ts3, cl = tr_y1, k=3, prob=TRUE)

#new <- cbind(test3_data, cluster6, cluster5, cluster4, cluster3, cluster2, cluster1)
new <- cbind(test3_data, cluster8, cluster7, cluster6, cluster5, cluster4, cluster3, cluster2, cluster1)



#write.csv(tmp, "cluster_tmp_1.csv")
write.csv(tmap, "cluster_tmap_1.csv")
#write.csv(virtual, "cluster_virtual_1.csv")
write.csv(new, "cluster_new_1.csv")

##############################################################################################################################

## model = origin // data = tmap & virtual // goal = data's result prediction

## DSI

rm(list=ls())

origin <- read.csv("cluster_origin_1.csv")                         # 2000,  7
tmap <- read.csv("cluster_tmap_1.csv")                             # 15,    7
#virtual <- read.csv("cluster_virtual_2.csv")                       # 182,   7
new <- read.csv("cluster_new_1.csv")                               # 12,    7

## regression
mod1 <- lm(result ~., data = origin)

## show model's summary
summary(mod1)

## setting data
answer <- origin[, "result"]
compl <- origin[, "compl"]
accel <- origin[, "accel"]
decel <- origin[, "decel"]
clust8 <- origin[, "cluster_r"]
clust7 <- origin[, "cluster_cr"]
clust6 <- origin[, "cluster_ar"]
clust5 <- origin[, "cluster_dr"]
clust4 <- origin[, "cluster_car"]
clust3 <- origin[, "cluster_cdr"]
clust2 <- origin[, "cluster_adr"]
clust1 <- origin[, "cluster_cadr"]

## create model
#prediction <- compl * coef(mod1)[2] + accel * coef(mod1)[3] + decel * coef(mod1)[4] + clust3 * coef(mod1)[5] +
# clust2 * coef(mod1)[5] + clust1 * coef(mod1)[6] + clust0 * coef(mod1)[7] + coef(mod1)[1]
prediction <- compl * coef(mod1)[2] + accel * coef(mod1)[3] + decel * coef(mod1)[4] +
  clust8 * coef(mod1)[5] + clust7 * coef(mod1)[6] + clust6 * coef(mod1)[7] + clust5 * coef(mod1)[8] + 
  clust4 * coef(mod1)[9] + clust3 * coef(mod1)[10] + clust2 * coef(mod1)[11] + clust1 * coef(mod1)[12] + coef(mod1)[1]
prediction <- round(prediction, 1)

prediction

## compare with real answer
compare <- cbind(prediction, answer, abs(prediction-answer))
#compare <- cbind(compl, accel, decel, clust3, clust2, clust1, clust0, compare)
compare <- cbind(compl, accel, decel, compare)
#colnames(compare) <- c("compl", "accel", "decel", "clust3", "clust_cd", "clust_ca", "clust_ad", "pred", "answer", "loss")
colnames(compare) <- c("compl", "accel", "decel", "pred", "answer", "loss")
compare

#write.csv(compare, "121_DSI.csv")

## calculate RMSE
RMSE <- sqrt( sum((prediction-answer)^2)/nrow(compare) )
RMSE

##############################################################################################################################

## model = origin // data = tmap & virtual // goal = data's result prediction

## RF

rm(list=ls())

library(randomForest)

origin <- read.csv("cluster_origin_1.csv")                         # 2000,  7
tmap <- read.csv("cluster_tmap_1.csv")                             # 15,    7
#virtual <- read.csv("cluster_virtual_1.csv")                       # 182,   7
new <- read.csv("cluster_new_1.csv")                               # 12,    7

ds.train <- origin[,]
ds.test <- origin[,]

d_score.rf <- randomForest(result ~ ., data=ds.train, ntree = 5000)
d_score.rf

d_score.pred <- predict(d_score.rf, ds.test)
d_score.pred <- round(d_score.pred, 1)

## compare with real answer
compare <- cbind(d_score.pred, ds.test[, "result"], abs(d_score.pred-ds.test[,"result"]))
compare <- cbind(ds.test[,c(1:3)], compare)
colnames(compare) <- c("compl", "accel", "decel", "pred", "answer", "loss")
compare

#write.csv(compare, "121_RF.csv")

## calculate RMSE
RMSE <- sqrt( sum( ( d_score.pred-ds.test[, "result"] )^2 )  / nrow(compare) )
RMSE

##############################################################################################################################

## model = origin // data = tmap & virtual // goal = data's result prediction

## SVM

rm(list=ls())

library(e1071)

origin <- read.csv("cluster_origin_1.csv")                         # 2000,  7
tmap <- read.csv("cluster_tmap_1.csv")                             # 15,    7
#virtual <- read.csv("cluster_virtual_1.csv")                       # 182,   7
new <- read.csv("cluster_new_1.csv")                               # 12,    7

x_train <- origin[, -4]
y_train <- origin[, 4]
x_test <- origin[, -4]
y_test <- origin[, 4]

# training with train data
#model <- svm(x_train, y_train, type = "nu-regression")
model <- svm(x_train, y_train, type = "nu-regression")
summary(model)

# test with test data
pred <- predict(model, x_test)
pred <- round(pred, 1)

## compare with real answer
compare <- cbind(x_test[,c(1:3)], pred, y_test, abs(pred-y_test))
colnames(compare) <- c("compl", "accel", "decel", "pred", "answer", "loss")
compare

#write.csv(compare, "121_SVM.csv")

## calculate RMSE
RMSE <- sqrt( sum( ( pred-y_test )^2 )  / nrow(compare) )
RMSE

##############################################################################################################################
