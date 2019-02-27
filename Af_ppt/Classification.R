
##############################################################################################################################

rm(list=ls())

require(class)

## Preparing training data set
train_data <- read.csv("cluster_fil11.csv")

head(train_data)
dim(train_data)


## for ca cluster
tr_x4 <- train_data[, -c(4:8)]

## for cd cluster
tr_x3 <- train_data[, -c(4:8)]

## for ad cluster
tr_x2 <- train_data[, -c(4:8)]

## for cad cluster
tr_x1 <- train_data[, -c(4:8)]



## cluster_ca
tr_y4 <- train_data[, 4]

## cluster_cd
tr_y3 <- train_data[, 5]

## cluster_ad
tr_y2 <- train_data[, 6]

## cluster_cad
tr_y1 <- train_data[, 7]


## Preparing Tmap data set - 12ea, k=3
test1_data <- read.csv("driving_score_new.csv")
ts1 <- test1_data[, -4]


## Classification data

## Classification new - cluster ca
cluster4 <- knn(train = tr_x4, test = ts1, cl = tr_y4, k=3)

## Classification new - cluster cd
cluster3 <- knn(train = tr_x3, test = ts1, cl = tr_y3, k=3)

## Classification new - cluster ad
cluster2 <- knn(train = tr_x2, test = ts1, cl = tr_y2, k=3)

## Classification new - cluster cad
cluster1 <- knn(train = tr_x1, test = ts1, cl = tr_y1, k=3)

new <- cbind(test1_data[, c(1:3)], cluster4, cluster3, cluster2, cluster1, test1_data[, 4])
colnames(new) <- c("compliance", "acceleration", "deceleration", "clust_ca", 
                      "clust_cd", "clust_ad", "clust_cad", "result")

new

## save result to csv file
write.csv(new, "cluster_new_fil11.csv")

##############################################################################################################################
