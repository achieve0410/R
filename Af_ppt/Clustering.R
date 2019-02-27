
##############################################################################################################################

## origin data clustering

rm(list=ls())

library("NbClust")

m_data <- read.csv("driving_score_fil11.csv")

head(m_data)
dim(m_data)

v_data <- scale(m_data[, c(1, 2)]) ## compliance & acceleration
t_data <- scale(m_data[, c(1, 3)]) ## compliance & deceleration
s_data <- scale(m_data[, c(2, 3)]) ## acceleration & deceleration
n_data <- scale(m_data[, -4]) ## compliance & acceleration & deceleration



######################################################
## sum of squares

#wssplot <- function(data, nc=15, seed=1234){
#  wss <- (nrow(data)-1)*sum(apply(data,2,var))
#  for (i in 2:nc){
#    set.seed(seed)
#    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
#  plot(1:nc, wss, type="b", xlab="Number of Clusters",
#       ylab="Within groups sum of squares")}

#wssplot(v_data)
######################################################


## Find the best # of Clust
ca_nbclust <- NbClust(v_data, distance = "euclidean",
                      min.nc = 2, max.nc = 15, 
                      method = "kmeans")
cd_nbclust <- NbClust(t_data, distance = "euclidean",
                      min.nc = 2, max.nc = 15, 
                      method = "kmeans")
ad_nbclust <- NbClust(s_data, distance = "euclidean",
                      min.nc = 2, max.nc = 15, 
                      method = "kmeans")
cad_nbclust <- NbClust(n_data, distance = "euclidean",
                       min.nc = 2, max.nc = 15, 
                       method = "kmeans")

## compliance & acceleration clustering
rst1 <- kmeans(x=v_data, centers=15, iter.max = 100, nstart = 1)


## compliance & deceleration clustering
rst2 <- kmeans(x=t_data, centers=2, iter.max = 100, nstart = 1)


## acceleration & deceleration clustering
rst3 <- kmeans(x=s_data, centers=13, iter.max = 100, nstart = 1)


## compliance & acceleration & deceleration clustering
rst4 <- kmeans(x=n_data, centers=2, iter.max = 100, nstart = 1)


## make dataset
clust1 <- rst1[1]
clust2 <- rst2[1]
clust3 <- rst3[1]
clust4 <- rst4[1]

result <- cbind(m_data[ , c(1:3)], clust1, clust2, clust3, clust4, m_data[, 4])
colnames(result) <- c("compliance", "acceleration", "deceleration", "clust_ca", 
                      "clust_cd", "clust_ad", "clust_cad", "result")
result

## save dataset to csv file
write.csv(result, "cluster_fil11.csv")

##############################################################################################################################
