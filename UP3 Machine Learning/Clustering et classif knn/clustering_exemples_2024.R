library(stats)
library(graphics)
library(cluster)
library(fpc)

### exemple pour des donn?es al?atoire
nn = 50
x <- rbind(matrix(rnorm(2*nn, mean = 0.5, sd = 0.3), ncol = 2),
           matrix(rnorm(2*nn, mean = 4, sd = 0.3), ncol = 2), 
           matrix(rnorm(2*nn, mean = 2, sd = 0.5), ncol = 2))
colnames(x) <- c("x", "y")
plot(x, col = cbind(matrix(1,50,1), matrix(2,50,1), matrix(3,50,1)))
title('Distribution initiale')
n <- dim(x)[1]

cl3 <- kmeans(x,3)
plot(x, col=cl3$cluster)
title("Apres le k-means")
points(cl3$centers, col = 'yellow', pch = 8, cex=2)


cl3
cl3$tot.withinss

### K-medoid (pam)

cp <- pam(x,3)
plot(cp)
cp

### exemple pour tenter ? determiner le bon nombre de clusters
set.seed(2024)
nn = 50
x <- rbind(matrix(rnorm(2*nn, mean = 0, sd = 0.3), ncol = 2),
           matrix(rnorm(2*nn, mean = 4, sd = 0.3), ncol = 2), 
           matrix(rnorm(2*nn, mean = 2.5, sd = 0.3), ncol = 2),
           matrix(rnorm(2*nn, mean = -1.5, sd = 0.3), ncol = 2))
colnames(x) <- c("x", "y")
plot(x, col = 6)
title('Distribution initiale')
n <- dim(x)[1]
set.seed(1500)
K = 2:10;
J<- matrix(0,length(K),1);
JJ<- matrix(0,length(K),1);
for (k in K)
{
  cl <- kmeans(x,k)
  plot(x, col=cl$cluster)
  
  title(paste("Apres le k-means pour k =", k))
  #points(cl$centers, col = 'yellow', pch = 8, cex=2)
  
  J[k-1] <- 1/n * cl$tot.withinss
  xx <- x- cl$center[cl$cluster]
  JJ[k-1] <- 1/n * sum(xx * xx)
}

plot(K,JJ, type='l')
points(K,J)

library(amap)

### clustering hierarchique
# on tente d'appliquer sur les m?me donn?es
x<- as.data.frame(x)
md <- Dist(x)
hh <- hclust(md,'complete')
plot(hh)

hh <- hclust(md,'ward')
plot(hh)


### mieux comprendre les m?thodes 

set.seed(10)
nn = 5
petitx <- rbind(matrix(rnorm(2*nn, mean = 0.5, sd = 0.3), ncol = 2),
                matrix(rnorm(2*nn, mean = 4, sd = 0.3), ncol = 2), 
                matrix(rnorm(2*nn, mean = 2, sd = 0.5), ncol = 2))
colnames(petitx) <- c("x", "y")
plot(petitx, col = cbind(matrix(1,nn,1), matrix(2,nn,1), matrix(3,nn,1)))
title('Distribution initiale')
n <- dim(petitx)[1]

md <- dist(petitx)
hclust_methods <- c("ward", "single", "complete", "average", "mcquitty", 
                    "median", "centroid")
for(i in seq_along(hclust_methods)) {
  hh <- hclust(md, method = hclust_methods[i])   
  plot(hh)
}

library(fpc)
db_x <- dbscan(x,eps=0.2,MinPts=3)
db_x
db_x$cluster
plot(x, col = db_x$cluster + 1)
title("DBSCAN pour eps = 0.2")


db_x <- dbscan(x,eps=0.3,MinPts=3)
db_x
db_x$cluster
plot(x, col = db_x$cluster + 1)
title("DBSCAN pour eps = 0.3")

db_x <- dbscan(x,eps=0.1,MinPts=3)
db_x
db_x$cluster
plot(x, col = db_x$cluster + 1)
title("DBSCAN pour eps = 0.1")


db_x <- dbscan(x,eps=0.3,MinPts=10)
db_x
db_x$cluster
plot(x, col = db_x$cluster + 1)
title("DBSCAN pour eps = 0.1 et voisinage ? 10")

