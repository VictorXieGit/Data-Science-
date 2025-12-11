library(stats)
library(graphics)
library(cluster)
library(fpc)
setwd("C:\\Users\\victo\\OneDrive\\Bureau\\COURS général\\cours mine 2526\\sdd\\UP3 Machine Learning\\Clustering et classif knn\\Données pour le TP-20251118")
data <- read.csv(file = "md_classes (3).csv", header = TRUE,sep =";" )
data <- data[,-c(1,ncol(data))]
### exemple pour des donn?es al?atoire
nn = 50
x <- rbind(matrix(rnorm(2*nn, mean = 0.5, sd = 0.3), ncol = 2),
           matrix(rnorm(2*nn, mean = 4, sd = 0.3), ncol = 2), 
           matrix(rnorm(2*nn, mean = 2, sd = 0.5), ncol = 2))
colnames(x) <- c("x", "y")
plot(x, col = cbind(matrix(1,50,1), matrix(2,50,1), matrix(3,50,1)))
title('Distribution initiale')
n <- dim(x)[1]

distance <- function(a, b) sqrt(sum((a - b)^2))

ownkmean <- function(data, k,iteration){
  n <- nrow(data)
  Ks <- data[sample(nrow(data),k),]
  affect <- integer(n)
  for (i in 1:iteration){
    for (p in 1:n){
      
      distances <- apply(Ks,1, function(x)  distance(x, data[p,]))
      idx <- which.min(distances)
      affect[p] <- idx
    }
    for(l in 1:k){
      Ks[l, ] <- colMeans(data[affect == l, , drop = FALSE])    }
    }
  return (affect)
}

affectation <- ownkmean(x,3,10)

colors <- rainbow(max(affectation))

plot(x[,1], x[,2], type = "n", xlab = "X1", ylab = "X2")  # canvas vide

for(i in 1:max(affectation)) {
  points(x[affectation == i, 1], x[affectation == i, 2], col = colors[i], pch = 19)
}

# max K à tester
Kmax <- 10

# vecteur pour stocker les inerties
inerties <- numeric(Kmax)

for (k in 1:Kmax) {
  km <- kmeans(data, centers = k, nstart = 10)
  # inertie = somme des distances au carré (tot.withinss)
  inerties[k] <- km$tot.withinss
}

# tracer la courbe
plot(1:Kmax, inerties, type = "b", pch = 19,
     xlab = "Nombre de clusters K",
     ylab = "Inertie intra-cluster",
     main = "Méthode du coude")

affectdata <- ownkmean(data,15,10)
colors <- rainbow(max(affectdata))

plot(data[,1], data[,2], type = "n", xlab = "X1", ylab = "X2")  # canvas vide

for(i in 1:max(affectation)) {
  points(data[affectdata == i, 1], data[affectdata == i, 2], col = colors[i], pch = 19)
}

meth <- kmeans(data,15)
plot(x, col=cl3$cluster)
title("Apres le k-means")
points(cl3$centers, col = 'yellow', pch = 8, cex=2)



# Charger les données
data(iris)
head(iris)

# On prend uniquement les variables numériques
X <- iris[, 1:4]

# Les classes réelles pour comparaison
true_labels <- iris$Species
set.seed(123)
k <- 3
km <- kmeans(X, centers = k, nstart = 25)

# cluster assignés
km$cluster

# Comparaison avec les vraies classes
table(km$cluster, true_labels)

# Visualisation (2 premières dimensions)
plot(X[,1], X[,2], col = km$cluster, pch=19,
     xlab="Sepal.Length", ylab="Sepal.Width",
     main="K-means clustering")
points(km$centers[,1:2], col=1:3, pch=8, cex=2)

# Calcul de la distance
dist_X <- dist(X)

# Clustering hiérarchique
hc <- hclust(dist_X, method="ward.D2")

# Dendrogramme
plot(hc, labels = FALSE, main="Dendrogramme CHA")

# Découper en 3 clusters
hc_clusters <- cutree(hc, k = 3)

# Comparaison
table(hc_clusters, true_labels)

# Visualisation
plot(X[,1], X[,2], col = hc_clusters, pch=19,
     xlab="Sepal.Length", ylab="Sepal.Width",
     main="Clustering hiérarchique")

install.packages("dbscan")   # si pas déjà installé
library(dbscan)

# epsilon choisi avec kNNdistplot éventuellement
db <- dbscan(X, eps = 0.5, minPts = 3)

# Clusters assignés (0 = bruit)
db$cluster

# Comparaison
table(db$cluster, true_labels)

# Visualisation
plot(X[,1], X[,2], col = db$cluster + 1, pch=19,
     xlab="Sepal.Length", ylab="Sepal.Width",
     main="DBSCAN clustering")

library(class)

# Diviser en train/test (70/30)
set.seed(123)
train_idx <- sample(1:nrow(X), 0.7*nrow(X))
X_train <- X[train_idx, ]
y_train <- true_labels[train_idx]
X_test <- X[-train_idx, ]
y_test <- true_labels[-train_idx]

# Appliquer K-NN
k_nn <- 5
y_pred <- knn(X_train, X_test, y_train, k = k_nn)

# Matrice de confusion
table(y_test, y_pred)

