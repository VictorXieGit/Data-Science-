# g?n?ration al?atoire des donn?es ? classifier
# avec une premi?re moiti? dans une classe et la seconde dans une autre classe
library(class)
nn = 500
x <- rbind(matrix(rnorm(2*nn, mean = 0.1, sd = 0.3), ncol = 2),
           matrix(rnorm(2*nn, mean = 1, sd = 0.3), ncol = 2))
colnames(x) <- c("x", "y")
z <- rbind(matrix(1,nn, 1), matrix(2,nn, 1))
colnames(z) <- c("z")

donnees <- cbind(x,z)
donnees

plot(donnees[,1:2], col = z)

# on pr?pare un sous-ensemble d'apprentissage et un autre de test
n <- nrow(donnees)

I <- sample(1:n,(2*n)/3)
J <- setdiff(1:n,I)

# on pr?pare les donn?es : on construit le classifieur K-NN
# pour les valeurs num?rique et on extrait explicitement la classe 
# ? predire

cl <- donnees[I,3]

dlrn <- donnees[I,1:2]
dtest <- donnees[J,1:2]

library (class)

mknn1 <- knn(dlrn, dtest,cl, k=1)
mknn1
table(mknn1, donnees[J,3])


mknn3 <- knn(dlrn, dtest,cl, k=3)
mknn3
table(mknn3, donnees[J,3])

mknn7 <- knn(dlrn, dtest,cl, k=7)
mknn7
table(mknn7, donnees[J,3])

mknn11 <- knn(dlrn, dtest,cl, k=11)
mknn11
table(mknn11, donnees[J,3])

min <- 1
for (k in 1:5)
{
  ki <- (k-1)*2 +1
  modell <- knn(dlrn, dtest,cl, ki)
  modell
  tt <- table(donnees[J,3], modell)
  tt
  err <- (tt[1,2] + tt[2,1])/nn
  if (err < min)
  {
    kmin <- ki
    min <- err
  }
}

kmin
min

xx <- c(0.1,0.2)
knn(dlrn, xx,cl, k=kmin)


# validation crois?e

train <- donnees[,1:2]
cl <- donnees[,3]
model <- knn.cv(train,cl,k=3)
model
table(cl,model)


library(caret)
confusionMatrix(cl,model)

## m?thode LVQ (Learning Vector Quantitation ) avec une classification par prototypes
cl <- as.factor(cl)
cd <- lvqinit(train, cl, 10)

cd

lv1 <- lvqtest(cd, train)
table(cl,lv1)

cd0 <- olvq1(train, cl, cd)
lv2 <- lvqtest(cd0, train)
table(cl,lv2)

cd1 <- lvq1(train, cl, cd0)
lv3 <- lvqtest(cd1, train)
table(cl,lv3)

knn(cd1$x, xx,cd1$cl, k=kmin)
