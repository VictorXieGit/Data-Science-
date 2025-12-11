# Majeure Science des données 2025-2026
# UP2 - Apprentissage statistique
# TP2 sur la Régression LASSO
# Code R pour répondre aux questions du TP2
# Dernière partie très incomplète...

# PARTIE 1 : sur la méthode MCO lorsque p > n !

# Question 1
# Chargement des données

data <- read.table(file="data_DNA.txt", header=TRUE)

# Préparation des données
head(data[,1:8])
n <- dim(data)[1]
p <- dim(data)[2] - 1 # p > n !
y_data <- data$y
X_data <- data[,2:(p+1)]

# Centrage + réduction seulement pour X
X <- scale(X_data, center = TRUE, scale = TRUE) 
y <- scale(y_data, center = TRUE, scale = FALSE)

# Question 2

XpX <- crossprod(X) # calcule la matrice X'X
dim(XpX)
beta_MCO <- solve(XpX, t(X)%*%y) # RLM avec solve()

# Question 3

# RLM avec lm()
model_lm <- lm(y ~ . -1, data=as.data.frame(X))
summary(model_lm)
plot(model_lm$fitted.values, y)
abline(a=0, b=1, col='red', lwd=2)
grid()
title('Sur-apprentissage')

# Question 4 : on regarde la solution ridge lorsque lambda est proche de 0

lambda <- exp((-20):5) # choix des valeurs de lambda
beta.ridge <- matrix(0, ncol=p, nrow=length(lambda)) # initialisation

for (k in 1:length(lambda)) {
  beta.ridge[k,] <- solve(XpX + diag(lambda[k],p), t(X)%*%y)
}

# Chemins de régularisation ridge
matplot(log(lambda), beta.ridge[,1:10], type='b', pch=19, col=c("black","red"), 
        xlab = expression(paste(log(lambda)," (paramètre de régularisation)")),
        ylab = "Coefficients (estimés)")  
grid()
title('Chemins de régularisation')

# valeur limite
beta_ridge0 <- beta.ridge[1, ]
y_ajuste <- X%*%beta_ridge0
plot(y_ajuste, y)
abline(a=0, b=1, col='red', lwd=2)
grid()
title('Sur-apprentissage')

# Comparaison des normes 2
beta_MCO_lm <- model_lm$coefficients
index <- is.na(beta_MCO_lm)
beta_MCO_lm[index] <- 0

norm(beta_MCO_lm, type='2')
norm(beta_ridge0, type='2')

# PARTIE 2 : régression ridge lorsque p > n 

library(glmnet)

# On revient aux données initiales sous format matrice
X <- as.matrix(X_data)
y <- as.matrix(y_data)

model_ridge <- glmnet(X, y, alpha = 0)
plot(model_ridge, xvar="lambda")
grid()

model_ridge$lambda # choix par défaut des lambda

# On modifie la plage des valeurs de lambda
lambda <- exp(-((-5):20))
model_ridge <- glmnet(X, y, alpha = 0, lambda=lambda)
plot(model_ridge, xvar="lambda")
grid()

# Choix du paramètre de régularisation par validation croisée
set.seed(2024)
lambda <- exp( -seq(-5, 5, 0.2) )
Lridge <- cv.glmnet(X, y, alpha=0, lambda=lambda)
plot(Lridge)
grid()

lambda_min <- Lridge$lambda.min
lambda_1se <- Lridge$lambda.1se

# MSE associée à lambda_min
index <- which.min(Lridge$cvm)
MSE_ridge <- Lridge$cvm[index]
MSE_ref <- var(y)

# Réponse y contre réponse prédite
coeffs.ridge.cv = coef(Lridge, s="lambda.min")
print(coeffs.ridge.cv)
X1 <- cbind(rep(1,n),X)
y_ajuste <- X1%*%coeffs.ridge.cv
plot(y_ajuste, y)
abline(a=0, b=1, col='red', lwd=2)
title('Réponse réelle contre réponse prédite par ridge')
grid()

# PARTIE 3 : régression LASSO lorsque p > n 

model_LASSO <- glmnet(X, y, alpha = 1)
plot(model_LASSO, xvar="lambda")
grid()

# On modifie la plage des valeurs de lambda
lambda <- exp(-(1:10))
model_LASSO <- glmnet(X, y, alpha = 1, lambda=lambda)
plot(model_LASSO, xvar="lambda")
grid()

# Choix du paramètre de régularisation par validation croisée
set.seed(2024)
lambda <- exp( -seq(2, 10, 0.2) )
LLASSO <- cv.glmnet(X, y, alpha=1, lambda=lambda)
plot(LLASSO)
grid()

lambda_min <- LLASSO$lambda.min
lambda_1se <- LLASSO$lambda.1se

# MSE associée à lambda_min
index <- which.min(LLASSO$cvm)
MSE_LASSO <- LLASSO$cvm[index]

# Réponse y contre réponse prédite
coeffs.LASSO.cv = coef(LLASSO, s="lambda.min")
print(coeffs.LASSO.cv)
X1 <- cbind(rep(1,n),X)
y_ajuste <- X1%*%coeffs.LASSO.cv
plot(y_ajuste, y)
abline(a=0, b=1, col='red', lwd=2)
title('Réponse réelle contre réponse prédite par LASSO')
grid()

# PARTIE 4

set.seed(2024)
n.app <- 0.8*n
index_app <- sample(1:n, n.app, replace=FALSE)

# Données d'apprentissage
X.app <- X[index_app,]
y.app <- y[index_app,1]

# Données test
X.test <- X[-index_app,]
y.test <- y[-index_app,1]

# Modèle LASSO
lasso <- glmnet(X.app, y.app)

# Modèle RIDGE
ridge <- glmnet(X.app, y.app, alpha = 0)

# Chemins de régularisation
par(mfrow=c(1,2))
plot(lasso, xvar="lambda"); grid()
plot(ridge, xvar="lambda"); grid()
par(mfrow=c(1,1))

# Sélection du paramètre de régularisation lambda
set.seed(2024)
Llasso <- cv.glmnet(X.app, y.app)
lambda.lasso <- Llasso$lambda.min

Lridge <- cv.glmnet(X.app, y.app, alpha=0)
lambda.ridge <- Lridge$lambda.min

par(mfrow=c(1,2))
plot(Llasso)
plot(Lridge)
par(mfrow=c(1,1))

# Prédictions
pred.lasso <- predict(Llasso, newx=X.test, s=lambda.lasso)
pred.ridge <- predict(Lridge, newx=X.test, alpha=0, s=lambda.ridge)

par(mfrow=c(1,2))
plot(y.test ~ pred.lasso, pch=1)
abline(a=0,b=1,col='red',lwd=2)
plot(y.test ~ pred.ridge, pch=1)
abline(a=0,b=1,col='red',lwd=2)
par(mfrow=c(1,1))

n.test <- n - n.app
RMSE.lasso <- sqrt( sum( (y.test - pred.lasso)^2 )/n.test )
RMSE.ridge <- sqrt( sum( (y.test - pred.ridge)^2 )/n.test )
print(RMSE.lasso)
print(RMSE.ridge)
RMSE.ref <- sd(y)

