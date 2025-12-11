# Majeure Science des données 2025-2026
# UP2 Apprentissage statistique - Méthodes de Régression Avancées
# TP sur la Régression Ridge

# Q1 - Simulation d'un jeu de données (p = 2 prédicteurs)
n <- 200
rho <-  - 0.99
beta1 <- 0.5
beta2 <- -0.5
sigX <- 1
sig <- 1

set.seed(2023)
X1 <- rnorm(n, mean=0, sd=sigX)
X2 <- rho*X1 + sqrt(1 - rho^2)*rnorm(n, mean=0, sd=sigX)

X <- matrix(c(X1,X2), ncol=2, byrow=FALSE)
colnames(X) <- c("X1","X2")

residus <- rnorm(n, mean=0, sd=sig)
y <- beta1*X1 + beta2*X2 + residus

data <- cbind(y, X)
pairs(data)

summary(lm(y ~ X1 + X2 - 1))

# Q2 - Régression linéaire usuelle (MCO)
XpX <- t(X)%*%X
beta.MCO <- solve(XpX, t(X)%*%y)
print(beta.MCO)

# Comparaison avec lm()
reg <- lm(y ~ X1 + X2 - 1)
summary(reg)

# Erreurs standard
summary(reg)$coefficients[,2]

# Q3 - Régression ridge 
lambda <- seq(0,200,5)
beta.ridge <- matrix(0, ncol=2, nrow=length(lambda)) 

for (k in 1:length(lambda)) {
  beta.ridge[k,] <- solve(XpX + diag(lambda[k],2), t(X)%*%y)
}

# Chemins de régularisation ridge
matplot(lambda, beta.ridge, type='b', pch=19, col=c("black","red"), ylim=c(-0.6,1.2), 
        xlab = expression(paste(lambda," (paramètre de régularisation)")),
        ylab = "Coefficients (estimés)")  
grid()
abline(h=c(-0.5,0.5), lty=2, col='blue', lwd=1)
legend("topleft", expression(beta[1], beta[2]),
            pch=19, col=c("black","red"), horiz=TRUE)
title('Chemins de régularisation')

# Q4 - Choix du paramètre de régularisation lambda
# On simule des données de test

ntest <- 1000
X1.test <- rnorm(ntest, mean=0, sd=sigX)
X2.test <- rho*X1 + sqrt(1 - rho^2)*rnorm(ntest, mean=0, sd=sigX)

X.test <- matrix(c(X1.test,X2.test), ncol=2, byrow=FALSE)
colnames(X.test) <- c("X1","X2")

residus.test <- rnorm(ntest, mean=0, sd=sig)

y.test <- beta1*X1.test + beta2*X2.test + residus.test

RMSE <- rep(0, length(lambda))

for (k in 1:length(lambda)) {
  erreurs <- y.test - (beta.ridge[k,1]*X1.test+beta.ridge[k,2]*X2.test)
  RMSE[k] <- sqrt( sum( erreurs^2 )/ntest )
}

plot(lambda, RMSE, type='b', pch=19, 
     xlab=expression( lambda ))
grid()
title('Choix du paramètre de régularisation')

# Q5 - Détermination du lambda optimal 

k <- which.min(RMSE)
lambda_opt <- lambda[k]
# Coefficients ridge associés
beta.ridge[k,]
# Coefficients estimés initialement par MCO
reg$coefficients

print(lambda_opt)

# Q6 - Biais et risque de l'estimateur RIDGE correspondant

NMC <- 1000 # nombre de simulations Monte-Carlo
beta.RIDGE.MC <- matrix(0, nrow=NMC, ncol=2)
colnames(beta.RIDGE.MC) <- c("beta Ridge 1", "beta Ridge 2")
beta.MCO.MC <- matrix(0, nrow=NMC, ncol=2)
colnames(beta.MCO.MC) <- c("beta MCO 1", "beta MCO 2")

for (k in 1:NMC){
  residusk <- rnorm(n, mean=0, sd=sig)
  yk <- beta1*X1 + beta2*X2 + residusk
  beta.RIDGEk <- solve(XpX + diag(lambda_opt,2), t(X)%*%yk)
  beta.MCOk <- solve(XpX, t(X)%*%yk)
  beta.RIDGE.MC[k,] <- beta.RIDGEk
  beta.MCO.MC[k,] <- beta.MCOk
}

boxplot(cbind(beta.MCO.MC,beta.RIDGE.MC), las=1)
grid()

mu_beta.RIDGE <- colMeans(beta.RIDGE.MC)
print(mu_beta.RIDGE - c(beta1, beta2) )
abline(h=c(beta1, beta2), lty=2, col='red')

var_beta.RIDGE <- apply(beta.RIDGE.MC, 2, var)
var_beta.MCO <- apply(beta.MCO.MC, 2, var)
print(var_beta.RIDGE)
print(var_beta.MCO)

biais.RIDGE <- mu_beta.RIDGE - c(beta1, beta2)
risque.RIDGE <- var_beta.RIDGE + biais.RIDGE^2
print(risque.RIDGE)
print(var_beta.MCO)

# Q7 - visualisation du compromis biais-variance

lambda <- seq(0,150,5)
risque_beta.RIDGE <- matrix(0, ncol=2, nrow=length(lambda))

for (indice in 1:length(lambda)){
  lamb <- lambda[indice]
  NMC <- 1000
  beta.RIDGE.MC <- matrix(0, nrow=NMC, ncol=2)
  for (k in 1:NMC){
    residusk <- rnorm(n, mean=0, sd=sig)
    yk <- beta1*X1 + beta2*X2 + residusk
    beta.RIDGEk <- solve(XpX + diag(lamb,2), t(X)%*%yk)
    beta.RIDGE.MC[k,] <- beta.RIDGEk
  }
  mu_beta.RIDGE <- colMeans(beta.RIDGE.MC)
  var_beta.RIDGE <- apply(beta.RIDGE.MC, 2, var)
  risque_beta.RIDGE[indice, ] <- var_beta.RIDGE + ( mu_beta.RIDGE - c(0.5,-0.5) )^2
}

matplot(lambda, risque_beta.RIDGE, type='b', pch=19, col=c("black","red"),
        xlab = expression(paste(lambda," (paramètre de régularisation)")),
        ylab = "Risque") 
grid()
legend("topright", expression(beta[1], beta[2]),
       pch=19, col=c("black","red"),bty="n")
title('Compromis biais-variance')

