# Majeure Science des données 2025-2026
# UP2 Apprentissage statistique - Méthodes de Régression Avancées
# Régression pénalisée avec le package glmnet

# Simulation d'un jeu de données (p = 2 prédicteurs)

n <- 200
rho <- - 0.99
beta1 <- 0.5
beta2 <- -0.5
sigX <- 1
sig <- 1

set.seed(2023)

X1 <- rnorm(n,mean=0,sd=sigX)
X2 <- rho*X1 + sqrt(1 - rho^2)*rnorm(n,mean=0,sd=sigX)

X <- matrix(c(X1,X2), ncol=2, byrow=FALSE)
colnames(X) <- c("X1","X2")

residus <- rnorm(n, mean=0,sd=sig)
y <- beta1*X1 + beta2*X2 + residus

data <- cbind(y,X)
pairs(data)

# Chemins de régularisation obtenus directement (from scratch)
lambda <- seq(0,200,5)
beta.ridge <- matrix(0, ncol=2, nrow=length(lambda)) 

R <- t(X)%*%X

for (k in 1:length(lambda)) {
  beta.ridge[k,] <- solve(R + diag(lambda[k],2), t(X)%*%y)
}

matplot(lambda, beta.ridge, type='b', pch=19, col=c("black","red"), ylim=c(-0.6,1.2), 
        xlab = expression(paste(lambda," (paramètre de régularisation)")),
        ylab = "Coefficients (estimés)")  
grid()
abline(h=c(-0.5,0.5), lty=2, col='blue', lwd=1)
title('Chemins de régularisation')

# Avec le package glmnet : General Linear Model with elastic NET regularization
# install.packages('glmnet')

library(glmnet)

# On utilise la fonction glmnet() pour ajuster un modèle ridge. 
# La régularisation se fait avec deux paramètres lambda et alpha via 
# la pénalité suivante : 
# lambda * [ alpha * ||beta||_L1 + (1-alpha) * ||beta||_L2 ^2 / 2 ]
# Par défaut, alpha = 1 --> LASSO

# Ridge "pur" avec donc alpha=0

reg.ridge <- glmnet(X, y, alpha = 0)

# Pour connaître les 100 valeurs de lambda choisies par défaut, faire

lambda <- reg.ridge$lambda
print(lambda)

# Pour tracer l'évolution des coefficients beta en fonction de lambda
# (chemins de régularisation)

plot(reg.ridge, xvar='lambda', main="Ridge", ylim=c(-0.6, 0.6), lwd=2)
grid()
abline(h=beta1, lty=2)
abline(h=beta2, lty=2)

# Sélection du paramètre de régularisation lambda
reg.ridge.cv = cv.glmnet(X, y, alpha=0)
plot(reg.ridge.cv, main="Ridge") # trace l'erreur de validation croisée
grid()

# On peut afficher maintenant la valeur de lambda choisie par validation croisée 
# ainsi que les valeurs des coefficients beta de l'estimation ridge associée

lambda.opt.ridge = reg.ridge.cv$lambda.min
print(lambda.opt.ridge)
nobs <- n
print(lambda.opt.ridge*nobs)
coeffs.ridge.cv = coef(reg.ridge.cv, s="lambda.min")
print(coeffs.ridge.cv)  

lambda.opt.ridge = reg.ridge.cv$lambda.1se # autre valeur qui tient compte de l'erreur
print(lambda.opt.ridge)
nobs <- n
print(lambda.opt.ridge*nobs)
coeffs.ridge.cv = coef(reg.ridge.cv, s="lambda.1se")
print(coeffs.ridge.cv)  

# Pour faire des prédictions à partir du modèle ridge ajusté par CV
X1.new <- rnorm(n,mean=0,sd=sigX)
X2.new <- rho*X1 + sqrt(1 - rho^2)*rnorm(n,mean=0,sd=sigX)

X.new <- matrix(c(X1.new,X2.new), ncol=2, byrow=FALSE)
colnames(X.new) <- c("X1","X2")

y.new <- beta1*X1 + beta2*X2 + rnorm(n, mean=0,sd=sig)

par(mfrow=c(1,2))
predictions.ridge <- predict(reg.ridge.cv, newx=X.new,
                             type="response", s='lambda.min')
plot(predictions.ridge, y, xlim=c(-3, 3), ylim=c(-4,4))
abline(a=0, b=1, col='red', lwd=2)
grid()
title('Ridge')
RMSE_ridge <- sqrt( mean( (y-predictions.ridge)^2 ) )

reg.MCO <- lm(y ~ X1+X2)
predictions.MCO <- predict(reg.MCO, newdata = as.data.frame(X.new))
plot(predictions.MCO, y, xlim=c(-3, 3), ylim=c(-4,4))
abline(a=0, b=1, col='red', lwd=2)
grid()
title('MCO')
par(mfrow=c(1,1))
RMSE_MCO <- sqrt( mean( (y-predictions.MCO)^2 ) )
