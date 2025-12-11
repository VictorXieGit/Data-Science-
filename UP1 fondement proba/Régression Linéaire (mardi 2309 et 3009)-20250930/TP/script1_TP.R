# Majeure Science des Données 2025-2026
# Exemple de Régression sous R
# Prédiction de ventes de stations-service sur autoroutes en France
# (ancien challenge de data science)

# Préliminaires : chargement des données d'apprentissage et premières analyses
data <- read.table('Data_app.txt', header=TRUE)
n <- dim(data)[1]
names(data)
head(data)

# variable à prédire
ventes <- data$ventes 

# prédicteurs
temperature <- data$temperature
jour <- data$jour

# moyenne et écart-type des ventes observées
ventes.mean <- mean(ventes)
print(ventes.mean)
ventes.sd <- sd(ventes)
print(ventes.sd)
cat(' min des ventes :', min(ventes), '\n', 'max des ventes :', max(ventes))

boxplot(ventes, main='Ventes', col='orange')
grid()

# Données de test sans les ventes (prédictions de vente à calculer)   

data.test <- read.table('Data_test.txt', header=TRUE)
m <- dim(data.test)[1]
temp.test <- data.test$temperature
jour.test <- data.test$jour

# Données de test avec les ventes (pour comparer ventes prédites et ventes réelles)
data.test_avec_ventes <- read.table('Ventes_test.txt', header=TRUE)
ventes.test <- data.test_avec_ventes$ventes
y.test <- ventes.test

# Question 1
# Régression linéaire simple ventes ~ temperature

rls <- lm(ventes ~ temperature)
rls.s <- summary(rls)
print(rls.s)

plot(temperature, ventes, ylab="ventes", xlab="temperature")
abline(rls, col="red", lwd=2)
abline(a=ventes.mean, b=0, col="blue", lty=2)
grid()
title('RLS ventes ~ température')

# Calcul du score  
 
predict.rls <- matrix(0, nrow=m, ncol=1)

beta <- rls$coefficients
for (k in 1:m) {
    predict.rls[k] = beta[1]+beta[2]*temp.test[k]
  }

plot(ventes.test, predict.rls, pch=20, asp=1)
abline(a=0, b=1, col='red')
grid()
title('Prédictions contre ventes réelles')

RMSE.rls <- sqrt( mean((y.test-predict.rls)^2) )
print(RMSE.rls)

# Calcul du score de référence (prédiction constante par la moyenne des ventes)
RMSE.0 <- sqrt( mean((y.test-ventes.mean)^2) )
print(RMSE.0)

# Résidus du modèle de rls

plot(temperature, rls$residuals)
grid()
abline(h=0, col='red')
title('Résidus bruts')


# On regarde le type de jour pour mieux comprendre les données

plot(temperature, ventes, ylab="ventes", xlab="temperature")
abline(rls, col="red", lwd=2)
abline(a=ventes.mean, b=0, col="blue", lty=2)
grid()
title('RLS ventes ~ température')
couleurs <- rainbow(7)
index.L <- which(jour=="L")
points(temperature[index.L], ventes[index.L], col=couleurs[1])
index.Ma <- which(jour=="Ma")
points(temperature[index.Ma], ventes[index.Ma], col=couleurs[2])
index.Me <- which(jour=="Me")
points(temperature[index.Me], ventes[index.Me], col=couleurs[3])
index.J <- which(jour=="J")
points(temperature[index.J], ventes[index.J], col=couleurs[4])
index.V <- which(jour=="V")
points(temperature[index.V], ventes[index.V], col=couleurs[5])
index.S <- which(jour=="S")
points(temperature[index.S], ventes[index.S], col=couleurs[6])
index.D <- which(jour=="D")
points(temperature[index.D], ventes[index.D], col=couleurs[7])

legend("topleft",c("L","M","Me","J","V","S","D"),
       pch=rep(1,7), col=couleurs, cex=0.6)

# Question 2
# Distinguer au moins 3 populations : D, S et jours ordinaires
# 6 données aberrantes en apparence: 3L, 2M et 1J

ind.sem <- which(jour!="S" & jour!="D")
ind.S <- which(jour=="S")
ind.D <- which(jour=="D")

plot(temperature, ventes, ylab="ventes", xlab="temperature")
abline(a=ventes.mean,b=0,lty=2)
grid()
rls.sem <- lm(ventes[ind.sem] ~ temperature[ind.sem])
abline(rls.sem, col='red')
points(temperature[ind.sem], ventes[ind.sem],col='red')
rls.S <- lm(ventes[ind.S] ~ temperature[ind.S])
abline(rls.S,col='green')
points(temperature[ind.S],ventes[ind.S],col='green')
rls.D <- lm(ventes[ind.D] ~ temperature[ind.D])
abline(rls.D,col='blue')
points(temperature[ind.D], ventes[ind.D],col='blue')
title('RLS selon chaque population')

# calcul du score  

predict.3rls <- matrix(0, nrow=m, ncol=1)

beta.sem <- rls.sem$coefficients
beta.S <- rls.S$coefficients
beta.D <- rls.D$coefficients
for (k in 1:m) {
  if (jour.test[k]== "D") {
    predict.3rls[k] = beta.D[1]+beta.D[2]*temp.test[k] 
  }
  else if (jour.test[k]== "S") {
    predict.3rls[k] = beta.S[1]+beta.S[2]*temp.test[k] 
  }
  else {
    predict.3rls[k] = beta.sem[1]+beta.sem[2]*temp.test[k] 
  }
}

RMSE.3rls <- sqrt( mean((y.test-predict.3rls)^2) )
print(RMSE.3rls)

plot(ventes.test, predict.3rls, pch=20, asp=1)
abline(a=0, b=1, col='red')
grid()
title('Prédictions contre ventes réelles')

# Question 3

index <- which(jour=="S" | jour=="D")
jour.we <- rep(0,n)
jour.we[index] <- 1
jour.D <- rep(0,n)
jour.D[which(jour=="D")] <- 1

rlm <- lm(ventes ~ temperature + jour.we + jour.D)
summary(rlm)

# Visualisation de la réponse prédite par le modèle RLM
plot(temperature, ventes, ylab="ventes", xlab="temperature")
abline(a=ventes.mean, b=0, lty=2)
grid()
abline(rlm$coeff[1], rlm$coeff[2], col='red', lwd=2)
points(temperature[ind.sem], ventes[ind.sem], col='red')
abline(rlm$coeff[1]+rlm$coeff[3], rlm$coeff[2], col='green', lwd=2)
points(temperature[ind.S], ventes[ind.S],col='green')
abline(rlm$coeff[1]+rlm$coeff[3]+rlm$coeff[4], rlm$coeff[2], col='blue', lwd=2)
points(temperature[ind.D],ventes[ind.D],col='blue')
title('RLM sans interaction')

plot(rlm$fitted.values, ventes)
grid()
abline(a=0,b=1,col='red',lwd=2)
title("Ventes contre ventes prédites par le modèle")

# calcul du score

index <- which(jour.test=="S" | jour.test=="D")
jour.test.we <- rep(0,m)
jour.test.we[index] <- 1
jour.test.D <- rep(0,m)
jour.test.D[which(jour.test=="D")] <- 1

datanew.test <- data.frame(temperature=temp.test, jour.we=jour.test.we, 
                           jour.D=jour.test.D)
predict.rlm <- predict(rlm, newdata = datanew.test)

RMSE.rlm <- sqrt( mean((y.test-predict.rlm)^2) )
print(RMSE.rlm)

plot(ventes.test, predict.rlm, pch=20, asp=1)
abline(a=0, b=1, col='red')
grid()
title('Prédictions contre ventes réelles')

# Question 4
# On introduit les interactions

rlmi <- lm(ventes ~ temperature + jour.we + jour.D + temperature*jour.we +
            temperature*jour.D)
summary(rlmi)

# Visualisation de la réponse prédite par le modèle RLM avec interactions
plot(temperature, ventes, ylab="ventes", xlab="temperature")
abline(a=ventes.mean, b=0, lty=2)
grid()
abline(rlmi$coeff[1], rlmi$coeff[2], col='red', lwd=2)
points(temperature[ind.sem], ventes[ind.sem], col='red')
abline(rlmi$coeff[1]+rlmi$coeff[3], rlmi$coeff[2]+rlmi$coeff[5], col='green', lwd=2)
points(temperature[ind.S], ventes[ind.S], col='green')
abline(rlmi$coeff[1]+rlmi$coeff[3]+rlmi$coeff[4], 
       rlmi$coeff[2]+rlmi$coeff[5]+rlmi$coeff[6], col='blue', lwd=2)
points(temperature[ind.D],ventes[ind.D],col='blue')
title('RLM avec interaction')

# calcul du score

predict.rlmi <- predict(rlmi, newdata = datanew.test)

RMSE.rlmi <- sqrt( mean((y.test-predict.rlmi)^2) )
print(RMSE.rlmi)

plot(ventes.test, predict.rlmi, pch=20, asp=1)
abline(a=0, b=1, col='red')
grid()
title('Prédictions contre ventes réelles')

# Résidus du modèle
plot(temperature, rlmi$residuals)
grid()
abline(h=0, col='red')
title('Résidus du modèle de RLM avec interactions')

boxplot(rlmi$residuals ~ jour)
grid()

plot(rlmi$fitted.values, rlmi$residuals)
grid()
abline(h=0, col='red')
title('Résidus du modèle de RLM avec interactions')


# Question 5
# Il faut traiter les 6 données aberrantes
# puis corriger les résidus qui montrent encore une dépendance
# avec la température

# Oubli
# RLS en tenant compte que du jour

plot(ventes ~ as.factor(jour))
jour.fac <- factor(jour, levels=c("L","Ma","Me","J","V","S","D"))
plot(ventes ~ jour.fac)
grid()

rlm.jour <- lm(ventes ~ jour.fac)
summary(rlm.jour)

# Ventes réelles contre ventes prédites par le modèle avec interaction
plot(rlmi$fitted.values, ventes)
grid()
abline(a=0, b=1, col='red', lwd=2)
title("Ventes contre ventes prédites par le modèle")

