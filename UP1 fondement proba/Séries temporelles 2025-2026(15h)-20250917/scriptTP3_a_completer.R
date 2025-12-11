# Script du TP n°3, Séries Temporelles
# Majeure Science des Données 2021-22

# *********************************************************************************
# On étudie la série de trafic aérien international "airline" (série Airpassengers)
# C'est l'exemple 1 du support de cours : voir pages 4 et 5
# Exécuter le code pas à pas situé entre deux commentaires signalés par # 
# Il n'y a aucune instruction à compléter. Par contre, vous pouvez changer certains
# paramètres et visualiser l'effet obtenu
# *********************************************************************************

# Chargement des données et mise sous la forme d'une série chronologique avec ts
# Série mensuelle de janvier 1949 à décembre 61, soit 12x12=144 valeurs

airline.data <- read.table("airline.dat")
airline <- airline.data$V1
airline <- ts(airline, start = c(1949,1), freq = 12)

# Chronogramme et légendes...

plot(airline, type='o', xlab="Date", ylab="Passagers (en milliers)", 
     main="Trafic aérien international de janvier 1949 à déc. 1960")
grid()

# Question 1
# Méthodologie de Box&Jenkins, première transformation simple par passage au
# logarithme et visualisation de l'effet obtenu

logair <- log(airline)
op <- par(mfrow = c(2,1))
plot(airline,type='o', main = "Série x initiale")
grid()
plot(logair, type='o', main = "log(x)")
grid()
par(op)

# On élimine la tendance (linéaire) par différenciation simple

difflogair <- diff(logair)
plot(difflogair, type='o', main = "Série log(x) différenciée",
     xlab="Temps", ylab = expression(paste("(",I-B,")",log(x[t])) ))
grid()
abline(h=0, col="red", lwd=2)

# puis différenciation saisonnière (comparer avec la figure du support page 13)
# pour éliminer la composante périodique de période s = 12 mois

diff2logair <- diff(difflogair, lag=12)
op <- par(cex.lab = 0.8)
plot(diff2logair, type='o', main = "Différenciation simple et différenciation saisonnière",
     xlab="Temps", ylab = expression(paste("(",I-B^12,")","(",I-B,")",log(x[t]))))
grid()
par(op)
abline(h=0, col="red", lwd=2)

# ********************************************************************************
# On déroule la méthodologie de Box et Jenkins en partant de la série qui vient
# d'être obtenue par 2 différenciations successives (série diff2logair). 
# On l'analyse comme une série stationnaire à l'aide des ACF et PACF. On vérifie
# alors que le modèle SARIMA semble bien adapté.
# On estime ensuite ce modèle, on vérifie que les coefficients sont bien ceux 
# du support et on valide graphiquement. Enfin, on utilise le modèle pour faire 
# de la prévision à un an et on visualise la qualité de prévision à un an par une
# technique de back-testing (voir support page 45, analyse post-sample)...
# ********************************************************************************

# On rebaptise la série obtenue (sans Tendance et DésaiSonnalisée = dts) 

airdts <- diff2logair

# ACF et PACF de airdts (comparer avec le support page 38)

op <- par(mfrow = c(1,2))
airdts <- as.vector(airdts)
ro <- acf(airdts , lag=25, ylim = c(-1,1), main = expression("ACF série stationnarisée"),
          xlab="Lag (en mois)",lwd=2)
grid()
alpha <- pacf(airdts , lag=25, ylim = c(-1,1), main = expression("PACF"), 
              xlab="Lag (en mois)", lwd=2)
grid()
par(op)

# Ajustement d'un modèle SARIMA(0,1,1)(0,1,1) avec saisonnalité s = 12 et comparaison
# des coefficients obtenus avec ceux du support (voir page 38 pour l'expression complète
# du modèle et les valeurs des coefficients)

modele <- arima(logair, order = c(0,1,1), seasonal = list(order=c(0,1,1), period = 12))
print(modele)

# Validation du modèle
# Fonction tsdiag() de R

tsdiag(modele)

# Extraction des "résidus" du modèle (processus de bruit sous-jacent) et "normal qqplot"

residus <- modele$residuals
qqnorm(residus, main = "Normal Q-Q Plot", xlab = "Theoretical Quantiles", 
       ylab = "Sample Quantiles", plot.it = TRUE)
grid()
qqline(residus, probs=c(0.1,0.9), col="red", lwd=2)

# Back-testing du modèle SARIMA : on enlève les 12 dernières valeurs que l'on cherche 
# ensuite à prévoir. On compare alors avec les valeurs réelles de la série!

# On ré-estime le modèle sur les seuls données d'apprentissage (pour ne pas tricher!)
nair <- length(airline)
airfit <- airline[1:(nair - 12)]	# on enlève les 12 dernières valeurs

logairfit <- ts(log(airfit), start = c(1949,1), freq = 12)

# Ajustement d'un modèle SARIMA(0,1,1)(0,1,1) avec s = 12 sur les données 
# d'apprentissage logairfit

modele <- arima(logairfit, order = c(0,1,1), seasonal = list(order=c(0,1,1), period = 12))
print(modele)

tsdiag(modele)

residus <- modele$residuals
qqnorm(residus, main = "Normal Q-Q Plot", xlab = "Theoretical Quantiles", 
       ylab = "Sample Quantiles", plot.it = TRUE)
grid()
qqline(residus, probs=c(0.1,0.9), col="red", lwd=2)

# Prévision sur un an ou d'horizon h = 12

logair <- log(airline)

plot(logair, type='o', xlab='Temps', ylab='Log Nbre Passagers',
     main ="Prévision SARIMA et valeurs réelles", ylim=c(4.5,6.8))
grid()

prevision <- predict(modele, n.ahead=12, prediction.interval=T)

lines( ts(prevision$pred, start=c(1960,1), freq=12), type='o', col='red', lwd=2 )
lines( ts(prevision$pred + 2*prevision$se, start=c(1960,1), freq=12),  col='blue', lwd=2 )
lines( ts(prevision$pred - 2*prevision$se, start=c(1960,1), freq=12),  col='blue', lwd=2 )

# **************************************************************************************** 
# Travail à faire : le dernier graphique obtenu pour back-tester la méthode de prévision 
# porte sur le logarithme de la série. Obtenir la même analyse graphique mais sur la série
# initiale. Pour cela, compléter le code ci-dessous... 
# **************************************************************************************** 


plot( ... , type='o', xlab='Année', ylab='Passagers (en milliers)',
     main ="Prévision SARIMA du trafic aérien et valeurs réelles", ylim=c(100,700))
grid()

lines( ts( ... , freq=12), type='o', col='red', lwd=2 )
lines( ts( ... , start=c(1960,1), freq=12),  col='blue', lwd=2 )
lines( ts( ... , start=c(1960,1), freq=12),  col='blue', lwd=2 )

# Calcul du RMSE

...

# Question 2

...

# Question 3 :
# On estime de manière paramétrique la tendance et saisonnalité du log de la série,
# et on traite la série obtenue comme une série stationnaire. On travaille sur 
# l'historique privé des 12 dernières valeurs pour tester à nouveau la qualité 
# de la prévision obtenue.


rm(list=ls()) # on efface tout

airline.data <- read.table("airline.dat")
airline <- airline.data$V1
airline <- ts(airline, start = c(1949,1), freq = 12)

logair <- log(airline)
nair <- length(logair)

# Données pour estimer le modèle (on enlève les 12 dernières valeurs)

logairfit <- logair[1:(nair-12)]
logairfit <- ts(logairfit, start=c(1949,1), freq=12)

# On estime tendance et saisonnalité à l'aide d'un modèle linéaire

logairfit <- as.vector(logairfit)

# prédicteurs avec predict1 qui est la variable temps

predict1 <- 1:(nair-12)
predict2 <- ...
predict3 <- ...

# On utilise la fonction lm de R pour estimer le modèle linéaire

mod.lm <- lm( ... ~ ... )

# on en déduit la tendance et on visualise

tend <- mod.lm$coef[1] + mod.lm$coef[2]*predict1
tend <- ts(tend, start=c(1949,1), freq=12)
plot(logairfit, type='o', xlab="Temps",
     ylab="Log Nbre Passagers",main="Tendance estimée")
lines(tend, col='red',lwd=2)

# On calcule la saisonnalité 

sais <- mod.lm$coef[3]*predict2 + mod.lm$coef[4]*predict3
sais <- ts(sais, start=c(1949,1), freq=12)
lines(tend + sais, col='blue',lwd=2)

# Série log(airline) sans tendance et composante saisonnière

logairdts <- logairfit - (tend + sais)
logairdts <- ts(logairdts, start=c(1949,1), freq=12)
op <- par(cex.lab = 0.8)
plot(logairdts, type='o', main = "Résidus estimés avec le modèle linéaire", xlab="Année")
par(op)
abline(0, 0, col="red", lwd=2)

# ACF et PACF de logairdts 

op <- par(mfrow = c(1,2))
logairdts <- as.vector(logairdts)
ro <- acf(logairdts , lag=25, ylim = c(-1,1), main = expression("ACF série stationnarisée"),
          xlab="Lag (en mois)", lwd=2)
alpha <- pacf(logairdts , lag=25, ylim = c(-1,1), main = expression("PACF"),
          xlab="Lag (en mois)", lwd=2)
par(op)


# Question 4

...

# Question 5

...

