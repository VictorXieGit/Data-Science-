# Majeure Science des Donn?es 2021-2022
# Script du TP n?1, S?ries Temporelles 

# Partie 1 : ?tude d'un MA(1)
# Mod?le X[t] = mu + Z[t] + theta*Z[t-1]
# Z[t] bruit blanc gaussien N(0,varZ)
rm(list = ls())
# Question 1
mu <- 0	    # moyenne du processus X[t]
theta <- 1  # param?tre MA(1)
sigZ <- 1   # ?cart-type du bruit Z[t]

# Simulation d'un MA(1) de taille n
n <- 220
x1 <- rep(0,n) # initialisation de la s?rie x[t]
z0 <- sigZ*rnorm(1)     # simulation de Z[0]
z <- sigZ*rnorm(n)      # simulation de Z[1], ... , Z[n]

x1[1] <- mu + z[1] + theta*z0
for (t in 2:n) {
	x1[t] <- mu + z[t] + theta*z[t-1]
}
x1=x1[21:220]
# Chronogramme de la s?rie simul?e
plot(x1, type='l', xlab="Temps t", main = "MA(1) simul")
length(x1)
abline(h=mu, col="red", lwd=2)
grid()

# Diagrammes retard?s
lag.plot(x,lags=4)

# Question 2

# ACF empirique
acf(x, 20, main="Fonction d'autocorr?lation empirique",
    ylim=c(-1,1), lwd=2)
grid()

# Question 3

# On utilise ici la vision "MA(1) = moyenne mobile calcul?e sur un bruit",
# l'occasion de d?couvrir la fonction filter() de R
# Faire un help(filter)
n <- 200
theta <- 1
poids <- c(1,theta) # reverse time order

op <- par(mfrow=c(3,1))
sigZ <- 0.1
x <- filter(sigZ*rnorm(n+1), poids, sides=1, method="conv")
plot(x, type='o', xlab="", main=expression(paste(sigma[Z]," = 0.1")))
abline(h=mu, col="red", lwd=2)
sigZ <- 1
x <- filter(sigZ*rnorm(n+1), poids, sides=1, method="conv")
plot(x, type='o', xlab="", main=expression(paste(sigma[Z]," = 1")))
abline(h=mu, col="red", lwd=2)
sigZ <- 10
x <- filter(sigZ*rnorm(n+1), poids, sides=1, method="conv")
plot(x, type='o', xlab="Temps t", main=expression(paste(sigma[Z]," = 10")))
abline(h=mu, col="red", lwd=2)
par(op)
# On voit que l'?cart-type sigZ du bruit Z est un facteur d'?chelle!

# Question 4

n <- 200
sigZ <- 1
theta <- 10
poids <- c(1,theta)
x <- filter(sigZ*rnorm(n+1), poids, sides=1, method="conv")
plot(x, type='o', xlab="Temps t", main = "MA(1) simul?")
abline(h=mu, col="red", lwd=2)
grid()

# Attention ? x[1] = NA (donn?e manquante)
acf(x[2:(n+1)],20,main="Fonction d'autocorr?lation empirique",ylim=c(-1,1))

# ACF empirique = ACF empirique d'un bruit, ce qu s'explique par la fait que
# X[t] ~= theta*EPS[t-1] car theta grand, d'o? aussi la variance importante!

# Question 5

n <- 200
sigZ <- 1
theta <- 1
mu <- 5
z0 <- sigZ*rnorm(1)     # simulation de Z[0]
z <- sigZ*rnorm(n)      # simulation de Z[1], ... , Z[n]

x <- rep(0,n)
x[1] <- mu + z[1] + theta*z0
for (t in 2:n) {
  x[t] <- mu + z[t] + theta*z[t-1]
}
plot(x, type='o', xlab="Temps t", main = "MA(1) simul? d?centr?", ylim=c(-max(abs(x)),max(abs(x))))
abline(h=mu, col="red", lwd=2)
abline(h=0,lty=2)
grid()

acf(x,20,main="Fonction d'autocorr?lation empirique",ylim=c(-1,1))
# mu est la moyenne du processus, param?tre de position centrale = param?tre de translation

# Partie 2 

rm(list=ls())

unemptab <- read.table("C:\\Users\\youssef.salman\\Desktop\\Post-doc\\Teaching\\Time series\\New cours\\Cours EMSE without changes\\unemp.dat")
unemp <- unemptab$V1

unemp <- ts(unemp, start = c(1961,1), freq = 12) # s?rie initiale x(t)
plot(unemp, type='l', xlab="Ann?e", ylab="nombre (en milliers)",
     main="Ch?mage des femmes de 16 ? 19 ans aux USA - p?riode 1961-1985",
     cex.main=0.8)
grid()

# Stationnarisation par diff?renciation simple y(t) = x(t) - x(t-1) et visualisation : 
diffunemp <- diff(unemp) # s?rie y(t)
mu <- mean(diffunemp)
print(mu)
(unemp[300]-unemp[1])/299

plot(diffunemp, type='o', main = "Variations mensuelles du nombre de ch?meurs",
     xlab="Ann?e", ylab = expression(y[t]),cex.main=1)
abline(h=mu, col="red", lwd=2)
grid()

# ACF empirique  
ro <- acf(as.vector(diffunemp), lag=20, ylim = c(-1,1), 
          main = expression(paste("ACF empirique de la s?rie ", y[t])),
          xlab="D?calage h (mois)",lwd=2)

# Estimation du coefficient d'auto-corr?lation d'ordre 1 de la s?rie y
rho = ro$acf[2]; print(rho)

# Identification du MA(1) (m?thode des moments) : Y(t) = mu + Z(t) + theta*Z(t-1)
theta <- (1 - sqrt(1 - 4*rho*rho) )/(2*rho) # choix de la racine en module <= 1
print(theta)

n <- length(diffunemp)
sigmaY <- sd(diffunemp) # estimation de l'?cart-type sigmaY de la s?rie Y
sigma <- sigmaY/sqrt(1+theta^2) # sigma = sigZ = ?cart-type du bruit Z  

# "Vraie" s?rie de ch?mage en noir et simulations via le mod?le MA(1) pour la s?rie des variations
plot(unemp, type='l', col=1, xlab="Ann?e", main = "S?rie initiale et simulations",
     ylab="nombre en milliers", ylim=c(0,1200))
grid()

n <- length(unemp) # simulations sur n = 300 mois = 25 ann?es
Nsimu <- 3         # nombre de simulations = trajectoires du processus
Xsimu <- matrix(0,nrow=n,ncol=Nsimu) # simulations en colonnes
rnorm(1)
mu0 <- mu
for (k in 1:Nsimu) {
  Xsim <- rep(0,n) # initialisation de la simulation n?k
  Xsim[1] <- unemp[1] # on d?marre de la valeur du nombre de ch?meurs de janvier 1961
  bruit <- sigma*rnorm(n)
  for (t in 2:n) Xsim[t] <-  Xsim[t-1] + mu0 + bruit[t] + theta*bruit[t-1] 
  Xsim <- ts(Xsim, start = c(1961,1), freq = 12)
  lines(Xsim,col=k+1)
  Xsimu[,k] <- Xsim
}

# Variance du processus X par m?thode Monte-Carlo (moyenne spatiale)
# On effectue pour cela Nsimu simulations du processus X avec Nsimu grand
Nsimu <- 1000
Xsimu <- matrix(0,nrow=n,ncol=Nsimu)
for (k in 1:Nsimu) {
  Xsim <- rep(0,n)
  Xsim[1] <- unemp[1]
  bruit <- sigma*rnorm(n)
  for (t in 2:n) Xsim[t] <-  Xsim[t-1] + mu + bruit[t] + theta*bruit[t-1] 
  Xsimu[,k] <- Xsim
}

varX <- rep(0,n)
for (t in 1:n) { 
  varX[t] = var(Xsimu[t,])
}
# Graphique
plot(1:n,varX,type="l",xlab="Num?ro d'observation (temps)",
     ylab='variance de X',lwd=2)
grid()
title('Variance estim?e du processus X',cex.main=1)
time = 1:n
abline(lm(varX ~ time - 1),col="red",lwd=2)
abline(a=0,b=sigma^2*(1+theta)^2,col='blue',lwd=2)

plot(1:n,sqrt(varX),type="l",xlab="Num?ro d'observation (temps)",
     ylab='?cart-type de X',lwd=2)
grid()
title('Ecart-type estim? du processus X',cex.main=1)

# Simulations sur le long terme de la s?rie de ch?mage (50 ans)
plot(as.vector(unemp), type='l', col=1, xlab="Temps", ylab="", main = "S?rie initiale et simulations", 
     xlim=c(0,600), ylim=c(0,2000))
grid()
lines(1:n, unemp[1]+mu*(0:(n-1)), lty=2)
n <- 600  # 50 ans
Nsimu <- 3         # nombre de simulations = trajectoires du processus
Xsimu <- matrix(0,nrow=n,ncol=Nsimu) # simulations en colonnes
for (k in 1:Nsimu) {
  Xsim <- rep(0,n) # initialisation de la simulation n? k
  Xsim[1] <- unemp[1] # on d?marre de la valeur du nombre de ch?meurs de janvier 1961
  bruit <- sigma*rnorm(n)
  for (t in 2:n) Xsim[t] <-  Xsim[t-1] + mu + bruit[t] + theta*bruit[t-1] 
  lines(Xsim,col=k+1)
  Xsimu[,k] <- Xsim
}

# Question 4
ro <- acf(unemp, lag=20, ylim = c(-1,1), 
          main = "ACF empirique de la s?rie de ch?mage", xlab="D?calage h (ann?e)")


# Partie 3 - ?tude d'un AR(1) 
# Mod?le X(t) = phi*X(t-1) + Z(t) 
# Z[t] bruit blanc gaussien N(0,varZ)

rm(list=ls())

# Simulation AR(1) de taille n
n <- 200
mu <- 0
phi <- 0.9		        
sigZ <- 1			                  # ?cart-type sigma du bruit Z[t]  
sigX <- sigZ/sqrt(1 - phi^2)    # ?cart-type de X[t]

x <- rep(0,n)
x0 <- mu + sigX*rnorm(1)        # valeur initiale en date t = 0
# x0 <- mu + 10
 
x[1] <- mu + phi*(x0-mu) + sigZ*rnorm(1)
for (t in 2:n) x[t] <- mu + phi*(x[t-1]-mu) + sigZ*rnorm(1)

# Chronogramme de la s?rie simul?e
plot((0:n),c(x0,x), type='o', xlab="Temps t", main = "AR(1) simul?",ylab="x(t)")
abline(v=0,lty=2)
abline(h=0, col="red", lwd=2)
grid()

# Visualisation des auto-corr?lations d'un AR(1)
lag.plot(x,4)
acf(x,20,ylim=c(-1,1),main="ACF empirique d'un AR(1)",cex.main=1,lwd=2)
pacf(x,20,ylim=c(-1,1),main="PACF empirique d'un AR(1)",cex.main=1,lwd=2)

# Chronogramme de la s?rie "renvers?e"
plot(1:n, x[n:1], type='o', xlab="", xaxt="n",
     main = "M?me s?rie ? rebours dans le temps",ylab="x(t)",cex.main=0.8)
abline(v=0,lty=2)
abline(h=0, col="red", lwd=2)
grid()





