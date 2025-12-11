# Script du TP n?2, S?ries Temporelles
# Majeure Science des Donn?es 2021-22
library(ggplot2)
library(patchwork)  # Pour combiner les graphiques
library(dplyr)      # Pour la manipulation de données
##############################################################################
# PARTIE 1 : Etude d'un AR(2)
# Mod?le X(t) = mu + phi1*(X(t-1)-mu) + phi2*(X(t-2)-mu) + Z(t)
# Z[t] bruit blanc gaussien N(0,varZ)
# simulation d'un AR(2) par une phase initiale de stationnarisation
##############################################################################

# z1 et z2 racines du polyn?me P(z) = 1 - phi1*z - phi2*z^2 ? l'ext?rieur du
# cercle unit? pour un AR(2) causal

# r1 = 1/z1 et r2 = 1/z2 racines du polyn?me z^2 - phi1*z - phi2 ? l'int?rieur
# du cercle unit?

# cas de deux racines r1 et r2 r?elles dans ]-1, 1[
r1 <- 0.1
r2 <- -0.9
phi1 <- r1 + r2  
phi2 <- -r1*r2			# param?tres AR(2) 

# cas de deux racines complexes conjugu?es 
r <- 0.9
theta <- 20 # en degr?s
phi1 <- 2*r*cos(2*pi*theta/360)  
phi2 <- - r*r			# param?tres AR(2) 

mu <- 0			  # moyenne du processus X[t]
sigZ <- 1	          # ?cart-type du bruit Z[t]

# simulation avec r?gime transitoire de taille ninit = 50
ninit <- 50
n <- 500
ntot <- ninit + n

xtot <- rep(0,ntot)
xtot[1] <- 0
xtot[2] <- 0

for (t in 3:ntot) {
  xtot[t] <- phi1*xtot[t-1] + phi2*xtot[t-2] + sigZ*rnorm(1)
} 

xtot <- mu + xtot             # d?centrage
xinit <- xtot[1:ninit]        # r?gime transitoire (initial)

xstat <- xtot[(ninit+1):ntot] # r?gime stationnaire --> AR(2) de taille n

# visualisation r?gime transient
# Séquence complète pour tout reset
dev.new(width = 10, height = 8)  # Fenêtre de 10x8 pouces

plot(xtot, type='o', xlab="Temps t",
main = "AR(2) simul? avec r?gime transitoire", col="grey")

lines((ninit+1):ntot, xstat, type='o')
abline(mu, 0, col="red", lwd=2)

# analyse graphique - chronogramme de la s?rie xstat  

plot(xstat,type='o',xlab='Temps t',main = "Simulation d'un AR(2)")
abline(mu,0,col='red')

# acf et pacf de la s?rie simul?e
op <- par(mfrow = c(1,2))
ro <- acf(xstat, lag=15, ylim = c(-1,1), main = "ACF empirique")
alpha <- pacf(xstat, lag=15, ylim = c(-1,1), main = "et PACF", xlim=c(0,15))
par(op)


###############################################################################
# PARTIE 2 : identification de mod?les
############################################################################### 

# On commence avec la premi?re s?rie de donn?es, fichier "serie1.Rdata"

rm(list=ls())           # clear all 
#si erreur de load on utilise setwd pour mettre le path où chercher le fichier
load("serie1.Rdata")
ls.str()

par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))
#dev.new(width = 10, height = 8)  # Fenêtre de 10x8 pouces

# chronogramme de la s?rie  
plot(serie, type='o', xlab="Temps t", ylab="", main = "data")
abline(h=0, col="red", lwd=2)


# acf et pacf de la s?rie  
#op <- par(mfrow = c(1,2))
ro <- acf(serie, lag=15, ylim = c(-1,1), main = "ACF empirique")
alpha <- pacf(serie, lag=15, ylim = c(-1,1), main = "et PACF empirique", xlim=c(0,15))
#par(op)

# seconde s?rie : fichier "serie2.Rdata"

...
