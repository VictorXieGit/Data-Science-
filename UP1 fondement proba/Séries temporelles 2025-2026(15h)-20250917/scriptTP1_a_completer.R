# Majeure Science des Données 2021-2022
# Script du TP n°1, Séries Temporelles 
# Acompléter
# Partie 1 : étude d'un MA(1) de moyenne mu
# Modèle X[t] = mu + Z[t] + theta*Z[t-1]
# Z[t] bruit blanc gaussien N(0,varZ)
mu <- c(0,1,10,20)	     # moyenne du processus X[t]
theta <- 1   # paramètre MA(1)
sigZ <- c(0.5,1,2)	   # écart-type du bruit Z[t]
theta2 <- -1

# Simulation d'un MA(1) de taille n

n <- 200
x <- matrix(0,n,3)

z0 <- sigZ*rnorm(1)     # simulation de Z[0]
z <- sigZ*rnorm(n)      # simulation du bruit blanc Z[1], ... , Z[n]
for (k in 1:3){
  z0 <- sigZ[k]*rnorm(1)     # simulation de Z[0]
  z <- sigZ[k]*rnorm(n)  
  x[,k] <- mu[k] + z[1] + theta2*z0
  for (t in 2:n) {
    x[t,k] <- mu[k] + z[t] + theta*z[t-1]
  }
  }
# Chronogramme de la série simulée

plot(x[,3], type='o', xlab="Temps t", main = "MA(1) simulé", cex.main=1)
abline(h=mu, col="red", lwd=2)
lines(x[,2],"blue")
lines(x[,1], 'cyan')
# ACF
ro <- acf(x, 20, main="Fonction d'autocorrélation empirique", ylim=c(-1,1))
#le modèle est sujet au Z0 choisis, l
#le modèle devient stationnaire à partir d'un certain n il 
#faut donc enlever les premières observations pour qu'on soit vraiment en train d'étudier notre série

#Partie 2 "méthode de simulation beaucoup de série introduire une matrice 
#avec les 5 série en 5 ligne et 200 colonne pr 200 valeurs puis bouclé
x <- diff(unemp[,1])
modele_ma1 <- arima(x, order = c(0,0,1))

theta_estime <- modele_ma1$coef
chi2_estime <- modele_ma1$sigma2

n <- 299  # taille de la série différenciée

# Fonction de simulation de la série différenciée MA(1)
simuler_serie_diff <- function(theta, sigma2, n) {
  # Simulation du bruit blanc N(0, σ²)
  epsilon <- rnorm(n + 100, mean = 0, sd = sqrt(sigma2))  # +100 pour burn-in
  
  # Simulation de la série MA(1) : X_t = ε_t + θ*ε_{t-1}
  x_sim <- numeric(n + 100)
  for(t in 2:(n + 100)) {
    x_sim[t] <- epsilon[t] + theta * epsilon[t-1]
  }
  
  # Suppression du burn-in (100 premières valeurs)
  return(x_sim[101:(n + 100)])
}
serie_diff_simulee <- simuler_serie_diff(theta_estime, chi2_estime, n)

# Sous RStudio, agrandissez la fenêtre "Plots" avec votre souris
# puis réexécutez le code

# Ou spécifiez une taille de fenêtre
dev.new(width = 10, height = 8)  # Fenêtre de 10x8 pouces
#par(mfrow = c(2, 2))

# Graphique 1 : Série différenciée
plot(x, type = "l", col = "blue", lwd = 2,
     main = "Série différenciée : Réelle vs Simulée",
     xlab = "Temps", ylab = "Différence")
lines(serie_diff_simulee, col = "red", lwd = 2)
legend("topright", legend = c("Réelle", "Simulée"), 
       col = c("blue", "red"), lwd = 2)
xsimulée <- numeric(300)
xsimulée[1] <- unemp[1,1]
for (k in 2:n+1){
  xsimulée[k] <- xsimulée[k-1]-serie_diff_simulee[k-1]
}
plot(unemp[,1],ylim=c(-200,700))
lines(xsimulée)
