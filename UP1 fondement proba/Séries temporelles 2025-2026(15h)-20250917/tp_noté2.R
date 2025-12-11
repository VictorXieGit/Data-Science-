#chargement des librairies
library(FinTS)
library(dplyr)
library(lubridate)
library(moments)
library(tseries)
library(forecast)
library(rugarch)

data <- read.csv("C:\\Users\\victo\\OneDrive\\Bureau\\COURS général\\cours mine 2526\\sdd\\UP1 fondement proba\\Séries temporelles 2025-2026(15h)-20250917\\data2.csv",skip = 1)


data$Date <- as.Date(data$Date)

data$TAVG_C <- (data$TAVG..Degrees.Fahrenheit. - 32) * 5/9
data$TMAX_C <- (data$TMAX..Degrees.Fahrenheit. - 32) * 5/9
data$TMIN_C <- (data$TMIN..Degrees.Fahrenheit. - 32) * 5/9

# Agréger les données en mensuel (moyennes par mois)
data_monthly <- data %>%
  group_by(Year = year(Date), Month = month(Date)) %>%
  summarise(
    TAVG_C = mean(TAVG_C, na.rm = TRUE),
    TMAX_C = mean(TMAX_C, na.rm = TRUE),
    TMIN_C = mean(TMIN_C, na.rm = TRUE),
    .groups = "drop"
  )

# Filtrer les données à partir de 1973
data_monthly_1973 <- data_monthly %>%
  filter(Year >= 1973)

# Créer un ts pour la température moyenne (TAVG_C)
ts_TAVG <- ts(data_monthly_1973$TAVG_C,
              start = c(1973, 1),  # Janvier 1973
              frequency = 12)

plot(ts_TAVG, main = "Températures mensuelles (°C) à partir de 1973")

grid(
  col = "black",
  lty = "dotted",
  lwd = 0.5
)

summary_stats <- function(x) {
  c(
    Moyenne = mean(x, na.rm = TRUE),
    Mediane = median(x, na.rm = TRUE),
    Variance = var(x, na.rm = TRUE),
    Skewness = skewness(x, na.rm = TRUE),
    Kurtosis = kurtosis(x, na.rm = TRUE)
  )
}


desc_TAVG <- summary_stats(ts_TAVG)
print(desc_TAVG)

des_data<- diff(ts_TAVG, lag=12)
plot(des_data, main ="Série des températures sans saisonnalité")
grid(
  col = "black",
  lty = "dotted",
  lwd = 0.5
)



# Série originale
plot(ts_TAVG, 
     ylab = "Température (°C)", 
     col = "blue", 
     lwd = 2)
title("Série Originale (avec saisonnalité annuelle)")

# Série désaisonnalisée
lines(des_data, 
     ylab = "Température (°C)", 
     col = "red", 
     lwd = 2)
title("Série Désaisonnalisée (des_data)")

test_adf <- adf.test(na.omit(des_data))
print("=== TEST ADF (Augmented Dickey-Fuller) ===")
print(test_adf)

# Interprétation
if (test_adf$p.value < 0.05) {
  print("p-value < 0.05 : On rejette H0 - La série est STATIONNAIRE")
} else {
  print("p-value >= 0.05 : On ne rejette pas H0 - La série est NON STATIONNAIRE")
}

# Test KPSS - Hypothèses inverses:
# H0: Série stationnaire
# H1: Série non stationnaire

test_kpss <- kpss.test(na.omit(des_data))
print("=== TEST KPSS ===")
print(test_kpss)

# Interprétation
if (test_kpss$p.value < 0.05) {
  print("p-value < 0.05 : On rejette H0 - La série est NON STATIONNAIRE")
} else {
  print("p-value >= 0.05 : On ne rejette pas H0 - La série est STATIONNAIRE")
}
par(mfrow=c(1,1))
acf(des_data,lag.max=50)
pacf(des_data,lag.max=50)

#D'après lecture du PACF et ACF, nous avons soit AR(1) soit MA(1) à MA(8) soit arma(1,1)
#Regardons les criètes AIC et BIC pour faire notre choix

# Initialiser les data frames pour stocker les résultats
resultats_AR <- data.frame()
resultats_MA <- data.frame()
# Modèles MA de 1 à 10
for(q in 1:10) {
  modele <- try(arima(des_data[1:(length(des_data)-12)], order = c(0, 0, q)), silent = TRUE)
  if(!inherits(modele, "try-error")) {
    resultats_MA <- rbind(resultats_MA, data.frame(
      Modele = paste0("MA(", q, ")"),
      AIC = AIC(modele),
      BIC = BIC(modele)
    ))
  }
}
for(p in 1:10) {
  modele <- try(arima(des_data[1:(length(des_data)-12)], order = c(p, 0, 0)), silent = TRUE)
  if(!inherits(modele, "try-error")) {
    resultats_AR <- rbind(resultats_AR, data.frame(
      Modele = paste0("AR(", p, ")"),
      AIC = AIC(modele),
      BIC = BIC(modele)
    ))
  }
}

# Combiner les résultats
tableau_comparatif <- rbind(resultats_AR, resultats_MA)

# Trier par AIC (meilleur modèle en premier)
tableau_comparatif <- tableau_comparatif[order(tableau_comparatif$BIC), ]

# Ajouter un rang pour voir le classement
tableau_comparatif$Rang <- 1:nrow(tableau_comparatif)

# Afficher le tableau complet
print("Tableau comparatif complet (trié par BIC) :")
print(tableau_comparatif, row.names = FALSE)

#Question 2 on en déduit le meilleur modèle MA(8) ou AR(1)
#question 3 on choisis d'étudier AR(1) et ARMA(1,0,8)

# Ajuster les modèles
modele_AR1 <- arima(des_data, order = c(1, 0, 0))
modele_ARMA18 <- arima(des_data, order = c(1, 0, 8))

# AR(1) - ACF des résidus
acf(residuals(modele_AR1), main = "ACF Résidus - AR(1)", lag.max = 24)
# AR(1) - Test Ljung-Box 
checkresiduals(modele_AR1)

# ARMA(1,8) - ACF des résidus
acf(residuals(modele_ARMA18), main = "ACF Résidus - ARMA(1,8)", lag.max = 24)
# ARMA(1,8) - Test Ljung-Box
checkresiduals(modele_ARMA18)

par(mfrow = c(1, 1))

# Tests Ljung-Box comparés
cat("=== AR(1) ===\n")
print(Box.test(residuals(modele_AR1), lag = 24, type = "Ljung-Box"))
cat("\n=== ARMA(1,8) ===\n") 
print(Box.test(residuals(modele_ARMA18), lag = 24, type = "Ljung-Box"))

# Comparaison AIC/BIC
cat("\n=== COMPARAISON AIC/BIC ===\n")
cat("AR(1) - AIC:", AIC(modele_AR1), "BIC:", BIC(modele_AR1), "\n")
cat("ARMA(1,8) - AIC:", AIC(modele_ARMA18), "BIC:", BIC(modele_ARMA18), "\n")

#Dans les deux cas la p-value n'est pas bonne, il y a une corrélation dans les résidues et ne sont donc pas un bruit blanc


#Partie 3
# Test ARCH d'Engle sur les résidus de AR(1)
cat("=== TEST ARCH - Résidus AR(1) ===\n")
arch_test_AR1 <- ArchTest(residuals(modele_AR1), lags = 12)
print(arch_test_AR1)

# Test ARCH d'Engle sur les résidus de ARMA(1,8)  
cat("\n=== TEST ARCH - Résidus ARMA(1,8) ===\n")
arch_test_ARMA18 <- ArchTest(residuals(modele_ARMA18), lags = 12)
print(arch_test_ARMA18)

# Test ARCH sur les résidus de MA(8) aussi pour comparaison
cat("\n=== TEST ARCH - Résidus MA(8) ===\n")
modele_MA8 <- arima(des_data, order = c(0, 0, 8))
arch_test_MA8 <- ArchTest(residuals(modele_MA8), lags = 12)
print(arch_test_MA8)


#lest test sur MA(8) et ARMA(1,8) conclue une homosdteacité mais pas pour AR(1)
#prenons des modèle ARCH1 et GARCH11

# 1. SPÉCIFICATION des modèles
## ARCH(1)
spec_arch1 <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 0)),  # ARCH(1)
  mean.model = list(armaOrder = c(1, 0), include.mean = TRUE)     # AR(1) dans la moyenne
)

## GARCH(1,1)  
spec_garch11 <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),  # GARCH(1,1)
  mean.model = list(armaOrder = c(1, 0), include.mean = TRUE)     # AR(1) dans la moyenne
)

# 2. ESTIMATION des modèles
## ARCH(1)
modele_arch1 <- ugarchfit(spec_arch1, data = des_data)
print("=== ARCH(1) ===")
print(modele_arch1)

## GARCH(1,1)
modele_garch11 <- ugarchfit(spec_garch11, data = des_data)  
print("=== GARCH(1,1) ===")
print(modele_garch11)

# Recréer la comparaison sur la même échelle
comparaison_corrigee <- data.frame(
  Modele = c("AR(1) simple", "ARCH(1)", "GARCH(1,1)"),
  LogLik = c(
    logLik(modele_AR1),
    likelihood(modele_arch1),
    likelihood(modele_garch11)
  ),
  Parametres = c(
    length(coef(modele_AR1)),
    length(coef(modele_arch1)),
    length(coef(modele_garch11))
  )
)

# Calculer AIC/BIC manuellement sur la même base
comparaison_corrigee$AIC_calc <- -2 * comparaison_corrigee$LogLik + 2 * comparaison_corrigee$Parametres
comparaison_corrigee$BIC_calc <- -2 * comparaison_corrigee$LogLik + log(length(des_data)) * comparaison_corrigee$Parametres

# Afficher la comparaison corrigée
print("=== COMPARAISON CORRIGÉE ===")
print(comparaison_corrigee[, c("Modele", "AIC_calc", "BIC_calc")])

# Calculer l'amélioration réelle
aic_ref <- comparaison_corrigee$AIC_calc[1]
cat("\nAmélioration réelle :\n")
cat("ARCH(1) vs AR(1):", round(aic_ref - comparaison_corrigee$AIC_calc[2], 2), "points AIC\n")
cat("GARCH(1,1) vs AR(1):", round(aic_ref - comparaison_corrigee$AIC_calc[3], 2), "points AIC\n")

# Vérification des résidus standardisés
par(mfrow = c(2, 3))

# ARCH(1)
residus_arch_std <- residuals(modele_arch1, standardize = TRUE)
acf(residus_arch_std, main = "ACF résidus ARCH(1) std")
pacf(residus_arch_std, main = "PACF résidus ARCH(1) std")
qqnorm(residus_arch_std, main = "QQ-Plot ARCH(1)")
qqline(residus_arch_std)

# GARCH(1,1)
residus_garch_std <- residuals(modele_garch11, standardize = TRUE)
acf(residus_garch_std, main = "ACF résidus GARCH(1,1) std")
pacf(residus_garch_std, main = "PACF résidus GARCH(1,1) std")
qqnorm(residus_garch_std, main = "QQ-Plot GARCH(1,1)")
qqline(residus_garch_std)

par(mfrow = c(1, 1))

cat("=== SYNTHÈSE DE LA QUALITÉ ===\n")

# Fonction pour évaluer la qualité
evaluer_qualite <- function(residus_std, nom_modele) {
  lb_residus <- Box.test(residus_std, lag = 12)$p.value
  lb_carres <- Box.test(residus_std^2, lag = 12)$p.value
  arch_test <- ArchTest(residus_std, lags = 12)$p.value
  
  cat(nom_modele, ":\n")
  cat("  Résidus ~ bruit blanc:",
      ifelse(lb_residus > 0.05, "✓ OK", "✗ PROBLÈME"), "(p =", round(lb_residus, 4), ")\n")
  cat("  Variance constante:",
      ifelse(lb_carres > 0.05, "✓ OK", "✗ PROBLÈME"), "(p =", round(lb_carres, 4), ")\n")
  cat("  Pas d'ARCH résiduel:",
      ifelse(arch_test > 0.05, "✓ OK", "✗ PROBLÈME"), "(p =", round(arch_test, 4), ")\n")
}

evaluer_qualite(residus_arch_std, "ARCH(1)")
evaluer_qualite(residus_garch_std, "GARCH(1,1)")
#arch et garch n'améliorent pas le AIC et BIC

#Partie 4 : utilisation de AR1 comme meilleur modèle

#prévisions des 12 prochains mois
#refaison un modele AR1 sans prendre les 12 dernières valeurs de des_data
#cela permet d'avoir des données d'entrainements et tests 
modele_AR1_propre <- arima(des_data[1:(length(des_data)-12)], order =c(1,0,0))
previsions_des_data <- forecast(modele_AR1_propre,h=12)

#repartir sur la série originale depuis des_data
derniere_annee <- tail(ts_TAVG,12)

# Reconstruire les prévisions niveau original
previsions_original <- numeric(12)
for(i in 1:12) {
  previsions_original[i] <- derniere_annee[i] + previsions_des_data$mean[i]
}

# 3. Créer un objet ts pour les prévisions
previsions_ts <- ts(previsions_original, 
                    start = end(ts_TAVG) + c(0, 1),  # Début après la dernière observation
                    frequency = 12)

# 4. Tracer la série originale + prévisions
plot(ts_TAVG, main = "Prévisions des températures - 12 mois", 
     xlab = "Temps", ylab = "Température (°C)", lwd = 1.5)
lines(previsions_ts, col = "red", lwd = 2)
legend("topleft", legend = c("Historique", "Prévisions"), 
       col = c("black", "red"), lwd = 2)

# 5. Afficher les valeurs des prévisions
cat("Prévisions des 12 prochains mois :\n")
print(previsions_ts)



#Comparaison prévision et réalité sur dernière année
# 1. Convertir ts_TAVG en vecteur avec indices
data_vector <- as.numeric(ts_TAVG)
n <- length(data_vector)

# 2. Définir les périodes par indices
# Dernière année : indices (n-11) à n
# Avant-dernière année : indices (n-23) à (n-12)

indices_avant_derniere <- (n-23):(n-12)  # Année N-1
indices_derniere <- (n-11):n             # Année N

# 3. Prévisions pour la dernière année basée sur l'avant-dernière
previsions_derniere <- numeric(12)
for(i in 1:12) {
  previsions_derniere[i] <- data_vector[indices_avant_derniere[i]] + previsions_des_data$mean[i]
}

# 4. Réalité de la dernière année
realite_derniere <- data_vector[indices_derniere]


# Reconstruire les intervalles au niveau original
previsions_lower <- numeric(12)
previsions_upper <- numeric(12)

for(i in 1:12) {
  previsions_lower[i] <- data_vector[indices_avant_derniere[i]] + previsions_des_data$lower[i, 2]
  previsions_upper[i] <- data_vector[indices_avant_derniere[i]] + previsions_des_data$upper[i, 2]
}


# 5. TRACER simple
mois <- 1:12
plot(mois, previsions_derniere, type = "l", col = "red", lwd = 2,
     main = "Prévisions vs Réalité - Dernière année",
     xlab = "Mois", ylab = "Température (°C)", 
     ylim = range(c(previsions_derniere, realite_derniere)))
lines(mois, realite_derniere, col = "blue", lwd = 2, type = "o")
points(mois, realite_derniere, col = "blue", pch = 16)

# Ajouter la zone d'incertitude en fond rouge transparent
polygon(c(mois, rev(mois)), 
        c(previsions_lower, rev(previsions_upper)),
        col = rgb(1, 0, 0, 0.2), border = NA)

# Redessiner les lignes par-dessus
lines(mois, previsions_derniere, col = "red", lwd = 2)
lines(mois, realite_derniere, col = "blue", lwd = 2, type = "o")
points(mois, realite_derniere, col = "blue", pch = 16)

# Mettre à jour la légende
legend("bottomright", 
       legend = c("Prévisions", "IC 95%", "Réalité"),
       col = c("red", rgb(1, 0, 0, 0.3), "blue"), 
       lwd = c(2, 10, 2), pch = c(NA, NA, 16),
       bty = "n",
       cex = 1.0 )

# 6. METRIQUES
mse <- mean((previsions_derniere - realite_derniere)^2)
mape <- mean(abs((previsions_derniere - realite_derniere)/realite_derniere)) * 100
rmse <- sqrt(mse)

cat("=== PERFORMANCE ===\n")
cat("MSE:", round(mse, 3), "\n")
cat("RMSE:", round(rmse, 3), "°C\n")
cat("MAPE:", round(mape, 2), "%\n")

# 7. TABLEAU détaillé
comparaison <- data.frame(
  Mois = month.abb,
  Indice = indices_derniere,
  Réel = round(realite_derniere, 1),
  Prévision = round(previsions_derniere, 1),
  Erreur = round(previsions_derniere - realite_derniere, 1)
)

print("=== COMPARAISON DÉTAILLÉE ===")
print(comparaison)

#RMSE de 2.052°C et MAPE de 16.23% confirme un bon modèle (attention au overfitting)

#Avec arima auto 
# Extraire les prévisions auto.arima pour la dernière année
previsions_auto_derniere <- previsions_auto$mean

# Tracer comparaison sur les 12 derniers mois
plot(mois, as.numeric(previsions_auto_derniere), type = "l", col = "red", lwd = 2,
     main = "ARIMA auto vs Réalité - Dernière année",
     xlab = "Mois", ylab = "Température (°C)",
     ylim = range(c(previsions_auto_derniere, realite_derniere)))

# Ajouter IC 95%
polygon(c(mois, rev(mois)), 
        c(previsions_auto$lower[,2], rev(previsions_auto$upper[,2])),
        col = rgb(1, 0, 0, 0.2), border = NA)

# Relancer les lignes
lines(mois, as.numeric(previsions_auto_derniere), col = "red", lwd = 2)
lines(mois, realite_derniere, col = "blue", lwd = 2, type = "o")

# Légende
legend("bottomright", 
       legend = c("ARIMA auto", "IC 95%", "Réalité"),
       col = c("red", rgb(1, 0, 0, 0.3), "blue"), 
       lwd = c(2, 10, 2), pch = c(NA, NA, 1),
       bty = "n",
       cex = 0.3 )

# Comparaison avec tes prévisions AR(1) + désaisonnalisation manuelle
mse_ar1 <- mean((previsions_derniere - realite_derniere)^2)
mse_arima <- mean((as.numeric(previsions_arima$mean) - realite_derniere)^2)

mape_ar1 <- mean(abs((previsions_derniere - realite_derniere)/realite_derniere)) * 100
mape_arima <- mean(abs((as.numeric(previsions_arima$mean) - realite_derniere)/realite_derniere)) * 100

# Tableau comparatif
comparaison <- data.frame(
  Modèle = c("AR(1) + désaisonnalisation", "ARIMA direct"),
  MSE = c(mse_ar1, mse_arima),
  MAPE = c(mape_ar1, mape_arima)
)
print(comparaison)


plot(mois, realite_derniere, type = "o", col = "black", lwd = 2,
     main = "Comparaison AR1 vs ARIMA auto",
     xlab = "Mois", ylab = "Température (°C)")
lines(mois, previsions_derniere, col = "red", lwd = 2)  # Ton AR1
lines(mois, as.numeric(previsions_auto_derniere), col = "blue", lwd = 2)  # ARIMA auto
legend("topright", bty = "n", cex = 0.7,
       legend = c("Réalité", "AR1", "ARIMA auto"),
       col = c("black", "red", "blue"), lwd = 2)

#Le modèle utilisé AR1 est satisfaisant mais arima auto prend mieux en compte la saisonnalité car il applique aussi AR1 a la série B12
