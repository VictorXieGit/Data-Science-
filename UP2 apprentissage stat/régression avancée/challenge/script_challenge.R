# Script R challenge 2025-2026
# Majeure SD, UP2 : apprentissage statistique
# Techniques de Régression pour la prédiction

# Chargement des librairies
library(tidyverse)  # Pour la manipulation de données (dplyr, tidyr) et ggplot2
library(caret)      # MACHINE LEARNING - Création de partitions, validation croisée, etc.
library(glmnet)     # Pour Ridge et Lasso
library(pls)        # Pour PCR et PLS
library(randomForest)

# Pour la reproductibilité
set.seed(123)

# 0. Données d'apprentissage

data <- read.table(file="data.txt", header=TRUE)
y <- data[,1]             # réponse
X <- data[,-1]            # matrice des prédicteurs

names(X[,1:5])            # noms des 5 premiers prédicteurs

# --- DÉBUT AJOUT INTELLIGENT (Section 0) ---

# --- DÉBUT AJOUT INTELLIGENT CORRIGÉ (Section 0) ---

# 1. Suppression des variables à variance quasi-nulle (ON GARDE)
nzv <- nearZeroVar(X)
if(length(nzv) > 0) {
  X <- X[, -nzv]
  cat("Variables à variance nulle supprimées :", length(nzv), "\n")
}

# 2. Suppression des variables trop corrélées (ON ENLÈVE)
# EXPLICATION : Ridge et PLS aiment la corrélation. On ne supprime PAS ça ici.
# On laisse les modèles gérer la redondance.

# 3. Sauvegarde des noms
vars_to_keep <- colnames(X)
print(paste("Nombre final de variables utilisées :", ncol(X)))

n <- dim(data)[1]
p <- dim(data)[2] - 1     # p = nombre de prédicteurs < n


# 1. Prédiction par la moyenne (prédicteur constant)

prediction_constante <- mean(y)

plot(y, xlab="numéro d'observation")
abline(h=c(0, prediction_constante), lty=c(2,1), col=c("black","red"))
title('Prédiction par la moyenne')
grid()

RMSE_ref <- sd(y)         # écart-type de y
print(round(RMSE_ref,2))

# 2. Différentes techniques de régression
# A vous de jouer et faire beaucoup mieux que RMSE_ref

# 3. Prédictions associées au prédicteur constant (qui ne tient pas compte des prédicteurs) 

# Données test pour le calcul des prédictions demandées dans le challenge

Xnew <- read.table(file="Xtest.txt", header=TRUE)

# --- AJOUT CRUCIAL ICI ---
# On ne garde que les mêmes variables que celles retenues lors de l'apprentissage
Xnew <- Xnew[, vars_to_keep]

head(Xnew[,1:8])   # pour vérifier la bonne lecture des données test

n.test <- dim(Xnew)[1] # nombre de prédictions à faire

predictions <- matrix(0, ncol=1, nrow=n.test) # initialisation
for (k in 1:n.test){
  predictions[k] <- prediction_constante
}

# Fichier texte des prédictions 

write.table(predictions, row.names=FALSE, col.names=FALSE,
            file="BAY_Xavier.txt")

# S'assurer que ce fichier a le bon format en l'ouvrant avec un éditeur
# de texte quelconque et le déposer sur Campus

#Préparation données
#Mise en place d'une méthode de cross-validation multiple pour vérifier à travers les données
fitControl <- trainControl(
  method = "repeatedcv",
  number = 5,           # 10 folds
  repeats = 3,
  savePredictions = "final",
  selectionFunction = "best",
  summaryFunction = defaultSummary,
  returnData = FALSE     # Économiser la mémoire
)
fit_oob <- trainControl(
  method = "boot",           # Bootstrap
  number = 100,              # 100 réplications bootstrap
  savePredictions = "final",
  summaryFunction = defaultSummary,
  verboseIter = TRUE
)


# Fonction pour calculer le RMSE
calc_rmse <- function(observations, predictions) {
  sqrt(mean((observations - predictions)^2))
}

#Mise en place des fonctions pour calculs des modèles 
# 1. Ridge (Laisse Caret choisir Lambda)
train_ridge_cv <- function(X, y, fitControl) {  
  train(x=X, y=y, method = "glmnet",
        preProcess = c("center", "scale"),
        trControl = fitControl,
        tuneGrid = expand.grid(alpha = 0, lambda = 10^seq(-3, 1, length = 100)), # Grille plus large
        metric = "RMSE")
}

# 2. Lasso (Grille élargie vers les petites valeurs)
train_lasso_cv <- function(X, y, fitControl) {  
  train(x=X, y=y, method = "glmnet",
        preProcess = c("center", "scale"),
        trControl = fitControl,
        tuneGrid = expand.grid(alpha = 1, lambda = 10^seq(-4, 0, length = 100)), # On cherche plus petit
        metric = "RMSE")
}

train_elasticnet_optimized <- function(X, y, fitControl) {
  train(
    x = X, y = y,
    method = "glmnet",
    preProcess = c("center", "scale"),
    trControl = fitControl,
    tuneGrid = expand.grid(
      # On zoome entre 0.8 et 1 (proche du Lasso)
      alpha = seq(0.8, 1, length = 10),
      # On zoome autour de ta meilleure valeur trouvée (0.2482)
      lambda = seq(0.1, 0.4, length = 50) 
    ),
    metric = "RMSE"
  )
}
# N'oublie pas de l'appeler : models$elasticnet <- train_elasticnet_optimized(X, y, fitControl)

# 4. PLS (Très bon sur ce type de données)
train_pls_cv <- function(X, y, fitControl) {  
  train(x=X, y=y, method = "pls",
        preProcess = c("center", "scale"),
        trControl = fitControl,
        tuneLength = 20, # Teste jusqu'à 20 composantes
        metric = "RMSE")
}

# 5. Fonction pour Random Forest
train_rf_cv <- function(X, y, fitControl) {  
  # Attention : le calcul peut être un peu plus long que pour Ridge/Lasso
  model <- train(
    x = X, y = y,
    method = "rf",  # Utilise le package randomForest
    # preProcess : Moins critique pour RF, mais on peut le garder pour la cohérence
    # trControl utilise ta configuration (repeatedcv)
    trControl = fitControl,
    # tuneLength : Caret va tester automatiquement plusieurs valeurs de 'mtry'
    # (le nombre de variables testées à chaque embranchement de l'arbre)
    tuneLength = 2, 
    metric = "RMSE",
    importance = TRUE # Pour voir quelles variables sont importantes après
  )
  return(model)
}
library(ranger) # Plus rapide que randomForest standard

train_rf_optimized <- function(X, y, fitControl) {
  train(
    x = X, y = y,
    method = "ranger", # Version rapide de Random Forest
    trControl = fitControl,
    tuneLength = 10,   # Laisse caret chercher le mtry optimal
    num.trees = 500,   # Nombre d'arbres
    importance = "impurity",
    metric = "RMSE"
  )
}
# models$rf <- train_rf_optimized(X, y, fitControl)
# 1. Récupérer les variables importantes du Lasso
coefs <- coef(models$lasso$finalModel, models$lasso$bestTune$lambda)
# On prend les indices des coefficients non-nuls (en enlevant l'intercept)
vars_selected <- rownames(coefs)[which(coefs != 0)]
vars_selected <- vars_selected[vars_selected != "(Intercept)"]

cat("Nombre de variables retenues par Lasso :", length(vars_selected), "\n")

# 2. Créer un dataset réduit
X_reduced <- X[, vars_selected]

# 3. Entraîner un modèle Ridge sur ces variables "propres"
cat("Entrainement Ridge sur variables sélectionnées...\n")
model_ridge_refined <- train(
    x = X_reduced, y = y,
    method = "glmnet",
    preProcess = c("center", "scale"),
    trControl = fitControl,
    tuneGrid = expand.grid(alpha = 0, lambda = 10^seq(-3, 1, length = 50))
)
print(min(model_ridge_refined$results$RMSE)) # Regarde si c'est mieux que 17.68
# Chronométrage pour voir les temps d'exécution
start_time <- Sys.time()

# Entraînement des modèles avec repeatedcv
models <- list()

models$ridge <- train_ridge_cv(X, y, fitControl)
cat("Ridge terminé -", round(Sys.time() - start_time, 1), "secondes\n")

models$lasso <- train_lasso_cv(X, y, fitControl) 
cat("Lasso terminé -", round(Sys.time() - start_time, 1), "secondes\n")

models$lasso_oob <- train_lasso_oob(X, y, fit_oob)

models$pcr <- train_pcr_cv(X, y, fitControl)
cat("PCR terminé -", round(Sys.time() - start_time, 1), "secondes\n")

models$pls <- train_pls_cv(X, y, fitControl)
cat("PLS terminé -", round(Sys.time() - start_time, 1), "secondes\n")

models$elasticnet <- train_elasticnet_optimized(X, y , fitControl)
cat("elasticnet terminé -", round(Sys.time() - start_time, 1), "secondes\n")

#models$rf <- train_rf_optimized(X, y, fitControl)

# Affichage du temps total
total_time <- round(Sys.time() - start_time, 1)
cat("TOTAL :", total_time, "secondes\n")

# Fonction pour extraire les résultats
extract_results <- function(models_list) {
  results <- data.frame(
    Model = names(models_list),
    RMSE_CV = sapply(models_list, function(x) min(x$results$RMSE)),
    RSquared = sapply(models_list, function(x) max(x$results$Rsquared)),
    Best_Params = sapply(models_list, function(x) {
      params <- x$bestTune
      paste(names(params), "=", sapply(params, function(p) {
        if(is.numeric(p)) round(p, 4) else p
      }), collapse = ", ")
    })
  )
  
  return(results[order(results$RMSE_CV), ])
}

# Affichage des résultats
results_df <- extract_results(models)
print(results_df)
# Visualisation
library(ggplot2)
ggplot(results_df, aes(x = reorder(Model, RMSE_CV), y = RMSE_CV, fill = Model)) +
  geom_col(alpha = 0.8) +
  geom_text(aes(label = round(RMSE_CV, 4)), vjust = -0.5, size = 3) +
  labs(title = "Comparaison des modèles par Repeated CV (10-fold × 3)",
       subtitle = paste("Temps total :", total_time, "secondes"),
       x = "Modèle", y = "RMSE") +
  theme_minimal() +
  theme(legend.position = "none")


# ==============================================================================
# ÉTAPE FINALE : OPTIMISATION "RELAXED LASSO" ET GÉNÉRATION DU FICHIER
# ==============================================================================

cat("\n--- Démarrage de l'optimisation finale ---\n")

# 1. Récupération des variables clés du Lasso
# On extrait les coefficients du meilleur Lasso trouvé
coefs <- as.matrix(coef(models$lasso$finalModel, models$lasso$bestTune$lambda))
vars_selected <- rownames(coefs)[coefs != 0]
vars_selected <- vars_selected[vars_selected != "(Intercept)"] # On enlève l'intercept

cat("Le Lasso a sélectionné", length(vars_selected), "variables importantes.\n")

# Variable pour stocker le meilleur modèle final (par défaut : Elastic Net)
best_model_final <- models$elasticnet
best_rmse <- min(models$elasticnet$results$RMSE)
method_used <- "ElasticNet (Original)"

# 2. Tentative de 'Ridge Raffiné' (Seulement si Lasso a sélectionné des variables)
if (length(vars_selected) > 0 && length(vars_selected) < ncol(X)) {
  
  cat("Tentative d'amélioration avec un Ridge sur ces variables...\n")
  
  # Création du dataset réduit
  X_reduced <- X[, vars_selected, drop = FALSE]
  
  # Contrôle rapide (pas de répétition pour aller vite)
  fitControl_fast <- trainControl(method = "cv", number = 10, allowParallel = TRUE)
  
  # Entraînement rapide
  model_ridge_refined <- train(
    x = X_reduced, y = y,
    method = "glmnet",
    preProcess = c("center", "scale"),
    trControl = fitControl_fast,
    tuneGrid = expand.grid(alpha = 0, lambda = 10^seq(-3, 1, length = 20)) # Grille rapide
  )
  
  rmse_refined <- min(model_ridge_refined$results$RMSE)
  cat("RMSE Ridge Raffiné :", round(rmse_refined, 4), "\n")
  cat("RMSE ElasticNet Ref:", round(best_rmse, 4), "\n")
  
  # Décision : On garde le nouveau si il est meilleur
  if (rmse_refined < best_rmse) {
    cat("=> SUCCÈS ! Le modèle raffiné est meilleur.\n")
    best_model_final <- model_ridge_refined
    best_rmse <- rmse_refined
    method_used <- "Ridge Raffiné (Optimisé)"
  } else {
    cat("=> Pas d'amélioration. On garde l'Elastic Net.\n")
  }
  
} else {
  cat("Pas de sélection de variables suffisante pour tenter l'optimisation.\n")
}

# ==============================================================================
# GÉNÉRATION DES PRÉDICTIONS (FICHIER FINAL)
# ==============================================================================

cat("\n--- Génération des prédictions avec :", method_used, "---\n")

# Chargement propre des données test
Xnew <- read.table(file="Xtest.txt", header=TRUE)

# Prédiction selon la méthode choisie
if (method_used == "Ridge Raffiné (Optimisé)") {
  # Si on utilise le modèle raffiné, il faut réduire Xnew aux MÊMES colonnes
  # On vérifie que les colonnes existent bien
  cols_missing <- setdiff(vars_selected, colnames(Xnew))
  if(length(cols_missing) > 0) stop("Erreur : Colonnes manquantes dans Xtest")
  
  Xnew_reduced <- Xnew[, vars_selected, drop = FALSE]
  final_predictions <- predict(best_model_final, newdata = Xnew_reduced)
  
} else {
  # Sinon (Elastic Net classique), on utilise toutes les colonnes (filtrées au début du script)
  # Rappel : On applique le filtre vars_to_keep défini au début du script
  if(exists("vars_to_keep")) {
     Xnew <- Xnew[, vars_to_keep] 
  }
  final_predictions <- predict(best_model_final, newdata = Xnew)
}

# Écriture du fichier
output_name <- "BAY_Xavier.txt"
write.table(final_predictions, row.names=FALSE, col.names=FALSE, file=output_name)

cat("Fichier généré avec succès :", output_name, "\n")
cat("RMSE espéré :", round(best_rmse, 4), "\n")