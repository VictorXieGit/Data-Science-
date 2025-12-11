# ==============================================================================
# SCRIPT FINAL - CHALLENGE DATA SCIENCE (VERSION GAGNANTE)
# Stratégie : Blending (Mélange) Lasso + Elastic Net avec SpatialSign
# ==============================================================================

library(tidyverse)
library(caret)
library(glmnet)
library(doParallel)

set.seed(123)

# 1. ACTIVATION PARALLÈLE
# ==============================================================================
n_cores <- detectCores() - 1
cl <- makePSOCKcluster(n_cores)
registerDoParallel(cl)
cat("🚀 Calcul sur", n_cores, "cœurs.\n")

# 2. CHARGEMENT
# ==============================================================================
data <- read.table(file="data.txt", header=TRUE)
y <- data[,1]
X <- data[,-1]

# Nettoyage de base (Zero Variance)
nzv <- nearZeroVar(X)
if(length(nzv) > 0) X <- X[, -nzv]
vars_to_keep <- colnames(X)

cat("Données chargées :", ncol(X), "variables retenues.\n")

# 3. ENTRAÎNEMENT SYNCHRONISÉ (CRUCIAL POUR LE BLENDING)
# ==============================================================================
# On force les deux modèles à utiliser EXACTEMENT les mêmes découpages (Folds)
# pour que la comparaison de leurs erreurs soit valide mathématiquement.
folds <- createMultiFolds(y, k = 10, times = 10)

fitControl <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 10,
  index = folds,             # Synchronisation des folds
  savePredictions = "final", # Obligatoire pour le blending
  selectionFunction = "best",
  allowParallel = TRUE
)

cat("\n🏋️ Entraînement des champions...\n")

# Modèle 1 : ELASTIC NET (Le meilleur actuel : RMSE ~16.52)
# On utilise les hyperparamètres que tu as trouvés (alpha ~0.9, lambda ~0.2)
grid_enet <- expand.grid(
  alpha = seq(0.85, 0.95, length = 10), 
  lambda = seq(0.15, 0.25, length = 20)
)
model_enet <- train(
  x = X, y = y, method = "glmnet",
  preProcess = c("center", "scale", "spatialSign"), # La recette secrète
  trControl = fitControl,
  tuneGrid = grid_enet,
  metric = "RMSE"
)

# Modèle 2 : LASSO (Le challenger : RMSE ~16.59)
grid_lasso <- expand.grid(
  alpha = 1, 
  lambda = seq(0.15, 0.25, length = 20)
)
model_lasso <- train(
  x = X, y = y, method = "glmnet",
  preProcess = c("center", "scale", "spatialSign"),
  trControl = fitControl,
  tuneGrid = grid_lasso,
  metric = "RMSE"
)

# 4. CALCUL DU MÉLANGE OPTIMAL (BLENDING)
# ==============================================================================
cat("\n⚗️  Recherche du mélange parfait...\n")

# Récupération des prédictions sur les données de validation (Out-of-Fold)
# On trie par index pour être sûr que la ligne 1 correspond bien à la ligne 1
get_preds <- function(model) {
  model$pred %>% arrange(rowIndex) %>% group_by(rowIndex) %>% summarise(pred = mean(pred), obs = mean(obs))
}

preds_enet <- get_preds(model_enet)
preds_lasso <- get_preds(model_lasso)
truth <- preds_enet$obs # Les vraies valeurs de Y

best_rmse <- Inf
best_w <- 0.5

# On teste tous les poids de 0 à 1 (0% Enet à 100% Enet)
for(w in seq(0, 1, by = 0.01)) {
  # Mélange pondéré
  blend <- (w * preds_enet$pred) + ((1 - w) * preds_lasso$pred)
  rmse <- sqrt(mean((truth - blend)^2))
  
  if(rmse < best_rmse) {
    best_rmse <- rmse
    best_w <- w
  }
}

cat("\n🏆 RÉSULTATS FINAUX :\n")
cat("   - RMSE ElasticNet seul :", round(min(model_enet$results$RMSE), 4), "\n")
cat("   - RMSE Lasso seul      :", round(min(model_lasso$results$RMSE), 4), "\n")
cat("   ------------------------------------------------\n")
cat("   - MEILLEUR MÉLANGE     :", round(best_rmse, 4), "\n")
cat("   - Proportion           :", best_w*100, "% ElasticNet +", (1-best_w)*100, "% Lasso\n")

# 5. GÉNÉRATION FICHIER
# ==============================================================================
Xnew <- read.table(file="Xtest.txt", header=TRUE)
Xnew <- Xnew[, vars_to_keep]

# Prédictions brutes
p_enet_final <- predict(model_enet, newdata = Xnew)
p_lasso_final <- predict(model_lasso, newdata = Xnew)

# Mélange final avec le poids optimal trouvé
final_blend <- (best_w * p_enet_final) + ((1 - best_w) * p_lasso_final)

write.table(final_blend, row.names=FALSE, col.names=FALSE, file="BAY_Xavier.txt")
cat("\n✅ Fichier 'BAY_Xavier.txt' généré avec succès.\n")

# Arrêt propre
if(require(doParallel)) {
  try(stopCluster(cl), silent=TRUE)
  try(registerDoSEQ(), silent=TRUE)
}