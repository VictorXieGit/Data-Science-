# ==============================================================================
# SCRIPT DE SECOURS - CHALLENGE DATA SCIENCE
# Version Autonome (Gère les librairies manquantes)
# ==============================================================================

# 1. GESTION DES LIBRAIRIES (AUTO-REPARATION)
# ==============================================================================
required_packages <- c("tidyverse", "caret", "glmnet", "doParallel")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

library(tidyverse)
library(caret)
library(glmnet)

# Gestion du Parallélisme (Sécurité)
if(require(doParallel)) {
  n_cores <- detectCores() - 1
  cl <- makePSOCKcluster(n_cores)
  registerDoParallel(cl)
  cat("🚀 Cluster activé sur", n_cores, "cœurs.\n")
  USE_PARALLEL <- TRUE
} else {
  cat("⚠️ doParallel non trouvé. Mode séquentiel activé.\n")
  USE_PARALLEL <- FALSE
}

set.seed(123)

# 2. CHARGEMENT ET NETTOYAGE
# ==============================================================================
if(!file.exists("data.txt") || !file.exists("Xtest.txt")) {
  stop("ERREUR : Les fichiers 'data.txt' ou 'Xtest.txt' sont absents du dossier de travail !")
}

data <- read.table(file="data.txt", header=TRUE)
y <- data[,1]
X <- data[,-1]

# Suppression Variance Nulle
nzv <- nearZeroVar(X)
if(length(nzv) > 0) X <- X[, -nzv]
vars_keep <- colnames(X)

cat("✅ Données chargées :", ncol(X), "variables retenues.\n")


# 3. ENTRAÎNEMENT FINAL (BLENDING OPTIMISÉ)
# ==============================================================================
cat("\n🏆 Démarrage de l'entraînement final...\n")

# Configuration Robuste
folds <- createMultiFolds(y, k = 10, times = 5)
ctrl <- trainControl(
  method = "repeatedcv", 
  number = 10, 
  repeats = 5,
  index = folds,             # Synchronisation pour le blending
  savePredictions = "final", 
  allowParallel = USE_PARALLEL
)

# Modèle A : LASSO (SpatialSign)
cat("   - Entraînement Lasso...\n")
mod_lasso <- train(
  x = X, y = y, method = "glmnet",
  preProcess = c("center", "scale", "spatialSign"),
  tuneGrid = expand.grid(alpha = 1, lambda = seq(0.1, 0.3, length=20)),
  trControl = ctrl,
  metric = "RMSE"
)

# Modèle B : ELASTIC NET (SpatialSign)
cat("   - Entraînement Elastic Net...\n")
mod_enet <- train(
  x = X, y = y, method = "glmnet",
  preProcess = c("center", "scale", "spatialSign"),
  tuneGrid = expand.grid(alpha = seq(0.85, 0.95, length=10), lambda = seq(0.1, 0.3, length=20)),
  trControl = ctrl,
  metric = "RMSE"
)

# 4. OPTIMISATION DU BLENDING
# ==============================================================================
cat("\n⚗️  Calcul du mélange optimal...\n")

# Extraction propre des prédictions CV
get_preds <- function(model) {
  model$pred %>% 
    arrange(rowIndex) %>% 
    group_by(rowIndex) %>% 
    summarise(pred = mean(pred), obs = mean(obs))
}

p_lasso <- get_preds(mod_lasso)
p_enet  <- get_preds(mod_enet)
truth   <- p_lasso$obs

best_w <- 0.5
best_rmse <- Inf

for(w in seq(0, 1, by=0.01)) {
  blend <- (w * p_lasso$pred) + ((1-w) * p_enet$pred)
  rmse_val <- sqrt(mean((truth - blend)^2))
  if(rmse_val < best_rmse) { best_rmse <- rmse_val; best_w <- w }
}

cat("✅ RÉSULTATS :\n")
cat("   RMSE Lasso Seul :", min(mod_lasso$results$RMSE), "\n")
cat("   RMSE Enet Seul  :", min(mod_enet$results$RMSE), "\n")
cat("   RMSE BLENDING   :", best_rmse, "\n")
cat("   Poids Optimal   :", best_w, "Lasso +", (1-best_w), "Enet\n")


# 5. GÉNÉRATION DU FICHIER
# ==============================================================================
Xnew <- read.table(file="Xtest.txt", header=TRUE)
# On s'assure de garder les mêmes colonnes
Xnew <- Xnew[, vars_keep]

pred_L <- predict(mod_lasso, newdata = Xnew)
pred_E <- predict(mod_enet, newdata = Xnew)

final_submission <- (best_w * pred_L) + ((1-best_w) * pred_E)

output_name <- "BAY_Xavier.txt"
write.table(final_submission, row.names=FALSE, col.names=FALSE, file=output_name)

cat("\n🎉 FICHIER GÉNÉRÉ :", output_name, "\n")
cat("   Tu peux le déposer sur Campus !\n")

# Nettoyage
if(USE_PARALLEL) try(stopCluster(cl), silent=TRUE)