#si il y a des zeros on enlève la ligne, pareil pour une colonne, sinon on peut aussi essayer de la reconstruire # nolint
#nolint start
setwd("C:\\Users\\victo\\OneDrive\\Bureau\\COURS général\\cours mine 2526\\sdd\\UP2 apprentissage stat\\analyse de données ACP AFC") # nolint
data <- read.csv("TP2_ACP.csv", head = 1, sep = ";")
dim(data)
data <- data[, -1]

colnames(data)
#cherchons les valeurs nulles ou manquantes
#################  nombre de données manquante

# Load required libraries
library(readr)
library(dplyr)
library(FactoMineR)
library(ggplot2)
library(psych)

# Display basic information about the dataset
cat("Dataset dimensions:", dim(data), "\n")
cat("Column names:\n")
print(names(data))
cat("\nFirst look at the data:\n")
print(head(data))

# Check for missing values and zeros
cat("\n=== MISSING VALUES AND ZEROS ANALYSIS ===\n")

# Check for "?" values
question_marks <- sum(apply(data, 2, function(x) sum(x == "?", na.rm = TRUE)))
cat("Number of '?' values:", question_marks, "\n")

# Check for zeros in quantitative variables
quantitative_vars <- c("B", "T", "E", "X", "X9_ane", "X10_ane", "X13_ane", "X14_ane","X1_M_2_PA", "BTM", "FormicAcid", "aceticacid", "NonaDecanoicAc", "Tot_OcNoDecana") # nolint

zero_counts <- sapply(data[quantitative_vars], function(x) sum(x == 0, na.rm = TRUE)) # nolint
cat("\nZero counts in quantitative variables:\n")
print(zero_counts)

# Check data types
cat("\nData types:\n")
print(sapply(data, class))

# Convert quantitative variables to numeric and handle "?" values
data_clean <- data

# Replace "?" with NA in all columns
data_clean[data_clean == "?"] <- NA

for (var in quantitative_vars) {
  # Remplacer les virgules par des points (séparateur décimal)
  data_clean[[var]] <- gsub(",", ".", data_clean[[var]])
  # Convertir en numérique
  data_clean[[var]] <- as.numeric(data_clean[[var]])
}

# Une seule ligne pour enlever les lignes avec 0 ou NA
data_clean <- data_clean[complete.cases(data_clean[quantitative_vars]) & rowSums(data_clean[quantitative_vars] == 0, na.rm = TRUE) == 0, ] # nolint

# Check remaining missing values
missing_after_cleaning <- sapply(data_clean[quantitative_vars], function(x) sum(is.na(x))) #nolint
cat("\nMissing values after cleaning:\n")
print(missing_after_cleaning)

# Check for outliers using boxplots
cat("\n=== OUTLIER DETECTION ===\n")
par(mfrow = c(4, 4))
for (var in quantitative_vars) {
  boxplot(data_clean[[var]], main = var)
}

# Log transformation to handle skewness and outliers
data_transformed <- data_clean
for (var in quantitative_vars) {
  data_transformed[[var]] <- log1p(data_transformed[[var]])
}
# Visualiser avant/après transformation
par(mfrow = c(1, 2))
boxplot(data_clean[quantitative_vars], main = "Avant transformation")
boxplot(data_transformed[quantitative_vars], main = "Après log1p")

# Final dataset check
cat("\n=== FINAL DATASET SUMMARY ===\n")
cat("Final dimensions:", dim(data_transformed), "\n")
cat("Data types:\n")
print(sapply(data_transformed, class))

# Save cleaned dataset
write.csv(data_transformed, "TP2_ACP_cleaned.csv", row.names = FALSE)
cat("\nCleaned dataset saved as 'TP2_ACP_cleaned.csv'\n")


# Résumé statistique complet
stats_descriptives <- describe(data_transformed[quantitative_vars])
print(stats_descriptives[, c("n", "mean", "sd", "median", "min", "max", "skew", "kurtosis")]) #nolint

# Sauvegarder dans un dataframe pour rapport
stats_summary <- data.frame(
  Variable = quantitative_vars,
  Moyenne = round(sapply(data_transformed[quantitative_vars], mean, na.rm = TRUE), 3),
  Ecart_Type = round(sapply(data_transformed[quantitative_vars], sd, na.rm = TRUE), 3),
  Mediane = round(sapply(data_transformed[quantitative_vars], median, na.rm = TRUE), 3),
  Minimum = round(sapply(data_transformed[quantitative_vars], min, na.rm = TRUE), 3),
  Maximum = round(sapply(data_transformed[quantitative_vars], max, na.rm = TRUE), 3),
  CV = round(sapply(data_transformed[quantitative_vars], function(x) sd(x, na.rm = TRUE)/mean(x, na.rm = TRUE)), 3)
)

cat("\nTableau récapitulatif (après transformation):\n")
print(stats_summary)

#Examen de l'influence des opérations sur la présence de produits chimiques

# Créer la variable Période (Avant/Après)
data_transformed <- data_transformed %>%
  mutate(Periode = ifelse(grepl("BF", Campagne), "Avant", "Après"))

# BOXPLOTS PAR PÉRIODE (AVANT/APRÈS)
par(mfrow = c(7, 2))
for(var in quantitative_vars[0:14]) {
  boxplot(data_transformed[[var]] ~ data_transformed$Periode, 
          main = paste("Distribution de", var, "- Avant vs Après"),
          xlab = "Période", ylab = var,
          col = c("lightgreen", "lightyellow"))
}



# Prepare data for PCA (only quantitative variables)
pca_data <- data_transformed[quantitative_vars]

# Standardize the data (important for PCA)
pca_data_scaled <- scale(pca_data)

cat("\n=== DATA READY FOR PCA ===\n")
cat("PCA data dimensions:", dim(pca_data_scaled), "\n")
cat("Summary of scaled data:\n")
print(summary(pca_data_scaled))

# You can now proceed with PCA using:
pca_result <- PCA(pca_data_scaled, graph = FALSE)
cat("=== VALEURS PROPRES ET POURCENTAGE D'INERTIE ===\n")
eigenvalues <- pca_result$eig
print(round(eigenvalues, 4))#Nous avons trouvé les valeurs propres, il faut maintenant savoir combien de dimensions on garde.
barplot(eigenvalues[, 2], 
        names.arg = paste("Dim", 1:nrow(eigenvalues)),
        main = "Scree Plot - Pourcentage d'inertie expliquée",
        xlab = "Dimensions",
        ylab = "Pourcentage d'inertie (%)",
        col = "steelblue",
        ylim = c(0, max(eigenvalues[, 2]) + 5))
abline(h = 100/ncol(data_transformed[quantitative_vars]), col = "red", lty = 2)
legend("topright", legend = "Seuil de Kaiser", col = "red", lty = 2)

# Version de secours avec plot de base
dev.new()
barplot(eigenvalues[, 2], 
        names.arg = paste("Dim", 1:nrow(eigenvalues)),
        main = "Scree Plot - Pourcentage d'inertie expliquée",
        xlab = "Dimensions",
        ylab = "Pourcentage d'inertie (%)",
        col = "steelblue",
        ylim = c(0, max(eigenvalues[, 2]) + 5))
lines(1:nrow(eigenvalues), eigenvalues[, 3], type = "b", col = "red", lwd = 2)
points(1:nrow(eigenvalues), eigenvalues[, 3], col = "red", pch = 16)
legend("topright", legend = c("Variance", "Variance cumulée"), 
        col = c("steelblue", "red"), pch = c(15, 16))

#On observe une très bonne cassure à partir de la 3ème composante "méthode de Cattel"
#On va donc se limiter à deux composantes principales
# Se concentrer sur les 2 premières dimensions
cat("\n=== ANALYSE DES 2 PREMIÈRES DIMENSIONS ===\n")

# Variables les plus contributives à Dim1
cat("\nTop variables - Dimension 1 :\n")
dim1_contrib <- sort(pca_result$var$contrib[, 1], decreasing = TRUE)[1:5]
print(round(dim1_contrib, 1))

# Variables les plus contributives à Dim2  
cat("\nTop variables - Dimension 2 :\n")
dim2_contrib <- sort(pca_result$var$contrib[, 2], decreasing = TRUE)[1:5]
print(round(dim2_contrib, 1))

# Qualité de représentation
cos2_2d <- pca_result$var$cos2[, 1] + pca_result$var$cos2[, 2]
cat("\nVariables bien représentées (cos2 > 0.7) :\n")
print(names(cos2_2d)[cos2_2d > 0.7])
#nolint end
