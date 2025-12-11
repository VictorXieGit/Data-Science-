#TP 2 ACP Pierre LUCIEN et Victor XIE
#nolint start

# Chargement des librairies requises
library(readr)
library(dplyr)
library(FactoMineR) #librairie pour l'ACP
library(factoextra)
library(ggplot2) #tracer de meilleur plots
library(psych)
library(corrplot) #tracer le diagramme des corrélations
library(VIM) 

#J'avais des problèmes de visualisation des plots sur vscode
#Il faut récuperer l'URL html dans le Terminal donné par hgd() et le mettre sur un site internet
#Si vous n'avez pas de problème de visualisation, vous pouvez très bien utiliser les plots sous Rstudio

library(httpgd)
hgd()
plot(1:10)


#Changer le setwsd à chaque fois !
setwd("C:\\Users\\victo\\OneDrive\\Bureau\\COURS général\\cours mine 2526\\sdd\\UP2 apprentissage stat\\analyse de données ACP AFC\\ACP") # nolint
data <- read.csv("TP2_ACP.csv", head = 1, sep = ";")

#Enlevons la première colonne d'indice car se répète
data <- data[, -1]

#Visualisation globale du set de data
dim(data)
colnames(data)
print(head(data))

# Regardons le type des colonnes
cat("\nData types:\n")
print(sapply(data, class))
#Chaque colonnes est en character pour l'instant.

#Nous allons sélectionner que les données quantitatives pour travailler avec elles
quantitative_vars <- c("B", "T", "E", "X", "X9_ane", "X10_ane", "X13_ane", "X14_ane","X1_M_2_PA", "BTM", "FormicAcid", "aceticacid", "NonaDecanoicAc", "Tot_OcNoDecana")

#Puis les passer en valeurs numériques
for (var in quantitative_vars) {
  # Remplacer les virgules par des points (séparateur décimal)
  data[[var]] <- gsub(",", ".", data[[var]])
  # Convertir en numérique
  data[[var]] <- as.numeric(data[[var]])
}

#Tracer des histogrammes
par(mfrow = c(2, 2))  # 3x3 graphiques par page
for (col in names(data[quantitative_vars])) {
  hist(data[quantitative_vars][[col]], 
       main = paste("Histogramme de", col),
       col = "lightgreen", 
       border = "white")
}
par(mfrow = c(1, 1))

# Recherche des valeur "?"
question_marks <- sum(apply(data, 2, function(x) sum(x == "?", na.rm = TRUE)))
cat("Le nombre de ? est de", question_marks, "\n")
#Ayant que deux lignes avec des ? nous allons directement les enlever sans perdre en qualité
data <- data[-c(6,59),]
 
# Cherchons les zéros dans nos colonnes
zero_counts <- sapply(data[quantitative_vars], function(x) sum(x == 0, na.rm = TRUE)) # nolint
cat("\nLe nombre de zéros par colonnes est de :\n")
print(zero_counts)

# Nous allons convertir en numérique et remplacer par NA les ?
data_clean <- data
data_clean[data_clean == "0"] <- NA
data_clean[quantitative_vars] <- as.data.frame(lapply(data_clean[quantitative_vars], function(x) as.numeric(as.character(x))))
data_imputed <- data_clean
data_imputed[quantitative_vars] <- kNN(data_clean[quantitative_vars], k = 5)


zero_counts <- sapply(data_imputed[quantitative_vars], function(x) sum(x == 0, na.rm = TRUE)) 
cat("\nLe nombre de zéros par colonnes est de :\n")
print(zero_counts)
#Nous avons bien régler le problème des zéros

#Recherche des valeurs aberrantes par PCA
pca_result <- PCA(data_imputed[quantitative_vars], graph = FALSE)
cat("=== VALEURS PROPRES ET POURCENTAGE D'INERTIE ===\n")
eigenvalues <- pca_result$eig
par(mfrow=c(1,1))
print(round(eigenvalues, 4))#Nous avons trouvé les valeurs propres, il faut maintenant savoir combien de dimensions on garde.
barplot(eigenvalues[, 2], 
        names.arg = paste("Dim", 1:nrow(eigenvalues)),
        main = "Scree Plot - Pourcentage d'inertie expliquée",
        xlab = "Dimensions",
        ylab = "Pourcentage d'inertie (%)",
        col = "steelblue",
        ylim = c(0, max(eigenvalues[, 2]) + 5))
abline(h = 100/ncol(data_imputed[quantitative_vars]), col = "red", lty = 2)
legend("topright", legend = "Seuil de Kaiser", col = "red", lty = 2)

plot(pca_result, choix = "ind", axes = c(1, 2))
plot(pca_result, choix = "ind", axes = c(1, 3))
plot(pca_result, choix = "ind", axes = c(2, 3))
plot(pca_result, choix = "var", axes = c(1, 2))


#D'après l'étude de l'ACP, nous relevons deux valeurs données aberrantes, 78 et 55.
data_transformed <- data[!(rownames(data_imputed) %in% c("78", "55")), ]
for (var in quantitative_vars) {
  data_transformed[[var]] <- scale(log1p(data_transformed[[var]]))
}
# Visualiser avant/après transformation
par(mfrow = c(1, 2))
boxplot(data_clean[quantitative_vars], main = "Avant transformation")
boxplot(data_transformed[quantitative_vars], main = "Après log1p")

#Examen de l'influence des opérations sur la présence de produits chimiques

# Créer la variable Période (Avant/Après)
data_transformed <- data_transformed %>%
  mutate(Periode = ifelse(grepl("BF", Campagne), "Avant", "Après"))

# BOXPLOTS PAR PÉRIODE (AVANT/APRÈS)
for(var in quantitative_vars[0:14]) {
  boxplot(data_transformed[[var]] ~ data_transformed$Periode, 
          main = paste("Distribution de", var, "- Avant vs Après"),
          xlab = "Période", ylab = var,
          col = c("lightgreen", "lightyellow"))
}
outlier_info <- sapply(data_transformed[quantitative_vars], function(col) {
  bp <- boxplot.stats(col)
  list(outliers = bp$out, count = length(bp$out))
})

# On peut maintenant faire l'ACP
pca_data <- data_transformed[quantitative_vars]
pca_result <- PCA(pca_data, graph = FALSE)
cat("=== VALEURS PROPRES ET POURCENTAGE D'INERTIE ===\n")
eigenvalues <- pca_result$eig
par(mfrow=c(1,1))
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

#Regardons de plus la contribution de chaque colonnes
# Tri des variables par contribution sur Dim.1 (axe 1)
round(pca_result$var$contrib[order(-pca_result$var$contrib[, "Dim.1"]), "Dim.1"],2)

# Tri des variables par contribution sur Dim.2 (axe 2)
round(pca_result$var$contrib[order(-pca_result$var$contrib[, "Dim.2"]), "Dim.2"],2)

# Tri des variables par contribution sur Dim.3 (axe 3)
round(pca_result$var$contrib[order(-pca_result$var$contrib[, "Dim.3"]), "Dim.3"],2)


#Nous allons garder 3 composantes principales 


# Extraire les coordonnées des individus et s'assurer que c'est un data.frame
ind_coords <- as.data.frame(pca_result$ind$coord[, 1:3])

# Ajouter les noms des individus
ind_coords$ind <- rownames(ind_coords)

# Ajouter les indices originaux comme colonne
ind_coords$row_index <- 1:nrow(ind_coords)  # indices 1,2,3,... correspondant aux lignes originales

# Créer le groupe selon le seuil
ind_coords$group <- ifelse(ind_coords$row_index < 38, "Avant", "Après")

#Plan factoriel (1,2) avant/après
ggplot(ind_coords, aes(x = Dim.1, y = Dim.2, label = ind, color = group)) +
  geom_point(size = 3) +
  geom_text(vjust = -0.5, size = 3) +
  labs(title = "Plan factoriel (1,2) (ACP) avant et après intervention",
       x = "PC1", y = "PC2") +
  scale_color_manual(values = c("Avant" = "steelblue", "Après" = "tomato")) +
  theme_minimal()

#Plan factoriel (1,3) avant/après
ggplot(ind_coords, aes(x = Dim.1, y = Dim.3, label = ind, color = group)) +
  geom_point(size = 3) +
  geom_text(vjust = -0.5, size = 3) +
  labs(title = "Plan factoriel (1,3) (ACP) avant et après intervention",
       x = "PC1", y = "PC3") +
  scale_color_manual(values = c("Avant" = "steelblue", "Après" = "tomato")) +
  theme_minimal()

#Plan factoriel (2,3) avant/après
ggplot(ind_coords, aes(x = Dim.2, y = Dim.3, label = ind, color = group)) +
  geom_point(size = 3) +
  geom_text(vjust = -0.5, size = 3) +
  labs(title = "Plan factoriel (2,3) (ACP) avant et après intervention",
       x = "PC2", y = "PC3") +
  scale_color_manual(values = c("Avant" = "steelblue", "Après" = "tomato")) +
  theme_minimal()
#ajoutons la comparaison entre hiver et été
ind_coords$SAISON <- data_transformed$SAISON
ggplot(ind_coords, aes(x = Dim.1, y = Dim.2, label = ind, color = SAISON)) +
  geom_point(size = 3) +
  geom_text(vjust = -0.5, size = 3) +
  labs(title = "Plan factoriel (1,2) (ACP) selon la saison",
       x = "PC1", y = "PC2") +
  scale_color_manual(values = c("hiver" = "steelblue", "ete" = "tomato")) +
  theme_minimal()
ggplot(ind_coords, aes(x = Dim.1, y = Dim.3, label = ind, color = SAISON)) +
  geom_point(size = 3) +
  geom_text(vjust = -0.5, size = 3) +
  labs(title = "Plan factoriel (1,3) (ACP) selon la saison",
       x = "PC1", y = "PC3") +
  scale_color_manual(values = c("hiver" = "steelblue", "ete" = "tomato")) +
  theme_minimal()
ggplot(ind_coords, aes(x = Dim.2, y = Dim.3, label = ind, color = SAISON)) +
  geom_point(size = 3) +
  geom_text(vjust = -0.5, size = 3) +
  labs(title = "Plan factoriel (2,3) (ACP) selon la saison",
       x = "PC2", y = "PC3") +
  scale_color_manual(values = c("hiver" = "steelblue", "ete" = "tomato")) +
  theme_minimal()

#combinaison des deux plots avant 
ggplot(ind_coords, aes(x = Dim.1, y = Dim.2, label = ind,
                       color = group, shape = SAISON)) +
  geom_point(size = 3) +
  geom_text(vjust = -0.5, size = 3) +
  labs(title = "Plan factoriel ACP selon Avant/Après et Saison",
       x = "PC1", y = "PC2") +
  scale_color_manual(values = c("Avant" = "steelblue", "Apres" = "tomato")) +
  scale_shape_manual(values = c("hiver" = 16, "ete" = 17)) +
  theme_minimal()

#ajoutons la comparaison selon le Type de localisation
typelieux <- c('rural','urbain','compostage','sourceindustriel')
ind_coords$TYPE <-data_transformed$TYPE

ggplot(ind_coords, aes(x = Dim.1, y = Dim.2, label = ind, color = TYPE)) +
  geom_point(size = 3) +
  geom_text(vjust = -0.5, size = 3) +
  labs(title = "Plan factoriel (1,2) (ACP) selon le type de lieux",
       x = "PC1", y = "PC2") 
  #scale_color_manual(values = typelieux) +
  #theme_minimal()
ggplot(ind_coords, aes(x = Dim.1, y = Dim.3, label = ind, color = TYPE)) +
  geom_point(size = 3) +
  geom_text(vjust = -0.5, size = 3) +
  labs(title = "Plan factoriel (1,3) (ACP) selon le type de lieux",
       x = "PC1", y = "PC3") 
  #scale_color_manual(values = typelieux) +
  #theme_minimal()

ggplot(ind_coords, aes(x = Dim.2, y = Dim.3, label = ind, color = TYPE)) +
  geom_point(size = 3) +
  geom_text(vjust = -0.5, size = 3) +
  labs(title = "Plan factoriel (2,3) (ACP) selon le type de lieux",
       x = "PC2", y = "PC3") 
  #scale_color_manual(values = typelieux) +
  #theme_minimal()

#ajoutons selon directement la localisation
points_a_nommer <- sprintf("P%02d", 1:33)
ind_coords$Localisation = data_transformed$Localisation
library(ggrepel)
# Plot principal
ggplot(ind_coords, aes(x = Dim.1, y = Dim.2, color = SAISON)) +
  geom_point() +
  
  # Ajout des étiquettes seulement pour les PXX
  geom_text(
    data = subset(ind_coords, Localisation %in% points_a_nommer),
    aes(label = Localisation),
    color = "black",   # texte noir
    vjust = -0.5,     # texte au-dessus du point

    size = 3,
    show.legend = FALSE  # ⬅️ empêche l’ajout dans la légende
  ) +
  
  labs(title = "Plan factoriel (ACP)", x = "Axe 1", y = "Axe 2") +
  theme_minimal()
#plot loca et type
ggplot(ind_coords, aes(x = Dim.1, y = Dim.2, color = TYPE)) +
  geom_point() +
  
  # Ajout des étiquettes seulement pour les PXX
  geom_text(
    data = subset(ind_coords, Localisation %in% points_a_nommer),
    aes(label = Localisation),
    color = "black",   # texte noir
    vjust = -0.5,     # texte au-dessus du point

    size = 3,
    show.legend = FALSE  # ⬅️ empêche l’ajout dans la légende
  ) +
  
  labs(title = "Plan factoriel (ACP)", x = "Axe 1", y = "Axe 2") +
  theme_minimal()

# Extraire les cos² des individus (quality of representation)
cos2_df <- as.data.frame(pca_result$ind$cos2[, 1:3])
cos2_df$ind <- rownames(cos2_df)  # ajouter les noms des individus
# Transformer en format long pour ggplot
library(tidyr)
cos2_long <- pivot_longer(cos2_df, cols = c("Dim.1", "Dim.2","Dim.3"), 
                          names_to = "Axis", values_to = "Cos2")

# Plot simple
ggplot(cos2_long, aes(x = ind, y = Cos2, fill = Axis)) +
  geom_col(position = "dodge") +
  labs(title = "Qualité de représentation (cos²) des individus",
       x = "Individus", y = "Cos²") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Calculer le cos² total sur Dim.1 et Dim.2
cos2_total <- pca_result$ind$cos2[, 1] + pca_result$ind$cos2[, 2] + pca_result$ind$cos2[,3]

# Créer un dataframe
cos2_df <- data.frame(
  ind = rownames(pca_result$ind$cos2),
  cos2_total = cos2_total
)

# Seuil pour individu mal représenté
seuil <- 0.5

# Sélectionner les individus mal représentés
mal_represented <- cos2_df[cos2_df$cos2_total < seuil, ]
nrow(mal_represented)/nrow(pca_result$ind$coord)*100

#Cercle de corrélation
fviz_pca_var(pca_result,
             axes = c(1, 2),      # Axes 1 et 2
             col.var = "contrib", # Couleur selon la contribution
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)        # Évite le chevauchement des noms

#nolint end