#Mise en place du dossier pour les dats
setwd("C:\\Users\\victo\\OneDrive\\Bureau\\COURS général\\cours mine 2526\\sdd\\UP2 apprentissage stat\\analyse de données ACP AFC")

data <- read.csv('data_PDE20.csv', sep=';' )

#check si on a bien les bonnes data bien séparées
print(data)

#conversion en numérique de nos valeurs et on enlève la colonne d'indice et X
data_acp <- data[,2:(length(data)-1)]
str(data_acp)
print(data_acp)
data_acp[] <- lapply(data_acp, function(x) as.numeric(gsub(",", ".", as.character(x))))
print(data_acp)

#fonction permettant l'analyse acp basique avec en paramètre un dataser supposé propre et k le nombre de taux d'inertie voulu
#la fonction effectue quelques vérification préliminaires avant de se lancer dans le centrage.
acp_analyse <- function(data_acp, k = 5) {
    # Vérification 1 : valeurs manquantes
    if (any(is.na(data_acp))) {
        stop("Erreur : Le dataset contient des valeurs manquantes (NA).")
    }
    # Vérification 2 : toutes les colonnes sont numériques
    if (!all(sapply(data_acp, is.numeric))) {
        stop("Erreur : Toutes les colonnes du dataset doivent être numériques.")
    }
    # Vérification 3 : colonnes constantes
    if (any(apply(data_acp, 2, function(x) var(x) == 0))) {
        stop("Erreur : Le dataset contient au moins une colonne constante (variance nulle).")
    }
    # Centrage
    col_moyenne <- colMeans(data_acp)
    data_centre <- round(data_acp - col_moyenne, 2)
    
    # Matrice de covariance
    mat_cov <- cov(data_centre)
    
    # Valeurs et vecteurs propres
    eig <- eigen(mat_cov)
    
    # Normalisation des vecteurs propres
    vecteurs_directeurs <- apply(eig$vectors, 2, function(x) x / sqrt(sum(x^2)))
    
    # Affichage des inerties
    for (i in 1:length(eig$values)){
        print(sprintf("La composante principale numéro %d a comme inertie %.2f", i, eig$values[i]))
    }
    total_inertie <- sum(eig$values)
    taux_inertie <- eig$values / total_inertie
    for (i in 1:k){
        print(sprintf("Le taux d'inértie de la composante %d est de %.2f", i, taux_inertie[i]))
    }
    # Tracé des taux d'inertie (scree plot)
    plot(taux_inertie, type = "b", pch = 19, xlab = "Composante principale", 
         ylab = "Taux d'inertie", main = "Scree plot des taux d'inertie")
    abline(v = 1:k, col = "red", lty = 2)

    # Nouvelles coordonnées dans le repère ACP
    coord_acp <- as.matrix(data_centre) %*% vecteurs_directeurs
    print("Coordonnées des individus dans le nouveau repère ACP :")
    print(round(coord_acp, 2))
    
    # Retourner les résultats utiles
    return(list(
        valeurs_propres = eig$values,
        vecteurs_directeurs = vecteurs_directeurs,
        coord_acp = coord_acp
    ))
}

# Exemple d'appel :
resultats <- acp_analyse(data_acp, k = 5)
