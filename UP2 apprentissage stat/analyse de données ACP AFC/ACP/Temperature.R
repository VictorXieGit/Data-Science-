if(!require(FactoMineR)) install.packages("FactoMineR")
library(FactoMineR)

setwd("C:\\Users\\tgalt\\Documents\\ACP\\codes R")
temperature <- read.csv2(file="C:\\Users\\tgalt\\Documents\\ACP\\codes R\\temperat.csv",
                        header = T,
                        sep=";",
                        dec=".",
                        row.names=1)

dim(temperature) # nb d'observations, puis nb de variables

sum(is.na(temperature)) # nombre de données manquante


diag(cov(temperature[,-17])) # variance des variables  quantitatives
#   normalisation


### PCA
res <- PCA(temperature[1:22,c(1:12,17)],  quali.sup=13,
           scale.unit = T #   normalisation
           )

summary(res) # première composantes capte 82% de l'info , 2 premières 98% => pas besoin d'analyser les autres


# plan factoriel
plot.PCA(res, choix="ind")


plot.PCA(res, choix="ind", habillage=13, cex=0.8)



### PCA avec var supplémentaire
res <- PCA(temperature, ind.sup=24:35,
           quanti.sup=13:16, # on fit le repère sans utilser les variable latitude, longitude, amplitude
           quali.sup=17, # on met la variable qualide côté
           scale.unit = T #   normalisation
)

summary(res) # première composantes capte 82% de l'info , 2 premières 98% => pas besoin d'analyser les autres


# plan factoriel
plot.PCA(res, choix="ind")


plot.PCA(res, choix="ind", habillage=17, cex=0.8)

dimdesc(res)
