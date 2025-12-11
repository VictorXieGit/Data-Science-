


setwd("C:\\Users\\victo\\OneDrive\\Bureau\\COURS général\\cours mine 2526\\sdd\\UP2 apprentissage stat\\analyse de données ACP AFC")
conso <- read.csv(file="conso.csv",
                        header = TRUE,
                        sep=";",
                        dec=".",
                        row.names=1)
dim(conso) # nb d'observations, puis nb de variables

colnames(conso)
rownames(conso)

#################  nombre de données manquante
sum(is.na(conso))

#################  tableaux croisés
#install necessary libraries
install.packages('ggplot2',lib = "C:/Users/victo/R/library", repos = "https://cran.r-project.org/")
install.packages('GGally',lib = "C:/Users/victo/R/library", repos = "https://cran.r-project.org/")

#load libraries
library(ggplot2)
library(GGally)

#create pairs plot
ggpairs(conso[,1:10])

ggpairs(conso[,10:20])
ggpairs(conso[-18,10:20])
# -> on met D10 à part





### PCA
if(!require(FactoMineR)) install.packages("FactoMineR")
library("FactoMineR") 

res <- PCA(conso[  ,1:29], ind.sup=c(18),  
           scale.unit = T #   normalisation
)
 
summary(res) # première composantes capte 89% de l'info , 2 premières 94% => pas besoin d'analyser les autres

# cascade valeur propre
barplot(res$eig[,1])


# plan factoriel
plot.PCA(res,axes = c(1,2))
# dim1 capte le revenu
plot.PCA(res,axes = c(2,3))
plot.PCA(res,axes = c(1,3))


# cercles des correlations 
PCA(conso[9:18,1:29],
    ind.sup=18-8,  
    scale.unit = T,
    axes =c(1,2)  
)

# dim1 capte l'ampleur de la consomation = le revenu
# dim2 capte le tabac
#   tabac décorrélé des autres var

PCA(conso[9:18,1:29],
    ind.sup=18-8,  
    scale.unit = T,
    axes =c(1,3)  
)
# dépense de logement à part   

PCA(conso[9:18,1:29],
    ind.sup=18-8,  
    scale.unit = T,
    axes =c(2,3)  
)
# tabac décorrélé logement

dimdesc(res, proba = 0.05)

