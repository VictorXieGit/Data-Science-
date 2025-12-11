# Majeure Science des Données 2025-2026
# Cours de Régression - Exemple 1    

# Modèle de Régression sur données réelles
# Données de pluie

data1 <- read.table("data_rendement_pluie.txt", header=TRUE)
pluie <- data1$pluie               
rend <- data1$rendement

# Modèle 1 : Régression Linéaire Simple (RLS)  

mod1 <- lm(rend ~ pluie)     
mod1.s <- summary(mod1)
print(mod1.s)

plot(pluie, rend, xlab="hauteur précipitations cumulées (m)",
     ylab="rendement (sans unité)", lwd=2)
grid()
abline(mod1, lwd=2, lty=1, col="red")
title(main=list("Régression linéaire : rendement de blé contre quantité de pluie", cex=1))

# Analyse des résidus

res1 <- residuals(mod1)
n <- length(res1)
plot(1:n,res1, ylim=c(-0.2,0.15), xlab="numéro d'observation i = 1, ..., n ", ylab="résidus estimés")
grid()
abline(h=0, lty=2)
title(main=list("Tracé séquentiel des résidus estimés", cex=0.8))

# Tracé en fonction du prédicteur

plot(pluie, res1, ylim=c(-0.2,0.15), xlab="Hauteur de pluie (m)", ylab="résidus estimés")
abline(h=0, lty=2)
grid()
title(main=list("Tracé des résidus estimés en fonction du prédicteur", cex=0.8))

# Tracé des résidus en fonction de la réponse ajustée par le modèle (graphique équivalent dans la cas p=1)

plot(mod1$fitted.values, res1, ylim=c(-0.2,0.15), xlab="réponse ajustée", ylab="résidus estimés")
abline(h=0, lty=2)
grid()
title(main=list("Tracé des résidus estimés en fonction de la réponse ajustée", cex=0.8))

# L'analyse des résidus montre une "petite" dépendance avec le prédicteur d'où le modèle suivant.

# Modèle 2 : modèle avec terme quadratique

pluie2 <- pluie*pluie               # on définit le second prédicteur
mod2 <- lm(rend ~ pluie + pluie2)   # modèle 2 : RLM avec p = 2 prédicteurs    
mod2.s <- summary(mod2)
print(mod2.s)
print(mod1.s) # pour comparer

plot(pluie, rend, xlab="hauteur précipitations cumulées (m)",
     ylab="rendement (sans unité)", lwd=2)
grid()
abline(mod1, lwd=2, lty=1, col="red")
title(main=list("Régression linéaire : rendement de blé contre quantité de pluie", cex=1))
x.new <- seq(0, max(pluie)*1.1, 0.01)
y.fit <- mod2$coef[1]+mod2$coef[2]*x.new+mod2$coef[3]*x.new*x.new
lines(x.new, y.fit, col="blue", lwd=2)

# Analyse de la variance

anova(mod1, mod2)          # test de Fisher pour modèles emboîtés

# Analyse des résidus pour le modèle n°2 : y ~ x + x^2 

res2 <- residuals(mod2)
plot(1:n, res2, ylim=c(-0.2,0.15), xlab="numéro d'observation i = 1, ..., n ", ylab="résidus estimés")
grid()
abline(h=0, lty=2)
title(main=list("Tracé séquentiel des résidus estimés", cex=0.8))

plot(mod2$fitted.values, res2, ylim=c(-0.2,0.15), xlab="réponse ajustée", ylab="résidus estimés")
grid()
abline(h=0, lty=2)
title(main=list("Tracé des résidus estimés en fonction de la réponse ajustée", cex=0.8))

# Normalité des résidus

qqnorm(res2)
qqline(res2, probs=c(0.1,0.9), col="red")
grid()

# On valide le modèle n°2 avec terme quadratique

# Prédiction avec un modèle de Régression Linéaire
# Intervalle de confiance pour la réponse espérée

pluie.new <- seq(min(pluie), max(pluie), by=0.01)
newdata <- data.frame(pluie=pluie.new, pluie2=pluie.new^2) 
int.conf <- predict(mod2, newdata, interval="confidence") 
plot(pluie, rend, xlab="hauteur précipitations cumulées (m)",
     ylab="rendement (sans unité)", lwd=2)
grid()

x.new <- seq(0, max(pluie)*1.1, 0.01)
y.fit <- mod2$coef[1]+mod2$coef[2]*x.new+mod2$coef[3]*x.new*x.new
lines(x.new, y.fit, col="blue", lwd=2)
lines(newdata[,1], int.conf[,2], col="red", lty=2, lwd=2)
lines(newdata[,1], int.conf[,3], col="red", lty=2, lwd=2)

# Intervalle de prédiction pour la réponse

int.pred <- predict(mod2, newdata, interval="prediction") 
lines(newdata[,1], int.pred[,2], col="black", lty=2)
lines(newdata[,1], int.pred[,3], col="black", lty=2)
title(main=list("Intervalle de confiance pour la réponse espérée et intervalle de prédiction", cex=0.8))

# Résidus studentisés

plot(mod1$fitted.values, rstandard(mod2), xlab="réponse estimée", ylab="Résidus", ylim=c(-3,3), lwd=2)
grid()
abline(h=c(0,2,-2), lty=2)

points(mod1$fitted.values, rstudent(mod2), col="red", pch=3)
abline(h=qt(p=0.025,df=mod1$df.residual-1), col="red", lwd=2, lty=2)
abline(h=qt(p=0.975,df=mod1$df.residual-1), col="red", lwd=2, lty=2)
title(main=list("Résidus standardisés et studentisés",cex=0.8))
