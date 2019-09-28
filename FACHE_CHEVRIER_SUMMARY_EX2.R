# Data frame à partir d'un fichier csv
path="http://www.math.univ-toulouse.fr/~besse/Wikistat/data/"
#path=""
ozone=read.table(paste(path,"ozone.csv",sep=""),sep=";",dec=",",header=T)
# vérification
summary(ozone);
# Supprimer la variable inutile "obs" 
ozone=ozone[,-1]
#sapply(ozone[,-c(12,13)], sd)    # écarts-types
#sapply(ozone[,-c(12,13)], mean)  # moyennes
#matrice pour trouver des similitudes
options(repr.plot.width=8, repr.plot.height=8)
pairs(ozone[,1:11]);
#droite de Henry ( aperçu graphique de la normalité de la distribution avant de calculer le test.)
# qq-plots
qqnorm(ozone$maxO3)
qqline(ozone$maxO3,col=2)
# Test de shapiro-Wilks
shapiro.test(log(ozone$maxO3))
#test paramétrique si non noramlement distribué:
#Comparaison de deux médianes : Wilcoxon
#tapply(ozone$LmaxO3, ozone$pluie, median)
wilcox.test(maxO3 ~ pluie, data=ozone)
#test d'indépendance de 2 valeurs (chi 2):
chisq.test(table(ozone$pluie,ozone$vent))
ozone=data.frame(ozone,LmaxO3=log(ozone$maxO3),LmaxO3v=log(ozone$maxO3v))


#MODELE:
# retracer le nuage de point
plot(LmaxO3 ~ LmaxO3v,data=ozone)
# estimation du modèle
res1.reg=lm(LmaxO3 ~ LmaxO3v, data = ozone)
# nuage de point
# normalité des résidus
qqnorm(res1.reg$residuals)
qqline(res1.reg$residuals)
shapiro.test(res1.reg$residuals)

# Repérage d'une structure particulière du nuage
# ou de la présence de "grands" résidus
res.student=rstudent(res1.reg)
ychap=res1.reg$fitted.values
plot(res.student~ychap,ylab="Résidus")
# ajouter des lignes
abline(h=c(-2,0,2),lty=c(2,1,2))

summary(res1.reg)

#aide au choix des paramètres influents :
res.pca=prcomp(ozone[,c(2:10,14,15)],scale=T)
# décroissance des valeurs propres
plot(res.pca)
# parts de variance expliquée
summary(res.pca)

#MODELE DE REGRESSION MULTIPLE
# estimation
res2.reg=lm(LmaxO3 ~ LmaxO3v+T9+T12+T15+Ne9+Ne12+Ne15+Vx9+Vx12+Vx15, data = ozone)
# diagnostics
plot(res2.reg)
# résultats
summary(res2.reg)

#COMPARAISON DES MODELES
# définition de la fonction PRESS
press=function(model) {
  h=influence(model)$hat
  e=influence(model)$wt.res
  n=length(e)
  sum((e/(1-h))^2)/n
}
# application aux différents modèles
press(res1.reg)
press(res2.reg)


#MODELE ACOVA (AUTO)
res.acova=glm(LmaxO3 ~ T9+T12+T15+Ne9+Ne12+Ne15+Vx9+Vx12+Vx15+vent+pluie+LmaxO3v,data=ozone)
# Recherche du meilleur modèle au sens 
# du critère d'Akaïke par méthode descendante
res.acova.step=step(res.acova,direction="backward")
# paramètres retenus
anova(res.acova.step,test="F")
# Extraction des valeurs ajustées et des résidus
fit.acova=res.acova.step$fitted.values
resid.acova=res.acova.step$residuals
# Graphe des résidus
plot(fit.acova,resid.acova)
summary(res.acova.step)

#VALIDATION MODELE CROISEE
#à tester dans l'environnement anaconda
library(boot) # chargement de la bibliothèque
# validation croisée  10-plis
# meilleur modèle linéaire 
res3.reg=glm(LmaxO3~LmaxO3v+T12+Ne9+Vx9,data=ozone)
res4.reg=glm(LmaxO3~LmaxO3v+T12+Ne9+Vx9+pluie,
             data=ozone)
set.seed(111)
cv.glm(ozone, res3.reg, K=10)$delta[1] 
set.seed(111)
# modèle d'analyse de covariance
cv.glm(ozone, res.acova.step, K=10)$delta[1] 

