# Formation "Big Data" Toulouse Tech 
# Premier regroupement
# Pascal Floquet - Florent Bourgeois 
# Universite de Toulouse - INP-ENSIACET
# Travail sur les donn?es "ozone" 
# Data frame a partir d'un fichier csv
# path="http://www.math.univ-toulouse.fr/~besse/Wikistat/data/"

#@authors: Rémi Fache, Charles-Antoine Chevrier
#@date: 27/09/2019



#########
# Exploration statistique élémentaire

# Data frame à partir d'un fichier csv
path="http://www.math.univ-toulouse.fr/~besse/Wikistat/data/"
ozone=read.table(paste(path,"ozone.csv",sep=""),sep=";",dec=",",header=T)
# vérification
summary(ozone);
# Supprimer la variable inutile "obs" 
ozone=ozone[,-1]

#écarts-types et moyennes
sapply(ozone[,-c(12,13)], sd)    
sapply(ozone[,-c(12,13)], mean) 

# Diagramme en boîtes
# dimensions des graphiques
options(repr.plot.width=4, repr.plot.height=4)
boxplot(ozone[,2:4]) # boîtes par groupe
#com: la température augmente au cours de la journée
boxplot(ozone[,5:7])
#com: les nuages diminuent au cours de la journée
#présence de valeurs atypiques (ronds)
boxplot(ozone[,8:10])
#com: le vent est constant sur la journée
#valeur atypique détectée pour vx12
boxplot(ozone[,c(1,11)])
#com: le graphisme ne montrant pas les jours (effectuant une moyenne), les 2 graphes sont logiquement identiques (le jour même est la veille du lendemain ^^)
#COMMENTAIRE : Les variables de concentration d'ozone (maxO3 et maxO3v) ont des valeurs mal réparties, il y a plusieurs valeurs aberrantes ou mal conditionnées en dehors des moustaches du diagramme-boîte utilisé.



# Les deux variables de concentration d'ozone demandent plus d'attention.
# Utilisation du logarithme pour les variables de concentration
hist(ozone$maxO3)
hist(ozone$maxO3v)
hist(log(ozone$maxO3))
hist(log(ozone$maxO3v))
boxplot(log(ozone[,c(1,11)]))
# Les distributions semblent alors plus symétriques et ne présentent plus de valeurs atypiques.

# Variables qualitatives

barplot(table(ozone$pluie))
barplot(table(ozone$vent))
pie(table(ozone$vent))


# Description bidimensionnelle : matrice pour trouver des similitudes
options(repr.plot.width=8, repr.plot.height=8)
pairs(ozone[,1:11]);

options(repr.plot.width=4, repr.plot.height=4)
plot(maxO3~maxO3v,data=ozone)

plot(log(maxO3)~log(maxO3v),data=ozone)

# COMMENTAIRE : Les variables T semblent très corrélées entre elles, les variables T et maxO3 semblent également légèrement corrélées, les variables Ne ne présentent pas de corrélation entre elles, les variables Vx sembles fortement corrélées entre elles.
# Com: Ne9, Ne12, et Ne15 sont corrélées à maxO3v, plus il y a de nuages moins il y a d'ozone. Corrélation faible du vent. MaxO3v et Max03 très corrélées : plus il y a d'ozone la veille, plus il y en a le lendemain (assez logique ^^) (T9,T12,T15 très corréllées entre elles (presque linéaire), Vx9,Vx12, Vx15 aussi)

# Variables qualitatives
table(ozone$vent,ozone$pluie)
mosaicplot(table(ozone$vent,ozone$pluie))


#COMMENTAIRE: Les vents de Sud et d'Est semblent être liées à un temps sec à 80%, tandis que le vent d'ouest ne semble pas lié à un temps particulier (environ 50-50 pluie/sec). Le vent de nord semble légerement plus lié à un temps sec à 70% environ. L'océan Atlantique est à l'Ouest, la Manche au Nord tandis qu'au sud et à l'est c'est l'intérieur des Terres, ce qui pourrait expliquer cette observation... 
# Com: Si on souhaite de la pluie il faut espérer un vent d'ouest. Si on souhaite du temps sec, un vent d'est ou de sud est optimal, à défaut un vent de Nord offre 2/3 chances qu'il fasse sec.

# Variables qualitatives et quantitatives
boxplot(maxO3~pluie,data=ozone)
boxplot(maxO3~vent,data=ozone)


#COMMENTAIRE : Quelques valeurs sortent des diagrammes, il pourrait être judicieux de passer les données au log pour avoir une meilleur représentation des données. En cas de pluie, plus de 50% des valeurs de maxO3 sont entre 60 et 80, tandis qu'en cas de temps sec plus de 75% des valeurs de maxO3 sont au-dessus de 80. Il semble que de fortes valeurs de maxO3 soient corrélées avec un vent d'Est, la quasi totalité des valeurs dans ce cas sont au-dessus de 90. De manière générale une forte valeur de maxO3 semble être corrélée avec un vent d'Est ou du Sud (75% des valeurs au-dessus de 80). Néanmoins il semble difficile de différencier un vent d'Ouest ou de Nord avec les valeurs du maxO3 qui sont très similaires. Notons que les valeurs de maxO3 pour le vent de Sud sont très étendues et hormis éventuellement pour les valeurs les plus hautes, il semble difficile de différencier vent d'Ouest, du Nord et du Sud.









#########
# Tests de comparaison



#droite de Henry ( aperçu graphique de la normalité de la distribution avant de calculer le test.)
# qq-plots
qqnorm(log(ozone$maxO3))
qqline(log(ozone$maxO3),col=2)


# Test de shapiro-Wilks
shapiro.test(log(ozone$maxO3))


# Test de Kolmogorov-Smirnov (peu adapté ici, cf commentaires)
# ks.test(ozone$maxO3,pnorm)
# ks.test(log(ozone$maxO3),pnorm)
# COMMENTAIRES : Les p-values sont significatives (i.e. très faibles), ce qui signifie que les variables testées ne suivent pas une loi normale. On notera que l'utilisation du log améliore (augmente) la p-value, ce qui augmente le caractère gaussien de la distribution.
# Com: Pas vraiment non: The K-S test is for a continuous distribution and so MYDATA should not contain any ties (repeated values).
# The theory underlying the K-S test does not let you estimate the parameters of the distribution from the data as you have done. The help for ks.test explains this.

ozone=data.frame(ozone,LmaxO3=log(ozone$maxO3),LmaxO3v=log(ozone$maxO3v))
summary(ozone)
t.test(ozone$LmaxO3, conf.level=.95)

#COMMENTAIRE : Il est clair d'après la p-value que la moyenne n'est pas 0, la moyenne obtenue est 4.459 avec un intervalle de confiance à 95% de [4.404 - 4.514]. A noter que ces valeurs sont des log des valeurs de maxO3 initiales.

# Comparaison de deux variances : Fisher

# Normalité des distributions (facultatif)
shapiro.test(ozone[ozone$pluie=="Pluie","LmaxO3"])
shapiro.test(ozone[ozone$pluie=="Sec","LmaxO3"])
# égalité des variances (test de Fisher)
var.test(LmaxO3~pluie,data=ozone)

#COMMENTAIRES : Le premier test de Shapiro donne une p-value élevé pour les jours de pluie, ainsi la distribution est bien normale, néanmoins le second test sur les jours sec donne une p-value trop faible pour considérer que la distribution est normale. On s'affranchit néanmoins de l'hypothèse de normalité car les échantillons sont considérés comme étant de taille suffisamment grande. Le test de Fisher indique que la p-value n'est pas significative, ainsi les deux variances sont considérées comme proches... Néanmoins le ratio des variances est de 0.69, i.e. une des deux variances est 30% plus grande que l'autre...
#Com: Pour alpha = 0.05 (95%) la pluie suit une loi normale (p-value~=0.27 >alpha) Sec ne suit pas une loi normale (p-value~=0.003 <alpha) Test variance: H0=les variances sont égales (p-value~=0.21 >alpha), H0 accepté, les variances sont considéres comme égales The null-hypothesis of this test is that the population is normally distributed. Thus, on the one hand, if the p value is less than the chosen alpha level, then the null hypothesis is rejected and there is evidence that the data tested are not normally distributed. On the other hand, if the p value is greater than the chosen alpha level, then the null hypothesis that the data came from a normally distributed population can not be rejected (e.g., for an alpha level of .05, a data set with a p value of less than .05 rejects the null hypothesis that the data are from a normally distributed population)


# Comparaison de deux moyennes
# Si les variances sont différentes, il s'agit d'un test de Welch.
t.test(LmaxO3~pluie,var.equal=F, data=ozone)
#Dans le cas où elles sont considérées égales, c'est un test de Student.
t.test(LmaxO3~pluie,var.equal=T, data=ozone)

# COMMENTAIRES : Dans les deux cas, le test de comparaison de moyennes indique que les moyennes sont différentes car les p-value sont significatives (<<0.01).

# Echantillons appariés
t.test(ozone$maxO3, ozone$maxO3v,paired=TRUE)
#COMMENTAIRE : La p-value est grande ici, on considère donc que l'hypothèse nulle est vérifiée et on rejet l'hypothèse alternative. On considère donc que la moyenne en concentration n'a pas variée (ou très peu) entre ces deux jours.

#test non paramétrique si non normalement distribué:
#Comparaison de deux médianes : Wilcoxon
# Echantillons indépendants
tapply(ozone$LmaxO3, ozone$pluie, median)
wilcox.test(maxO3 ~ pluie, data=ozone)
# Echantillons appariés
median(ozone$LmaxO3 - ozone$LmaxO3v)
wilcox.test(ozone$LmaxO3, ozone$LmaxO3v,paired=TRUE)
#COMMENTAIRES : Pour les échantillons indépendants on obtient la même conclusion : les deux moyennes sont bien différentes. On peut conclure qu'il y a un lien entre la concentration en O3 et le temps (pluie/sec). On observe qu'un temps sec sera lié à une concentration d'O3 importante tandis qu'un temps pluvieux sera lié à une concentration d'O3 plus faible. Pour les échantillons appariés on obtient aussi la même conclusion : il n'y a pas eu de variation de concentration d'O3 conséquente entre les deux jours de mesure.
# Même resultats,pour paramètriques ou non : le 1er, H0 est rejeté, le deuxième H0 est accepté.

######
# Tests de liaison

# Test d'indépendance de 2 valeurs (chi 2):
chisq.test(table(ozone$pluie,ozone$vent))
#ozone=data.frame(ozone,LmaxO3=log(ozone$maxO3),LmaxO3v=log(ozone$maxO3v))

# COMMENTAIRE :  les effectifs théoriques de certaines cellules sont trop faibles pour utiliser le test, il faut alors regrouper des modalités.

# Cas Gaussien ANOVA - Fisher
# test de Bartlett
bartlett.test(LmaxO3 ~ vent, data=ozone)
# ANOVA à un facteur
# estimation des paramètres
res.anova=aov(LmaxO3 ~ vent, data=ozone)
# normalité des résidus au modèle d'ANOVA
qqnorm(res.anova$residuals)
qqline(res.anova$residuals)
shapiro.test(res.anova$residuals)
# Interprétation du test
summary(res.anova)

#COMMENTAIRES : Le test de Bartlett montre l'égalité des variances au sein de chaque groupe grâce à une p-value élevée. La normalité des résidus du modèle n'est pas vérifiée par le test de Shapiro qui indique une p-value significative : l'hypothèse nulle de normalité est rejetée, que l'on peut voir graphiquement avec la Q-Q line, les points aux extrêmes ne suivent pas la droite moyenne tracée. Un test non-paramétrique est donc à envisager.
# Test de Bartlett ok (pvalue~=0.9 >0.05) et Shapiro-Wilk pas validé pour alpha =0.05 (p-value ~= 0.006)

# Cas non-paramétrique : Kruskal-Wallis
kruskal.test(maxO3 ~ vent, data=ozone)
# COMMENTAIRE : La p-value obtenue est significative <0.01, ainsi l'hypothèse nulle est rejetée, c'est à dire que la médiane des différents groupes analysés sont différents, il y a donc un lien entre le maxO3 et le vent, ces deux variables ne sont pas indépendantes.
# Com : les médianes ne sont pas égales (H0 refuté) the null hypothesis is that the medians of all groups are equal, and the alternative hypothesis is that at least one population median of one group is different from the population median of at least one other group. 

# Deux variables quantitatives

# MODELE:
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

# repérage des points influents
cook=cooks.distance(res1.reg)
plot(cook~ychap,ylab="Distance de Cook")
abline(h=c(0,1),lty=c(1,2))

summary(res1.reg)



# COMMENTAIRE : Le test de Shapiro-Wilk a une p-value non significative, les données analysées vérifient l'hypothèse de normalité. Quelques résidus "studentisés" dépassent 2 en valeur absolue, néanmoins ils semblent répartis aléatoirement, l'hypothèse d'homoscédasticité semble vérifiée. Il n'y a pas de point à grand effet de levier car la distance de Cook la plus grande observée est environ 0.09<<1. Les p-values obtenues pour les coefficients sont significatifs (trois étoiles) ainsi les coefficients de la régression linéaire ne sont pas nuls. Néanmoins le coefficient de corrélation obtenu est mauvais (0.4), ainsi il n'y a pas de relation linéaire significative entre le seuil d'ozone de la veille et du jour actuel. 

######
# ACP et régression multiple

#aide au choix des paramètres influents :
res.pca=prcomp(ozone[,c(2:10,14,15)],scale=T)
# décroissance des valeurs propres
plot(res.pca)
# parts de variance expliquée
summary(res.pca)
# biplot du premier plan principal
biplot(res.pca)

# COMMENTAIRE : L'axe 1 (abscisse) représente la composante principale, le vecteur propre le plus représentatif du set de données analysés et l'axe 2 (ordonnée) représente la seconde composante la plus importante après la PC1. Ce graphe permet de projeter l'ensemble de données sur les deux vecteurs propres principaux du set de données. On remarque d'ailleurs des groupements en fonction de la nature des données : les Vx sont similaires ainsi que les Ne par exemple, car ils possèdent des valeurs de projection sur PC1 et PC2 proches. Ceci permettrait de faire de l'identification de type de données avec une nouvelle colonne de donnée non étiquetée. Cette méthode permet de réduire la dimensionnalité des données à analyser.


#MODELE DE REGRESSION MULTIPLE
# estimation
res2.reg=lm(LmaxO3 ~ LmaxO3v+T9+T12+T15+Ne9+Ne12+Ne15+Vx9+Vx12+Vx15, data = ozone)
# diagnostics
plot(res2.reg)
# résultats
summary(res2.reg)

# La ligne Q-Q montre que les résidus semblent satisfaire l'hypothèse de normalité, néanmoins certains points peut-être sont aberrants ou possède un grand effet de levier : les points 34, 58 et 52. Les variables LmaxO3v et Ne9 présentent des p-values significatives et ainsi sont explicatives du Lmax03.
# Com: Les tests de student auraient permis d'éliminer les variables non significatives (par exemple Vx15 estimate =0+/-3.10^-4). Les variables présentant de fortes colinéarités avec O3 semblent significatives, mais elles peuvent parfois l'être seulement par leur colinéarité avec une vraie variable d'influence sur 03 (ex: même si T15 présente de la colinéarité avec 03 c'est uniquement parce qu'il présente une colinéarité avec T12, qui lui semble 4 fois plus significatif de T15) .


# Sous modèle 3
res3.reg=lm(LmaxO3~LmaxO3v+T12+Ne9+Vx9,data=ozone)
# diagnostics
plot(res3.reg)
# résultats
summary(res3.reg)

# COMMENTAIRES : On a gardé ici que les variables explicatives qui avaient la plus faible p-value, ce qui permet d'enlever l'influence des autres variables qui apportaient du "bruit" dans la régression. On observe en effet des p-value très significatives, plus significatives que lorsque toutes les variables étaient prises en compte. Il y a donc des couplages entre les différentes variables qui ont été supprimés en simpifiant les variables explicatives choisies pour la régression.
# Com: l'erreur standard a diminué, les paramètres concervés présentent un bon niveau de colinéarité. Le plus important étant Lmax03v


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
press(res3.reg)


# Le coefficient R2 permet de comparer les qualités d’ajustement mais la meilleure prévision n’est pas nécessairement fournie par un modèle de R2 maximum. Le PRESS encore appelé leave one out cross validation (loo CV) est plus pertinent pour atteindre cet objectif. Ici le meilleur modèle est le dernier, le PRESS obtenu est le plus faible. On notera ici que le R2 croit avec les modèle (du numéro 1 à 3) tandis que le PRESS diminue.
# Com: PRESS calculant la somme des carrés des erreurs, plus le résultat est faible, meilleur est le modèle. On remarque donc que le modèle 3, qui ajuste le mieux les données est le plus précis !

######
# Annexe

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

# Même si la variable pluie est significative dans le modèle, l"amélioration" de la qualité de prévision n'est pas franchement significative.
