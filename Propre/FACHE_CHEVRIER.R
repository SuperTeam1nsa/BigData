#@authors: Rémi Fache, Charles-Antoine Chevrier
#@date: 20/09/2019

#Génération de nombres aléatoires
#pour générer les variables X1 ou X2
#ex: rCertificatBigData()
#@in:
#n:nombres de tirages voulus (défaut:10000),
#min:borne inférieure de l'intervalle de génération de la loi uniforme(défaut:0)
#max:borne supérieure de l'intervalle de génération de la loi uniforme(défaut:1)
#@out:
#une variable suivant une loi uniforme sur l'intervalle [min,max]
rCertificatBigData<-function(n=10000,min=0, max=1){
  return(runif(n,min,max));# n tirages loi uniforme
}

#Quartile
#ex: qCertificatBigData(rCertificatBigData(),rCertificatBigData())
#maliste=qCertificatBigData(rCertificatBigData(),rCertificatBigData())
#puis : maliste['qY'] (renvoie les quartiles de Y)
#@in:
#X1: variable aléatoire 
#X2: variable aléatoire
#@out:
#liste des quartiles de chacunes des variables caclculées (Y,Z,W).
qCertificatBigData<-function(X1,X2){
  #si on souhaite un quatile précis (ici le 3ème )quantile(X,.75);
  #sinon tous les quartiles
  qY=quantile(X1+X2);
  qZ=quantile(X1*X2);
  qW=quantile(X1/X2);
  return(list('qY'=qY,'qZ'=qZ,'qW'=qW));
}

#Variance 
#Rappel: Var(X+Y)=Var(X)+Var(Y) +2.cov(X,Y) si VA indépendantes 2.cov(X,Y) =0 (X1 et X2 indépendantes ici)
#ex: vCertificatBigData(rCertificatBigData(),rCertificatBigData())
#maliste=vCertificatBigData(rCertificatBigData(),rCertificatBigData())
#puis : maliste['vY'] (renvoie la variance de Y)
#@in:
#X1: variable aléatoire 
#X2: variable aléatoire
#@out:
#liste des variances de chacunes des variables caclculées (Y,Z,W).
vCertificatBigData = function(X1,X2) {
  return(list('vY'=var(X1+X2), 'vZ'=var(X1*X2), 'vW'=var(X1/X2)))
}

#Moyenne
#ex: eCertificatBigData(rCertificatBigData(),rCertificatBigData())
#maliste=eCertificatBigData(rCertificatBigData(),rCertificatBigData())
#puis : maliste['mY'] (renvoie la moyenne de Y)
#@in:
#X1: variable aléatoire 
#X2: variable aléatoire
#@out:
#liste des moyennes de chacunes des variables caclculées (Y,Z,W).
eCertificatBigData = function(X1,X2) {
  return(list('mY'=mean(X1+X2), 'mZ'=mean(X1*X2), 'mW'=mean(X1/X2)))
}

#Densité de probabilité (PDF)
#rq:non-additivité loi uniforme https://ilovestatistics.be/probabilite/loi-uniforme.html
#https://www.johndcook.com/blog/2009/02/12/sums-of-uniform-random-values/
#ex:dCertificatBigData(rCertificatBigData(),rCertificatBigData(),0.5)
#@in:
#X1: variable aléatoire 
#X2: variable aléatoire
#x : valeur où évaluer la fonction
#plotGraph: booléen permettant l'affichage ou non des graphes.
#@out:
#liste des densités de probabilité de chacunes des variables calculées (Y,Z,W) en x.
dCertificatBigData = function(X1,X2,x, plotGraph=T) {

  Y = density(X1+X2)
  Z = density(X1*X2)
  W = density(X1/X2)
  
  
  if(plotGraph) {
    par(mfrow=c(2,2))
    plot(Y)
    plot(Z)
    plot(W)
  }
  dy=Y$y[x*512/max(Y$x)]
  dz=Z$y[x*512/max(Z$x)]
  dw=W$y[x*512/max(W$x)]
  
  return(list('dY'=dy, 'dZ'=dz, 'dW'=dw));
}

#Fonction de répartition (CDF)
#ex:pCertificatBigData(rCertificatBigData(),rCertificatBigData())
#@in:
#X1: variable aléatoire 
#X2: variable aléatoire
#plotGraph: booléen permettant l'affichage ou non des graphes.
#@out:
#liste des fonctions de répartions de chacunes des variables caclculées (Y,Z,W).
pCertificatBigData = function(X1,X2,plotGraph=T) {
  Y = ecdf(X1+X2)
  Z = ecdf(X1*X2)
  W = ecdf(X1/X2)
  
  if(plotGraph) {
    par(mfrow=c(2,2))
    plot(Y)
    plot(Z)
    plot(W)
  }
  
  return(list('pY'=Y,'pZ'=Z,'pW'=W))
}
