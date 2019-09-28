  #http://ljk.imag.fr/membres/Bernard.Ycart/mel/dr/node7.html
  #http://wikistat.fr/
  
  #génération de nombres aléatoires
  rCertificatBigData<- function(affichage=0,min=0, max=1){
  X1<-runif(10000,min, max);# 10000 tirages loi uniforme
  X2<-runif(10000,min, max);# 10000 tirages loi uniforme
  Y=X1+X2;
  Z=X1*X2;
  W=X1/X2;
  #affichage
  if(affichage)
  {#boxplot(X);
  #propriétés:
  print(summary(Y));
    print(summary(Z));
    print(summary(W));
    boxplot(Y);
    boxplot(Z);
    boxplot(W);
  }
  
  return(X1);
  }
  
  #fonction densité, pour loi uniforme définie entre [a,b]
  #paramètres optionnels pour valeur théorique
  #check OK maths point of view
  #ex: dCertificatBigData(rCertificatBigData(),rCertificatBigData(),0.5)
  dCertificatBigData<- function(X,X2,c,affichage=0, a=min(X),b=max(X),a2=min(X2),b2=max(X2)){
    #cat("variable: ", a,b, c)
    if(affichage){
    P=density(X*X2);
    plot(P)
  }
    if(c<b && c>a){
      return((1/(b-a))*1/(b2-a2));
    }
    else{
      return(0);
    }
  }
  
  #fonction de répartition
  pCertificatBigData<- function(X1,X2,a,affichage=0){
    P=ecdf(X1*X2);
    if(affichage)
    plot(P);
    return (P(a));
    #à faire
    #rq: mod les fonctions pour calculer les bonnes var alea 
    #=> X1,X2 en entrée + aff : Y=, Z= ,W= avec appels aux sous fonctions pour calculer individuellement les données de chaque var
  }
  
  #moyenne de la var X
  eCertificatBigData<- function(X1,X2){
    mX1=mean(X1);
    mX2=mean(X2);
    mY=mX1+mX2;
    mZ=mX1*mX2; #car indep
    mW=mX1/mX2;
      cat("mY :",mY, "mZ:",mZ,"mW: ",mW);
  }
  
  #variance 
  #Rappel: Var(X+Y)=Var(X)+Var(Y) +2.cov(X,Y) si va indépendantes 2.cov(X,Y) =0 (X1 et X2 indépendantes ici)
  #Var(XY) = E[(XY)^2] - E[XY]^2
  #=E(X^2)E(Y^2) + (E(X)E(Y))^2=var(X*Y)
  vCertificatBigData<- function(X1,X2){
   varY=var(X1)+var(X2);
   varZ=var(X1*X2);
   varW=var(X1/X2);
   #varW2=mean((X1/X2)^2)-mean(X1/X2)^2;
   cat("varY :",varY, "varZ:",varZ,"varW: ",varW);
  }
  
  #quartile
  qCertificatBigData<- function(X1,X2){
    #si on souhaite un quatile précis (ici le 3ème )quantile(X,.75);
    #sinon tous les quartiles
    print("qY:");
    print(quantile(X1+X2));
    print("qZ");
    print(quantile(X1*X2));
    print("qW");
    print(quantile(X1/X2));
  }