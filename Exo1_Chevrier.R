pCertificatBigData = function(X1,X2, plotGraph=T) {
  CDF_S = ecdf(X1+X2)
  CDF_P = ecdf(X1*X2)
  CDF_Q = ecdf(X1/X2)
  
  if(plotGraph) {
    par(mfrow=c(2,2))
    plot(CDF_S)
    plot(CDF_P)
    plot(CDF_Q)
  }
  
  return(list(SumCDF=CDF_S, ProductCDF = CDF_P, QuotientCDF=CDF_Q))
}



dCertificatBigData = function(X1,X2, plotGraph=T) {
  SumPDF = density(X1+X2)
  ProductPDF = density(X1*X2)
  QuotientPDF = density(X1/X2)
  
  if(plotGraph) {
    par(mfrow=c(2,2))
    plot(SumPDF)
    plot(ProductPDF)
    plot(QuotientPDF)
  }
  
  return(list(SumPDF=SumPDF, ProductPDF=ProductPDF, QuotientPDF=QuotientPDF))
}

qCertificatBigData = function(X1,X2) {
  
  SumSummary = summary(X1+X2)
  ProductSummary = summary(X1*X2)
  QuotientSummary = summary(X1/X2)
  
  return(list(Sum1stQu=SumSummary["1st Qu."], Sum3rdQu=SumSummary["3rd Qu."], 
              Product1stQu=ProductSummary["1st Qu."], Product3rdQu=ProductSummary["3rd Qu."],
              Quotient1stQu=QuotientSummary["1st Qu."], Quotient3rdQu=QuotientSummary["3rd Qu."]))
  
}

rCertificatBigData = function(n) {
  return(list(X1=runif(n), X2=runif(n)))
  
}

eCertificatBigData = function(X1,X2) {
  return(list(SumMean=mean(X1+X2), ProductMean=mean(X1*X2), QuotientMean=mean(X1/X2)))
}

vCertificatBigData = function(X1,X2) {
  return(list(SumVar=var(X1+X2), ProductVar=var(X1*X2), QuotientVar=var(X1/X2)))
}

