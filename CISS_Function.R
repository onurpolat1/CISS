## Thıs scrıpt create composıte ındıcator of systemıc stress for a country 
## by usıng ıts sub-fınancıal market ındexes

# CISS analysis

fsi<-merge(bankfsi,bondfsi,moneyfsi,equityfsi,fxfsi)

##Function
##x is merge of sub-market indexes
CISS<-function(x){
  garch.spec<-ugarchspec(mean.model = list(armaOrder = c(1,1)), 
  variance.model = list(garchOrder = c(1,1), model = "sGARCH"), 
  distribution.model = "norm")
  dcc.spec<-dccspec(uspec = multispec(replicate(5, garch.spec)), dccOrder = c(1,1), distribution = "mvnorm")
  dcc.fit<-dccfit(dcc.spec, data =x , fit.control=list(scale=TRUE))
  cormat<-rcor(dcc.fit, type="R")
  cormatrix<-cormat[1:5,1:5,]
  weight<-0.2 # Equal weights
  y<-xts(x)
  y<-weight*x
  S<-matrix(y,nrow=length(y[,1]),ncol=5)
  S_tr<-t(S)
  ciss<-xts(y[,1])
  for (i in length(y[,1])){
    ciss[i]=(S[i,]%*%cormat[1:5,1:5,i]%*%S_tr[,i])
  }
  return(ciss)
}
