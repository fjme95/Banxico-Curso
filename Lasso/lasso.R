library(plyr)
library(readr)
library(dplyr)
library(caret)
library(ggplot2)
library(repr)
library(glmnet)


# dataBRA<-read.csv("data/Brazil.csv",header=TRUE,sep=";")
# names(dataBRA)
# 
# glimpse(dataBRA)

####matriz mXn m-renglones o filas n-columnas

data("BRinf")
Y1=as.matrix(dataBRA[,2:59])# remove expectation variables
datesBRAZIL<-names(BRinf[,1])
datesBr<-as.Date(datesBRAZIL,"%Y-%m-%d")
namesBRAZIL<-names(BRinf[1,])
namesBRAZIL<-c("DATE",namesBRAZIL)

names(BRinf)<-names(BRinf[1,])
#Y1<-as.matrix(Y1)

#Y=BRinf[,1:59]# remove expectation variables
#lambda=fitLambda(Y,variables=c(1,4,10),lambdaseq = seq(0,0.1,0.005),p=2,p.reduced = 2)

#model=lbvar(Y,p=2,lambda=lambda)


#aux = embed(BRinf,2)
#y=aux[,1]
#x=aux[,-c(1:ncol(BRinf))]
#'
#' ## == LASSO == ##
#lasso=ic.glmnet(x,y,crit = "bic")
#plot(lasso)


#lasso=ic.glmnet(x,y,crit = "bic")
#' coef(lasso)
#' fitted(lasso)
#' residuals(lasso)
#' lasso$ic


eval_results<-function(true,predicted,df)
{
  SSE<-sum((predicted-true)^2)
  SST<-sum((true-mean(true))^2)
  R_square<-1-SSE/SST
  RMSE=sqrt(SSE/nrow(df))
  data.frame(
    RMSE=RMSE, Rsquare=R_square
  )
}

#############################Ejercicio 1.


X1<-seq(1,100,1)
X2<-X1+rnorm(100,0,sd=.01)

Y<-2*X1+3*X2+rnorm(100,0,sd=.5)

##### POR lo tanto: 2*X1+3*X2=2*X1+3*(X1+E)=2*X1+3*X1+E=5*X1+E

###REGRESIÓN SIMPLE
prueba<-lm(Y~X1+X2)


summary(prueba)
###resultados raros...
####tMODELO usando LASSO
pruebalambda<-cv.glmnet(cbind(X1,X2),Y,alpha=1,lambda=seq(0,0.3,0.005),standardize=TRUE,nfold=5)

lambdamin1<-pruebalambda$lambda.min


lasso_model_prueba<-glmnet(cbind(X1,X2),Y,alpha=1,lambda=lambdamin1,standardize=TRUE,nfolds=5)


lasso_model_prueba$beta

prueba2<-lm(Y~X1)
summary(prueba2)

####lasso es util

predictions_prueba<-predict(lasso_model_prueba,s=0.005,newx=cbind(X1,X2))


sum((predictions_prueba-Y)^2)
sum((prueba$residuals)^2)


SSElasso<-sum((predictions_prueba-Y)^2)
SSEMSE<-sum((prueba$residuals)^2)

SST<-sum((Y-mean(Y))^2)
R_squarelasso<-1-SSElasso/SST
R_squareMSE<-1-SSEMSE/SST

1-(1-R_squarelasso^2)*99/98
1-(1-R_squareMSE^2)*99/97



Regularización

#########################Ejercicio 2


MSEbrazil<-lm(Y1[,1]~Y1[,2:58])


Y2<-Y1[1:55,]# remove expectation variables


MSEbrazil2<-lm(Y2[,1]~Y2[,2:58])



summary(MSEbrazil2)


lasso_reg<-cv.glmnet(Y2[,2:58],Y2[,1],alpha=1,lambda=seq(0,0.3,0.005),standardize=TRUE,nfolds=5)
lambda_best<-lasso_reg$lambda.min

lasso_model<-glmnet(Y2[,2:58],Y2[,1],alpha=1,lambda=lambda_best,standardize=TRUE,nfolds=5)


predictions_train<-predict(lasso_model,s=lambda_best,newx=Y2[,2:58])

eval_results(Y2[,1],predictions_train,Y2[,2:58])







#lasso_reg<-cv.glmnet(BRinfo[1:70,2:59],BRinfo[1:70,1],alpha=1,lambda=seq(0,0.1,0.005),standardize=TRUE,nfolds=5)

