
#-------------------------------------------------------
#Functionality
#-------------------------------------------------------
#Code created by Data Science team to predict clusters for VCA data

#-------------------------------------------------------
#Clean workspace
#-------------------------------------------------------
rm(list=ls())
#-------------------------------------------------------
#Libraries
#-------------------------------------------------------
library(nleqslv)
#-------------------------------------------------------
#Setup
#-------------------------------------------------------


#----------------------------------------------------------------------------------------
#Zero-inflated poisson regression with right type II censored  data
#----------------------------------------------------------------------------------------
##################################################################
#MLE
#mass function and survival function 
#X: predictive matrix
#ys: response 
#beta: coefficients 
#delta: parameter control the logit
#lambdas: the mean of poisson related to beta
#phis: the proability to determine the zero-inflatted 

pdf.y<-function(y, lambda, phi){
  if (y == 0)
    Fy = phi + (1-phi) * exp(-lambda)
  else
    Fy = (1-phi) * exp(-lambda) * lambda^y / factorial(y)
  return (Fy)
}  

surv.y<-function(y, lambda, phi, float=1E-10 ){
  s.y <- 0
  if (y > 0){
    for (k in 0:(y-1)){
      s.y <- s.y - pdf.y(k, lambda, phi)
    }
  }
  return(s.y +1 + float )  
}  

ll.czip<-function(ys, ds, lambdas, phis){
  ll <- 0
  for (i in 1:length(ys)){
    ll <- ll + (1-ds[i]) *((as.numeric(ys[i]==0)) * log(phis[i]+ (1-phis[i])*exp(-lambdas[i]))
                          + (as.numeric(ys[i] > 0)) *(log(1-phis[i]) + ys[i]*log(lambdas[i])- log(factorial(ys[i]))- lambdas[i] ) )
             + ds[i] * (log(surv.y(ys[i], lambdas[i], phis[i])))
    
  }
  return(ll)
}

#
#ll.czip<-function(ys, ds, lambdas, phis){
#  ll <- 0
#  for (i in 1:length(ys)){
#    ll <- ll + (1-ds[i]) * log(pdf.y(ys[i], lambdas[i], phis[i]))
#    + ds[i] * (log(surv.y(ys[i], lambdas[i], phis[i])))  }
#  return(ll)
#}



#derivative of the survival to coefficient beta_r
dsurv<-function(y, lambda, phi, x, r, float =  1E-10){
  dsurv <- 0
  if (y >0){
    for (k in 0:(y-1))
      dsurv <- dsurv - pdf.y(k, lambda, phi) * (k - lambda) *x
  }
  return(dsurv+float)
}  

#derivative of the likelyhood to the coefficient beta_r
dl.dbr<-function(ys=y.vec, ds=ind.cen, betas=beta.vec , deltas=delta.vec, X=pred.mat, Z=cov.mat, r=r.ind, float = 1E40){
  lambdas <-  pmin(exp(X %*% as.matrix(betas, ncol = 1)), float)
  deltas <- as.matrix(deltas, ncol = 1)
  phis <-  exp(Z %*% deltas)/(1+exp(Z %*% deltas))
  ws <-  exp(Z %*% deltas)
  dll <- 0 
  for (i in 1:length(ys)){
    dll<- dll + (1- ds[i]) *(as.numeric(ys[i]==0)*(-ws[i]^(-1)*exp(-lambdas[i]))/(1+ws[i]^(-1)*exp(-lambdas[i]))*X[i,r] *lambdas[i]
                             + as.numeric(ys[i]>0)* (ys[i] - lambdas[i])* X[i,r]  ) 
    +ds[i]/surv.y(ys[i], lambdas[i], phis[i])*dsurv(ys[i], lambdas[i], phis[i], X[i,r], r)
  }
  return(dll)
}


#derivative of the likelyhood to the logit parameter delta_t
dl.ddt<-function(ys = y.vec, ds= ind.cen, betas= beta.vec, deltas = delta.vec, X=pred.mat, Z=cov.mat, t=t.ind,  float = 1E40){
  lambdas <-  pmin(exp(X %*% as.matrix(betas, ncol = 1)), float)
  deltas <- as.matrix(deltas, ncol = 1)
  phis <-  exp(Z %*% deltas)/(1+exp(Z %*% deltas))
  ws <-  exp(Z %*% deltas)
  dld <- 0 
  for (i in 1:length(ys)){
    dld <- dld + (1- ds[i]) *(as.numeric(ys[i]==0)*(1-exp(-lambdas[i]))/(ws[i] + exp(-lambdas[i])) 
                              -as.numeric(ys[i]>0)) * ws[i]/(1 + ws[i])*Z[i, t] +
      ds[i] *(1-ppois(ys[i]-1, lambdas[i]))/surv.y(ys[i], lambdas[i], phis[i]) * (phis[i] - phis[i]^2)* Z[i, t]
  }
  return(dld)
}




#beta functions to solve later, given all other parameters fixed
beta.fs<-function(beta, y=y.vec, d=ind.cen, delta=delta.vec, X=pred.mat, Z=cov.mat){
  n.beta <- ncol(X)
  beta.f<-numeric()
  for (i in 1:n.beta){
    beta.f[i] <- dl.dbr(ys=y, ds=d, betas=beta , deltas= delta, X=X, Z=Z,r=i)
  }
  beta.f
}

#delta functions to solve later, given all other parameters fixed
delta.fs<-function(delta, y = y.vec, d=ind.cen, beta = beta.vec, X = pred.mat, Z =cov.mat){
  n.delta <- ncol(Z)
  delta.f<-numeric()
  for (i in 1:n.delta){
    delta.f[i] <- dl.ddt(ys=y, ds=d, betas=beta , deltas= delta, X=X, Z=Z,t=i)
  }
  delta.f
}

#solve the unknown parameters 
#solv.b<-  nleqslv(beta.init, beta.fs, control=list(btol=.01), 
#                  y=y.vec, d=ind.cen,delta=delta.iter, X=pred.mat, Z=cov.mat)
#solv.d <-  nleqslv(delta.init, delta.fs, control=list(btol=.01), 
#                   y = y.vec, d=ind.cen, beta = beta.iter, X = pred.mat, Z =cov.mat)

#finalize the likelihood
CZIPR<-function(beta.init= beta0, delta.init = delta0, 
                resp= y.vec, censor.d=ind.cen, dependX = pred.mat, logitZ =cov.mat,
                iter.max = 1E5, tol = 1E-3){
  beta.iter <- beta.init
  delta.iter <- delta.init
  conv <- 1
  i=0
  while (conv > tol & i < iter.max){
    i <- i+1
    beta.init <- beta.iter
    delta.init<- delta.iter
    solv.b<- tryCatch(nleqslv(beta.iter, beta.fs, control=list(btol=.01), 
                              y=resp, d=censor.d,delta=delta.iter,X=dependX, Z=logitZ), 
                      error = function(e) list(converge = -999))
    beta.iter <- solv.b$x
    
    solv.d<- tryCatch(nleqslv(delta.iter, delta.fs, control=list(btol=.01), 
                              y=resp, d=censor.d,beta=beta.iter,X=dependX, Z=logitZ), 
                      error = function(e) list(converge = -999))
    delta.iter <- solv.d$x
    if (length(beta.iter) > 0 & length(delta.iter) > 0){
      conv = max(abs(beta.iter - beta.init), abs(delta.iter - delta.init))
    }
    else {
      break
      return(list(convergence = "No"))
    }
  }
  if (i == iter.max){
    return(list(convergence = "No"))
  }
  else {
    paramX = names(dependX[1,])
    beta = data.frame(parameters = paramX, estimate = beta.iter)
    paramZ = names(logitZ[1,])
    delta = data.frame(parameters = paramZ, estimate = delta.iter)
    return (list(beta.est = beta, delta.est = delta))
  }
}


#prediction 
#generate zero-inflatted poisson distribution 

CZIPR.pred<-function(X= pred.mat, Z=cov.mat, beta=param.coeff, delta =param.inf ){
  lambdas <- exp(X %*% beta)
  phis <- exp(Z %*% delta)/(1+exp(Z %*% delta))
  rpred.pois<-rzipois(nrow(X), lambdas, phis)
  mean.zipois <- (1-phis)*lambdas
  var.zipois <- (1-phis) *lambdas *(1+phis*lambdas)
  return(list(lambda = lambdas, phi = phis,  mean.zipois = mean.zipois, var.zipois = var.zipois,pred.random = rpred.pois))
}


#goodness of fit
good.fit<- function(ys= y.vec, X= pred.mat, Z= cov.mat, ds = ind.cen, beta=param.coeff, delta =param.inf  ){
  preds <- CZIPR.pred(X, Z, beta, delta)
  ll.fit <- ll.czip(preds$mean.zipois, ds, preds$lambda, preds$phi)
  ll.null <- ll.czip(ys, ds, preds$lambda, preds$phi)
  Dev <- -2*(ll.fit - ll.null) 
  p.Dev <- 1-pchisq(Dev, nrow(X) - ncol(X)-ncol(Z) -1)
  AIC <- (ncol(X) + ncol(Z)) - 2*ll.null
  BIC <- log(nrow(X))*(ncol(X) + ncol(Z)) - 2*ll.null
  
  return(list(Log.Likelihood = ll.null, Deviance = Dev, p.Deviance = p.Dev, AIC = AIC, BIC = BIC))
}

#########################################################################################
#simulation
#set simulated response
set.seed(1234)
ssize = 1000
beta.vec<-as.matrix( c(0.1, 0.5, 5 ), ncol= 1) 
delta.vec<- as.matrix( c(.1, 1), ncol= 1) 

x1 <- runif(ssize)
x2 <- runif(ssize, min = 0, max = 3.2)
x3 <- runif(ssize, 0, 0.1)
pred.mat<-cbind(x1, x2, x3)
cov.mat <-cbind(rep(1, ssize), runif(ssize, 0, 1))
lambdas <- exp(pred.mat %*% beta.vec)
phis <- exp(cov.mat %*% delta.vec)/(1+exp(cov.mat %*% delta.vec))

y.true<-rep(NA, ssize)
for (i in 1:ssize){
  u<-runif(1)
  if (u < phis[i])
    y.true[i] <- 0
  else y.true[i] <- rpois(1, lambdas[i])
}

h <- 20 # h is the threshold of the type-II censoring
ind.cen <- as.numeric(y.true > h)
y.vec <- pmin(h, y.true)

beta0<-c(0, .4, .4)
delta0<-c(.2, 1)

fit<-CZIPR(beta.init= beta0, delta.init = delta0, 
      resp= y.vec, censor.d=ind.cen, dependX = pred.mat, logitZ =cov.mat)

pred.sample<- CZIPR.pred(X= pred.mat, Z=cov.mat, beta=fit.sample$beta.est, delta =fit.sample$delta.est )

test.sample <- good.fit(y.vec, X= pred.mat, Z= cov.mat, ds = ind.cen, beta = fit.sample$beta.est, delta = fit.sample$delta.est)

# using the vglm to test 
data.test <-as.data.frame(cbind(y.vec, X))
fit.zipois <- vglm(y.vec ~ x1+x2+x3, zipoisson, data = data.test, trace = TRUE)
coef(fit.zipois, matrix = TRUE) # These should agree with the above values
aa<-predict(fit.zipois, data.test, type = "response")


###simulation example 2
set.seed(1234)
ssize = 1000
beta.vec<-as.matrix( c(0.1,  1 ), ncol= 1) 
delta.vec<- as.matrix( c(.2), ncol= 1) 

x1 <- runif(ssize)
x2 <- runif(ssize, min = 0, max = 3)
x3 <- runif(ssize, 0, 0.1)
pred.mat<-cbind(x1, x2)
cov.mat <-cbind(rep(1, ssize))
lambdas <- exp(pred.mat %*% beta.vec)
phis <- exp(cov.mat %*% delta.vec)/(1+exp(cov.mat %*% delta.vec))
y.true<-rep(NA, ssize)
for (i in 1:ssize){
  u<-runif(1)
  if (u < phis[i])
    y.true[i] <- 0
  else y.true[i] <- rpois(1, lambdas[i])
}

h <- 12 # h is the threshold of the type-II censoring
ind.cen <- as.numeric(y.vec > h)
y.vec <- pmin(h, y.true)

beta0<-c(0, .4)
delta0<-c(.5)

CZIPR(beta.init= beta0, delta.init = delta0, 
      resp= y.vec, censor.d=ind.cen, dependX = pred.mat, logitZ =cov.mat)


