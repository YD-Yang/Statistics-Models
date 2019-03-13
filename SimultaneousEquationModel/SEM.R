library("systemfit")
 data( "Kmenta" )
 attach( Kmenta )

 
set.seed(1234)
 
X1<-matrix(rnorm(500, 1, .5), ncol=5)
X2<-matrix(rgamma(500, 1, 2), ncol=5)
X3<-matrix(rnorm(500, 1, .3), ncol =5)                                                                         
X4<-matrix(rgamma(500, 2, 4), ncol =5)
test_data<-data.frame(y1, y2, x1, x2, x3, x4)

y1<-3*test_data$x1+2*test_data$x2+rnorm(100, 0, .3)
y2<-0.8*test_data$y1+2*test_data$x3+3*test_data$x4+rnorm(100, 0, .2)

m1=5
m2=5
m3=5
m4=5

est_low<-c(0, 0, 0, 0,0)
est_upp<-c(1, 5,5, 5,5)



SEM_all<-function(alpha, est_low, est_upp){
  coef_output<-numeric()
  p_value_output<-numeric()
  index_output <-numeric()
for (i1 in 1:m1){
  x1 = X1[, i1]
  for (i2 in 1:m2){
    x2 = X2[, i2]
    for (i3 in 1:m3){
      x3 = X3[, i3]
      for (i4 in 1:m4){
        x4 = X4[, i4]
        eq1<- y1 ~ x1 + x2
        eq2<- y2 ~ y1 + x3 + x4
        eqSystem <- list( Eq1 = eq1, Eq2 = eq2 )  
        fit2sls<- systemfit( eqSystem, method = "2SLS",
                        inst = ~ x1+x2+x3+x4)
        coef_est<-coef(summary(fit2sls))[c(5, 2,3, 6,7), c(1, 4)]
        inds<-(sum(coef_est[,1] > est_low)/num_cov)*(sum(coef_est[,1] < est_upp)/num_cov)*(sum(coef_est[,2] < alpha)/5)
        if (inds == 1) 
          {coef_output<-rbind(coef_output, c(coef_est[, 1]))
           p_value_output<-rbind(p_value_output, c(coef_est[,2]))
           index_output<-rbind(index_output, c(i1, i2, i3, i4, i5)) 
           }
      }
    }
  }
}
  
  return(list(coef=coef_output, p_value = p_value_output, index = index_output))
}


var_N = 4
lag_N = 5
#i the ith variable 
#j the jth lag
list_check<-rep(0, var_N)



#start 
list_check <-sample(0:lag_N, var_N, replace=TRUE)

for (i in 1:var_N){
  for (j in 1: lag_N){
    
  }
}  
  
  





########################################################
#original example
test_data<-data.frame(y1, y2, x1, x2, x3, x4)

y1<-3*test_data$x1+2*test_data$x2+rnorm(100, 0, .3)
y2<-0.8*test_data$y1+2*test_data$x3+3*test_data$x4+rnorm(100, 0, .2)

eq1<- y1 ~ test_data$x1 + test_data$x2
eq2<- y2 ~ y1 + test_data$x3 + test_data$x4
eqSystem <- list( Eq1 = eq1, Eq2 = eq2 )
fit2sls<- systemfit( eqSystem, method = "2SLS",
                     inst = ~ test_data$x1 + test_data$x2+test_data$x3 + test_data$x4)
coef_est<-coef(summary(fit2sls))[c(5, 2,3, 6,7), c(1, 4)]

est_low<-c(0, 0, 0, 0,0)
est_upp<-c(1, 5,5, 5,5)
est_simu <-c(0,8, 3, 2, 2, 3)



num_cov = 5
inds<-(sum(coef_est[,1] > est_low)/num_cov)*(sum(coef_est[,1] < est_upp)/num_cov)



fit<- systemfit( eqSystem)

#############################################
#example;
  eqDemand <- consump ~ price + income
 eqSupply <- consump ~ price + farmPrice + trend
 eqSystem <- list( demand = eqDemand, supply = eqSupply )
fit2sls <- systemfit( eqSystem, method = "2SLS",
                           inst = ~ income + farmPrice + trend )
fit2sls$coefficients
 coef(summary(fit2sls))
