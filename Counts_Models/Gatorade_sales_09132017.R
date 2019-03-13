
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
library(statmod)
library(nleqslv)
library(VGAM)
#-------------------------------------------------------
#Setup
#-------------------------------------------------------
cRoot<-"H:/Gatorade/model2017test"
gat<-read.csv( file=paste0(cRoot,"/Gatorade_hs_sales.csv"), head = TRUE)
names(gat) <- tolower(names(gat))


#-------------------------------------------------------
#modeling
#-------------------------------------------------------
#zero inflatted poisson
gat.fit0 <- vglm( f , zipoisson, data = gat, trace = TRUE)

coef(gat.fit0, matrix = TRUE)
gat.pred0<-predict(gat.fit0, gat,  type = "response")

hist(gat.pred0)
resid<- gat.pred0 - data.model$tot_kits_cap
mean(abs(resid))


##############################################################################
#model 0
var_lists<- c("football",  "total_sports"  , "property_size",  "gforce"  ,                
              "temperature" ,  "rainfall"  ,   "msa_arod" ,  "msa_jh", "presence_scc1",           
              "population_score_reverse", "wealth_score_reverse" )

data.model<-gat[, which(names(gat) %in% c("tot_kits_cap", var_lists))]
data.model<-data.model[complete.cases(data.model), ]

intercpt<- rep(1, nrow(data.model))
pred.mat <-as.matrix(cbind(intercpt, data.model[,  which(names(data.model) %in% var_list)]),
                     nrow = nrow(data.model))
y.vec <- data.model[, which(names(data.model) %in% c("tot_kits_cap")) ]
cov.mat<- as.matrix(cbind( rep(1, nrow(pred.mat)) ), nrow = nrow(data.model))

beta0<-rep(.001, ncol(pred.mat) )
delta0 <- rep(.1, ncol(cov.mat) )

ind.cen <- as.numeric(y.vec >= 15)
fit.coef<- CZIPR(beta.init= beta0, delta.init = delta0, 
      resp= y.vec, censor.d=ind.cen, dependX = pred.mat, logitZ =cov.mat)

pred.gat<- CZIPR.pred(X= pred.mat, Z=cov.mat, beta=fit.coef$beta.est$estimate, delta =fit.coef$delta.est$estimate )
valid.gat <- good.fit(y.vec, X= pred.mat, Z= cov.mat, ds = ind.cen, beta = fit.coef$beta.est$estimate, delta = fit.coef$delta.est$estimate)


#check
mean(abs(pred.gat$mean.zipois - data.model$tot_kits_cap))
mean(abs(pred.gat$pred.random - data.model$tot_kits_cap))
mean((pred.gat$mean.zipois - data.model$tot_kits_cap)^2)
mean((pred.gat$pred.random - data.model$tot_kits_cap)^2)

sum(data.model$tot_kits_cap)
sum(pred.gat$pred.random)
sum(pred.gat$mean.zipois)

output.model1<-list(fit.coef= fit.coef, pred.gat = pred.gat, valid.gat = valid.gat)
save(output.model1, file = paste0(cRoot, "/model0.RData" ))

##############################################################################
#model 1


var_list1<-c( "total_sports",	"property_size",	"temperature",	"rainfall",	"football",	
              	"gforce",	"presence_scc1",	"msa_jh",	
             	"population_score_reverse"		
)

data.model<-gat[, which(names(gat) %in% c("tot_kits_cap", var_list1))]
data.model<-data.model[complete.cases(data.model), ]



intercept<- rep(1, nrow(data.model))
pred.mat <-as.matrix(cbind(intercept, data.model[,  which(names(data.model) %in% var_list1)]),
                     nrow = nrow(data.model))
y.vec <- data.model[, which(names(data.model) %in% c("tot_kits_cap")) ]
cov.mat<- as.matrix(cbind( intercept), nrow = nrow(data.model))

beta0<-rep(.001, ncol(pred.mat) )
delta0 <- rep(.1, ncol(cov.mat) )

ind.cen <- as.numeric(y.vec >= 15)
fit.coef<- CZIPR(beta.init= beta0, delta.init = delta0, 
                 resp= y.vec, censor.d=ind.cen, dependX = pred.mat, logitZ =cov.mat)

pred.gat<- CZIPR.pred(X= pred.mat, Z=cov.mat, beta=fit.coef$beta.est$estimate, delta =fit.coef$delta.est$estimate )
valid.gat <- good.fit(y.vec, X= pred.mat, Z= cov.mat, ds = ind.cen, beta = fit.coef$beta.est$estimate, delta = fit.coef$delta.est$estimate)

output.model1<-list(fit.coef= fit.coef, pred.gat = pred.gat, valid.gat = valid.gat)
save(output.model1, file = paste0(cRoot, "/model1.RData" ))


#check
mean(abs(pred.gat$mean.zipois - data.model$tot_kits_cap))
mean(abs(pred.gat$pred.random - data.model$tot_kits_cap))
mean((pred.gat$mean.zipois - data.model$tot_kits_cap)^2)
mean((pred.gat$pred.random - data.model$tot_kits_cap)^2)

sum(data.model$tot_kits_cap)
sum(pred.gat$pred.random)
sum(pred.gat$mean.zipois)

output.model1<-list(fit.coef= fit.coef, pred.gat = pred.gat, valid.gat = valid.gat)


#check
mean(abs(pred.gat$mean.zipois - data.model$tot_kits_cap))
mean(abs(pred.gat$pred.random - data.model$tot_kits_cap))
mean((pred.gat$mean.zipois - data.model$tot_kits_cap)^2)
mean((pred.gat$pred.random - data.model$tot_kits_cap)^2)

sum(data.model$tot_kits_cap)
sum(pred.gat$pred.random)
sum(pred.gat$mean.zipois)



##############################################################################
#model 2


var_list2<-c(							
  "football",	"total_sports",	"private",	"charter",	"gforce",	
  "temperature",	"rainfall",	"msa_jh",	"presence_at1",		"presence_scc1",
  "population_score_reverse",	"wealth_score_reverse",	
	"rpm_decile_high"
)

data.model<-gat[, which(names(gat) %in% c("tot_kits_cap", var_list2))]
data.model<-data.model[complete.cases(data.model), ]



intercept<- rep(1, nrow(data.model))
pred.mat <-as.matrix(cbind(intercept, data.model[,  which(names(data.model) %in% var_list2)]),
                     nrow = nrow(data.model))
y.vec <- data.model[, which(names(data.model) %in% c("tot_kits_cap")) ]
cov.mat<- as.matrix(cbind( intercept), nrow = nrow(data.model))

beta0<-rep(.001, ncol(pred.mat) )
delta0 <- rep(.1, ncol(cov.mat) )

ind.cen <- as.numeric(y.vec >= 15)
fit.coef<- CZIPR(beta.init= beta0, delta.init = delta0, 
                 resp= y.vec, censor.d=ind.cen, dependX = pred.mat, logitZ =cov.mat)

pred.gat<- CZIPR.pred(X= pred.mat, Z=cov.mat, beta=fit.coef$beta.est$estimate, delta =fit.coef$delta.est$estimate )
valid.gat <- good.fit(y.vec, X= pred.mat, Z= cov.mat, ds = ind.cen, beta = fit.coef$beta.est$estimate, delta = fit.coef$delta.est$estimate)

output.model2<-list(fit.coef= fit.coef, pred.gat = pred.gat, valid.gat = valid.gat)
save(output.model2, file = paste0(cRoot, "/outjput/model2.RData" ))


#check
mean(abs(pred.gat$mean.zipois - data.model$tot_kits_cap))
mean(abs(pred.gat$pred.random - data.model$tot_kits_cap))
mean((pred.gat$mean.zipois - data.model$tot_kits_cap)^2)
mean((pred.gat$pred.random - data.model$tot_kits_cap)^2)

sum(data.model$tot_kits_cap)
sum(pred.gat$pred.random)
sum(pred.gat$mean.zipois)

output.model2<-list(fit.coef= fit.coef, pred.gat = pred.gat, valid.gat = valid.gat)


#check
mean(abs(pred.gat$mean.zipois - data.model$tot_kits_cap))
mean(abs(pred.gat$pred.random - data.model$tot_kits_cap))
mean((pred.gat$mean.zipois - data.model$tot_kits_cap)^2)
mean((pred.gat$pred.random - data.model$tot_kits_cap)^2)

sum(data.model$tot_kits_cap)
sum(pred.gat$pred.random)
sum(pred.gat$mean.zipois)


##############################################################################
#model 3


var_list3<-c( "football",	"total_sports",		"gforce",
              "temperature",	"rainfall",		"msa_jh",	"presence_at1",		"presence_scc1",
              "population_score_reverse",	"wealth_score_reverse",	
              	"rpm_decile_high",	"ethnicity_african_american",	
              "ethnicity_asian",	"ethnicity_caucasian",	"ethnicity_hispanic",	"ethnicity_native_american"
              
)

data.model<-gat[, which(names(gat) %in% c("tot_kits_cap", var_list3))]
data.model<-data.model[complete.cases(data.model), ]



intercept<- rep(1, nrow(data.model))
pred.mat <-as.matrix(cbind(intercept, data.model[,  which(names(data.model) %in% var_list3)]),
                     nrow = nrow(data.model))
y.vec <- data.model[, which(names(data.model) %in% c("tot_kits_cap")) ]
cov.mat<- as.matrix(cbind( intercept), nrow = nrow(data.model))

beta0<-rep(.001, ncol(pred.mat) )
delta0 <- rep(.1, ncol(cov.mat) )

ind.cen <- as.numeric(y.vec >= 15)
fit.coef<- CZIPR(beta.init= beta0, delta.init = delta0, 
                 resp= y.vec, censor.d=ind.cen, dependX = pred.mat, logitZ =cov.mat)

pred.gat<- CZIPR.pred(X= pred.mat, Z=cov.mat, beta=fit.coef$beta.est$estimate, delta =fit.coef$delta.est$estimate )
valid.gat <- good.fit(y.vec, X= pred.mat, Z= cov.mat, ds = ind.cen, beta = fit.coef$beta.est$estimate, delta = fit.coef$delta.est$estimate)

output.model3<-list(fit.coef= fit.coef, pred.gat = pred.gat, valid.gat = valid.gat)

save(output.model3, file = paste0(cRoot, "/model3.RData" ))


##############################################################################
#model 4


var_list4<-c( "football",	"total_sports",		"gforce",
              "temperature",	"rainfall",		"msa_jh",		"presence_at1",		"presence_scc1",
              "population_score_reverse",	"wealth_score_reverse",
          	"rpm_decile_high"
              
)

data.model<-gat[, which(names(gat) %in% c("tot_kits_cap", var_list4))]
data.model<-data.model[complete.cases(data.model), ]



intercept<- rep(1, nrow(data.model))
pred.mat <-as.matrix(cbind(intercept, data.model[,  which(names(data.model) %in% var_list4)]),
                     nrow = nrow(data.model))
y.vec <- data.model[, which(names(data.model) %in% c("tot_kits_cap")) ]
cov.mat<- as.matrix(cbind( intercept), nrow = nrow(data.model))

beta0<-rep(.001, ncol(pred.mat) )
delta0 <- rep(.1, ncol(cov.mat) )

ind.cen <- as.numeric(y.vec >= 15)
fit.coef<- CZIPR(beta.init= beta0, delta.init = delta0, 
                 resp= y.vec, censor.d=ind.cen, dependX = pred.mat, logitZ =cov.mat)

pred.gat<- CZIPR.pred(X= pred.mat, Z=cov.mat, beta=fit.coef$beta.est$estimate, delta =fit.coef$delta.est$estimate )
valid.gat <- good.fit(y.vec, X= pred.mat, Z= cov.mat, ds = ind.cen, beta = fit.coef$beta.est$estimate, delta = fit.coef$delta.est$estimate)

output.model4<-list(fit.coef= fit.coef, pred.gat = pred.gat, valid.gat = valid.gat)
save(output.model4, file = paste0(cRoot, "/model4.RData" ))


##############################################################################
#model comparison

zip<-c(69496,	69447,	63106, 	66547)
df.zip<- c(20158,	20158,	17495,	18780)- c(9,	13,	16,	11)

zipc<-c (63857,	60992,	57727,	60994)
df.zipc<-c(20125,	18756,	17471,	18756) - c(9,	13,	16,	11) 

ldv<-c(63719,	63355,	57868,	60993)
df.ldv<-c(20158,	20158,	17495,	18780)

f.zip.zipc<- numeric()
for (i in 1:length(zip)){
  f.zip.zipc[i]<-pf(zip[i]/zipc[i], df.zip[i], df.zipc[i])
}

f.zip.ldv<- numeric()
for (i in 1:length(zip)){
  f.zip.ldv[i]<-pf(zip[i]/ldv[i], df.zip[i], df.ldv[i])
}

f.zipc.ldv<- numeric()
for (i in 1:length(zipc)){
  f.zipc.ldv[i]<-pf(zipc[i]/ldv[i], df.zip[i], df.ldv[i])
}