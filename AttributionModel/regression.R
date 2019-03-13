

#-------------------------------------------------------
#Functionality
#-------------------------------------------------------

#-------------------------------------------------------
#Clean workspace
#-------------------------------------------------------
rm(list=ls())
#-------------------------------------------------------
#Libraries
library(dplyr)
library(tidyr)
library(reshape2)
library(caret)
library(foreach)
library(relimp)
#-------------------------------------------------------
#install.packages("data.table","ggplot2")

#-------------------------------------------------------
#Setup
#-------------------------------------------------------
dir<-


#-------------------------------------------------------
#data prepare
#-------------------------------------------------------

# simulating the "real" data
set.seed(354)
df2 <- data.frame(client_id = sample(c(1:1000), 5000, replace = TRUE),
                  date = sample(c(1:32), 5000, replace = TRUE),
                  channel = sample(c(0:9), 5000, replace = TRUE,
                                   prob = c(0.1, 0.15, 0.05, 0.07, 0.11, 0.07, 0.13, 0.1, 0.06, 0.16)))
df2$date <- as.Date(df2$date, origin = "2015-01-01")
df2$channel <- paste0('channel_', df2$channel)

# count the occurent and dummy coding 
res <- df2 %>% group_by(client_id,channel) %>% summarise(Freq=n())
res<-as.data.frame(res)
res2 <- dcast(res, client_id  ~ channel, value.var="Freq")
res2[is.na(res2)]<- 0


#-------------------------------------------------------
#linear regression model
#-------------------------------------------------------
set.seed(123)
coeff <- runif(11, 0, max = 10)
indep <- cbind(rep(1, nrow(res2)), res2[, -1])

sales<- 10+ apply(indep, 1, function(x) sum(x*coeff)) +rnorm(nrow(indep), 0, 1)

df_reg <-cbind(sales, indep)
f<- as.formula(paste("sales ~", paste(names(indep[1, 2:11]), collapse = " + ")))

reg <-lm(f, data = df_reg)

#split the contribution: 
#by the coefficient
reg$coefficients[-1]/sum(reg$coefficients[-1]) *100

#by looking at the total contribution
apply(indep[, -1], 2, sum) * reg$coefficients[-1] /sum(apply(indep[, -1], 2, sum) * reg$coefficients[-1]) *100

#relative importance:
#PRE = (Residual Sum of Squares of M2 - Residual Sum of Squares of M1) / Residual Sum of Squares of M2
#the RSS of models 
#full model

SSR0 <-deviance(reg) 
PREs<-numeric()

for ( i in 2:ncol(indep)){
  reg_temp <-cbind(sales, indep[, -i])
  f<- as.formula(paste("sales ~", paste(names(reg_temp[1, 3:ncol(reg_temp)]), collapse = " + ")))
  reg_reduced <-lm(f, data = reg_temp)
  PREs[i-1]<- (deviance(reg_reduced)- SSR0)/deviance(reg_reduced)*100
  }


#-------------------------------------------------------
#logistic regression model --- the naive way
#-------------------------------------------------------
#simulate the conversion

set.seed(123)
coeff <- runif(11, -1, max = 1)
conv_lgt <- apply(indep, 1, function(x) sum(x*coeff))
conv_prob <- exp(conv_lgt)/(1+exp(conv_lgt))
conv <-numeric()
for( i in 1:nrow(indep)){
  conv[i] <-ifelse( conv_prob[i] < 0.5, 0, 1)
}

#modeling
df_lgt <-cbind(conv, indep)
f<- as.formula(paste("conv ~", paste(names(indep[1, 2:11]), collapse = " + ")))

#prediction check
lgt <-glm(f, data = df_lgt)
pred_glm <- ifelse( predict.glm(lgt, df_lgt, tyep = "response") > 0.5, 1, 0)
table(conv, pred_glm)

varImp(lgt)


#-------------------------------------------------------
#logistic regression model --- the bagging method or the bagged trees 
#-------------------------------------------------------

training      <- cbind(conv, res2[, -1]) 
samp_rate <- 0.5
var_rate <- 0.3
iterations     <- 10
name_ind <-names(training)
output <-numeric()

set.seed(123)
for(m in 1:iterations){
  training_positions <- sample(nrow(training), 
                               size=floor(nrow(training)*samp_rate))
  training_var <- sample((2:ncol(training)), 
                         size=floor(ncol(training)*var_rate))
  #  train_pos<-1:nrow(training) %in% training_positions
  glm_fit <- glm(conv ~ . ,
                 data=training[training_positions, c(1,training_var)],
                 family=binomial(logit))
  s<-summary(glm_fit)
  vars <- varImp(glm_fit)
  output<-rbind(output, cbind(rep(m, length(training_var)), name_ind[training_var],  s$coefficients[-1, 1], vars ))
  
  }

out<-data.frame(output)
names(out)<-c("iteration", "channel", "coeff", "imp")
est_table<- aggregate(out[, 3:4], list(out$channel), mean)
est_table[order(as.character(est_table$Group.1)),]


#-------------------------------------------------------
#survival  model - 
#-------------------------------------------------------
























































