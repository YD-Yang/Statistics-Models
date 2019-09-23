
#predict lifetime values 
#To create the data 

#-----------------------------------------------------------------------------------------------
#clean work space
#rm(list = ls())
#-----------------------------------------------------------------------------------------------



#-----------------------------------------------------------------------------------------------
#libraries
library(dplyr)
library(survival)
library(ranger)
library(ggplot2)
library(pec)
library(pROC)
library(data.table)
library(gplots)
library("RColorBrewer")
library(survivalROC)
library(survminer)
library(purrr)
library(rms) 
library(My.stepwise)
library(tidyverse)

#-----------------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------------
#set up
path <- c("")
path_temp<- c("")

#-----------------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------------
#functionality
source(file = paste0("/CommonFunctions", ".R"))
source(file = paste0("Functions_model", ".R"))
source(file = paste0("SurvImportance", ".R"))
#-----------------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------------
#load files
tenure_data<- readRDS(file = paste0(path_temp, ".rds"))
#rownames(tenure_df2) <- tenure_df2[, 1]
#tenure_df[, 1] <- NULL


#----------------------------------------------------------------------------------------------


#----------------------------------------------------------------------------------------------
#modeling data 

#all the candidates variables in the model 
full_list<-  c('a', 'b')


#train test split
set.seed(1234)
index <- sample(1:nrow(tenure_data), floor(nrow(tenure_data)*.8), replace = FALSE)
tenure_train<- tenure_data[index,  ]
tenure_test<- tenure_data[-index, ]


#----------------------------------------------------------------------------------------------
#terminated model

#final variables in the model
model_vars<- c('a', 'b')



#cox model 
tenure_cox<- coxph(Surv(tenure, terminate_flag) ~ .,
                   data = tenure_train[, which(names(tenure_train) %in% c(model_vars, "tenure", "terminate_flag"))],
                   x= TRUE, y= TRUE)

#stepwise for variables selection 
My.stepwise.coxph(Time = "tenure", Status = "terminate_flag", variable.list =model_vars ,
                  data = tenure_train, sle = 0.08, sls = 0.08)



#calculate the concordance of the test data 
survConcordance(Surv(cbind(time=tenure_test$tenure, event=tenure_test$terminate_flag))~
                  predict(tenure_cox, newdata=tenure_test))$concordance

#calculate the vif; vif is also checked during the variable selection procedure 
VIF<- vif(tenure_cox)
VIF<- as.data.frame(VIF)
#VIF<-imp[order(-VIF$VIF), , drop = FALSE]
#VIF$cumsum <- 100 * cumsum(VIF$VIF)/sum(VIF$VIF)


#plot the survival curve
cox_fit_tenure<- survfit(tenure_cox)
coxi <- rep("Cox",length(cox_fit_tenure$time ))
cox_df <- data.frame(cox_fit_tenure$time,cox_fit_tenure$surv,coxi)
names(cox_df) <- c("Time","Surv","Model")
p <- ggplot(cox_df, aes(x = Time, y = Surv, color = Model))
p + geom_line()


#double check the variable importance
VIF<- breiman(model=tenure_cox, time=tenure_test$tenure, event=tenure_test$terminate_flag,
              covdata=tenure_test[,which(names(tenure_test) %in% model_vars)],
              nrep=3)
VIF$var <- rownames(VIF)



#----------------------------------------------------------------------------------------------
##evaluation by time point 
pred_churn<-1- predictSurvProb(tenure_cox,
                               newdata=tenure_test,
                               times=c(100))
target <- ifelse(tenure_test$tenure <=100,tenure_test$terminate_flag, 0)

roc_obj <- roc(target, pred_churn)
auc(roc_obj)
pre
auc.mc(target, pred_churn)
#####################
#calculate survival ROC
#subset a sample of the test data for the time dependent ROC curve 
set.seed(1234)
test_data <- tenure_test[sample(1:nrow(tenure_test), 10000, replace = FALSE), 
                         which(names(tenure_test) %in% c(model_vars, "terminate_flag", "tenure"))]
pred_risk <- predict(tenure_cox, newdata =test_data, type = "lp")


survivalROC_helper <- function(t) {
  survivalROC(Stime        = test_data$tenure,
              status       = test_data$terminate_flag,
              marker       = pred_risk,
              predict.time = t,
              method       = "NNE",
              span = 0.25 * nrow(test_data)^(-0.20))
}

## Evaluate at time 
survivalROC_data <- data_frame(t = t0) %>%
  mutate(survivalROC = map(t, survivalROC_helper),
         ## Extract scalar AUC
         auc = map_dbl(survivalROC, magrittr::extract2, "AUC"),
         ## Put cut off dependent values in a data_frame
         df_survivalROC = map(survivalROC, function(obj) {
           as_data_frame(obj[c("cut.values","TP","FP")])
         })) %>%
  dplyr::select(-survivalROC) %>%
  unnest() %>%
  arrange(t, FP, TP)

## Plot
survivalROC_data %>%
  ggplot(mapping = aes(x = FP, y = TP)) +
  geom_point() +
  geom_line() +
  geom_label(data = survivalROC_data %>% dplyr::select(t,auc) %>% unique,
             mapping = aes(label = sprintf("%.3f", auc)), x = 0.5, y = 0.5) +
  facet_wrap( ~ t) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.key = element_blank(),
        plot.title = element_text(hjust = 0.5),
        strip.background = element_blank())



#----------------------------------------------------------------------------------------------
#predictions and usage 

#create prediction for the modeling time period 
table(tenure_data$year)
#find the tenure days in y1 for prediction y2-y6 
tenure_data$y1 <- ifelse (tenure_data$tenure - (tenure_data$year-y1) *12 > 0,tenure_data$tenure - (tenure_data$year-y1) *12, 0)

#prediction month, largest value 204+60
month<- seq(0, 264, by = 12)

#calculate the survival probabilities: the time of the probability 
Prob_Surv <- predictSurvProb(tenure_cox,
                             newdata=tenure_data,
                             times=month)

Prob_Surv<- as.data.frame(Prob_Surv)
colnames(Prob_Surv)<- paste0("month_", month)

Prob_Surv$id <- tenure_data$id
Prob_Surv$tenurey1 <- tenure_data$tenurey1

#subset the 5 years probability after the modeling peroid 
Prob_Surv<- Prob_Surv %>% 
  mutate(start_col = floor(tenurey1 / 12) + 1)

#cuts function helps to search the starting point of survival curve
cuts<- function(x=Prob_churn, start_col){
  if (start_col <0){
    y<-  c(rep(0, abs(start_col) + 1), unlist( x[1: (5- abs(start_col))]))
  }
  else( y <- x[start_col: (start_col + 5)])
  y
}

#aa combines the survival probabilities 
aa<- data.frame()
for ( i in 1:nrow(Prob_Surv)){
  a<- cuts(Prob_Surv[i,1:length(month)], Prob_Surv$start_col[i]) 
  aa <- rbindlist(list(aa, a))
}

Surv_score <- aa 

#impute the missing by the largest value 
Surv_score<- t( apply(Surv_score[, 1:6 ], 1, function(x) { x[is.na(x)] <- 
  max(x, na.rm=TRUE)
return(x)}
))

Surv_score <- as.data.frame(Surv_score)
colnames(Surv_score) <- paste0("month_", seq(0, 60, by = 12))

#use the average prob as the probability of survive 
Surv_score<- Surv_score %>%
  mutate(prob_surv1 = (month_0 + month_12)/2) %>%
  mutate(prob_surv2 = (month_12 + month_24)/2) %>%
  mutate(prob_surv3 = (month_24 + month_36)/2) %>%
  mutate(prob_surv4 = (month_36 + month_48)/2) %>%
  mutate(prob_surv5 = (month_48 + month_60)/2) 
Surv_score$id = tenure_data$id

#true survive or leave 
cost_prem2 <- cost_prem %>% 
  mutate(Surv1 = as.numeric(!is.na(ltv_y1))) %>% 
  mutate(Surv2 = as.numeric(!is.na(ltv_y2))) %>% 
  mutate(Surv3 = as.numeric(!is.na(ltv_y3))) %>% 
  mutate(Surv4 = as.numeric(!is.na(ltv_y4))) %>% 
  mutate(Surv5 = as.numeric(!is.na(ltv_y5))) %>%
  select(c("id", "Surv1", "Surv2", "Surv3", "Surv4", "Surv5"))

Surv_score <- merge(Surv_score, cost_prem2, by = "id")

#check the roc grided by year 
Roc_dt<- simple_roc(Surv_score$Surv5, Surv_score$month_48)
plot(Roc_dt$FPR, Roc_dt$TPR, xlab = "FPR", ylab  = "TPR", type = "l")

#evaluate the predictions by projecting the probabilities to the 2012 to 2015 and compare the true surviving or leaving 
Surv_score_test <- merge(Surv_score, tenure_test[,
                                                 which(names(tenure_test) %in% c("id", "year"))], by = "id") 

auc(Surv_score_test$Surv5, Surv_score_test$prob_surv5)
plot(roc(Surv_score_test$Surv5, Surv_score_test$prob_surv5, direction="<"),
     col="blue", lwd=3, main="ROC for 2016 prediction" )
text(0, .8, "AUC: 0.84")

#----------------------------------------------------------------------------------------------
#predict LTV 
#load the predicted profit 
load( file = paste0(path_temp, "/scores", ".RData"))  


Surv_score$weight <- Surv_score$prob_surv1 + Surv_score$prob_surv2 + Surv_score$prob_surv3 + Surv_score$prob_surv4 + Surv_score$prob_surv5 

Surv_score<- merge(Surv_score, cp_score, by = "id")

Surv_score$LTV_hat <- Surv_score$profit * Surv_score$weight

#compare the ture and predicted by decile 
true_decile <- floor(rank(-Surv_score$LTV[!is.na(Surv_score$LTV_hat)]) / nrow(Surv_score[!is.na(Surv_score$LTV_hat), ]) *10) 
pred_decile <- floor(rank(-Surv_score$LTV_hat[!is.na(Surv_score$LTV_hat)]) / nrow(Surv_score[!is.na(Surv_score$LTV_hat), ]) *10) 
true_pred_table<- as.matrix(table(true_decile, pred_decile))[1:10, 1:10]/nrow(Surv_score[!is.na(Surv_score$LTV_hat), ])*10


#create heat map 
par(mar = c(1,1,1,1))
dev.off()

heatmap.2(true_pred_table,dendrogram='none', Rowv=FALSE, Colv=FALSE,trace='none',
          col=colorRampPalette(c("white","green","green4","blue"))(100),
          xlab = "true", ylab = "predicted", main = "Ture vs. Prediction" )

##save(Surv_score, tenure_cox,  file = paste0(path_temp, "/tenure_modelv03.RData"))


# comparison on the test data 
Surv_score_test_out <- Surv_score[Surv_score$id %in% tenure_test$id, ]
true_decile <- floor(rank(-Surv_score_test_out$LTV[!is.na(Surv_score_test_out$LTV_hat)]) / nrow(Surv_score_test_out[!is.na(Surv_score_test_out$LTV_hat), ]) *10) 
pred_decile <- floor(rank(-Surv_score_test_out$LTV_hat[!is.na(Surv_score_test_out$LTV_hat)]) / nrow(Surv_score_test_out[!is.na(Surv_score_test_out$LTV_hat), ]) *10) 
true_pred_table<- as.matrix(table(true_decile, pred_decile))[1:10, 1:10]/nrow(Surv_score_test_out[!is.na(Surv_score_test_out$LTV_hat), ])*10



par(mar = c(1,1,1,1))
dev.off()

heatmap.2(true_pred_table,dendrogram='none', Rowv=FALSE, Colv=FALSE,trace='none',
          col=colorRampPalette(c("white","green","green4","blue"))(100),
          xlab = "true", ylab = "predicted", main = "Ture vs. Prediction on Test Data" )

#----------------------------------------------------------------------------------------------
#Calibrate LTV estimation 
quantile (Surv_score$LTV[!is.na(Surv_score$LTV_hat)], seq(.1, .9, by = .1))
quantile (Surv_score$LTV_hat[!is.na(Surv_score$LTV_hat)], seq(.1, .9, by = .1))
plot(Surv_score_test_out$LTV, Surv_score_test_out$LTV_hat)

#by using linear regression to search for the calibration parameters 
x1<- Surv_score$LTV_hat[Surv_score$LTV_hat < 15000 & Surv_score$LTV > -50000  & Surv_score$LTV < 100000   ]
y1 <- Surv_score$LTV[Surv_score$LTV_hat < 15000 & Surv_score$LTV > -50000 & Surv_score$LTV < 100000]
plot(x1, y1)
lm1<- lm(y1~ x1)

x2<- Surv_score$LTV_hat[Surv_score$LTV_hat >= 15000 & Surv_score$LTV_hat <30000& Surv_score$LTV >-200000  ]
y2 <- Surv_score$LTV[Surv_score$LTV_hat >= 15000 & Surv_score$LTV_hat <30000 & Surv_score$LTV >-200000 ]
plot(x2, y2)
lm2<- lm(y2~ x2)

x3<- Surv_score$LTV_hat[Surv_score$LTV_hat >=30000 & Surv_score$LTV >-400000 
                        & Surv_score$LTV_hat < 150000]
y3 <- Surv_score$LTV[Surv_score$LTV_hat >= 30000  & Surv_score$LTV >-400000
                     & Surv_score$LTV_hat < 150000]

Surv_score$LTV_hat2<- ifelse(Surv_score$LTV_hat < 15000, -5000 + Surv_score$LTV_hat * .8, 
                             ifelse(Surv_score$LTV_hat < 30000, -2000 + Surv_score$LTV_hat * .7, 
                                    -6000 + Surv_score$LTV_hat * .9))

quantile(Surv_score$LTV_hat[!is.na(Surv_score$LTV_hat)], seq(.1, .9, by = .1))
quantile(Surv_score$LTV_hat2[!is.na(Surv_score$LTV_hat2)], seq(.1, .9, by = .1))
mean(Surv_score$LTV_hat2, na.rm =  TRUE)
quantile(Surv_score$LTV, seq(.1, .9, by = .1))


saveRDS(Surv_score,  file = paste0(path_temp, "/Surv_score_eoy.rds"))


Score_LTV[is.na(Score_LTV$profit), ]<- "."

Score_LTV <- Surv_score %>%
  select(one_of("id","profit",  "LTV", "LTV_hat", "LTV_hat2"))


write.csv(Score_LTV,  file = paste0(path_temp, "/Score_LTV.csv"))

save(tenure_train, tenure_test, tenure_cox, file = paste0(path_temp, "/model_tenure_surv.RData") )


