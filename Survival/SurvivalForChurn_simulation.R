
library(survival)
library(KMsurv) 
#############################################################
#############################################################
#time invariate covariates examples

#simulate survival data
ID<-seq (1:1000)
#0 for "F" and 1 for "M"
gender <-sample(c(0, 1),1000, replace = TRUE)
customer_since<-sample(seq(as.Date("2010/1/1"), as.Date("2016/1/1"), "day"), 1000, replace = TRUE)
#female has higher probability of attrition: female: 0.65; male: 0.5
churn =((gender == 0)*rbinom(10, 1, .3)| rbinom(1000, 1, .4))
#time to event
Time2Event = sample(seq(20:1000), 1000, replace = TRUE)
#customer age: young customer did not churn
age_init = rnorm(1000, 40, 10)
age = numeric()
for (i in 1:length(age_init))
{if (churn[i]  == "FALSE")
  age[i]= max(age_init[i]-5, 18) 
  else age[i] = age_init[i]}

surv_eg = data.frame(ID, gender, age, customer_since, churn, Time2Event)


eg_fit <- survfit(Surv(surv_eg$Time2Event, surv_eg$churn)~ 1, conf.type="none")
sum.surv <-summary(eg_fit)
surv.out.df <- data.frame("Time" =eg_fit[[2]],
                          "Cust-Available"=eg_fit[[3]],
                          "Cust-Attrited"=eg_fit[[4]],
                          "Survival-Rate"=eg_fit[[6]])
plot(eg_fit,
     xlab="Days since start",
     ylab="Survival Rate",
     main="Survival Rate at different time point")


eg_w_gender <- survfit(Surv(surv_eg$Time2Event, surv_eg$churn)~ surv_eg$gender, 
                       conf.type="none")
plot(eg_w_gender ,
     xlab="Subscription Days",
     ylab="Retention Probability",
     main="Retention Probability for Different Segments",
     lwd=2,
     col=c("blue","green"))
legend(30, .7, c("Segment 1", "Segment 2"),lwd=2,col=c("blue","green"))
abline(h=0.3, v=c(840, 930), lty =2, col = "red", lwd= 2 )
axis(1, at = c(840, 930))
 
text(300, .2, "Prefined cut off for churn: 0.3")
 
plot(eg_w_gender, fun="cumhaz", 
     xlab="days", ylab="Cumulative Hazard", main="Hazard Rate",
     lwd=2, col=c("red","green")) 
legend(30, 3, c("Group 1", "Group 2"),lwd=2,col=c("red","green"))

gender.surv <- survdiff(Surv(surv_eg$Time2Event, surv_eg$churn)~ surv_eg$gender)
gender.surv


##############################################################
##############################################################
#Cox regression model example

eg_fit_cox<- coxph(Surv(Time2Event, churn)~age,
                      data=surv_eg)
summary(eg_fit_cox)


#fit the base line of the hazard
base.hazard <- survfit(Surv(Time2Event, churn)~1,
                       data=surv_eg)


eg_cox_str <- coxph(Surv(Time2Event, churn)~(age)*strata(gender),
                      data=surv_eg)
summary(eg_cox_str)

eg_cox_gen <- coxph(Surv(Time2Event, churn)~age+gender,
                    data=surv_eg)
summary(eg_cox_gen)

eg_pred <- predict(eg_cox_gen,
                   type="lp",
                   data=surv_eg)

#survival plot for a certan customer
data_sample_cox1= data.frame(
  age =37, gender = 0 )
data_sample_cox2= data.frame(
  age =37*1.3, gender = 0 )


plot(survfit(eg_cox_gen, newdata = data_sample_cox1, type = "aalen"), 
     conf.int ="none", ylab = "Retention Probabiliy", xlab = " Days", 
     main = "Retention Plots of a Customer ",  
     col = "blue", lwd =2) 
par(new =TRUE)
plot(survfit(eg_cox_gen, newdata = data_sample_cox2, type = "aalen"), 
     conf.int ="none", ylab = "Retention Probabiliy", xlab = " Days",
     col = "green", lwd =2) 
legend(30, 0.4, c("Last Purchase Increase by 30%",
                 "Base Retention Rate"),
       lwd=2,col=c("blue","green"))
abline( v=c(600), lty =2, col = "red", lwd= 2 )
#abline(h=c(.44, .57), v=c(600), lty =2, col = "red", lwd= 2 )



#calculate the predicted hazard for each 
#Baseline Function
base <- basehaz(eg_cox_gen)
# Base value H0 = 0.3649053 at 494 Days
# Predicted Value at time 494 days
Pred.val <- 0.3649053*exp(eg_pred)

Pred.target <- ifelse(Pred.val>0.43,1,0)
target <- ifelse(surv_eg$Time2Event <=494,
                 0,
                 surv_eg$attrition)
table(Pred.target, target)

##############################################################
##############################################################
#time variate covariates example
#create a churn data
data(heart)
start_date<-heart$start
end_date<-heart$stop
churn<-heart$event
age<-heart$age
attr1<-heart$age * log(end_date) #attr1 is time dependent
# attr1 = ifelse(end_date > 1000, 1, 0)
attr2<-heart$year 
offer1<-heart$surgery
offer2<-heart$transplant
id<-heart$id
surv_eg2<-data.frame(start_date, end_date,age, attr1, attr2, offer1, offer2, churn, id)

fit2<-coxph(Surv(start_date, end_date, churn)~attr1+attr2+offer1+offer2, data = surv_eg2)
pred2<-predict(fit2, type="risk", se.fit2=TRUE)
#base hazard
base <- basehaz(fit2)
plot(base[,2], base[,1] , xlab="days", ylab = "hazard", type = "l", ylim = c(0, 2.5))
#if attri1 increase by 10%, the hazard function
test_attri1 = rep(2, nrow(surv_eg2))
points(base[,2],base[,1]*1.1*1.0275,  type = "l", col = 2)

#survival plot
#treat it as time invariate variables
data_sample= data.frame(
  offer1 =1, offer2 = 0, attr1=-2.484027,
  attr2 =3.453289 )
data_base  = data.frame(
  offer1 =0, offer2 = 0, attr1=-2.484027,
  attr2 =3.453289 )
plot(survfit(fit2, newdata = data_sample, type = "aalen"), 
     conf.int ="none", main ="retiention probability for a customer", ylab = "Survivorship", xlab = "Day", bty="L", col = "green") 
lines(survfit(fit2, newdata = data_base, type = "aalen"), 
     conf.int ="none", ylab = "Survivorship", xlab = "Day", bty="L", col = "blue") 
legend(1200, .8, c("offer-yes", "offer-no"),lwd=2,col=c("blue","green"))


#time  variable case
last = surv_eg2$id[which.max(surv_eg2$end_date)]
intervals<-surv_eg2[surv_eg2$id == last, c("start_date", "end_date", "churn")]
data_sample2= data.frame(
  offer1 =1,  age=2,
  attr2 =3.453289, intervals)
data_sample2$attr1 = data_sample2$age*log(data_sample2$end_date)
plot(survfit(fit2, newdata = data_sample2, individual  =TRUE),  ylab = "Survivorship", xlab = "Day", bty="L") 













