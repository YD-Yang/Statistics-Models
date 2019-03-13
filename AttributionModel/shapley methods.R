
#-------------------------------------------------------
#Functionality
#-------------------------------------------------------

#-------------------------------------------------------
#Clean workspace
#-------------------------------------------------------
rm(list=ls())
#-------------------------------------------------------
#Libraries
library(plyr)
library(dplyr)
library(tidyr)
library(reshape2)
library(caret)
library(foreach)
library(relimp)
library(GameTheory)
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
df3 <- data.frame(client_id = sample(c(1:1000), 5000, replace = TRUE),
                  date = sample(c(1:32), 5000, replace = TRUE),
                  channel = sample(c(0:4), 5000, replace = TRUE,
                                   prob = c( 0.17, 0.28, 0.15, 0.13, 0.27)))
df3$date <- as.Date(df3$date, origin = "2015-01-01")
df3$channel <- paste0('channel_', df3$channel)

# count the occurent and dummy coding 
res <- df3 %>% group_by(client_id,channel) %>% summarise(Freq=n())
res<-as.data.frame(res)
res2 <- dcast(res, client_id  ~ channel, value.var="Freq")

res2[is.na(res2)]<- 0
res2[res2 >0] <-1

count_table<-  plyr::count(res2, vars = c("channel_0", "channel_1", "channel_2", "channel_3", "channel_4"))


row_names<- apply(count_table[, 1:5], 1, function(x) as.numeric(paste0(which(x== 1), collapse = '') ) ) 

set_comb <- cbind(row_names, count_table$freq)
set_comb <- set_comb[order(set_comb[, 1]),]

COALITIONS <- set_comb[,2]
df3_game<-DefineGame(5,COALITIONS)
summary(df3_game)

df3_SHAPLEY<-ShapleyValue(df3_game)
summary(df3_SHAPLEY)

#####################################################################################################
#####################################################################################################
weights<-c(0.0136277355179151,	0.00212494967927264,	0.0294512701930129,	0.00795993694412995,
           0.0318937077161301,	0.0405248098736556,	0.00784991610616196,	0.0237342014543627,	
           0.0322380004169638,	0.0177390167782103,	0.0126822420869258,	0.0449701016405128,
           0.00149748426176493,	0.0521789769901948,	0.0197845276347997,	0.00446263594003767,
           0.048257892026953,	0.0172393894176625,	0.0575629197824669,	0.0408496266512291,	
           0.0704616143330214,	0.0135529095000945,	0.0918566056257537,	0.0487825724676561,
           0.00836912106648745,	0.0269723576753016,	0.0182596859442351,	0.0303698364348013,	
           0.0551192690597434,	0.0319441480023317,	0.0976825387782114)


freq2<- round(weights *1000)

count_table2<-cbind(count_table, freq2)

row_names2<- apply(count_table2[, 1:5], 1, function(x) as.numeric(paste0(which(x== 1), collapse = '') ) ) 

set_comb2 <- cbind(row_names2, count_table2$freq2)
set_comb2 <- set_comb2[order(set_comb2[, 1]),]

COALITIONS2 <- set_comb2[,2]
df3_game2<-DefineGame(5,COALITIONS2)
summary(df3_game2)

df3_SHAPLEY2<-ShapleyValue(df3_game2,  Names = c(sort(unique(df3$channel))))
summary(df3_SHAPLEY2)

share <- df3_SHAPLEY2$SV/sum(df3_SHAPLEY2$SV)

share_table<- ( count_table2[, 1:5]*share/rowSums(count_table2[, 1:5]*share)) * count_table2$freq2

share_channel<- apply(share_table, 2, sum)
  
  
  
  
