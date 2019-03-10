library(plyr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(ggthemes)
library(ggrepel)
library(RColorBrewer)
library(ChannelAttribution)
library(markovchain)


library(GameTheory)
library(caret)



#detach("package:plyr", unload=TRUE)

attrition<-function(df, method, bagged = "True", samp_rate = 0.5,
                    var_rate = 0.5 ){
  
  if(method == 'heuristic'){
    mod <- heuristic_models(df, var_path = 'path', var_conv = 'conversion')
    mod <- mod[order(mod$channel_name),]
    
  }
  if (method == "MC"){
    # calculating the models (Markov and heuristics)
    mod <- markov_model(df,
                        var_path = 'path',
                        var_conv = 'conversion',
                        var_null = 'conv_null',
                        out_more = TRUE)
  }
  
  if (method == "Rule"){
    #the data is same as the shapley data
    mod<-data_rule(df)
    
  }
  
  else if (method == 'Shapley'){
    # count the occurent and dummy coding 
    uID <- df[df$conversion == 1, 1]
    df2<-merge(x = uID, y = df, by='client_id', all.x = TRUE           )
    df2 <- df2 %>% group_by(client_id,channel) %>% summarise(Freq=n())
    df2<-as.data.frame(df2)
    df2 <- dcast(df2, client_id  ~ channel, value.var="Freq")
    
    df2[is.na(df2)]<- 0
    df2[df2 >  0] <- 1
    n_channel <- length(unique(df$channel))
    
    #create the combination of channels 
    count_table<- plyr::count(df2, vars = sort(unique(df$channel)))
    
    #sort the combination table by channel layout and format for the shapley methods
    row_names<- apply(count_table[, 1:n_channel], 1, function(x) as.numeric(paste0(which(x== 1), collapse = '') ) ) 
    set_comb <- cbind(row_names, count_table$freq)
    set_comb <- set_comb[order(set_comb[, 1]),]
    
    COALITIONS <- set_comb[,2]
    df_game<-DefineGame(n_channel,COALITIONS)
    df_SHAPLEY<-ShapleyValue(df_game, Names = c(sort(unique(df$channel))))
    df_SHAPLEY$SV[df_SHAPLEY$SV <0] <- 0.01
    
    share <- df_SHAPLEY$SV/sum(df_SHAPLEY$SV)
    share_table<- ( count_table[, 1:5]*share/rowSums(count_table[, 1:5]*share)) * count_table$freq
    share_channel<- apply(share_table, 2, sum)
    mod <-list(games = df_game, shapley = df_SHAPLEY, share_channel= share_channel)
    
  }
  
  else if (method == 'regression'){
    f<- as.formula(paste("conv_value ~", paste(names(df[, -1]), collapse = " + ")))
    reg <-lm(f, data = df)
    SSR0 <-deviance(reg) 
    PREs<-numeric()
    
    for ( i in 2:(ncol(df))){
      f<- as.formula(paste("conv_value ~", paste(names(df[1, -c(1, i)]), collapse = " + ")))
      reg_reduced <-lm(f, data = df)
    #  PREs[i-1]<- (deviance(reg_reduced)- SSR0)/deviance(reg_reduced)*100
      PREs[i-1]<- (deviance(reg_reduced)- SSR0)/SSR0*100
    }
    importance <- varImp(reg)
    scale_deviance <- PREs/sum(PREs)
    scale_imp <- importance/sum(importance)
    deviance_alloc <-  sum(df_reg$conv_value) * scale_deviance
    imp_alloc<-  sum(df_reg$conv_value) * scale_imp
    mod = list(reduced_deviance = PREs, reg= reg, importance =importance, 
               deviance_alloc = deviance_alloc, imp_alloc = imp_alloc  )  
    
  }
  
  else if (method == 'logistic'){
    
    if (bagged == "False"){
      f<- as.formula(paste("conversion ~", paste(names(df[, -1]), collapse = " + ")))
      
      #prediction check
      lgt <-glm(f, data = df)
      pred_glm <- ifelse( predict.glm(lgt, df, tyep = "response") > 0.5, 1, 0)
      importance<- varImp(lgt)
      
      SSR0 <-deviance(lgt) 
      PREs<-numeric()
      
      for ( i in 2:(ncol(df))){
        f<- as.formula(paste("conversion ~", paste(names(df[1, -c(1, i)]), collapse = " + ")))
        reg_reduced <-glm(f, data = df)
        PREs[i-1]<- (deviance(reg_reduced)- SSR0)*100
      }
      
      
      mod = list(model.fit = lgt, importance = importance, reduced_deviance = PREs)
      
    }
    
    else if(bagged == "True"){
      
      training  <- df
      iterations <- 100
      name_ind <-names(training)
      output <-numeric()
      
      set.seed(123)
      for(m in 1:iterations){
        training_positions <- sample(nrow(training), 
                                     size=floor(nrow(training)*samp_rate))
        training_var <- sample((2:ncol(training)), 
                               size=floor(ncol(training)*var_rate))
        #  train_pos<-1:nrow(training) %in% training_positions
        glm_fit <- glm(conversion ~ . ,
                       data=training[training_positions, c(1,training_var)],
                       family=binomial(logit))
        s<-summary(glm_fit)
        vars <- varImp(glm_fit)
        output<-rbind(output, cbind(rep(m, length(training_var)), name_ind[training_var],  s$coefficients[-1, 1], vars ))
        
      }
      
      out<-data.frame(output)
      names(out)<-c("iteration", "channel", "coeff", "imp")
      est_table<- aggregate(out[, 3:4], list(out$channel), mean)
      est_table<- est_table[order(as.character(est_table$Group.1)),]
      mod = list(bagged= est_table)
    }
    }
    
  return(mod)
}

#-------------------------------------------------------
#create data files 

#seperate the path by conversion
data_pathN<-function(data_in, path_N){
  df_paths_i <- data_in %>%
    filter(path_no == path_N ) %>%
    select(-path_no)
  return(df_paths_i)
}


data_path<-function(data_pathn){
  df_p <- data_pathn %>%
    arrange(client_id, date) %>%
    group_by(client_id) %>%
    summarise(path = paste(channel, collapse = ' > '),
              # assume that all paths were finished with conversion
              conversion = sum(conversion),
              conv_value = sum(conv_value)) %>%
    ungroup()
  df_p$conversion[df_p$conversion > 1] <- 1
  return(df_p)
  
}


data_rule <-function(data_in = df_shapley){
  #first and last 
  df_rule<- data_in %>%
    group_by(client_id) %>%
    arrange(date) %>%
    filter(row_number()==1 | row_number()==n()
    )
  
  #df_test <- df_rule %>% group_by(client_id) %>% summarise(conv_ttl = sum(conversion))
  
  rule_convs<-as.data.frame(aggregate(data_in$conversion, by=list(client_id=data_in$client_id), FUN=sum))
  df_uniq <- as.data.frame(table(df_rule$client_id))
  
  
  df_rule<-merge(df_rule, df_uniq, by.x = 'client_id', by.y ='Var1') 
  df_rule<-merge(df_rule, rule_convs, by='client_id')
  
  
  df_rule$weight <- 1/df_rule$Freq*df_rule$x
  
  rule_FL<-as.data.frame(aggregate(df_rule$weight, by=list(channel_name=df_rule$channel), FUN=sum))
  
  names(rule_FL)[2]<- c('position')
  
  #decay 
  df_rule2<- ddply(data_in, .(client_id), mutate, id = seq_along(date))
  
  rule_decay<-as.data.frame(aggregate(df_rule2$id, by=list(client_id=df_rule2$client_id), FUN=sum))
  
  df_rule2<-merge(df_rule2, rule_convs, by='client_id')
  df_rule2<-merge(df_rule2, rule_decay, by='client_id')
  
  df_rule2$weight <- df_rule2$id *df_rule2$x.x/df_rule2$x.y
  
  decay<-as.data.frame(aggregate(df_rule2$weight, by=list(channel_name=df_rule2$channel), FUN=sum))
  
  names(decay)[2]<- c('decay')
  
  rule_based<-merge(rule_FL, decay, by = 'channel_name')
  
  
  return(rule_based)
}


data_mc <- function(data_in= df_paths){
  N <- max(data_in$path_no)
  df_path_ttl <- data.frame()
  
  for (i in 1:N){
    temp <- data_pathN(data_in, i)
    path_i <- data_path(temp)
    df_path_ttl<-rbind(df_path_ttl, path_i)
  }
  
  df_path_ttl$conv_null <- 1-df_path_ttl$conversion
  return(df_path_ttl)
}

data_stage_mc<-function(data_in = df_paths, i){
  temp <- data_pathN(data_in, i)
  path_i <- data_path(temp)
  path_i$conv_null <- 1- path_i$conversion
  return(path_i)
}


data_shapley <-function(data_in = df_paths){
  N <- max(data_in$path_no)
  
  df_path_ttl <- data.frame()
  for (i in 1:N){
    temp <- data_pathN(data_in, i)
    temp$client_id <-paste0(temp$client_id, "-", i)
    df_path_ttl<-rbind(df_path_ttl, temp)
  }
  return(df_path_ttl)
}

data_stage_shapley<-function(data_in = df_paths, i){
  temp <- data_pathN(data_in, i)
  temp$client_id <-paste0(temp$client_id, "-", i)
  return(temp)
}


data_reg<-function(data_in = df_paths){
  N <- max(data_in$path_no)
  
  df_path_ttl <- data.frame()
  
  for (i in 1:N){
    temp <- data_pathN(data_in, i)
    df_path_ttl<-rbind(df_path_ttl, temp)
  }
  
  
  res <- df_path_ttl %>% group_by(client_id,channel) %>% summarise(Freq=n())
  res<-as.data.frame(res)
  res <- dcast(res, client_id  ~ channel, value.var="Freq")
  res[is.na(res)]<- 0
  values <- df_path_ttl %>% group_by(client_id ) %>% summarise(conv_value = sum(conv_value))
  data_reg <-merge(values,res,  by = "client_id")
  return(data_reg)  
}


data_stage_reg<-function(data_in = df_paths, i){

    temp <- data_pathN(data_in, i)
 
  res <- temp %>% group_by(client_id,channel) %>% summarise(Freq=n())
  res<-as.data.frame(res)
  res <- dcast(res, client_id  ~ channel, value.var="Freq")
  res[is.na(res)]<- 0
  values <- temp %>% group_by(client_id ) %>% summarise(conv_value = sum(conv_value))
  data_reg <-merge(values,res,  by = "client_id")
  return(data_reg)  
}




data_lgt_comb<-function(data_in=pathI){
  res <- data_in %>% group_by(client_id,channel) %>% summarise(Freq=n())
  res<-as.data.frame(res)
  res <- dcast(res, client_id  ~ channel, value.var="Freq")
  res[is.na(res)]<- 0
  values <- data_in %>% group_by(client_id ) %>% summarise(conversion= sum(conversion))
  data_lgt <-merge(values, res,  by = "client_id")
  data_lgt$conversion[data_lgt$conversion > 1] <- 1
  return(data_lgt)
}



data_lgt<-function(data_in = df_paths){
  N <- max(data_in$path_no)
  df_lgt <- data.frame()
  
  for (i in 1:N){
    temp <- data_pathN(data_in, i)
    lgt_temp <- data_lgt_comb(temp)
    df_lgt<-plyr::rbind.fill(df_lgt, lgt_temp)
  }
  
  df_lgt[is.na(df_lgt)] <- 0
  
  
  return(df_lgt)
  
}


data_stage_lgt<-function(data_in = df_paths, i){
    temp <- data_pathN(data_in, i)
    lgt_temp <- data_lgt_comb(temp)
    lgt_temp[is.na(lgt_temp)] <- 0
    
  return(lgt_temp)
}

#-------------------------------------------------------
#-------------------------------------------------------
#-------------------------------------------------------
#simulate data 
#-------------------------------------------------------
#random sample
set.seed(123)
df_raw <- data.frame(client_id = paste0('id', sample(c(1:20000), replace = TRUE)),
                     date = as.Date(rbeta(80000, 0.7, 10) * 100, origin = "2016-01-01"), 
                     conv_value = runif( 80000),
                     channel = paste0('channel_', sample(c(1:5), 80000, replace = TRUE,
                     prob = c( 0.17, 0.28, 0.15, 0.25, 0.15))) ) %>%
  group_by(client_id) %>%
  dplyr::mutate(conversion = sample(c(0, 1), n(), prob = c(0.75, 0.25), replace = TRUE)) %>%
  ungroup() %>%
  arrange(client_id, date)

#create cumulative conversion 
df_paths <- df_raw %>%
  group_by(client_id) %>%
  dplyr::mutate(path_no = ifelse(is.na(lag(cumsum(conversion))), 0, lag(cumsum(conversion))) + 1) %>%
  ungroup()


df_path_dup <- df_paths
#-------------------------------------------------------
#sample markov chain

set.seed(123)
#define transition matrix: trans[i, j]: transition from state i to state j
trans_raw <-matrix(runif(25), nrow = 5)
trans<-trans_raw/rowSums(trans_raw)

#define the marginal probability of each channel: the probability of j 
p_state <- colSums(trans_raw)/sum(trans_raw)

df_trans <-df_raw[order(df_raw$client_id, df_raw$date), ]
df_trans$channel <- 0

df_trans$first<-as.numeric(!duplicated(df_trans$client_id))

for (i in 1:nrow(df_trans)){
  if(df_trans$first[i]==1){
    df_trans$channel[i] <- sample(1:5, 1, prob=p_state)
    
  }
  else{
    df_trans$channel[i] <- sample(1:5,1, prob = trans[df_trans$channel[i-1], ] )
  }
}

df_trans$channel <- paste0('channel_', df_trans$channel)



#create cumulative conversion 
df_paths2 <- df_trans %>%
  group_by(client_id) %>%
  dplyr::mutate(path_no = ifelse(is.na(lag(cumsum(conversion))), 0, lag(cumsum(conversion))) + 1) %>%
  ungroup()

df_path_mc <- df_paths2


#add some random into paths to make path different 
#set.seed(123)
#rpath <- sample(c(-2: 3), 80000,replace = TRUE, prob = c(.2, .15, .3, .15, .1, .1))
#df_paths$path_no <- df_paths$path_no + rpath

#df_paths$path_no[df_paths$path_no <1] <- 1


#-------------------------------------------------------
#-------------------------------------------------------
#-------------------------------------------------------
#run models 

df_paths <- df_path_dup

df_mc <- data_mc(df_paths)
#heuristic
h_mod<- attrition(df_mc, method='heuristic')


#Rule based
df_shapley<-data_shapley(df_paths)
rule_mod<- attrition(df_shapley, method='Rule')

rules <- merge(h_mod, rule_mod, by = 'channel_name')

h_mod_ = melt(rules, id.vars=c("channel_name"))
ggplot(h_mod_, aes(channel_name, value, fill=variable)) +
  geom_bar(stat='Identity',position=position_dodge()) + 
  ggtitle("Rule Based Model")





#MC_mod
df_mc <- data_mc(df_paths)

MC_mod<- attrition(df_mc, method='MC')
colnames(MC_mod$result)[2]<- c('MC')

MC_mod_ = melt(MC_mod$result, id.vars=c("channel_name"))
ggplot(MC_mod_, aes(channel_name, value, fill=variable)) +
  geom_bar(stat='Identity',position=position_dodge()) + 
  ggtitle("Markov Chain Model")




df_plot_trans <- MC_mod$transition_matrix

cols <- c("#e7f0fa", "#c9e2f6", "#95cbee", "#0099dc", "#4ab04a", "#ffd73e", "#eec73a",
          "#e29421", "#e29421", "#f05336", "#ce472e")
t <- max(df_plot_trans$transition_probability)

ggplot(df_plot_trans, aes(y = channel_from, x = channel_to, fill = transition_probability)) +
  theme_minimal() +
  geom_tile(colour = "white", width = .9, height = .9) +
  scale_fill_gradientn(colours = cols, limits = c(0, t),
                       breaks = seq(0, t, by = t/4),
                       labels = c("0", round(t/4*1, 2), round(t/4*2, 2), round(t/4*3, 2), round(t/4*4, 2)),
                       guide = guide_colourbar(ticks = T, nbin = 50, barheight = .5, label = T, barwidth = 10)) +
  geom_text(aes(label = round(transition_probability, 2)), fontface = "bold", size = 4) +
  theme(legend.position = 'bottom',
        legend.direction = "horizontal",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 20, face = "bold", vjust = 2, color = 'black', lineheight = 0.8),
        axis.title.x = element_text(size = 24, face = "bold"),
        axis.title.y = element_text(size = 24, face = "bold"),
        axis.text.y = element_text(size = 8, face = "bold", color = 'black'),
        axis.text.x = element_text(size = 8, angle = 90, hjust = 0.5, vjust = 0.5, face = "plain")) +
  ggtitle("Transition matrix heatmap")


#Rule based
df_shapley<-data_shapley(df_paths)
rule_mod<- attrition(df_shapley, method='Rule')


#shapley value
df_shapley<-data_shapley(df_paths)
shapley_mod<- attrition(df_shapley, method='Shapley')


shapley_allc<-data.frame(shapley_mod$share_channel)

  
shapley_allc<-data.frame(  channel_name = row.names(shapley_allc), Shapley =shapley_allc$shapley_mod.share_channel,
                           row.names = NULL)

shapley_allc_ = melt(shapley_allc, id.vars=c("channel_name"))
ggplot(shapley_allc_, aes(channel_name, value, fill=variable)) +
  geom_bar(stat='Identity',position=position_dodge()) + 
  ggtitle("Shapeley Method Allocation")




#regression
df_reg<- data_reg(df_paths)[, -1]
reg_mod<- attrition(df_reg, method='regression')


reg_allc<-data.frame(channel_name = row.names(reg_mod$imp_alloc), Marginal = reg_mod$deviance_alloc,
                    Importance = reg_mod$imp_alloc, row.names = NULL)
colnames(reg_allc)<-c("channel_name",'Marginal', 'Importance')
reg_allc_ = melt(reg_allc, id.vars=c("channel_name"))
ggplot(reg_allc_, aes(channel_name, value, fill=variable)) +
  geom_bar(stat='Identity',position=position_dodge()) + 
  ggtitle("Regression for sale attribution")





#logistic regresion

df_lgt<- data_lgt(data_in = df_paths)

lgt_mod_b<- attrition(df_lgt[, -1], method='logistic', bagged = "True" )
lgt_mod<- attrition(df_lgt[, -1], method='logistic', bagged = "False" )


lgt_allc<- data.frame(channel_name = row.names(lgt_mod$importance),
                           Logit_basic = lgt_mod$importance/sum(lgt_mod$importance) *  sum(df_lgt$conversion),
                           Marginal =lgt_mod$reduced_deviance/sum(lgt_mod$reduced_deviance)* sum(df_lgt$conversion),
                           Bagged = lgt_mod_b$bagged$imp/sum(lgt_mod_b$bagged$imp)* sum(df_lgt$conversion), 
                           row.names = NULL)
colnames(lgt_allc)<-c("channel_name",'Logit_basic', 'Marginal', 'Bagged')

lgt_allc_ = melt(lgt_allc, id.vars=c("channel_name"))
ggplot(lgt_allc_, aes(channel_name, value, fill=variable)) +
  geom_bar(stat='Identity',position=position_dodge()) + 
  ggtitle("logistic regression for conversion attribution")

#-------------------------------------------------------
#compare different approaches 
comb_allc<-merge(rules, shapley_allc, by = 'channel_name')

comb_allc_ = melt(comb_allc, id.vars=c("channel_name"))
ggplot(comb_allc_, aes(channel_name, value, fill=variable)) +
  geom_bar(stat='Identity',position=position_dodge()) + 
  ggtitle(" Attribution Based on Conversions")


comb_allc2 <- merge(MC_mod$result, lgt_allc, by = 'channel_name')

comb_allc2_ = melt(comb_allc2, id.vars=c("channel_name"))
ggplot(comb_allc2_, aes(channel_name, value, fill=variable)) +
  geom_bar(stat='Identity',position=position_dodge()) + 
  ggtitle(" Attribution Based on Conversions & Non-Conversions")

  

#-------------------------------------------------------
#-------------------------------------------------------
#-------------------------------------------------------
# Compare the path differences 


#-------------------------------------------------------
# heristic 
h_mod_stage <-list()
N <- max(df_paths$path_no)
for (i in 1:N){
  stageI <-data_stage_mc(df_paths, i)
  h_mod_stage[[i]] <- attrition(stageI, method='heuristic')
}

stages <- 5

comb_h <- h_mod_stage[[1]]
colnames(comb_h)[-1]<-paste0(colnames(comb_h)[-1],'_', 1)

for (j in 2: (stages)){
  temp <- h_mod_stage[[j]]
  colnames(temp)[-1]<-paste0(colnames(temp)[-1],'_', j)
  comb_h <- merge(comb_h, temp, by = 'channel_name')
}

comb_h_ = melt(comb_h, id.vars=c("channel_name"))
ggplot(comb_h_, aes(channel_name, value, fill=variable)) +
  geom_bar(stat='Identity',position=position_dodge()) + 
  ggtitle("Rule Based Model with Stages")



#-------------------------------------------------------
# MC 
MC_mod_stage <-list()
stages <- 5

N <- max(df_paths$path_no)
for (i in 1:stages){
  stageI <-data_stage_mc(df_paths, i)
  MC_mod_stage[[i]] <- attrition(stageI, method='MC')
  colnames(MC_mod_stage[[i]]$result)[2]<- c('MC')
  
}


comb_MC<- MC_mod_stage[[1]]$result
colnames(comb_MC)[-1]<-paste0(colnames(comb_MC)[-1],'_', 1)

for (j in 2: (stages)){
  temp <- MC_mod_stage[[j]]$result
  colnames(temp)[-1]<-paste0(colnames(temp)[-1],'_', j)
  comb_MC <- merge(comb_MC, temp, by = 'channel_name')
}

MC_mod_ = melt(comb_MC, id.vars=c("channel_name"))
ggplot(MC_mod_, aes(channel_name, value, fill=variable)) +
  geom_bar(stat='Identity',position=position_dodge()) + 
  ggtitle("Markov Chain Model with Stages")



#-------------------------------------------------------
# Shapley

Shapley_mod_stage <-list()
N <- max(df_paths$path_no)

stages <- 5

for (i in 1:stages){
  stageI <-data_stage_shapley(df_paths, i)
  Shapley_mod_stage[[i]] <- attrition(stageI, method='Shapley')
}




shapley_allc<-data.frame(Shapley_mod_stage[[1]]$share_channel)
shapley_allc<-data.frame(  channel_name = row.names(shapley_allc), 
                           Shapley = shapley_allc[1],
                           row.names = NULL)
comb_Shapley<- shapley_allc
colnames(comb_Shapley)[-1]<-paste0('Shapley','_', 1)

for (j in 2: (stages)){
  shapley_allc<-data.frame(Shapley_mod_stage[[j]]$share_channel)
  shapley_allc<-data.frame(  channel_name = row.names(shapley_allc), 
                             Shapley = shapley_allc[1],
                             row.names = NULL)
  colnames(shapley_allc)[-1]<-paste0('Shapley','_', j)
  comb_Shapley <- merge(comb_Shapley, shapley_allc, by = 'channel_name')
}


shapley_allc_ = melt(comb_Shapley, id.vars=c("channel_name"))
ggplot(shapley_allc_, aes(channel_name, value, fill=variable)) +
  geom_bar(stat='Identity',position=position_dodge()) + 
  ggtitle("Shapley Method Allocation")


#-------------------------------------------------------
#regression

reg_mod_stage <-list()
N <- max(df_paths$path_no)

stages <- 5

for (i in 1:stages){
  stageI <-data_stage_reg(df_paths, i)
  reg_mod_stage[[i]] <-  attrition(stageI[, -1], method='regression')
}


reg_allc<-data.frame(channel_name = row.names(reg_mod_stage[[1]]$imp_alloc),
                     Marginal =  reg_mod_stage[[1]]$deviance_alloc,
                     Importance =  reg_mod_stage[[1]]$imp_alloc, row.names = NULL)
colnames(reg_allc)<-c("channel_name",'Marginal_1', 'Importance_1')

comb_reg<-reg_allc

for (j in 2: (stages)){
  reg_allc<-data.frame(channel_name = row.names(reg_mod_stage[[j]]$imp_alloc),
                       Marginal =  reg_mod_stage[[j]]$deviance_alloc,
                       Importance =  reg_mod_stage[[j]]$imp_alloc, row.names = NULL)
  colnames(reg_allc)<-c("channel_name",'Marginal', 'Importance')
  
  colnames(reg_allc)[-1]<-paste0(colnames(reg_allc)[-1],'_', j)
  comb_reg <- merge(comb_reg, reg_allc, by = 'channel_name')
}



reg_allc_ = melt(comb_reg, id.vars=c("channel_name"))
ggplot(reg_allc_, aes(channel_name, value, fill=variable)) +
  geom_bar(stat='Identity',position=position_dodge()) + 
  ggtitle("Regression Attribution with Stages")


#-------------------------------------------------------
#logistic regression

lgt_mod_stageB <-list()
lgt_mod_stage <-list()
N <- max(df_paths$path_no)

stages <- 5



stageI <-data_stage_lgt(df_paths, 1)
lgt_mod_stageB[[1]] <-  attrition(stageI[, -1], method='logistic', bagged = "True")
lgt_mod_stage[[1]] <-  attrition(stageI[, -1], method='logistic', bagged = "False")
lgt_allc<- data.frame(channel_name = row.names(lgt_mod_stage[[1]]$importance),
                       Logit_basic = lgt_mod_stage[[1]]$importance/sum(lgt_mod_stage[[1]]$importance) *  sum(stageI$conversion),
                       Marginal =lgt_mod_stage[[1]]$reduced_deviance/sum(lgt_mod_stage[[1]]$reduced_deviance)* sum(stageI$conversion),
                       Bagged = lgt_mod_stageB[[1]]$bagged$imp/sum(lgt_mod_stageB[[1]]$bagged$imp)* sum(stageI$conversion), 
                       row.names = NULL)
colnames(lgt_allc)<-c("channel_name",'Logit_basic_1', 'Marginal_1', 'Bagged_1')

comb_lgt <- lgt_allc
for (i in 2:stages){
  stageI <-data_stage_lgt(df_paths, i)
  lgt_mod_stageB[[i]] <-  attrition(stageI[, -1], method='logistic', bagged = "True")
  lgt_mod_stage[[i]] <-  attrition(stageI[, -1], method='logistic', bagged = "False")
  
  lgt_allc<- data.frame(channel_name = row.names(lgt_mod_stage[[i]]$importance),
                        Logit_basic = lgt_mod_stage[[i]]$importance/sum(lgt_mod_stage[[i]]$importance) *  sum(stageI$conversion),
                        Marginal =lgt_mod_stage[[i]]$reduced_deviance/sum(lgt_mod_stage[[i]]$reduced_deviance)* sum(stageI$conversion),
                        Bagged = lgt_mod_stageB[[i]]$bagged$imp/sum(lgt_mod_stageB[[i]]$bagged$imp)* sum(stageI$conversion), 
                        row.names = NULL)
  colnames(lgt_allc)<-c("channel_name", paste0(c('Logit_basic', 'Marginal', 'Bagged'), '_', i))
  comb_lgt <- merge(comb_lgt, lgt_allc, by = 'channel_name')
  
}


comb_allc_ = melt(comb_lgt, id.vars=c("channel_name"))
ggplot(comb_allc_, aes(channel_name, value, fill=variable)) +
  geom_bar(stat='Identity',position=position_dodge()) + 
  ggtitle("Logistic Regression for  attribution")



comb_lgt_basic <- comb_lgt[, c(1, 2, 5, 8, 11, 14 )]

comb_lgt_basic_ = melt(comb_lgt_basic, id.vars=c("channel_name"))
ggplot(comb_lgt_basic_, aes(channel_name, value, fill=variable)) +
  geom_bar(stat='Identity',position=position_dodge()) + 
  ggtitle("Logistic Regression for  Attribution")


comb_lgt_marginal <- comb_lgt[, c(1, 3, 6, 9, 12, 15 )]

comb_lgt_marginal_ = melt(comb_lgt_marginal, id.vars=c("channel_name"))
ggplot(comb_lgt_marginal_, aes(channel_name, value, fill=variable)) +
  geom_bar(stat='Identity',position=position_dodge()) + 
  ggtitle(" Logistic Regression for  Attribution")



comb_lgt_bagged <- comb_lgt[, c(1, 4, 7, 10, 13, 16 )]

comb_lgt_bagged_ = melt(comb_lgt_bagged, id.vars=c("channel_name"))
ggplot(comb_lgt_bagged_, aes(channel_name, value, fill=variable)) +
  geom_bar(stat='Identity',position=position_dodge()) + 
  ggtitle(" Logistic Regression for  Attribution")



#-------------------------------------------------------
#-------------------------------------------------------
#-------------------------------------------------------
#Markov transitiom model

#-------------------------------------------------------
#split the data set into one channle and multi-channel model 

df_path_1_clean <- df_shapley%>%
  group_by(client_id) %>%
  mutate(uniq_channel_tag = ifelse(length(unique(channel)) == 1, TRUE, FALSE)) %>%
  ungroup()

df_path_1_clean_uniq <- df_path_1_clean %>%
  filter(uniq_channel_tag == TRUE) %>%
  select(-uniq_channel_tag)

df_path_1_clean_multi <- df_path_1_clean %>%
  filter(uniq_channel_tag == FALSE) %>%
  select(-uniq_channel_tag)


#calculate the attribution of the multi channles 
df_multi_paths <- df_path_1_clean_multi %>%
  group_by(client_id) %>%
  summarise(path = paste(channel, collapse = ' > '),
            conversion = sum(conversion)) %>%
  ungroup() %>%
  filter(conversion == 1)

mod_attrib_alt <- markov_model(df_multi_paths,
                               var_path = 'path',
                               var_conv = 'conversion',
                               out_more = TRUE)
mod_attrib_alt$removal_effects
mod_attrib_alt$result

#combine with the unique paths 
df_uniq_paths <- df_path_1_clean_uniq %>%
  filter(conversion == 1) %>%
  group_by(channel) %>%
  summarise(conversions = sum(conversion)) %>%
  ungroup()

d_multi <- data.frame(mod_attrib_alt$result)

d_split <- full_join(d_multi, df_uniq_paths, by = c('channel_name' = 'channel')) %>%
  mutate(result = total_conversions + conversions)

sum(d_split$result)


MC_split<-function(data_in = )

#-------------------------------------------------------
#visulization
df_path_prob <- df_shapley%>%
  group_by(client_id) %>%
  summarise(path = paste(channel, collapse = ' > '),
            conversion = sum(conversion)) %>%
  ungroup() %>%
  mutate(null_conversion = ifelse(conversion == 1, 0, 1))

mod_attrib_complete <- markov_model(
  df_path_prob,
  var_path = 'path',
  var_conv = 'conversion',
  var_null = 'null_conversion',
  out_more = TRUE
)

trans_matrix_prob <- mod_attrib_complete$transition_matrix %>%
  map_at(c(1, 2), as.character)


edges <-
  data.frame(
    from = trans_matrix_prob$channel_from,
    to = trans_matrix_prob$channel_to,
    label = round(trans_matrix_prob$transition_probability, 2),
    font.size = trans_matrix_prob$transition_probability * 100,
    width = trans_matrix_prob$transition_probability * 15,
    shadow = TRUE,
    arrows = "to",
    color = list(color = "#95cbee", highlight = "red")
  )

nodes <-
  data_frame(id = c(
    c(trans_matrix_prob$channel_from),
    c(trans_matrix_prob$channel_to)
  )) %>%
  distinct(id) %>%
  arrange(id) %>%
  mutate(
    label = id,
    color = ifelse(
      label %in% c('(start)', '(conversion)'),
      '#4ab04a',
      ifelse(label == '(null)', '#ce472e', '#ffd73e')
    ),
    shadow = TRUE,
    shape = "box"
  )


visNetwork(nodes,
           edges,
           height = "2000px",
           width = "100%",
           main = "Generic Probabilistic model's Transition Matrix") %>%
  visIgraphLayout(randomSeed = 123) %>%
  visNodes(size = 5) %>%
  visOptions(highlightNearest = TRUE)

#-------------------------------------------------------
#create transition matrix
df_dummy <- data.frame(channel_from = c('(start)', '(conversion)', '(null)'),
                       channel_to = c('(start)', '(conversion)', '(null)'),
                       n = c(0, 0, 0),
                       tot_n = c(0, 0, 0),
                       perc = c(0, 1, 1))


trans_matrix_complete <- mod_attrib_complete$transition_matrix
trans_matrix_complete <- rbind(trans_matrix_complete,
                               df_dummy %>%
                                 mutate(transition_probability = perc) %>%
                                 select(channel_from, channel_to, transition_probability))
trans_matrix_complete$channel_to <- factor(trans_matrix_complete$channel_to, levels = c(levels(trans_matrix_complete$channel_from)))
trans_matrix_complete <- dcast(trans_matrix_complete, channel_from ~ channel_to, value.var = 'transition_probability')
trans_matrix_complete[is.na(trans_matrix_complete)] <- 0
rownames(trans_matrix_complete) <- trans_matrix_complete$channel_from
trans_matrix_complete <- as.matrix(trans_matrix_complete[, -1])

# creating empty matrix for modeling
model_mtrx <- matrix(data = 0,
                     nrow = nrow(trans_matrix_complete), ncol = 1,
                     dimnames = list(c(rownames(trans_matrix_complete)), '(start)'))
# adding modeling number of visits
model_mtrx['channel_5', ] <- 1000

c(model_mtrx) %*% (trans_matrix_complete %^% 5) # after 5 steps
c(model_mtrx) %*% (trans_matrix_complete %^% 100000) # after 100000 steps


# computing time lapses from the first contact to conversion/last contact
df_multi_paths_tl <- df_path_1_clean_multi %>%
  group_by(client_id) %>%
  summarise(path = paste(channel, collapse = ' > '),
            first_touch_date = min(date),
            last_touch_date = max(date),
            tot_time_lapse = round(as.numeric(last_touch_date - first_touch_date)),
            conversion = sum(conversion)) %>%
  ungroup()

# distribution plot
ggplot(df_multi_paths_tl %>% filter(conversion == 1), aes(x = tot_time_lapse)) +
  theme_minimal() +
  geom_histogram(fill = '#4e79a7', binwidth = 1)+
  ggtitle('Distribution of Time to Conversion')+
  labs(y="Counts of Conversion", x = "totle time to Conversion")



# cumulative distribution plot
ggplot(df_multi_paths_tl %>% filter(conversion == 1), aes(x = tot_time_lapse)) +
  theme_minimal() +
  stat_ecdf(geom = 'step', color = '#4e79a7', size = 2, alpha = 0.7) +
  geom_hline(yintercept = 0.95, color = '#e15759', size = 1.5) +
  geom_vline(xintercept = 23, color = '#e15759', size = 1.5, linetype = 2)

#-------------------------------------------------------

### for generic probabilistic model ###
df_multi_paths_tl_1 <- melt(df_multi_paths_tl[c(1:50), ] %>% select(client_id, first_touch_date, last_touch_date, conversion),
                            id.vars = c('client_id', 'conversion'),
                            value.name = 'touch_date') %>%
  arrange(client_id)
rep_date <- as.Date('2016-01-10', format = '%Y-%m-%d')

ggplot(df_multi_paths_tl_1, aes(x = as.factor(client_id), y = touch_date, color = factor(conversion), group = client_id)) +
  theme_minimal() +
  coord_flip() +
  geom_point(size = 2) +
  geom_line(size = 0.5, color = 'darkgrey') +
  geom_hline(yintercept = as.numeric(rep_date), color = '#e15759', size = 2) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = as.numeric(rep_date), ymax = Inf, alpha = 0.01, color = 'white', fill = 'white') +
  theme(legend.position = 'bottom',
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank()) +
  guides(colour = guide_legend(override.aes = list(size = 5)))





