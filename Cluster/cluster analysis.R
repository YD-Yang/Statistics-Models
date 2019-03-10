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
library(ggfortify)
library("flexclust")
library(clue)
library(flexclust)
library(EMCluster, quietly = TRUE)
library(mclust)
library(dbscan)
library(factoextra)
library(fpc)
library(NbClust)
#-------------------------------------------------------
#install.packages("data.table","ggplot2")

#-------------------------------------------------------
#Setup
#-------------------------------------------------------
# cRoot<-"C:/Users/mmadi/Documents/Work/VCA"
#cRootOutput<-"C:/Users/mmadi/Documents/Work/VCA/Data/Output"
cRootOutput<-"L:/DataScience/VCA/Data/Output"
cRootCode<-"L:/DataScience/VCA/Code"

cRoot<-"L:/DataScience/VCA/Data"
cRoot2<-"L:/DataScience/VCA/Data/RawData"
cRoot3<-"L:/DataScience/VCA/Data/Output/Results/Migration_models"


dir.create(path=cRootOutput,recursive = T)

dir<-"L:/DataScience/VCA/Data/Output/Results"

source(paste0(cRootCode,"/commonFunctions_v001.R"))
#-------------------------------------------------------
#data prepare
#-------------------------------------------------------

#############################################################################################
#read data 
load(paste0(cRoot, "/vca_rescoring_08072016.RData"))
names(vca_recoring_pets_v0) <- tolower(names(vca_recoring_pets_v0))

features<-read.csv( file=paste0(dir,"/Migration_models/feature_type.csv"), head = TRUE)
levels(features$Feature_name) <-tolower(levels(features$Feature_name))
vars_list <- features$Feature_name
sick_list <- features[features$Type == "Sick", 1 ]
demo_list <- features[features$Type == "Demo", 1 ]
demo_basic <-  c( "asian" , "african_american","hispanic"  , "white"  ,
                              "presence_children"   , "single" ,"married"  , "single_parent",
                              "home_owner"  ,"home_renter" ,"length_of_residence","indvl_age" ,
                              "hh_size" , "hh_income" )


vca_segment <- vca_recoring_pets_v0[, which(names(vca_recoring_pets_v0)
                                                    %in%  vars_list )] 
maxs <- apply(vca_segment, 2, max) 
mins <- apply(vca_segment, 2, min)
vca_scaled <- as.data.frame(scale(vca_segment, center = mins, scale = maxs - mins))

#############################################################################################
#data visual
seg_data_full = vca_scaled
original_clu<- vca_rescoring_pets$original_cluster_seg
#model the sick, demo, and demo_baisc 
#seg_data_sick = vca_scaled[,  which(names(vca_scaled) %in%  sick_list )]
#seg_data_demo = vca_scaled[,  which(names(vca_scaled) %in%  demo_list )]
#seg_data_basic = vca_scaled[,  which(names(vca_scaled) %in%  demo_basic )]


pca_vca_full = prcomp(seg_data_full)
autoplot(pca_vca_full, colour = "blue")

pca_vca_sick = prcomp(seg_data_sick)
autoplot(pca_vca_sick , colour = "red")

pca_vca_demo = prcomp(seg_data_demo)
autoplot(pca_vca_demo, colour = 3)

pca_vca_basic = prcomp(seg_data_basic)
autoplot(pca_vca_basic, colour = 5)



#read in the latent_gold results
LG_pred<-read.table("H:/VCA/cluster/LG_pred.dat", header = FALSE  , skip = 1)
LG_head<-read.table("H:/VCA/cluster/LG_pred.dat", nrows = 1)
LG_labels<-LG_pred[,]

names(LG_labels)<-c("oid_client_dim", "clu_" )
autoplot(pca_vca_basic, colour = LG_labels$clu_, main = "Latent GOLD clusters on Data_basic")

#-------------------------------------------------------
#cluster analysis
#-------------------------------------------------------
#############################################################################################
############partition based models 
###K means 
set.seed(1234)
km_full <- kmeans(seg_data_full, centers = 6, nstart = 123)
km_labels_full <- cl_predict(km_full, data = seg_data_full)
autoplot(pca_vca_full, colour = km_labels_full)
table(km_labels_full)
table(original_clu, km_labels_full)

set.seed(1234)
km_sick <- kmeans(seg_data_sick, centers = 6, nstart = 123)
km_labels_sick<- cl_predict(km_sick, data = seg_data_sick)
autoplot(pca_vca_sick, colour = km_labels_sick)
table(km_labels_sick)

set.seed(1234)
km_demo <- kmeans(seg_data_demo, centers = 6, nstart = 123)
km_labels_demo <- cl_predict(km_demo, data = seg_data_demo)
autoplot(pca_vca_demo, colour = km_labels_demo)
table(km_labels_demo)


set.seed(1234)
km_basic <- kmeans(seg_data_basic, centers = 6, nstart = 123)
km_labels_basic <-cl_predict(km_basic, data = seg_data_basic)
autoplot(pca_vca_basic, colour = km_labels_basic)
table(km_labels_basic)
table(original_clu, km_labels_basic)



table(km_labels_full, km_labels_sick)
table(km_labels_full, km_labels_demo)
table(km_labels_full, km_labels_basic)
table(km_labels_sick, km_labels_demo)
table(km_labels_sick, km_labels_basic)
table(km_labels_demo, km_labels_basic)


###k-centroid cluster: flexclust
fc_cont <- new("flexclustControl")
fc_cont@tolerance <- 0.1 
my_seed <- 0
my_family <- "ejaccard"
num_clust <- 6

set.seed(1234)
kc_full <- flexclust::kcca(seg_data_full, k = num_clust, save.data = TRUE, control = fc_cont,
                      family = kccaFamily(my_family))
kc_labels_full <-predict(kc_full, data = seg_data_full)
pop_av_dist_full <- with(kc_full@clusinfo, sum(size*av_dist)/sum(size))
# Neighborhood Graph on 1st principle components
plot(kc_full, data = as.matrix(seg_data_full), project = pca_vca_full,
     sub = paste("\nAv Dist = ", format(pop_av_dist, digits = 6),
                 ", k = ", kc_full@k, sep = ""))
# Activity Profiles for each segment
print(barchart(kc_full,  strip.prefix = "#",
               scales = list(cex = 0.6)))


set.seed(1234)
kc_sick <- flexclust::kcca(seg_data_sick, k = num_clust, save.data = TRUE, control = fc_cont,
                           family = kccaFamily(my_family))
kc_labels_sick <-predict(kc_sick, data = seg_data_sick)
pop_av_dist_sick <- with(kc_sick@clusinfo, sum(size*av_dist)/sum(size))
# Neighborhood Graph on 1st principle components
plot(kc_sick, data = as.matrix(seg_data_sick), project = pca_vca_sick,
     sub = paste("\nAv Dist = ", format(pop_av_dist, digits = 6),
                 ", k = ", kc_sick@k, sep = ""))
# Activity Profiles for each segment
print(barchart(kc_sick,  strip.prefix = "#",
               scales = list(cex = 0.6)))




set.seed(1234)
kc_demo <- flexclust::kcca(seg_data_demo, k = num_clust, save.data = TRUE, control = fc_cont,
                           family = kccaFamily(my_family))
kc_labels_demo <-predict(kc_demo, data = seg_data_demo)
pop_av_dist_demo <- with(kc_demo@clusinfo, sum(size*av_dist)/sum(size))
# Neighborhood Graph on 1st principle components
plot(kc_demo, data = as.matrix(seg_data_demo), project = pca_vca_demo,
     sub = paste("\nAv Dist = ", format(pop_av_dist, digits = 6),
                 ", k = ", kc_demo@k, sep = ""))
# Activity Profiles for each segment
print(barchart(kc_demo,  strip.prefix = "#",
               scales = list(cex = 0.6)))

set.seed(1234)
kc_basic <- flexclust::kcca(seg_data_basic, k = num_clust, save.data = TRUE, control = fc_cont,
                           family = kccaFamily(my_family))
kc_labels_basic <-predict(kc_basic, data = seg_data_basic)
pop_av_dist_basic <- with(kc_basic@clusinfo, sum(size*av_dist)/sum(size))
# Neighborhood Graph on 1st principle components
plot(kc_basic, data = as.matrix(seg_data_basic), project = pca_vca_basic,
     sub = paste("\nAv Dist = ", format(pop_av_dist, digits = 6),
                 ", k = ", kc_basic@k, sep = ""))
# Activity Profiles for each segment
print(barchart(kc_basic,  strip.prefix = "#",
               scales = list(cex = 0.6)))



table(km_labels_full, kc_labels_full)
table(km_labels_sick, kc_labels_sick)
table(km_labels_demo, kc_labels_demo)
table(km_labels_basic, kc_labels_basic)

table(kc_labels_full, kc_labels_sick)
table(kc_labels_full, kc_labels_demo)
table(kc_labels_full, kc_labels_basic)
table(kc_labels_sick, kc_labels_demo)
table(kc_labels_sick, kc_labels_basic)
table(kc_labels_demo, kc_labels_basic)

autoplot(pca_vca_full, colour = kc_labels_full)
autoplot(pca_vca_sick, colour = kc_labels_sick)
autoplot(pca_vca_demo, colour = kc_labels_demo)
autoplot(pca_vca_basic, colour = kc_labels_basic)

table(original_clu, kc_labels_basic)


############hierarchical clustering

hc_basic <- hclust(dist(seg_data_basic), "ave")
hc_labels_basic <- cutree(hc_basic, k = 4)
table(hc_labels_basic)

############EM based clustering
#emcluster
par(mfrow=c(2, 2))
            

set.seed(123)
emobj_basic <- simple.init(seg_data_basic, nclass = 2)
emobj_basic <- shortemcluster(seg_data_basic, emobj_basic)
em_clust_basic <- emcluster(seg_data_basic, emobj_basic, assign.class = TRUE)
em_labels_basic <-assign.class(seg_data_basic, em_clust_basic, return.all = FALSE)
autoplot(pca_vca_basic, colour = em_labels_basic$class, main = "data_basic with 2 clusters")


table(original_clu, em_labels_basic$class)


############density based clustering
#density-based spatial clustering of applications with noise
## find suitable eps parameter using a k-NN plot for k = dim + 1
## Look for the knee!

kNNdistplot(seg_data_basic, k = 4)
db_basic<- dbscan(seg_data_basic, eps =1.1, minPts = 100)
db_labels_basic <- predict(db_basic, data = seg_data_basic)+1

autoplot(pca_vca_basic, colour = db_label_basic, main = "data_basic DBSCAN ")

table(original_clu, db_label_basic)
#-------------------------------------------------------
#cluster evaluation
#-------------------------------------------------------


##################################################################
#internal clustering
#calculate the  index
LG_labels_basic<- LG_labels$clu_ 
km.full.stats <- cluster.stats(dist(seg_data_full),  km_labels_full)
km.sick.stats <- cluster.stats(dist(seg_data_sick),  km_labels_sick)
km.demo.stats <- cluster.stats(dist(seg_data_demo),  km_labels_demo)

kc.full.stats <- cluster.stats(dist(seg_data_full),  kc_labels_full)
kc.sick.stats <- cluster.stats(dist(seg_data_sick),  kc_labels_sick)
kc.demo.stats <- cluster.stats(dist(seg_data_demo),  kc_labels_demo)

km.basic.stats <- cluster.stats(dist(seg_data_basic),  km_labels_basic)
kc.basic.stats <- cluster.stats(dist(seg_data_basic),  kc_labels_basic)
hc.basic.stats <- cluster.stats(dist(seg_data_basic),  hc_labels_basic)
LG.basic.stats <- cluster.stats(dist(seg_data_basic),  LG_labels_basic)
EM.basic.stats <- cluster.stats(dist(seg_data_basic),  em_labels_basic$class)
db.basic.stats <- cluster.stats(dist(seg_data_basic),  db_labels_basic)

c(km.basic.stats$min.cluster.size, km.basic.stats$average.between , km.basic.stats$average.within, 
  km.basic.stats$avg.silwidth, km.basic.stats$dunn,km.basic.stats$entropy, km.basic.stats$ch, km.basic.stats$sindex  )
c(kc.basic.stats$min.cluster.size, kc.basic.stats$average.between , kc.basic.stats$average.within, 
  kc.basic.stats$avg.silwidth, kc.basic.stats$dunn,kc.basic.stats$entropy, kc.basic.stats$ch, kc.basic.stats$sindex  )
c(hc.basic.stats$min.cluster.size, hc.basic.stats$average.between , hc.basic.stats$average.within, 
  hc.basic.stats$avg.silwidth, hc.basic.stats$dunn,hc.basic.stats$entropy, hc.basic.stats$ch, hc.basic.stats$sindex  )
c(LG.basic.stats$min.cluster.size, LG.basic.stats$average.between , LG.basic.stats$average.within, 
  LG.basic.stats$avg.silwidth, LG.basic.stats$dunn,LG.basic.stats$entropy, LG.basic.stats$ch, LG.basic.stats$sindex  )
c(EM.basic.stats$min.cluster.size, EM.basic.stats$average.between , EM.basic.stats$average.within, 
  EM.basic.stats$avg.silwidth, EM.basic.stats$dunn,EM.basic.stats$entropy, EM.basic.stats$ch, EM.basic.stats$sindex  )
c(db.basic.stats$min.cluster.size, db.basic.stats$average.between , db.basic.stats$average.within, 
  db.basic.stats$avg.silwidth, db.basic.stats$dunn,db.basic.stats$entropy, db.basic.stats$ch, db.basic.stats$sindex  )


#-------------------------------------------------------
#predictive modeling
#-------------------------------------------------------
##################################################################
#tree models
##################################################################
##model data basic with with the predictive cluster from data basic 
#kmeans
label.clu<- matrix(km_labels_basic, ncol = 1)
dt.km <- cbind(label.clu, seg_data_basic)

#kjacc
label.clu<- matrix(kc_labels_basic, ncol = 1)
dt.km <- cbind(label.clu, seg_data_basic)

#kjacc
label.clu<- matrix(hc_labels_basic, ncol = 1)
dt.km <- cbind(label.clu, seg_data_basic)


#LatentGOLD
label.clu<- matrix(LG_labels_basic, ncol = 1)
dt.km <- cbind(label.clu, seg_data_basic)


#EM
EM_labels_basic<-em_labels_basic$class
label.clu<- matrix(EM_labels_basic, ncol = 1)
dt.km <- cbind(label.clu, seg_data_basic)

#DBSCAN
label.clu<- matrix(db_labels_basic, ncol = 1)
dt.km <- cbind(label.clu, seg_data_basic)


n_basic<- names(seg_data_basic)
f<- as.formula(paste("label.clu ~", paste(n_basic, collapse = " + ")))

set.seed(123)
index<-sample(1:length(label.clu), ceiling(0.7*length(label.clu)))
train_tree <-dt.km[index, ]
test_tree <- dt.km[-index, ]
tree_fit <- rpart(f, method="class", data=train_tree)

pred.test_prob<-predict(tree_fit,newdata=test_tree)
pred.test_label <-max.col(pred.test_prob)
confustion.test<-table(pred.test_label, test_tree$label.clu)
accuracy.test<-sum(pred.test_label == test_tree$label.clu)/nrow(test_tree)

results<- checks(pred.test_label, test_tree$label.clu)
c(mean(results$precision)*100, mean(results$recall)*100, results$accuracy.com)  

##model data basic with with the predictive cluster from data basic 
#kmeans and kJacc 
#kmeans
label.clu<- matrix(km_labels_basic, ncol = 1)
dt.km <- cbind(label.clu, seg_data_basic)


label.clu<- matrix(km_labels_demo, ncol = 1)
dt.km <- cbind(label.clu, seg_data_basic)

label.clu<- matrix(km_labels_sick, ncol = 1)
dt.km <- cbind(label.clu, seg_data_basic)

label.clu<- matrix(km_labels_full, ncol = 1)
dt.km <- cbind(label.clu, seg_data_basic)

label.clu<- matrix(kc_labels_demo, ncol = 1)
dt.km <- cbind(label.clu, seg_data_basic)

label.clu<- matrix(kc_labels_sick, ncol = 1)
dt.km <- cbind(label.clu, seg_data_basic)

label.clu<- matrix(kc_labels_full, ncol = 1)
dt.km <- cbind(label.clu, seg_data_basic)



n_basic<- names(seg_data_basic)
f<- as.formula(paste("label.clu ~", paste(n_basic, collapse = " + ")))

set.seed(123)
index<-sample(1:length(label.clu), ceiling(0.7*length(label.clu)))
train_tree <-dt.km[index, ]
test_tree <- dt.km[-index, ]
tree_fit <- rpart(f, method="class", data=train_tree)

pred.test_prob<-predict(tree_fit,newdata=test_tree)
pred.test_label <-max.col(pred.test_prob)
confusion.test<-table( test_tree$label.clu, pred.test_label)
accuracy.test<-sum(pred.test_label == test_tree$label.clu)/nrow(test_tree)

results<- checks(pred.test_label, test_tree$label.clu)
c(mean(results$precision)*100, mean(results$recall)*100, results$accuracy.com)  











