#-------------------------------------------------------
#Functionality
#-------------------------------------------------------

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

#-------------------------------------------------------
#data prepare
#-------------------------------------------------------

#############################################################################################
#read data 
load(paste0(cRoot, "/xx.RData"))

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


