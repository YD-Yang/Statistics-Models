
#-------------------------------------------------------
#Libraries
library(ggfortify)
library("flexclust")
library(clue)
library(flexclust)
library(factoextra)
library(NbClust)

library(ggplot2)
library(EMCluster, quietly = TRUE)
library(mclust)
library(colorspace)
library(dendextend)
library(circlize)
library(clusterSim)
library(htmltools)
library(dbscan)
library(fpc)
library(rpart)

#-------------------------------------------------------

#-------------------------------------------------------
#data
data("iris")
dim(iris)

ggplot(iris, aes(Petal.Length, Petal.Width, color = Species)) + geom_point()
write.csv(iris, file = "H:/VCA/cluster/iris.csv")
#-------------------------------------------------------
# K-means 

# Perform K-Means with 2 clusters
set.seed(20)
iris_km <- kmeans(iris[, 3:4], 3, nstart = 20)
aa <- cl_predict(iris_km, data = mydata)

iris_km$cluster <- as.factor(iris_km$cluster)
ggplot(iris, aes(Petal.Length, Petal.Width, color = iris_km$cluster)) + geom_point()

table(iris$Species, iris_km$cluster)

#elbow selection
mydata <- iris[, 3:4]
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))

for (i in 2:15) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares",
     main="Assessing the Optimal Number of Clusters with the Elbow Method",
     pch=20, cex=2)



#Bayesian Inference Criterion for k means
d_clust <- Mclust(as.matrix(mydata), G=1:15, 
                  modelNames = mclust.options("emModelNames"))


#-------------------------------------------------------
# Hierarchical clustering
clusters <- hclust(dist(iris[, 3:4]))
plot(clusters)


clusters <- hclust(dist(iris[, 3:4]))
plot(clusters)
Iris_Hclus <- cutree(clusters, 3)
table(iris$Species, Iris_Hclus)
#try different linkage method
clusters <- hclust(dist(iris[, 3:4]), method = 'average')
plot(clusters)


# by using the dendextend library
hc_iris <- hclust(dist(iris[, 3:4]), method = "complete")
iris_species <- rev(levels(iris[,5]))

dend <- as.dendrogram(hc_iris)
# order it the closest we can to the order of the observations:
dend <- rotate(dend, 1:150)

# Color the branches based on the clusters:
dend <- color_branches(dend, k=3) #, groupLabels=iris_species)

# Manually match the labels, as much as possible, to the real classification of the flowers:
labels_colors(dend) <-
  rainbow_hcl(3)[sort_levels_values(
    as.numeric(iris[,5])[order.dendrogram(dend)]
  )]

# We shall add the flower type to the labels:
labels(dend) <- paste(as.character(iris[,5])[order.dendrogram(dend)],
                      "(",labels(dend),")", 
                      sep = "")
# We hang the dendrogram a bit:
dend <- hang.dendrogram(dend,hang_height=0.1)
# reduce the size of the labels:
# dend <- assign_values_to_leaves_nodePar(dend, 0.5, "lab.cex")
dend <- set(dend, "labels_cex", 0.5)
# And plot:
par(mar = c(3,3,3,7))
plot(dend, 
     main = "Clustered Iris data set
     (the labels give the true flower species)", 
     horiz =  TRUE,  nodePar = list(cex = .007))
legend("topleft", legend = iris_species, fill = rainbow_hcl(3))


par(mar = rep(0,4))
circlize_dendrogram(dend)



#-------------------------------------------------------
# EM clustering


set.seed(123)
emobj_basic <- simple.init(mydata, nclass = 3)
emobj_basic <- shortemcluster(mydata, emobj_basic)
em_clust_basic <- emcluster(mydata, emobj_basic, assign.class = TRUE)
em_labels_basic <-assign.class(mydata, em_clust_basic, return.all = FALSE)
Iris_EM<- as.factor(em_labels_basic$class)
ggplot(iris, aes(Petal.Length, Petal.Width, color = Iris_EM)) + geom_point()

table(iris$Species, Iris_EM)


#read in the latent_gold results
LG_pred<-read.table("H:/VCA/cluster/LG_iris.dat", header = FALSE  , skip = 1)
LG_head<-read.table("H:/VCA/cluster/LG_iris.dat", nrows = 1)
Iris_LG = LG_pred$V8
table(iris$Species, Iris_LG)

#-------------------------------------------------------
#density based clustering
#density-based spatial clustering of applications with noise
## find suitable eps parameter using a k-NN plot for k = dim + 1
## Look for the knee!

stm<-shapes.two.moon(180)
plot(stm$data,col=rainbow(3)[stm$clusters])

kNNdistplot(stm$data, k = 3)
abline(h=0.1, col = "red", lty=2)

moon_db<- dbscan(stm$data, eps =.15, minPts = 5)
moon_db_label <- predict(moon_db, data = stm$data)+1

plot(stm$data, col = moon_db_label)



mydata <- as.matrix(iris[,3:4])
## find suitable eps parameter using a k-NN plot for k = dim + 1
## Look for the knee!
kNNdistplot(mydata, k = 5)
abline(h=.5, col = "red", lty=2)
res <- dbscan(mydata, eps = .5, minPts = 5)
res
pairs(mydata, col = res$cluster + 1L)
Iris_dbscan <- as.factor(predict(res, data = mydata)+1)
ggplot(iris, aes(Petal.Length, Petal.Width, color = Iris_dbscan)) + geom_point()

table(iris$Species, Iris_dbscan)


## use precomputed frNN
#fr <- frNN(mydata, eps = .5)
#dbscan(fr, minPts = 5)

#-------------------------------------------------------
#cluster evaluation
#-------------------------------------------------------

km.stats <- cluster.stats(dist(mydata),  as.numeric(iris_km$cluster))
hc.stats <-  cluster.stats(dist(mydata),  as.numeric(Iris_Hclus))
LG.stats <-  cluster.stats(dist(mydata), as.numeric(Iris_LG))
em.stats <-  cluster.stats(dist(mydata), as.numeric(Iris_EM))
db.stats <-  cluster.stats(dist(mydata), as.numeric(Iris_dbscan))



c(km.stats$min.cluster.size, km.stats$average.between , km.stats$average.within, 
  km.stats$avg.silwidth, km.stats$dunn,km.stats$entropy, km.stats$ch, km.stats$sindex  )

c(hc.stats$min.cluster.size, hc.stats$average.between , hc.stats$average.within, 
  hc.stats$avg.silwidth, hc.stats$dunn,hc.stats$entropy, hc.stats$ch, hc.stats$sindex  )

c(LG.stats$min.cluster.size, LG.stats$average.between , LG.stats$average.within, 
  LG.stats$avg.silwidth, LG.stats$dunn,LG.stats$entropy, LG.stats$ch, LG.stats$sindex  )


c(em.stats$min.cluster.size, em.stats$average.between , em.stats$average.within, 
  em.stats$avg.silwidth, em.stats$dunn,em.stats$entropy, em.stats$ch, em.stats$sindex  )

c(db.stats$min.cluster.size, db.stats$average.between , db.stats$average.within, 
  db.stats$avg.silwidth, db.stats$dunn,db.stats$entropy, db.stats$ch, db.stats$sindex  )


#kmeans
label.clu<- matrix(iris_km$cluster, ncol = 1)
dt.km <- cbind(label.clu, mydata)

#hierarchical cluster
label.clu<- matrix(Iris_Hclus, ncol = 1)
dt.km <- cbind(label.clu, mydata)

#latentgold
label.clu<- matrix(Iris_LG, ncol = 1)
dt.km <- cbind(label.clu, mydata)

#EM
EM_labels_basic<-Iris_EM
label.clu<- matrix(EM_labels_basic, ncol = 1)
dt.km <- cbind(label.clu, mydata)

#DBSCAN
label.clu<- matrix(Iris_dbscan, ncol = 1)
dt.km <- cbind(label.clu, mydata)



n_basic<- names(mydata)
f<- as.formula(paste("label.clu ~  ", paste(n_basic, collapse = " + ")))

set.seed(123)
index<-sample(1:length(label.clu), ceiling(0.7*length(label.clu)))
train_tree <-dt.km[index, ]
test_tree <- dt.km[-index, ]
tree_fit <- rpart(f, method="class", data=as.data.frame(train_tree))

pred.test_prob<-predict(tree_fit,newdata=test_tree)
pred.test_label <-max.col(pred.test_prob)
confusion.test<-table( test_tree$label.clu, pred.test_label)
accuracy.test<-sum(pred.test_label == test_tree$label.clu)/nrow(test_tree)

results<- checks(pred.test_label, as.numeric(test_tree$label.clu))
c(mean(results$precision)*100, mean(results$recall)*100, results$accuracy.com)  




checks <- function(pred_label, test_label){
  confusion <-table(test_label,factor(pred_label, 
                                      levels = min(test_label): max(test_label))) 
  precision <- numeric()
  recall <- numeric()
  for (i in 1: nrow(confusion)){
    precision[i] = confusion[i, i]/sum( confusion[, i])
    recall[i] =  confusion[i, i]/sum( confusion[i, ])
  }
  accuracy =   sum(diag(confusion)) /length(test_label)
  accuracy[is.nan(accuracy)]<-0
  precision[is.nan(precision)]<-0
  accuracy.com =   round(mean(diag(confusion)*100/rowSums(confusion)),2)
  
  
  return(list(confusion=confusion, precision=precision, recall=recall, 
              accuracy=accuracy,accuracy.com=accuracy.com))
}
