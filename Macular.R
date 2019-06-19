#####################################################################################

############ AUTHOR : NAVEEN BALARAJU
########### UNIVERISTY AT BUFFALO

#####################################################################################
library("cluster")
library("fpc")
library(ggplot2)
library(kohonen)


macular_data=read.csv("/Users/naveen/Desktop/project/Macular_antibiotic_filtered_20190204.csv")
## To convert the column sample as the row names
row.names(macular_data) = macular_data$sample
macular_data[1]=NULL
## The data set is of size 27 x 235




## To scale the datset before finding dissimilarity measure
scaled_dataset= scale(macular_data)

lab=c("Control","Intermediate","Control","Intermediate","Control","Intermediate",
      "Control","Control","Advanced","Intermediate","Intermediate","Advanced",
      "Control","Advanced","Control","Control","Control","Intermediate","Advanced",
      "Control","Control","Control","Advanced","Intermediate","Control","Control","Advanced")

####################################################################################
############################ HIERARCHIAL CLUSTERING ################################
####################################################################################

############# To perform hierarchial cluseting based on complete linkage #############

cluster_model = hclust(dist(scaled_dataset),method = "complete")
quartz()
plot(cluster_model,xlab = "",cex=0.6,main="DENDROGRAM WITH COMPLETE LINKAGE",labels = lab)
group_clusters=cutree(cluster_model,k=3)
rect.hclust(cluster_model,k=3,border = "green")

## silhouette plot to find k
s= silhouette(group_clusters,dist = dist(scaled_dataset))
quartz()
plot(s,main="silhouette plot for k=3 and complete linkage") 
sil_width=c()
for (i in 2:10){
  ct=cutree(cluster_model,k=i)
  st=silhouette(ct,dist(scaled_dataset))
  average_width=summary(st)$avg.width
  sil_width=c(sil_width,average_width)
}
sil_width

#k_means=kmeans(scaled_dataset, centers = 3, nstart = 10)


#plot(scaled_dataset, col = k_means$cluster, main = "Example k-means w/PC")
#points(k_means$centers, col = 1:3, pch = 8, cex= 2)





## K-medoids
medo_k=pamk(macular_data)
medo_k$nc

## From the analysis  of clusters produced by complete linkage and the silhouette
##  average width values, the average width value is maximum for k=2. Also 
## K-Medoids, algorithm gives the optimal value of k=2


cluster_1 = group_clusters[group_clusters==1]
cluster_2 = group_clusters[group_clusters==2]
print(cluster_1) #observations in cluster 1 for Complete Linkage
print(cluster_2) #observations in cluster 2 for Complete Linkage

############# To perform hierarchial cluseting based on Average linkage #############

cluster_model = hclust(dist(scaled_dataset),method = "average")
quartz()
plot(cluster_model,xlab = "",cex=0.6,main="DENDROGRAM WITH AVERAGE LINKAGE",labels = lab)
group_clusters=cutree(cluster_model,k=2)
rect.hclust(cluster_model,k=2,border = "green")

## silhouette plot to find k
s= silhouette(group_clusters,dist = dist(scaled_dataset))
quartz()
plot(s,main="silhouette plot for k=2 and Average linkage") 
sil_width=c()
for (i in 2:10){
  ct=cutree(cluster_model,k=i)
  st=silhouette(ct,dist(scaled_dataset))
  average_width=summary(st)$avg.width
  sil_width=c(sil_width,average_width)
}
sil_width

## From the analysis  of clusters produced by Average linkage and the silhouette
##  average width values, the average width value is maximum for k=2. Also 
## K-Medoids, algorithm gives the optimal value of k=2

cluster_1 = group_clusters[group_clusters==1]
cluster_2 = group_clusters[group_clusters==2]
print(cluster_1) #observation in cluster 1 for Average Linkage
print(cluster_2) #observation in cluster 2 for Average Linkage





## Gap statistic
library("cluster")
gap_sts=clusGap(macular_data,kmeans,nstart=20,K.max = 10,B=50)
quartz()
plot(gap_sts,main="Gap Statistic for k-means")

gap_sts_kmed=clusGap(macular_data,pam,K.max = 10,B=50)
quartz()
plot(gap_sts_kmed,main="Gap Statistic for k-medoids")

