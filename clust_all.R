
#####################################################################################
############# AUTHOR : NAVEEN BALARAJU
############ UNIVERISTY AT BUFFALO
#####################################################################################
library("cluster")
library("fpc")
library(ggplot2)
library(kohonen)
library("fpc")
library("cluster")

macular_data=read.csv("/Users/naveen/Desktop/project/Macular_antibiotic_filtered_20190204.csv")

## To convert the column sample as the row names
row.names(macular_data) = macular_data$sample
macular_data[1]=NULL
## The data set is of size 27 x 235




## principal component analysis
pc=prcomp(macular_data,center = TRUE,scale. = TRUE)
summary(pc)
quartz()
plot(pc,type="l",main = "PRINCIPAL COMPONENT ANALYSIS")



##Choosing First two Principal components that explains the maximum variance in the dataset
## and performing Hierarchical clustering with complete linkage 
comp1=pc$x[,1]
comp2=pc$x[,2]
#plot(comp1,comp2,col = 1:2, pch = 8, cex= 2)
#comp3=pc$x[,3]
#comp4=pc$x[,4]
#comp5=pc$x[,5]
#rownames(pc_data)

lab=c('CON','INT','CON','INT','CON','INT',
       'CON','CON','ADV','INT','INT','ADV',
       'CON','ADV','CON','CON','CON','INT',
       'ADV','CON','CON','CON','ADV','INT',
       'CON','CON','ADV')
lab=as.factor(lab)
pc_data =cbind(comp1,comp2)#,comp3)

#library(rgl)
#plot3d(x=comp1,y=comp2,z=comp3,
       #col=c("red","green","blue")[lab],
       #type="s",radius=1)

plot(comp1,comp2,main = "SCATTER PLOT OF THE DATA POINTS AFTER PCA ")
text(comp1,comp2,labels = lab,cex=0.9,col = "blue")


##Hierarchical clustering for PCA data
cluster_model = hclust(dist(pc_data),method = "complete")
cut_clust=cutree(cluster_model,k=2)
plot(cluster_model,main = "DENDROGRAM WITH COMPLETE LINKAGE")
rect.hclust(cluster_model,k=2,border = "blue")
s= silhouette(cut_clust,dist = dist(pc_data))
quartz()
plot(s,main="SILHOUETTE PLOT FOR K=2 FOR COMPLETE LINKAGE",col = rainbow(14)) 


# for three clusters

cut_clust3=cutree(cluster_model,h=20)
plot(cluster_model,main = "DENDROGRAM WITH COMPLETE LINKAGE")
rect.hclust(cluster_model,h=20,border = "blue")
s= silhouette(cut_clust3,dist = dist(pc_data))
quartz()
plot(s,main="SILHOUETTE PLOT FOR K=3 FOR COMPLETE LINKAGE",col = rainbow(14)) 



## K means

k_means=kmeans(pc_data, centers = 2, nstart = 10)
plot(pc_data, col = k_means$cluster, main = " K-MEANS CLUSTERING WITH PCA DATA")
points(k_means$centers, col = 1:2, pch = 17, cex= 2)
text(pc_data,labels=lab,col = "orange")
gap_sts=clusGap(pc_data,kmeans,nstart=20,K.max = 10,B=50)
quartz()
plot(gap_sts,main="Gap Statistic for k-means ")


###############################################################################
################################ T-SNE ########################################
###############################################################################

library(tsne)
set.seed(123)
macular_data['lab']=lab
macular_data[["lab"]]=as.factor(macular_data$lab)
colors=rainbow(length(unique(macular_data$lab)))
names(colors)=unique(macular_data$lab)
ecb = function(x,y,z){plot(x,t='n',main = "t-SNE PLOT",cex=0.9);text(x,labels = macular_data$lab,col = colors[macular_data$lab])}
tsne_mac = tsne(macular_data[,1:235],k=2,epoch_callback = ecb,perplexity=5)


#library(rgl)
#plot3d(x=tsne_mac[,1],y=tsne_mac[,2],z=tsne_mac[,3],
      # col=c("red","green","blue")[macular_data$lab],
      # type="s",radius=11)

c1=tsne_mac[,1]
c2=tsne_mac[,2]
lab1=c('CON','INT','CON','INT','CON','INT',
      'CON','CON','ADV','INT','INT','ADV',
      'CON','ADV','CON','CON','CON','INT',
      'ADV','CON','CON','CON','ADV','INT',
      'CON','CON','ADV')
tsne_data=cbind(c1,c2)
rownames(tsne_data)=lab1




##Hierarchical clustering for tsne data
cluster_model_tsne = hclust(dist(tsne_data),method = "complete")
cut_clust_tsne=cutree(cluster_model_tsne,k=3)
quartz()
plot(cluster_model_tsne,main = "DENDROGRAM(t-SNE data) WITH COMPLETE LINKAGE")
rect.hclust(cluster_model_tsne,k=3,border = "blue")
s= silhouette(cut_clust_tsne,dist = dist(tsne_data))
quartz()
plot(s,main="SILHOUETTE PLOT FOR K=3 FOR COMPLETE LINKAGE",col = rainbow(14)) 

summary(tsne_data)
v=scale(tsne_data)

summary(v)






##############################
## k-mediods
##############################
kmed = pamk(tsne_data)

# let the program decide optimal k
kmed$nc
gap_sts_kmed=clusGap(pc_data,pam,K.max = 10,B=50)
quartz()
plot(gap_sts_kmed,main="Gap Statistic for k-medoids")








## Gap statistic
gap_sts=clusGap(pc_data,kmeans,nstart=20,K.max = 10,B=50)
#k = maxSE(gap_sts$Tab[, "gap"], gap_sts$Tab[, "SE.sim"], method="Tibs2001SEmax")
#quartz()
plot(gap_sts,main="Gap Statistic for k-means")
#abline(v=k, lty=3, lwd=2, col="Blue")

gap_sts_kmed=clusGap(pc_data,pam,K.max = 10,B=50)
#quartz()
plot(gap_sts_kmed,main="Gap Statistic for k-medoids")

