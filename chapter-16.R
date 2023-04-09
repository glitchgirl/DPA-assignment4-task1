 install.packages(                                                 
  c("NbClust", "flexclust", "ggdendro", "rattle", "cluster",       
    "clusterability", "factoextra", "colorhcplot", "ggplot2"))  
 
 data(nutrient, package="flexclust")
 row.names(nutrient) <- tolower(row.names(nutrient))
 nutrient.scaled <- as.data.frame(scale(nutrient)) 
 
 d <- dist(nutrient.scaled)                                          
 fit.average <- hclust(d, method="average") 
 
 library(ggdendro) 
 library(ggplot2)
 ggdendrogram(fit.average) + 
   labs(title="Average Linkage Clustering")
 
 library(NbClust)
 library(factoextra)
 nc <- NbClust(nutrient.scaled, distance="euclidean", 
               min.nc=2, max.nc=15, method="average")
 fviz_nbclust(nc)
 
 library(dplyr)
 clusters <- cutree(fit.average, k=5)
 table(clusters)
 
 nutrient.scaled$clusters <- clusters
 profiles <- nutrient.scaled %>% 
   group_by(clusters) %>%
   summarize_all(median) 
 
 profiles %>% round(3) %>% data.frame()  
 
 library(colorhcplot)
 par(mfrow=c(1, 1))
 cl <-factor(clusters, levels=c(1:5), 
             labels=paste("cluster", 1:5))
 colorhcplot(fit.average, cl, hang=-1, lab.cex=.8, lwd=2,
             main="Average-Linkage Clustering\n5 Cluster Solution")
 
 wssplot <- function(data, nc=15, seed=1234){
   require(ggplot2)
   wss <- numeric(nc)
   for (i in 1:nc){
     set.seed(seed)
     wss[i] <- sum(kmeans(data, centers=i)$withinss)
   }
   results <- data.frame(cluster=1:nc, wss=wss)
   ggplot(results, aes(x=cluster,y=wss)) +
     geom_line(color="grey") +
     geom_point(color="steelblue", size=2) +
     theme_bw() +
     labs(x="Number of Clusters",
          y="Within groups sum of squares")
 }
 
 data(wine, package="rattle")
 library(NbClust)
 library(factoextra)
 head(wine)
 df <- scale(wine[-1]) 
 head(df)
 wssplot(df) 
 
 set.seed(1234)
 nc <- NbClust(df, min.nc=2, max.nc=15, method="kmeans")
 fviz_nbclust(nc)
 
 set.seed(1234)
 fit.km <- kmeans(df, 3, nstart=24)
 
 fit.km$centers
 aggregate(wine[-1], by=list(cluster=fit.km$cluster), mean)
 
 library(ggplot2)
 library(tidyr)
 means <- as.data.frame(fit.km$centers)                            
 means$cluster <- 1:nrow(means)                                    
 
 plotdata <- gather(means, key="variable", value="value", -cluster)  
 
 ggplot(plotdata,                                                  
        aes(x=variable,
            y=value,
            fill=variable,
            group=cluster)) +
   geom_bar(stat="identity") +
   geom_hline(yintercept=0) +
   facet_wrap(~cluster) +
   theme_bw() +
   theme(axis.text.x=element_text(angle=90, vjust=0),
         legend.position="none") +
   labs(x="", y="Standardized scores",
        title = "Mean Cluster Profiles")
 
 # plot clusters
 library(factoextra)
 fviz_cluster(fit.km, data=df, labelsize=8)
 
 # evaluate clustering
 ct.km <- table(wine$Type, fit.km$cluster)
 ct.km   
 library(flexclust)
 randIndex(ct.km)
 
 library(cluster)
 set.seed(1234)
 fit.pam <- pam(wine[-1], k=3, stand=TRUE)       
 fit.pam$medoids
 
 ct.pam <- table(wine$Type, fit.pam$clustering)
 ct.pam
 randIndex(ct.pam)
 
 library(fMultivar)
 library(ggplot2)
 set.seed(1234)
 df <- rnorm2d(1000, rho=.5)
 df <- as.data.frame(df)
 ggplot(df, aes(x=V1, y=V2)) + 
   geom_point(alpha=.3) + theme_minimal() + 
   labs(title="Bivariate Normal Distribution with rho=0.5")
 
 
 wssplot(df)
 library(NbClust)
 library(factoextra)
 nc <- NbClust(df, min.nc=2, max.nc=15, method="kmeans")
 fviz_nbclust(nc)
 
 
 library(ggplot2)
 fit <- kmeans(df, 2)
 df$cluster <- factor(fit$cluster)
 ggplot(data=df, aes(x=V1, y=V2, color=cluster, shape=cluster)) +  
   theme_minimal() +
   geom_point(alpha=.5) + 
   ggtitle("Clustering of Bivariate Normal Data")
 
 