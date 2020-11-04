library(cluster)
library(purrr)


# Install and load packages
install.packages("NbClust")
library(NbClust)

# Load data with the name cldata

# Review data
summary(cldata)

# Scale data
testdata <- cldata[,2:9]
testdata <- scale(testdata)

# Determine number of clusters. Option 1: visual rule
#ELBOW ANALYSIS
wss <- (nrow(testdata)-1)*sum(apply(testdata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(testdata,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")

#Silhouette Analysis
sil_width <- map_dbl(2:10,  function(k){
  model <- pam(x = testdata, k = k)
  model$silinfo$avg.width
})

sil_df <- data.frame(
  k = 2:10,
  sil_width = sil_width
)

ggplot(sil_df, aes(x = k, y = sil_width)) +
  geom_line() +
  scale_x_continuous(breaks = 2:10)
     

# Determine number of clusters. Option 2: more frequent optimal number
res <- NbClust(testdata, diss=NULL, distance = "euclidean",
     min.nc=2, max.nc=12,
     method = "kmeans", index = "all")
res$Best.partition

# K-Means Cluster Analysis (based on the proposed number by NbCluster)
options(digits = 2)
fit <- kmeans(testdata, 3)
table(fit$cluster)
# Calculate average for each cluster
aggregate(cldata[,2:9],by=list(fit$cluster),FUN=mean)
# Add segmentation to dataset
cldata.w.cluster <- data.frame(cldata, fit$cluster)


#EXTRA STEP
clusplot(testdata, fit$cluster, color=TRUE, shade=TRUE, 
         labels=4, lines=0, main="K-means cluster plot")

#Hierarchical Clustering


cldata.dist <- dist(testdata)
cldata.hc <- hclust(cldata.dist, method="complete")

plot(cldata.hc)
rect.hclust(cldata.hc, k=4, border="red")

cldata.hc.segment <- cutree(cldata.hc, k=4)     # membership vector for 4 groups
table(cldata.hc.segment)

