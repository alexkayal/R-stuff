# plotting SOM

mydata <- som.accel$codes 
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var)) 
for (i in 2:15) {
  wss[i] <- sum(kmeans(mydata, centers=i)$withinss)
}
plot(wss)

##
plot(som.accel, type="changes")
plot(som.accel, type="count")
plot(som.accel, type="codes")
plot(som.accel, type = "property", 
     property = som.accel$codes[,1], 
     main=names(som.accel$data)[1])

##

colors <- c("red", "green", "blue", "yellow", "brown")
plot(som.accel, type = "mapping", col = colors[accel.classes],
     pch = accel.classes, main = "accelerometer", keepMargins = TRUE)

## use hierarchical clustering to cluster the codebook vectors
som_cluster <- cutree(hclust(dist(som.accel$codes)), 3)
# plot these results:
plot(som.accel, type="mapping", bgcol = colors[som_cluster], main = "Clusters") 
