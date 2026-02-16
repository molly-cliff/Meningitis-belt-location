# Load required libraries
library(dplyr)
library(ggplot2)
library(reshape2)
library(terra)
library(tiff)
library(RStoolbox)
library(sf)
library(beepr)
library(raster)

# Set working directory
setwd("C:/Users/mvc32/OneDrive - University of Cambridge/Documents/Climate_meningitis_belt")


rastlist <- list.files(path = "windspeed", pattern='.nc$', full.names= TRUE)
allrasters <- stack(rastlist)
allrastersrotate <- rotate(allrasters)



africa_crs <- crs(allrastersrotate)
allrastersrotate <- projectRaster(allrastersrotate, crs = africa_crs)
r_resampledrotate <- aggregate(allrastersrotate, fact=5, fun = mean)
shape <- read_sf(dsn = ".", layer = "Shapefile_improved")
allrastersrotate <- crop(r_resampledrotate, shape)
allrastersrotate <- mask(allrastersrotate, shape)
allrastersrotate[is.na(allrastersrotate)] <- 0

# Perform PCA on the raster data for dimensionality reduction
rpc <- rasterPCA(allrastersrotate)
raster_stack <- stack(rpc$map)
summary(rpc$model)

# Extract the first two principal components
files_stack <- raster_stack[[1:2]]

# Prepare data for K-means clustering
km <- as.matrix(files_stack)
km[is.na(km)] <- mean(km, na.rm = TRUE)




####### testing here 
# Determine the optimal number of clusters using the elbow method
wss <- numeric(20)
for (i in 1:20) {
  kmeans_model <- kmeans(km, centers = i, nstart = 25, iter.max = 300)
  wss[i] <- sum(kmeans_model$withinss)
}

# Plot the elbow chart
plot(1:20, wss, type = "b", pch = 19, frame = FALSE, xlab = "Number of Clusters (k)", ylab = "Total Within Sum of Squares")

# Add a line for the potential elbow point
#abline(v = which(diff(wss) == max(diff(wss))) + 1, col = "red", lty = 2)

num_na <- sum(is.na(km))
#set seed makes this reproducible as kmeans clustering can vary


kmeans_result <- kmeans(km, centers = 15, nstart = 25, iter.max = 300)


library(cluster)
library(clusterCrit)
africa_mask <- rasterize(shape, files_stack[[1]], field = 1)

# Apply mask to all raster layers
files_stack <- mask(files_stack, africa_mask)
plot(files_stack)

library(cluster)
library(clusterCrit)
library(raster)
library(sf)

#------------------------------------------------------------
# 1. Prepare data
#------------------------------------------------------------
km <- as.matrix(files_stack)

km_mean <- mean(km, na.rm = TRUE)
km[is.na(km)] <-0

#------------------------------------------------------------
# 2. Define range of k
#------------------------------------------------------------
k_range <- 5:15

# Storage for results
elbow_wss   <- numeric(length(k_range))
sil_mean    <- numeric(length(k_range))
sil_median  <- numeric(length(k_range))
sil_no_out  <- numeric(length(k_range))
CH_index    <- numeric(length(k_range))
DB_index    <- numeric(length(k_range))
Dunn_index  <- numeric(length(k_range))

#------------------------------------------------------------
# 3. Detect outline cluster (silhouette ≈ 1)
#------------------------------------------------------------
detect_outline_cluster <- function(sil_obj, clusters) {
  
  sil_df <- data.frame(
    cluster = clusters,
    sil = sil_obj[, 3]
  )
  
  sil_by_cluster <- aggregate(sil ~ cluster, sil_df, mean)
  
  outline <- sil_by_cluster$cluster[sil_by_cluster$sil > 0.95]
  
  if (length(outline) == 0) return(NULL)
  return(outline)
}

#------------------------------------------------------------
# 4. Loop through all k-values
#------------------------------------------------------------
set.seed(420)

for (i in seq_along(k_range)) {
  
  k <- k_range[i]
  
  km_res <- kmeans(km, centers = k, nstart = 25, iter.max = 300)
  clusters <- km_res$cluster
  
  elbow_wss[i] <- sum(km_res$withinss)
  
  # Full silhouette (no sampling)
  sil <- silhouette(clusters, dist(km))
  sil_vals <- sil[, 3]
  
  sil_mean[i]   <- mean(sil_vals)
  sil_median[i] <- median(sil_vals)
  
  # ---- Detect outline cluster(s) ----
  outline_clusters <- detect_outline_cluster(sil, clusters)
  
  if (!is.null(outline_clusters)) {
    keep <- !(clusters %in% outline_clusters)
    sil_no_out[i] <- mean(sil_vals[keep], na.rm = TRUE)
  } else {
    sil_no_out[i] <- sil_mean[i]
  }
  
  # ---- Cluster criteria ----
  crit <- intCriteria(
    km,
    clusters,
    c("Calinski_Harabasz", "Davies_Bouldin", "Dunn")
  )
  
  CH_index[i]   <- crit$calinski_harabasz
  DB_index[i]   <- crit$davies_bouldin
  Dunn_index[i] <- crit$dunn
  
  cat("Finished k =", k, "\n")
}

#------------------------------------------------------------
# 5. Results table
#------------------------------------------------------------
results <- data.frame(
  k                = k_range,
  WSS              = elbow_wss,
  silhouette_mean  = sil_mean,
  silhouette_median= sil_median,
  silhouette_no_outline = sil_no_out,
  CalinskiHarabasz = CH_index,
  DaviesBouldin    = DB_index,
  Dunn             = Dunn_index
)

print(results)

#------------------------------------------------------------
# 6. Plot
#------------------------------------------------------------
par(mfrow=c(1,2))

# Elbow
plot(k_range, elbow_wss, type="b", pch=19,
     xlab="k", ylab="Total Within-Cluster Sum of Squares (WCSS)", main="Windspeed Elbow Curve")


# Silhouette without outline cluster
plot(k_range, sil_no_out, type="b", pch=19, 
     xlab="k", ylab="Mean Silhouette",
     main="Windspeed Silhouette Score")

par(mfrow=c(1,1))
library(cluster)
library(clusterCrit)
library(raster)

set.seed(420)

kmeans_result <- kmeans(km, centers =5,nstart = 25, iter.max = 300)
cluster_labels <- kmeans_result$cluster
sil <- silhouette(kmeans_result$cluster, dist(km))


similarity_matrix <- dist( kmeans_result$centers)
hc <- hclust(similarity_matrix)
reordered_labels <- cutree(hc,5) 
cluster_labels <- reordered_labels[cluster_labels]
cl = raster(files_stack)
cl[] = cluster_labels 

r_cluster <- setValues(cl, cluster_labels)
plot(r_cluster)
r_cluster <- mask(r_cluster, shape)
plot(hc)


cl2test<-data.frame(shape2, raster::extract(r_cluster, shape2, fun=modal, na.rm = TRUE))
cl2test$zonaltest<-cl2test$raster..extract.r_cluster..shape2..fun...modal..na.rm...TRUE.
cl2test$zonalcat <- with(cl2test, ifelse(zonaltest == 1, 'Class 4', 
                                         ifelse(zonaltest == 2, 'Class 5', 
                                                ifelse(zonaltest == 3, 'Class 1',
                                                       ifelse(zonaltest == 4, 'Class 3', 
                                                              ifelse(zonaltest == 5, 'Class 2',0))))))





cl2test <- subset(cl2test, COUNTRY != "Cabo Verde")
cl2test <- subset(cl2test, COUNTRY != "Mauritius")
cl2test <- subset(cl2test, COUNTRY != "Seychelles")
cl2test <- subset(cl2test, COUNTRY != "São Tomé and Príncipe")
cl2test <- subset(cl2test, COUNTRY != "Comoros")

cl2test_missing <- st_as_sf(cl2test)
sum(is.na(cl2test$extract.r_cluster..shape2..fun...modal..na.rm...TRUE.))
table(cl2test$zonalcat)


table(cl2test$zonalcat)
cl2testraster <- merge(cl2test,shape2,by="GID_2")
cl2testraster <- st_as_sf(cl2testraster)
cl2testraster2<-cl2testraster[ , c('COUNTRY.x','NAME_1.x', 'GID_2','NAME_2.x','zonalcat')]
total3test<-cl2testraster2[!duplicated(cl2testraster2[ , c("GID_2")]),]

table(total3test$zonalcat)
st_write(total3test, "windspeed5clusterstest.shp",append=FALSE)


