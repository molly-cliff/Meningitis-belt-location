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
library(dplyr)
library(raster)
library(ggplot2)
library(reshape2)
library(terra)
library(tiff)
library(RStoolbox)
library(sf)
setwd("C:/Users/mvc32/OneDrive - University of Cambridge/Documents/Climate_meningitis_belt")
rastlist <- list.files(path = "Rainfall", pattern='.tif$', all.files=TRUE, full.names=TRUE)


allrasters <- stack(rastlist)

allrasters2 <- aggregate(allrasters, fact=2, fun = mean)
r_resampled <- raster::resample(allrasters, allrasters2, method = "bilinear")
shape <- read_sf(dsn = ".", layer = "Shapefile_improved")

# Crop and mask the resampled raster to the shape of Africa
allrasters <- crop(r_resampled, shape)
allrasters <- mask(allrasters, shape)
allrasters[is.na(allrasters)] <- 0

# Perform Principal Component Analysis (PCA) on the raster data
rpc <- rasterPCA(allrasters, center = TRUE)
summary(rpc$model)
raster_stack <- stack(rpc$map)


summary(rpc$model)
raster_stack <- stack(rpc$map)
files_stack <- raster_stack[[1:22]]




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
set.seed(42)

# ---- SAMPLE rows for cluster evaluation ----
eval_n <- min(5000, nrow(km)) 
eval_id <- sample(seq_len(nrow(km)), eval_n)

km_eval <- km[eval_id, ]   # small matrix for distances

for (i in seq_along(k_range)) {
  
  k <- k_range[i]
  
  # Run k-means on ALL pixels
  km_res <- kmeans(km, centers = k, nstart = 25, iter.max = 300)
  clusters <- km_res$cluster
  
  elbow_wss[i] <- sum(km_res$withinss)
  
  # ---- Silhouette on sample only ----
  sil <- silhouette(clusters[eval_id], dist(km_eval))
  sil_vals <- sil[,3]
  
  sil_mean[i]   <- mean(sil_vals)
  sil_median[i] <- median(sil_vals)
  
  # Detect outline clusters
  outline_clusters <- detect_outline_cluster(sil, clusters[eval_id])
  if (!is.null(outline_clusters)) {
    keep <- !(clusters[eval_id] %in% outline_clusters)
    sil_no_out[i] <- mean(sil_vals[keep], na.rm = TRUE)
  } else {
    sil_no_out[i] <- sil_mean[i]
  }
  
  # ---- Cluster criteria on sample only ----
  crit <- intCriteria(
    km_eval,
    clusters[eval_id],
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
     xlab="k", ylab="Total Within-Cluster Sum of Squares (WCSS)", main="Rainfall Elbow Curve")


# Silhouette without outline cluster
plot(k_range, sil_no_out, type="b", pch=19, 
     xlab="k", ylab="Mean Silhouette",
     main="Rainfall Silhouette Score")

par(mfrow=c(1,1))


# Perform K-means clustering with 6 centers
set.seed(42)
kmeans_result <- kmeans(km, centers = 6)
cluster_labels <- kmeans_result$cluster

# Compare the original and new cluster labels
original_labels <- kmeans_result$cluster
comparison <- original_labels == cluster_labels
matching_percentage <- sum(comparison) / length(comparison) * 100

# Determine similarity matrix using hierarchical clustering
similarity_matrix <- dist(kmeans_result$centers)
hc <- hclust(similarity_matrix)
reordered_labels <- cutree(hc, 6)
cluster_labels <- reordered_labels[cluster_labels]

# Assign cluster labels to raster
cl <- raster(allrasters)
cl[] <- cluster_labels
plot(hc)
r_cluster <- setValues(cl, cluster_labels)
plot(r_cluster)
r_cluster <- mask(r_cluster, shape)
plot(hc)

# Extract the most common cluster value for each district in Africa
cl2test <- data.frame(shape, extract(r_cluster, shape, fun = modal, na.rm = TRUE))
cl2test$zonaltest <- cl2test$extract.r_cluster..shape..fun...modal..na.rm...TRUE.
plot(hc)

# Label the zones based on the hierarchical clustering
cl2test$zonalcat <- with(cl2test, ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 1, 'Class 3', 
                                         ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 2, 'Class 5', 
                                                ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 3, 'Class 6',
                                                       ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 4, 'Class 4', 
                                                              ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 5, 'Class 1',
                                                                     ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 6, 'Class 2',
                                                                            0 )))))))

# Check for NA values and the distribution of the zonal categories
sum(is.na(cl2test$extract.r_cluster..shape..fun...modal..na.rm...TRUE.))
table(cl2test$zonalcat)

# Remove islands from the data
cl2test <- subset(cl2test, COUNTRY != "Cabo Verde")
cl2test <- subset(cl2test, COUNTRY != "Mauritius")
cl2test <- subset(cl2test, COUNTRY != "Seychelles")
cl2test <- subset(cl2test, COUNTRY != "São Tomé and Príncipe")
cl2test <- subset(cl2test, COUNTRY != "Comoros")

# Merge shapefile and raster data
cl2testraster <- merge(cl2test, shape, by = "GID_2")
cl2testraster <- st_as_sf(cl2testraster)
cl2testraster2 <- cl2testraster[, c('COUNTRY.x', 'NAME_1.x', 'GID_2', 'NAME_2.x', 'zonalcat')]
total3test <- cl2testraster2[!duplicated(cl2testraster2[, c("GID_2")]),]

table(total3test$zonalcat)
st_write(total3test, "rainfall6clusterstest.shp",append=FALSE)
