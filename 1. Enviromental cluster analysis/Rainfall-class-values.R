library(dplyr)
library(ggplot2)
library(reshape2)
library(terra)
library(tiff)
library(RStoolbox)
library(sf)
library(beepr)
library(raster)

# List all .tif files in the "Rainfall" directory
rastlist <- list.files(path = "Rainfall", pattern='.tif$', all.files=TRUE, full.names=TRUE)

# Read and stack all the raster files
allrasters <- stack(rastlist)

# Aggregate the raster data by a factor of 2 using the mean function
allrasters2 <- aggregate(allrasters, fact=2, fun = mean)

# Perform bilinear interpolation to resample the raster
r_resampled <- resample(allrasters, allrasters2, method = "bilinear")

# Read the shapefile for Africa
shape <- read_sf(dsn = ".", layer = "Shapefile_improved")

# Crop and mask the resampled raster to the shape of Africa
allrasters <- crop(r_resampled, shape)
allrasters <- mask(allrasters, shape)

# Replace NA values in the aggregated raster with 0
allrasters2[is.na(allrasters2)] <- 0

# Perform Principal Component Analysis (PCA) on the raster data
rpc <- rasterPCA(allrasters2, center = TRUE)

# Print summary of the PCA model
summary(rpc$model)

# Stack the PCA result into a raster stack
raster_stack <- stack(rpc$map)

# Select the first 22 components from the raster stack
files_stack <- raster_stack[[1:22]]

# Assign the selected components back to allrasters
allrasters <- files_stack

# Convert the raster stack to a matrix for K-means clustering
km <- as.matrix(allrasters)

# Fill NA values in the matrix with the mean value of the matrix
num_na <- sum(is.na(km))
km[is.na(km)] <- mean(km, na.rm = TRUE)
num_na <- sum(is.na(km))

# Calculate total within sum of squares (WSS) for different numbers of clusters
wss <- numeric(15)
for (i in 1:15) {
  kmeans_model <- kmeans(km, centers = i, nstart = 15)
  wss[i] <- sum(kmeans_model$withinss)
}

# Plot the WSS vs. number of clusters
plot(1:15, wss, type = "b", pch = 15, frame = FALSE, xlab = "Number of Clusters (k)", ylab = "Total Within Sum of Squares", main = "Rainfall K-means Cluster Testing")

# Highlight points 6-9 in blue
points(6:9, wss[6:9], col = "blue", pch = 15)

# Add a legend to the plot
legend("topright", legend = c("WSS", "Tested k-means clusters 6-9"), col = c("black", "blue"), pch = c(15, 15))

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
                                          ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 2, 'Class 4', 
                                                 ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 3, 'Class 1',
                                                        ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 4, 'Class 5', 
                                                               ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 5, 'Class 6',
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
st_write(total3test, "rainfallbilinear.shp", append = FALSE)

# Perform K-means clustering with 7 centers
set.seed(432)
kmeans_result <- kmeans(km, centers = 7)
cluster_labels <- kmeans_result$cluster

# Repeat the clustering and extraction process for 7 centers
original_labels <- kmeans_result$cluster
comparison <- original_labels == cluster_labels
matching_percentage <- sum(comparison) / length(comparison) * 100
similarity_matrix <- dist(kmeans_result$centers)
hc <- hclust(similarity_matrix)
reordered_labels <- cutree(hc, 7)
cluster_labels <- reordered_labels[cluster_labels]
cl <- raster(allrasters)
cl[] <- cluster_labels
r_cluster <- setValues(cl, cluster_labels)
plot(r_cluster)
r_cluster <- mask(r_cluster, shape)
plot(hc)
cl2test <- data.frame(shape, extract(r_cluster, shape, fun = modal, na.rm = TRUE))
cl2test$zonaltest <- cl2test$extract.r_cluster..shape..fun...modal..na.rm...TRUE.
plot(hc)
cl2test$zonalcat <- with(cl2test, ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 1, 'Class 2', 
                                          ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 2, 'Class 4', 
                                                 ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 3, 'Class 6',
                                                        ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 4, 'Class 5', 
                                                               ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 5, 'Class 1',
                                                                      ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 6, 'Class 3',
                                                                             ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 7, 'Class 7', 
                                                                                    0 ))))))))
sum(is.na(cl2test$extract.r_cluster..shape..fun...modal..na.rm...TRUE.))
table(cl2test$zonalcat)
cl2test <- subset(cl2test, COUNTRY != "Cabo Verde")
cl2test <- subset(cl2test, COUNTRY != "Mauritius")
cl2test <- subset(cl2test, COUNTRY != "Seychelles")
cl2test <- subset(cl2test, COUNTRY != "São Tomé and Príncipe")
cl2test <- subset(cl2test, COUNTRY != "Comoros")
cl2testraster <- merge(cl2test, shape, by = "GID_2")
cl2testraster <- st_as_sf(cl2testraster)
cl2testraster2 <- cl2testraster[, c('COUNTRY.x', 'NAME_1.x', 'GID_2', 'NAME_2.x', 'zonalcat')]
total3test <- cl2testraster2[!duplicated(cl2testraster2[, c("GID_2")]),]
st_write(total3test, "rainfall7clusters.shp", append = FALSE)

# Perform K-means clustering with 8 centers
set.seed(42342)
kmeans_result <- kmeans(km, centers = 8)
cluster_labels <- kmeans_result$cluster

# Repeat the clustering and extraction process for 8 centers
original_labels <- kmeans_result$cluster
comparison <- original_labels == cluster_labels
matching_percentage <- sum(comparison) / length(comparison) * 100
similarity_matrix <- dist(kmeans_result$centers)
hc <- hclust(similarity_matrix)
reordered_labels <- cutree(hc, 8)
cluster_labels <- reordered_labels[cluster_labels]
cl <- raster(allrasters)
cl[] <- cluster_labels
plot(hc)
r_cluster <- setValues(cl, cluster_labels)
plot(r_cluster)
r_cluster <- mask(r_cluster, shape)
plot(hc)
cl2test <- data.frame(shape, extract(r_cluster, shape, fun = modal, na.rm = TRUE))
cl2test$zonaltest <- cl2test$extract.r_cluster..shape..fun...modal..na.rm...TRUE.
plot(hc)
cl2test$zonalcat <- with(cl2test, ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 1, 'Class 4', 
                                          ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 2, 'Class 7', 
                                                 ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 3, 'Class 1',
                                                        ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 4, 'Class 5', 
                                                               ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 5, 'Class 2',
                                                                      ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 6, 'Class 8',
                                                                             ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 7, 'Class 3',
                                                                                    ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 8, 'Class 6', 0)))))))))
cl2test <- subset(cl2test, COUNTRY != "Cabo Verde")
cl2test <- subset(cl2test, COUNTRY != "Mauritius")
cl2test <- subset(cl2test, COUNTRY != "Seychelles")
cl2test <- subset(cl2test, COUNTRY != "São Tomé and Príncipe")
cl2test <- subset(cl2test, COUNTRY != "Comoros")
cl2testraster <- merge(cl2test, shape, by = "GID_2")
cl2testraster <- st_as_sf(cl2testraster)
cl2testraster2 <- cl2testraster[, c('COUNTRY.x', 'NAME_1.x', 'GID_2', 'NAME_2.x', 'zonalcat')]
total3test <- cl2testraster2[!duplicated(cl2testraster2[, c("GID_2")]),]
st_write(total3test, "rainfall8clusters.shp", append = FALSE)

# Perform K-means clustering with 9 centers
set.seed(222)
kmeans_result <- kmeans(km, centers = 9)
cluster_labels <- kmeans_result$cluster

# Repeat the clustering and extraction process for 9 centers
original_labels <- kmeans_result$cluster
comparison <- original_labels == cluster_labels
matching_percentage <- sum(comparison) / length(comparison) * 100
similarity_matrix <- dist(kmeans_result$centers)
hc <- hclust(similarity_matrix)
reordered_labels <- cutree(hc, 9)
cluster_labels <- reordered_labels[cluster_labels]
cl <- raster(allrasters)
cl[] <- cluster_labels
plot(hc)
r_cluster <- setValues(cl, cluster_labels)
plot(r_cluster)
cl2test <- data.frame(shape, extract(r_cluster, shape, fun = modal, na.rm = TRUE))
cl2test$zonaltest <- cl2test$extract.r_cluster..shape..fun...modal..na.rm...TRUE.
plot(hc)
cl2test$zonalcat <- with(cl2test, ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 1, 'Class 8', 
                                          ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 2, 'Class 1', 
                                                 ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 3, 'Class 2',
                                                        ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 4, 'Class 3', 
                                                               ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 5, 'Class 7',
                                                                      ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 6, 'Class 6',
                                                                             ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 7, 'Class 9',
                                                                                    ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 8, 'Class 4',
                                                                                           ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 9, 'Class 5', 0)))))))))
cl2test <- subset(cl2test, COUNTRY != "Cabo Verde")
cl2test <- subset(cl2test, COUNTRY != "Mauritius")
cl2test <- subset(cl2test, COUNTRY != "Seychelles")
cl2test <- subset(cl2test, COUNTRY != "São Tomé and Príncipe")
cl2test <- subset(cl2test, COUNTRY != "Comoros")
cl2testraster <- merge(cl2test, shape, by = "GID_2")
cl2testraster <- st_as_sf(cl2testraster)
cl2testraster2 <- cl2testraster[, c('COUNTRY.x', 'NAME_1.x', 'GID_2', 'NAME_2.x', 'zonalcat')]
total3test <- cl2testraster2[!duplicated(cl2testraster2[, c("GID_2")]),]
st_write(total3test, "rainfall9clusters.shp", append = FALSE)

