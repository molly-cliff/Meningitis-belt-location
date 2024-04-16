library(dplyr)
library(raster)
library(ggplot2)
library(reshape2)
library(terra)
library(tiff)
library(RStoolbox)
library(sf)
library(raster)

# Set working directory
setwd("C:/Users/mvc32/OneDrive - University of Cambridge/Documents/Climate_meningitis_belt")

# Read in windspeed data
rastlist <- list.files(path = "windspeed", pattern='.nc$', full.names= TRUE)
allrasters <- stack(rastlist)

# Reduce resolution of the file
allrasters <- aggregate(allrasters, fact=5, fun = mean)

# Read in shapefile
shape <- read_sf("Shapefile_improved.shp")

# Crop and mask aerosol raster data to Africa shapefile
files_stack <- crop(allrasters, shape)
files_stack <- mask(files_stack, shape)

# Dimensionality reduction using PCA
rpc <- rasterPCA(files_stack)
raster_stack <- stack(rpc$map)

# Extracting rasters comprising 95% of the data
files_stack <- raster_stack[[1:12]]

# Extract k-means for Africa
km <- as.matrix(files_stack)
km[is.na(km)] <- mean(km, na.rm = TRUE)

# Determine optimal number of clusters using elbow method
wss <- numeric(20)
for (i in 1:20) {
  kmeans_model <- kmeans(km, centers = i, nstart = 20)
  wss[i] <- sum(kmeans_model$withinss)
}

# Plot elbow chart
plot(1:20, wss, type = "b", pch = 19, frame = FALSE, xlab = "Number of Clusters (k)", ylab = "Total Within Sum of Squares")
set.seed(1)

# Perform K-means clustering
kmeans_result <- kmeans(km, centers = 6)
cluster_labels <- kmeans_result$cluster
original_labels <- kmeans_result$cluster

# Calculate matching percentage
comparison <- original_labels == cluster_labels
matching_percentage <- sum(comparison) / length(comparison) * 100

# Determine similarity matrix using hierarchical clustering
similarity_matrix <- dist(kmeans_result$centers)
hc <- hclust(similarity_matrix)
reordered_labels <- cutree(hc, 6) 
cluster_labels <- reordered_labels[cluster_labels]

# Assign cluster labels to raster
cl <- raster(allrasters)
cl[] = cluster_labels 

# Plot dendrogram and raster
plot(hc)
r_cluster <- setValues(cl, cluster_labels)
plot(r_cluster)

# Extract most common cluster value for each district in Africa
cl2test <- data.frame(shape, extract(r_cluster, shape, fun = modal, na.rm = TRUE))
cl2test$zonaltest <- cl2test$extract.r_cluster..shape..fun...modal..na.rm...TRUE.

# Assign hierarchical clustering labels
cl2test$zonalcat <- with(cl2test, ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 1, 'Class 3', 
                                         ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 2, 'Class 1', 
                                                ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 3, 'Class 2',
                                                       ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 4, 'Class 5', 
                                                              ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 5, 'Class 4',
                                                                     ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 6, 'Class 6',
                                                                            0 )))))))

# Check for NA values
sum(is.na(cl2test$extract.r_cluster..shape..fun...modal..na.rm...TRUE.))

# Remove islands
cl2test <- subset(cl2test, COUNTRY != "Cabo Verde")
cl2test <- subset(cl2test, COUNTRY != "Mauritius")
cl2test <- subset(cl2test, COUNTRY != "Seychelles")
cl2test <- subset(cl2test, COUNTRY != "São Tomé and Príncipe")
cl2test <- subset(cl2test, COUNTRY != "Comoros")

# Merge shapefile and raster data
cl2testraster <- merge(cl2test, shape, by = "GID_2")
cl2testraster <- st_as_sf(cl2testraster)
cl2testraster2 <- cl2testraster[, c('COUNTRY.x', 'NAME_1.x', 'GID_2', 'NAME_2.x', 'zonalcat')]

# Remove duplicate rows and write to shapefile
total3test <- cl2testraster2[!duplicated(cl2testraster2[, c("GID_2")]),]
st_write(total3test, "windspeedcluster6.shp", append = FALSE)

set.seed(2)

# Perform K-means clustering with 7 centers
kmeans_result <- kmeans(km, centers = 7)
cluster_labels <- kmeans_result$cluster
original_labels <- kmeans_result$cluster

# Calculate matching percentage
comparison <- original_labels == cluster_labels
matching_percentage <- sum(comparison) / length(comparison) * 100

# Determine similarity matrix using hierarchical clustering
similarity_matrix <- dist(kmeans_result$centers)
hc <- hclust(similarity_matrix)
reordered_labels <- cutree(hc, 7) 
cluster_labels <- reordered_labels[cluster_labels]

# Assign cluster labels to raster
cl <- raster(allrasters)
cl[] = cluster_labels 

# Plot dendrogram and raster
plot(hc)
r_cluster <- setValues(cl, cluster_labels)
plot(r_cluster)

# Extract most common cluster value for each district in Africa
cl2test <- data.frame(shape, extract(r_cluster, shape, fun = modal, na.rm = TRUE))
cl2test$zonaltest <- cl2test$extract.r_cluster..shape..fun...modal..na.rm...TRUE.

# Assign hierarchical clustering labels
cl2test$zonalcat <- with(cl2test, ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 1, 'Class 4', 
                                         ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 2, 'Class 5', 
                                                ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 3, 'Class 1',
                                                       ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 4, 'Class 6', 
                                                              ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 5, 'Class 2',
                                                                     ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 6, 'Class 3',
                                                                            ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 7, 'Class 7',
                                                                            0 ))))))))

# Check for NA values
sum(is.na(cl2test$extract.r_cluster..shape..fun...modal..na.rm...TRUE.))

# Remove islands
cl2test <- subset(cl2test, COUNTRY != "Cabo Verde")
cl2test <- subset(cl2test, COUNTRY != "Mauritius")
cl2test <- subset(cl2test, COUNTRY != "Seychelles")
cl2test <- subset(cl2test, COUNTRY != "São Tomé and Príncipe")
cl2test <- subset(cl2test, COUNTRY != "Comoros")

# Merge shapefile and raster data
cl2testraster <- merge(cl2test, shape, by = "GID_2")
cl2testraster <- st_as_sf(cl2testraster)
cl2testraster2 <- cl2testraster[, c('COUNTRY.x', 'NAME_1.x', 'GID_2', 'NAME_2.x', 'zonalcat')]

# Remove duplicate rows and write to shapefile
total3test <- cl2testraster2[!duplicated(cl2testraster2[, c("GID_2")]),]
st_write(total3test, "windspeedcluster7.shp", append = FALSE)
set.seed(3)

# Perform K-means clustering with 8 centers
kmeans_result <- kmeans(km, centers = 8)
cluster_labels <- kmeans_result$cluster
original_labels <- kmeans_result$cluster

# Calculate matching percentage
comparison <- original_labels == cluster_labels
matching_percentage <- sum(comparison) / length(comparison) * 100

# Determine similarity matrix using hierarchical clustering
similarity_matrix <- dist(kmeans_result$centers)
hc <- hclust(similarity_matrix)
reordered_labels <- cutree(hc, 8) 
cluster_labels <- reordered_labels[cluster_labels]

# Assign cluster labels to raster
cl <- raster(allrasters)
cl[] <- cluster_labels 

# Plot dendrogram and raster
plot(hc)
r_cluster <- setValues(cl, cluster_labels)
plot(r_cluster)
r_cluster <- mask(r_cluster, shape)
plot(hc)

# Extract most common cluster value for each district in Africa
cl2test <- data.frame(shape, extract(r_cluster, shape, fun = modal, na.rm = TRUE))
cl2test$zonaltest <- cl2test$extract.r_cluster..shape..fun...modal..na.rm...TRUE.

# Assign hierarchical clustering labels
cl2test$zonalcat <- with(cl2test, ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 1, 'Class 3', 
                                         ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 2, 'Class 6', 
                                                ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 3, 'Class 4',
                                                       ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 4, 'Class 1', 
                                                              ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 5, 'Class 7',
                                                                     ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 6, 'Class 5',
                                                                            ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 7, 'Class 2',
                                                                                   ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 8, 'Class 8',
                                                                                   0 ))))))))

# Check for NA values
sum(is.na(cl2test$extract.r_cluster..shape..fun...modal..na.rm...TRUE.))

# Remove islands
cl2test <- subset(cl2test, COUNTRY != "Cabo Verde")
cl2test <- subset(cl2test, COUNTRY != "Mauritius")
cl2test <- subset(cl2test, COUNTRY != "Seychelles")
cl2test <- subset(cl2test, COUNTRY != "São Tomé and Príncipe")
cl2test <- subset(cl2test, COUNTRY != "Comoros")

# Merge shapefile and raster data
cl2testraster <- merge(cl2test, shape, by = "GID_2")
cl2testraster <- st_as_sf(cl2testraster)
cl2testraster2 <- cl2testraster[, c('COUNTRY.x', 'NAME_1.x', 'GID_2', 'NAME_2.x', 'zonalcat')]

# Remove duplicate rows and write to shapefile
total3test <- cl2testraster2[!duplicated(cl2testraster2[, c("GID_2")]),]

# Replace values in those rows with new values, replacing/editing classes with less than 50 districts
rows_to_replace <- total3test$zonalcat %in% c("Class 1")
total3test$zonalcat[rows_to_replace] <- "Class 2"
st_write(total3test, "windspeedcluster8.shp", append = FALSE)

set.seed(4)

# Perform K-means clustering with 9 centers
kmeans_result <- kmeans(km, centers = 9)
cluster_labels <- kmeans_result$cluster

# Determine similarity matrix using hierarchical clustering
similarity_matrix <- dist(kmeans_result$centers)
hc <- hclust(similarity_matrix)
reordered_labels <- cutree(hc, 9) 
cluster_labels <- reordered_labels[cluster_labels]

# Assign cluster labels to raster
cl <- raster(files_stack)
cl[] <- cluster_labels 

r_cluster <- setValues(cl, cluster_labels)
plot(r_cluster)
r_cluster <- mask(r_cluster, shape)
plot(hc)

# Extract most common cluster value for each district in Africa
cl2test <- data.frame(shape, extract(r_cluster, shape, fun = modal, na.rm = TRUE))
cl2test$zonaltest <- cl2test$extract.r_cluster..shape..fun...modal..na.rm...TRUE.

# Assign hierarchical clustering labels
cl2test$zonalcat <- with(cl2test, ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 1, 'Class 8', 
                                         ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 2, 'Class 5', 
                                                ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 3, 'Class 7',
                                                       ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 4, 'Class 2', 
                                                              ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 5, 'Class 6',
                                                                     ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 6, 'Class 4',
                                                                            ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 7, 'Class 9',
                                                                                   ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 8, 'Class 3',
                                                                                          ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 9 , 'Class 1',
                                                                                                  0))))))))))

# Check for NA values
sum(is.na(cl2test$extract.r_cluster..shape..fun...modal..na.rm...TRUE.))
table(cl2test$zonalcat)

# Remove islands
cl2test <- subset(cl2test, COUNTRY != "Cabo Verde")
cl2test <- subset(cl2test, COUNTRY != "Mauritius")
cl2test <- subset(cl2test, COUNTRY != "Seychelles")
cl2test <- subset(cl2test, COUNTRY != "São Tomé and Príncipe")
cl2test <- subset(cl2test, COUNTRY != "Comoros")

# Merge shapefile and raster data
cl2testraster <- merge(cl2test, shape, by = "GID_2")
cl2testraster <- st_as_sf(cl2testraster)
cl2testraster2 <- cl2testraster[, c('COUNTRY.x', 'NAME_1.x', 'GID_2', 'NAME_2.x', 'zonalcat')]

# Remove duplicate rows
total3test <- cl2testraster2[!duplicated(cl2testraster2[, c("GID_2")]),]

# Replace values in those rows with new values, replacing/editing classes with less than 50 districts
rows_to_replace <- total3test$zonalcat %in% c("Class 4")
total3test$zonalcat[rows_to_replace] <- "Class 5"

# Write to shapefile
st_write(total3test, "windspeed9classes.shp", append = FALSE)
set.seed(5)

# Perform K-means clustering with 10 centers
kmeans_result <- kmeans(km, centers = 10)
cluster_labels <- kmeans_result$cluster

# Determine similarity matrix using hierarchical clustering
similarity_matrix <- dist(kmeans_result$centers)
hc <- hclust(similarity_matrix)
reordered_labels <- cutree(hc, 10) 
cluster_labels <- reordered_labels[cluster_labels]

# Assign cluster labels to raster
cl <- raster(files_stack)
cl[] <- cluster_labels 

r_cluster <- setValues(cl, cluster_labels)
plot(r_cluster)
r_cluster <- mask(r_cluster, shape)
plot(hc)

# Extract most common cluster value for each district in Africa
cl2test <- data.frame(shape, extract(r_cluster, shape, fun = modal, na.rm = TRUE))
cl2test$zonaltest <- cl2test$extract.r_cluster..shape..fun...modal..na.rm...TRUE.

# Assign hierarchical clustering labels
cl2test$zonalcat <- with(cl2test, ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 1, 'Class 2', 
                                         ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 2, 'Class 4', 
                                                ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 3, 'Class 9',
                                                       ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 4, 'Class 6', 
                                                              ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 5, 'Class 8',
                                                                     ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 6, 'Class 1',
                                                                            ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 7, 'Class 7',
                                                                                   ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 8, 'Class 3',
                                                                                          ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 9, 'Class 5',
                                                                                                 ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 10, 'Class 10',
                                                                                                        0)))))))))))

# Check for NA values 
sum(is.na(cl2test$extract.r_cluster..shape..fun...modal..na.rm...TRUE.))
table(cl2test$zonalcat)

# Get rid of rows with missing values
rows_with_missing <- cl2test[is.na(cl2test$zonalcat), ]

# Merge shapefile and raster data
cl2testraster <- merge(cl2test, shape, by = "GID_2")
cl2testraster <- st_as_sf(cl2testraster)
cl2testraster2 <- cl2testraster[, c('COUNTRY.x', 'NAME_1.x', 'GID_2', 'NAME_2.x', 'zonalcat')]

# Write to shapefile
st_write(cl2testraster2, "windspeed10clusters.shp", append = FALSE)
set.seed(5)

# Perform K-means clustering with 11 centers
kmeans_result <- kmeans(km, centers = 11)
cluster_labels <- kmeans_result$cluster

# Determine similarity matrix using hierarchical clustering
similarity_matrix <- dist(kmeans_result$centers)
hc <- hclust(similarity_matrix)
reordered_labels <- cutree(hc, 11) 
cluster_labels <- reordered_labels[cluster_labels]

# Assign cluster labels to raster
cl <- raster(files_stack)
cl[] <- cluster_labels 

r_cluster <- setValues(cl, cluster_labels)
plot(r_cluster)
r_cluster <- mask(r_cluster, shape)
plot(hc)

# Extract most common cluster value for each district in Africa
cl2test <- data.frame(shape, extract(r_cluster, shape, fun = modal, na.rm = TRUE))
cl2test$zonaltest <- cl2test$extract.r_cluster..shape..fun...modal..na.rm...TRUE.

# Assign hierarchical clustering labels
cl2test$zonalcat <- with(cl2test, ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 1, 'Class 3', 
                                         ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 2, 'Class 7', 
                                                ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 3, 'Class 5',
                                                       ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 4, 'Class 8', 
                                                              ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 5, 'Class 9',
                                                                     ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 6, 'Class 1',
                                                                            ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 7, 'Class 10',
                                                                                   ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 8, 'Class 6',
                                                                                          ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 9, 'Class 4',
                                                                                                 ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 10, 'Class 11',
                                                                                                        ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 11, 'Class 2',
                                                                                                               0))))))))))))

# Check for NA values 
sum(is.na(cl2test$extract.r_cluster..shape..fun...modal..na.rm...TRUE.))
table(cl2test$zonalcat)

# Get rid of rows with missing values
rows_with_missing <- cl2test[is.na(cl2test$zonalcat), ]

# Merge shapefile and raster data
cl2testraster <- merge(cl2test, shape, by = "GID_2")
cl2testraster <- st_as_sf(cl2testraster)
cl2testraster2 <- cl2testraster[, c('COUNTRY.x', 'NAME_1.x', 'GID_2', 'NAME_2.x', 'zonalcat')]

# Write to shapefile
st_write(cl2testraster2, "windspeed11clusters.shp", append = FALSE)

