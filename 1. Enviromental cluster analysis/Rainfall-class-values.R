library(dplyr)
library(raster)
library(ggplot2)
library(reshape2)
library(terra)
library(tiff)
library(RStoolbox)
library(sf)
library(beepr)
# Set working directory
setwd("C:/Users/mvc32/Documents/OneDrive - University of Cambridge/Documents")

# Read in and stack rainfall raster data
rastlist <- list.files(path = "Rainfall", pattern = '.tif$', full.names = TRUE)
allrasters <- stack(rastlist)
allrasters <- aggregate(allrasters, fact = 2, fun = mean)

# Crop and mask aerosol raster data to Africa ADMN2 shapefile
shape <- read_sf(dsn = ".", layer = "Shapefile_improved")
allrasters <- crop(allrasters, shape)
allrasters <- mask(allrasters, shape)

# Dimensionality reduction of data using PCA
rpc <- rasterPCA(allrasters)
raster_stack <- stack(rpc$map)

# Stacking the raster layers
files_stack <- stack(raster_stack[[1]], raster_stack[[2]], raster_stack[[3]], raster_stack[[4]], raster_stack[[5]],
                    raster_stack[[6]], raster_stack[[7]], raster_stack[[8]], raster_stack[[9]], raster_stack[[10]],
                    raster_stack[[11]], raster_stack[[12]], raster_stack[[13]], raster_stack[[14]], raster_stack[[15]],
                    raster_stack[[16]], raster_stack[[17]], raster_stack[[18]], raster_stack[[19]], raster_stack[[20]],
                    raster_stack[[21]], raster_stack[[22]], raster_stack[[23]], raster_stack[[24]], raster_stack[[25]],
                    raster_stack[[26]])

# Masking the stacked raster layers
files_stack <- mask(files_stack, shape)

# Extract k means for Africa
km <- as.matrix(files_stack)
km[is.na(km)] <- mean(km, na.rm = TRUE)

# Determine the optimal number of clusters using the elbow method
wss <- numeric(20)
for (i in 1:20) {
  kmeans_model <- kmeans(km, centers = i, nstart = 20)
  wss[i] <- sum(kmeans_model$withinss)
}

# Plot the elbow chart
plot(1:20, wss, type = "b", pch = 19, frame = FALSE, xlab = "Number of Clusters (k)", ylab = "Total Within Sum of Squares")

set.seed(1)
# Perform K-means clustering with 6 centers
kmeans_result <- kmeans(km, centers = 6)
cluster_labels <- kmeans_result$cluster

# Calculate similarity matrix and perform hierarchical clustering
similarity_matrix <- dist(kmeans_result$centers)
hc <- hclust(similarity_matrix)
reordered_labels <- cutree(hc, 6) 
cluster_labels <- reordered_labels[cluster_labels]

# Assign cluster labels to raster
cl <- raster(files_stack)
cl[] = cluster_labels 

# Plot the clustered raster
plot(cl)

# Mask the raster to Africa shape
r_cluster <- mask(cl, shape)
plot(hc)

# Extract the most common cluster value for each district in Africa
cl2test <- data.frame(shape, extract(r_cluster, shape, fun = modal, na.rm = TRUE))
#hierachal clustering assign each cluster appropriately in order of hhow closely related they are based on dendorgram
cl2test$zonalcat  <- with(cl2test, ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 1, 'Class 2', 
                                          ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 2, 'Class 3', 
                                                 ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE.== 3, 'Class 4',
                                                        ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 4, 'Class 5', 
                                                               ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 5, 'Class 6',
                                                                      ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==6 , 'Class 1',
                         0 )))))))
# Check for missing values
sum(is.na(cl2test$extract.r_cluster..shape..fun...modal..na.rm...TRUE.))

# Remove islands
cl2test <- subset(cl2test, !(COUNTRY %in% c("Cabo Verde", "Mauritius", "Seychelles", "São Tomé and Príncipe", "Comoros")))

# Convert to sf object
cl2test_missing <- st_as_sf(cl2test)
sum(is.na(cl2test$extract.r_cluster..shape..fun...modal..na.rm...TRUE.))
table(cl2test$zonalcat)

# Merge shapefile and aerosol data
cl2testraster <- merge(cl2test, shape, by = "GID_2")
cl2testraster2 <- cl2testraster[, c('COUNTRY.x','NAME_1.x', 'GID_2','NAME_2.x','zonalcat')]
total3test <- cl2testraster2[!duplicated(cl2testraster2[, "GID_2"]), ]
st_write(total3test, "rainfall6clusers.shp",append=FALSE)


set.seed(2)

# Perform K-means clustering with 7 centers
kmeans_result <- kmeans(km, centers = 7)
cluster_labels <- kmeans_result$cluster

# Calculate similarity matrix and perform hierarchical clustering
similarity_matrix <- dist(kmeans_result$centers)
hc <- hclust(similarity_matrix)
reordered_labels <- cutree(hc, 7) 
cluster_labels <- reordered_labels[cluster_labels]

# Assign cluster labels to raster
cl <- raster(files_stack)
cl[] = cluster_labels 

# Plot the clustered raster
plot(cl)

# Mask the raster to Africa shape
r_cluster <- mask(cl, shape)
plot(hc)

# Extract the most common cluster value for each district in Africa
cl2test <- data.frame(shape, extract(r_cluster, shape, fun = modal, na.rm = TRUE))

# Assign hierarchical clustering classes
cl2test$zonalcat  <- with(cl2test, ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 1, 'Class 2', 
                                          ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 2, 'Class 4', 
                                                 ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE.== 3, 'Class 6',
                                                        ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 4, 'Class 5', 
                                                               ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 5, 'Class 1',
                                                                      ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==6 , 'Class 3',
                                                                             ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==7 , 'Class 7', 
                                                                              0 ))))))))
# Check for missing values
sum(is.na(cl2test$extract.r_cluster..shape..fun...modal..na.rm...TRUE.))

# Remove islands
cl2test <- subset(cl2test, !(COUNTRY %in% c("Cabo Verde", "Mauritius", "Seychelles", "São Tomé and Príncipe", "Comoros")))

# Convert to sf object
cl2test_missing <- st_as_sf(cl2test)
sum(is.na(cl2test$extract.r_cluster..shape..fun...modal..na.rm...TRUE.))
table(cl2test$zonalcat)

# Merge shapefile and aerosol data
cl2testraster <- merge(cl2test, shape, by = "GID_2")
cl2testraster2 <- cl2testraster[, c('COUNTRY.x','NAME_1.x', 'GID_2','NAME_2.x','zonalcat')]
total3test <- cl2testraster2[!duplicated(cl2testraster2[, "GID_2"]), ]
st_write(total3test, "rainfall7clusters.shp",append=FALSE)

set.seed(3)

# Perform K-means clustering with 8 centers
kmeans_result <- kmeans(km, centers = 8)
cluster_labels <- kmeans_result$cluster

# Calculate similarity matrix and perform hierarchical clustering
similarity_matrix <- dist(kmeans_result$centers)
hc <- hclust(similarity_matrix)
reordered_labels <- cutree(hc, 8) 
cluster_labels <- reordered_labels[cluster_labels]

# Assign cluster labels to raster
cl <- raster(files_stack)
cl[] = cluster_labels 

# Plot the clustered raster
plot(cl)

# Mask the raster to Africa shape
r_cluster <- mask(cl, shape)
plot(hc)

# Extract the most common cluster value for each district in Africa
cl2test <- data.frame(shape, extract(r_cluster, shape, fun = modal, na.rm = TRUE))

# Assign hierarchical clustering classes
cl2test$zonalcat  <- with(cl2test, ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 1, 'Class 4', 
                                          ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 2, 'Class 7', 
                                                 ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE.== 3, 'Class 1',
                                                        ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 4, 'Class 5', 
                                                               ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 5, 'Class 2',
                                                                      ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==6 , 'Class 8',
                                                                             ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==7 , 'Class 3',
                                                                                    ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==8 , 'Class 6',0 )))))))))
# Check for missing values
sum(is.na(cl2test$extract.r_cluster..shape..fun...modal..na.rm...TRUE.))

# Remove islands
cl2test <- subset(cl2test, !(COUNTRY %in% c("Cabo Verde", "Mauritius", "Seychelles", "São Tomé and Príncipe", "Comoros")))

# Convert to sf object
cl2test_missing <- st_as_sf(cl2test)
sum(is.na(cl2test$extract.r_cluster..shape..fun...modal..na.rm...TRUE.))
table(cl2test$zonalcat)

# Merge shapefile and aerosol data
cl2testraster <- merge(cl2test, shape, by = "GID_2")
cl2testraster2 <- cl2testraster[, c('COUNTRY.x','NAME_1.x', 'GID_2','NAME_2.x','zonalcat')]
total3test <- cl2testraster2[!duplicated(cl2testraster2[, "GID_2"]), ]
st_write(total3test, "rainfall8clusters.shp",append=FALSE)

set.seed(4)

# Perform K-means clustering with 6 centers
kmeans_result <- kmeans(km, centers = 9)
cluster_labels <- kmeans_result$cluster

# Calculate similarity matrix and perform hierarchical clustering
similarity_matrix <- dist(kmeans_result$centers)
hc <- hclust(similarity_matrix)
reordered_labels <- cutree(hc, 9) 
cluster_labels <- reordered_labels[cluster_labels]

# Assign cluster labels to raster
cl <- raster(files_stack)
cl[] = cluster_labels 

# Plot the clustered raster
plot(cl)

# Mask the raster to Africa shape
r_cluster <- mask(cl, shape)
plot(hc)

# Extract the most common cluster value for each district in Africa
cl2test <- data.frame(shape, extract(r_cluster, shape, fun = modal, na.rm = TRUE))
cl2test$zonalcat  <- with(cl2test, ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 1, 'Class 8', 
                                          ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 2, 'Class 1', 
                                                 ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE.== 3, 'Class 2',
                                                        ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 4, 'Class 3', 
                                                               ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 5, 'Class 7',
                                                                      ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==6 , 'Class 6',
                                                                             ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==7 , 'Class 9',
                                                                                    ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==8 , 'Class 4',
                                                                                           ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==9 , 'Class 5', 0 ))))))))))
# Check for missing values
sum(is.na(cl2test$extract.r_cluster..shape..fun...modal..na.rm...TRUE.))

# Remove islands
cl2test <- subset(cl2test, !(COUNTRY %in% c("Cabo Verde", "Mauritius", "Seychelles", "São Tomé and Príncipe", "Comoros")))

# Convert to sf object
cl2test_missing <- st_as_sf(cl2test)
sum(is.na(cl2test$extract.r_cluster..shape..fun...modal..na.rm...TRUE.))
table(cl2test$zonalcat)

# Merge shapefile and aerosol data
cl2testraster <- merge(cl2test, shape, by = "GID_2")
cl2testraster2 <- cl2testraster[, c('COUNTRY.x','NAME_1.x', 'GID_2','NAME_2.x','zonalcat')]
total3test <- cl2testraster2[!duplicated(cl2testraster2[, "GID_2"]), ]
st_write(total3test, "rainfall9clusters.shp",append=FALSE)



