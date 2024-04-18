library(dplyr)
library(raster)
library(ggplot2)
library(reshape2)
library(terra)
library(tiff)
library(RStoolbox)
library(sf)
library(purrr)
library(R.utils)
setwd("C:/Users/mvc32/OneDrive - University of Cambridge/Documents/Climate_meningitis_belt")
# Read in aerosol raster data
rastlist <- list.files(path = "aero", pattern='.nc$', full.names = TRUE)
allrasters <- stack(rastlist)

# Crop and mask aerosol raster data to Africa ADMN2 shapefile
shape <- read_sf(dsn = ".", layer = "Shapefile_improved")
allrasters <- mask(crop(allrasters, shape), shape)
allrasters[is.na(allrasters)] <- 0

# Reduction of dimensionality of data using PCA
rpc <- rasterPCA(allrasters)
raster_stack <- stack(rpc$map)

# Stack rasters which compromise 95% of raster data 
files_stack <- stack(raster_stack[[1:9]])
files_stack <- mask(files_stack, shape)

# Extract k means for Africa
km <- as.matrix(files_stack)
km[is.na(km)] <- mean(km, na.rm = TRUE)

# Determine the optimal number of clusters using the elbow method
wss <- numeric(20)
for (i in 1:20) {
  kmeans_model <- kmeans(km, centers = i, nstart = 10)
  wss[i] <- sum(kmeans_model$withinss)
}

# Plot the elbow chart
plot(1:20, wss, type = "b", pch = 19, frame = FALSE, xlab = "Number of Clusters (k)", ylab = "Total Within Sum of Squares")

# Uncomment the following line to add a line for the potential elbow point
# abline(v = which(diff(wss) == max(diff(wss))) + 1, col = "red", lty = 2)

library(raster)


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
detach("package:R.utils", unload=TRUE)
# Extract the most common cluster value for each district in Africa
cl2test <- data.frame(shape, raster::extract(r_cluster, shape, fun = modal, na.rm = TRUE))

# Assign hierarchical clustering classes
cl2test$zonalcat <- with(cl2test,
                         ifelse(raster..extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 1, 'Class 4', 
                                ifelse(raster..extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 2, 'Class 5', 
                                       ifelse(raster..extract.r_cluster..shape..fun...modal..na.rm...TRUE.== 3, 'Class 1',
                                              ifelse(raster..extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 4, 'Class 3', 
                                                     ifelse(raster..extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 5, 'Class 2',
                                                            ifelse(raster..extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==6 , 'Class 6', 0)))))))

# Check for missing values
sum(is.na(cl2test$raster..extract.r_cluster..shape..fun...modal..na.rm...TRUE.))

# Remove islands
cl2test <- subset(cl2test, !(COUNTRY %in% c("Cabo Verde", "Mauritius", "Seychelles", "São Tomé and Príncipe", "Comoros")))

# Convert to sf object
cl2test_missing <- st_as_sf(cl2test)
sum(is.na(cl2test$zonalcat))
table(cl2test$zonalcat)


# Merge shapefile and aerosol data
cl2testraster <- merge(cl2test, shape, by = "GID_2")
cl2testraster2 <- cl2testraster[, c('COUNTRY.x','NAME_1.x', 'GID_2','NAME_2.x','zonalcat')]
total3test <- cl2testraster2[!duplicated(cl2testraster2[, "GID_2"]), ]
# Replace values in rows with specified class label
rows_to_replace <- total3test$zonalcat %in% c("Class 6")
total3test$zonalcat[rows_to_replace] <- "Class 5"
st_write(total3test, "Aero6clusters.shp", append = FALSE)



# Set seed for reproducibility and perform K-means clustering with 7 centers
set.seed(2)
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
cl2test <- data.frame(shape, raster::extract(r_cluster, shape, fun = modal, na.rm = TRUE))

# Assign hierarchical clustering classes
cl2test$zonalcat <- with(cl2test,
                         ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 1, 'Class 6', 
                                ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 2, 'Class 2', 
                                       ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE.== 3, 'Class 6',
                                              ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 4, 'Class 1', 
                                                     ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 5, 'Class 3',
                                                            ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 6 , 'Class 4',
                                                                   ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 7 , 'Class 5', 0))))))))

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

st_write(total3test, "Aero7clusters.shp", append = FALSE)


# Set seed for reproducibility and perform K-means clustering with 8 centers
set.seed(3)
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
cl2test <- data.frame(shape, raster::extract(r_cluster, shape, fun = modal, na.rm = TRUE))

# Assign hierarchical clustering classes
cl2test$zonalcat <- with(cl2test,
                         ifelse(raster..extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 1, 'Class 2', 
                                ifelse(raster..extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 2, 'Class 7', 
                                       ifelse(raster..extract.r_cluster..shape..fun...modal..na.rm...TRUE.== 3, 'Class 3',
                                              ifelse(raster..extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 4, 'Class 5', 
                                                     ifelse(raster..extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 5, 'Class 6',
                                                            ifelse(raster..extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 6 , 'Class 8',
                                                                   ifelse(raster..extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 7 , 'Class 1',
                                                                          ifelse(raster..extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 8 , 'Class 4',0)))))))))

# Check for missing values
sum(is.na(cl2test$raster..extract.r_cluster..shape..fun...modal..na.rm...TRUE.))

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
# Replace values in rows with specified class label
rows_to_replace <- total3test$zonalcat %in% c("Class 8")
total3test$zonalcat[rows_to_replace] <- "Class 7"
table(total3test$zonalcat)
st_write(total3test, "Aero8clusters.shp", append = FALSE)



set.seed(41000000)
# Perform K-means clustering with 9 centers
kmeans_result <- kmeans(km, centers = 9)
cluster_labels <- kmeans_result$cluster
# Calculate similarity matrix and perform hierarchical clustering
similarity_matrix <- dist( kmeans_result$centers)
hc <- hclust(similarity_matrix)
reordered_labels <- cutree(hc,9) 
cluster_labels <- reordered_labels[cluster_labels]
# Assign cluster labels to raster
cl = raster(files_stack)
cl[] = cluster_labels 



# Plot the clustered raster
plot(cl)
# Set cluster values on raster
r_cluster <- setValues(cl, cluster_labels)
r_cluster <- mask(r_cluster, shape)
plot(r_cluster)
# Plot dendrogram from hierarchical clustering
plot(hc)


#extracts most common cluster value for each district in africa, most common as 
#opposed to average as clusters are distinct from each other

cl2test<-data.frame(shape,raster::extract(r_cluster, shape, fun=modal, na.rm = TRUE))
#plot(hc)
#labelling zones, into the linked classes based on hc
cl2test$zonalcat  <- with(cl2test, ifelse(raster..extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 1, 'Class 8', 
                                          ifelse(raster..extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 2, 'Class 6', 
                                                 ifelse(raster..extract.r_cluster..shape..fun...modal..na.rm...TRUE.== 3, 'Class 7',
                                                        ifelse(raster..extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 4, 'Class 3', 
                                                               ifelse(raster..extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 5, 'Class 2',
                                                                      ifelse(raster..extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==6 , 'Class 1',
                                                                             ifelse(raster..extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 7, 'Class 5', 
                                                                                    ifelse(raster..extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 8, 'Class 4',
                                                                                           ifelse(raster..extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==9 , 'Class 9',0))))))))))
# Remove NA values and islands
sum(is.na(cl2test$raster..extract.r_cluster..shape..fun...modal..na.rm...TRUE.))
cl2test <- subset(cl2test, !COUNTRY %in% c("Cabo Verde", "Mauritius", "Seychelles", "São Tomé and Príncipe", "Comoros"))


# Merge shapefile and data to form logistic regression dataset for 9 clusters
cl2testraster <- merge(cl2test, shape, by = "GID_2")
cl2testraster <- st_as_sf(cl2testraster)
cl2testraster2 <- cl2testraster[, c('COUNTRY.x', 'NAME_1.x', 'GID_2', 'NAME_2.x', 'zonalcat')]
total3test <- cl2testraster2[!duplicated(cl2testraster2[, "GID_2"]), ]
table(total3test$zonalcat)


rows_to_replace <- total3test$zonalcat %in% c("Class 3")

# Replace values in those rows with new values
total3test$zonalcat[rows_to_replace] <- "Class 2"
st_write(total3test, "Aeroclustertestingfinal.shp",append=FALSE)
st_write(total3test, "Aero9clusters.shp",append=FALSE)




# Set seed for reproducibility and perform K-means clustering with 10 centers
set.seed(5)
kmeans_result <- kmeans(km, centers = 10)
cluster_labels <- kmeans_result$cluster

# Calculate similarity matrix and perform hierarchical clustering
similarity_matrix <- dist(kmeans_result$centers)
hc <- hclust(similarity_matrix)
reordered_labels <- cutree(hc, 10) 
cluster_labels <- reordered_labels[cluster_labels]

# Assign cluster labels to raster
cl <- raster(files_stack)
cl[] = cluster_labels 

# Plot the clustered raster
plot(cl)

# Mask the raster to Africa shape
r_cluster <- mask(cl, shape)
plot(hc)


#extracts most common cluster value for each district in africa, most common as 
#opposed to average as clusters are distinct from each other

cl2test<-data.frame(shape,raster::extract(r_cluster, shape, fun=modal, na.rm = TRUE))
#plot(hc)
#labelling zones, into the linked classes based on hc
cl2test$zonalcat  <- with(cl2test, ifelse(raster..extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 1, 'Class 2', 
                                          ifelse(raster..extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 2, 'Class 3', 
                                                 ifelse(raster..extract.r_cluster..shape..fun...modal..na.rm...TRUE.== 3, 'Class 1',
                                                        ifelse(raster..extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 4, 'Class 4', 
                                                               ifelse(raster..extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 5, 'Class 6',
                                                                      ifelse(raster..extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==6 , 'Class 9',
                                                                             ifelse(raster..extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 7, 'Class 10', 
                                                                                    ifelse(raster..extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 8, 'Class 7',
                                                                                           ifelse(raster..extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==9 , 'Class 8',
                                                                                                  ifelse(raster..extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==10 , 'Class 5',0)))))))))))
# Remove NA values and islands
sum(is.na(cl2test$raster..extract.r_cluster..shape..fun...modal..na.rm...TRUE.))
cl2test <- subset(cl2test, !COUNTRY %in% c("Cabo Verde", "Mauritius", "Seychelles", "São Tomé and Príncipe", "Comoros"))


# Merge shapefile and data to form logistic regression dataset for 9 clusters
cl2testraster <- merge(cl2test, shape, by = "GID_2")
cl2testraster <- st_as_sf(cl2testraster)
cl2testraster2 <- cl2testraster[, c('COUNTRY.x', 'NAME_1.x', 'GID_2', 'NAME_2.x', 'zonalcat')]
total3test <- cl2testraster2[!duplicated(cl2testraster2[, "GID_2"]), ]
table(total3test$zonalcat)
rows_to_replace <- total3test$zonalcat %in% c("Class 7")

# Replace values in those rows with new values
total3test$zonalcat[rows_to_replace] <- "Class 8"

table(total3test$zonalcat)
st_write(total3test, "Aero10clusters.shp", append = FALSE)



# Set seed for reproducibility and perform K-means clustering with 11 centers
set.seed(6)
kmeans_result <- kmeans(km, centers = 11)
cluster_labels <- kmeans_result$cluster

# Calculate similarity matrix and perform hierarchical clustering
similarity_matrix <- dist(kmeans_result$centers)
hc <- hclust(similarity_matrix)
reordered_labels <- cutree(hc, 11) 
cluster_labels <- reordered_labels[cluster_labels]

# Assign cluster labels to raster
cl <- raster(files_stack)
cl[] = cluster_labels 

# Plot the clustered raster
plot(cl)

# Mask the raster to Africa shape
r_cluster <- mask(cl, shape)
plot(hc)



#extracts most common cluster value for each district in africa, most common as 
#opposed to average as clusters are distinct from each other

cl2test<-data.frame(shape,raster::extract(r_cluster, shape, fun=modal, na.rm = TRUE))
#plot(hc)
#labelling zones, into the linked classes based on hc
cl2test$zonalcat  <- with(cl2test, ifelse(raster..extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 1, 'Class 3', 
                                          ifelse(raster..extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 2, 'Class 5', 
                                                 ifelse(raster..extract.r_cluster..shape..fun...modal..na.rm...TRUE.== 3, 'Class 7',
                                                        ifelse(raster..extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 4, 'Class 10', 
                                                               ifelse(raster..extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 5, 'Class 1',
                                                                      ifelse(raster..extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==6 , 'Class 8',
                                                                             ifelse(raster..extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 7, 'Class 6', 
                                                                                    ifelse(raster..extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 8, 'Class 2',
                                                                                           ifelse(raster..extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==9 , 'Class 9',
                                                                                                  ifelse(raster..extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==10 , 'Class 11',
                                                                                                         ifelse(raster..extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==11 , 'Class 4',0))))))))))))
# Remove NA values and islands
sum(is.na(cl2test$raster..extract.r_cluster..shape..fun...modal..na.rm...TRUE.))
cl2test <- subset(cl2test, !COUNTRY %in% c("Cabo Verde", "Mauritius", "Seychelles", "São Tomé and Príncipe", "Comoros"))


# Merge shapefile and data to form logistic regression dataset for 9 clusters
cl2testraster <- merge(cl2test, shape, by = "GID_2")
cl2testraster <- st_as_sf(cl2testraster)
cl2testraster2 <- cl2testraster[, c('COUNTRY.x', 'NAME_1.x', 'GID_2', 'NAME_2.x', 'zonalcat')]
total3test <- cl2testraster2[!duplicated(cl2testraster2[, "GID_2"]), ]
table(total3test$zonalcat)
rows_to_replace <- total3test$zonalcat %in% c("Class 9")

# Replace values in those rows with new values
total3test$zonalcat[rows_to_replace] <- "Class 8"

table(total3test$zonalcat)
st_write(total3test, "Aero11clusters.shp", append = FALSE)
