library(profvis)
library(dplyr)
library(raster)
library(ggplot2)
library(reshape2)
library(terra)
library(tiff)
library(RStoolbox)
library(sf)
setwd("C:/Users/mvc32/OneDrive - University of Cambridge/Documents/Climate_meningitis_belt/Humidity")



# Read in vapor pressure and temperature to calculate absolute humidity
vapor_pressure <- stack("cru_ts4.07.2001.2010.vap.dat.nc",
                        "cru_ts4.07.2011.2020.vap.dat.nc",
                        "cru_ts4.07.2021.2022.vap.dat.nc")
temperature <- stack("cru_ts4.07.2001.2010.tmp.dat.nc",
                     "cru_ts4.07.2011.2020.tmp.dat.nc",
                     "cru_ts4.07.2021.2022.tmp.dat.nc")


# Convert temperature to kelvin and vapor pressure to Pa
temp_K <- temperature + 273.15
vapor_pressure_pa <- vapor_pressure *100


# formual to calculate absolute humidity
Rw<-461.5 
absolute_humidity = vapor_pressure_pa/(Rw * temp_K )


setwd("C:/Users/mvc32/OneDrive - University of Cambridge/Documents")
# Crop and mask humidity raster data to Africa ADMN2 shapefile
shape <- read_sf(dsn = ".", layer = "Shapefile_improved")
allrasters <- mask(crop(absolute_humidity, shape), shape)
allrasters[is.na(allrasters)] <- 0

# Reduction of dimensionality of data
rpc <- rasterPCA(allrasters)
raster_stack <- stack(rpc$map)

# Stack rasters account for 95% of data variance
files_stack <- mask(stack(raster_stack[[1]], raster_stack[[2]]), shape)

# Extract k means for Africa, we will go on to test different numbers of clusters
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
abline(v = which(diff(wss) == max(diff(wss))) + 1, col = "red", lty = 2)


set.seed(133)

# Perform K-means clustering with 8 clusters
kmeans_result <- kmeans(km, centers = 8)
cluster_labels <- kmeans_result$cluster
similarity_matrix <- dist(kmeans_result$centers)
hc <- hclust(similarity_matrix)
reordered_labels <- cutree(hc, 8)
cluster_labels <- reordered_labels[cluster_labels]
cl <- raster(files_stack)
cl[] <- cluster_labels 

# Set cluster values on raster
r_cluster <- setValues(cl, cluster_labels)
r_cluster <- mask(r_cluster, shape)

# Plot dendrogram from hierarchical clustering
plot(hc)

# Extract most common cluster value for each district in Africa
cl2test <- data.frame(shape, extract(r_cluster, shape, fun = modal, na.rm = TRUE))
cl2test$zonaltest <- cl2test$extract.r_cluster..shape..fun...modal..na.rm...TRUE.


# Label zones based on hierarchical clustering
cl2test$zonalcat  <- with(cl2test, ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 1, 'Class 3', 
                                          ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 2, 'Class 7', 
                                                 ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE.== 3, 'Class 5',
                                                        ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 4, 'Class 6', 
                                                               ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 5, 'Class 8',
                                                                      ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==6 , 'Class 1',
                                                                             ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==7 , 'Class 2',
                                                                                    ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==8 , 'Class 4',
                                                                                           0)))))))))
# Remove NA values and islands
sum(is.na(cl2test$extract.r_cluster..shape..fun...modal..na.rm...TRUE.))
cl2test <- subset(cl2test, !COUNTRY %in% c("Cabo Verde", "Mauritius", "Seychelles", "São Tomé and Príncipe", "Comoros"))
sum(is.na(cl2test$extract.r_cluster..shape..fun...modal..na.rm...TRUE.))

# Merge shapefile and data to form logistic regression dataset for 8 clusters
cl2testraster <- merge(cl2test, shape, by = "GID_2")
cl2testraster <- st_as_sf(cl2testraster)
cl2testraster2 <- cl2testraster[, c('COUNTRY.x', 'NAME_1.x', 'GID_2', 'NAME_2.x', 'zonalcat')]
total3test <- cl2testraster2[!duplicated(cl2testraster2[, "GID_2"]), ]
st_write(total3test, "Absolutehumidity8clusters.shp", append = FALSE)


set.seed(333)

# Perform K-means clustering with 9 clusters
kmeans_result <- kmeans(km, centers = 9)
cluster_labels <- kmeans_result$cluster
similarity_matrix <- dist(kmeans_result$centers)
hc <- hclust(similarity_matrix)
reordered_labels <- cutree(hc, 9)
cluster_labels <- reordered_labels[cluster_labels]
cl <- raster(files_stack)
cl[] <- cluster_labels 

# Set cluster values on raster
r_cluster <- setValues(cl, cluster_labels)
r_cluster <- mask(r_cluster, shape)

# Plot dendrogram from hierarchical clustering
plot(hc)
# Extract most common cluster value for each district in Africa
cl2test <- data.frame(shape, extract(r_cluster, shape, fun = modal, na.rm = TRUE))
cl2test$zonaltest <- cl2test$extract.r_cluster..shape..fun...modal..na.rm...TRUE.

# Label zones based on hierarchical clustering
cl2test$zonalcat  <- with(cl2test, ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 1, 'Class 8', 
                                          ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 2, 'Class 5', 
                                                 ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE.== 3, 'Class 1',
                                                        ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 4, 'Class 3', 
                                                               ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 5, 'Class 4',
                                                                      ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==6 , 'Class 9',
                                                                             ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==7 , 'Class 2',
                                                                                    ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==8 , 'Class 7',
                                                                                           ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==9 , 'Class 6',
                                                                                           0))))))))))
# Remove NA values and islands
sum(is.na(cl2test$extract.r_cluster..shape..fun...modal..na.rm...TRUE.))
cl2test <- subset(cl2test, !COUNTRY %in% c("Cabo Verde", "Mauritius", "Seychelles", "São Tomé and Príncipe", "Comoros"))
sum(is.na(cl2test$extract.r_cluster..shape..fun...modal..na.rm...TRUE.))

# Merge shapefile and data to form logistic regression dataset for 9 clusters
cl2testraster <- merge(cl2test, shape, by = "GID_2")
cl2testraster <- st_as_sf(cl2testraster)
cl2testraster2 <- cl2testraster[, c('COUNTRY.x', 'NAME_1.x', 'GID_2', 'NAME_2.x', 'zonalcat')]
total3test <- cl2testraster2[!duplicated(cl2testraster2[, "GID_2"]), ]
st_write(total3test, "Absolutehumidity9clusters.shp", append = FALSE)




set.seed(3113)

# Perform K-means clustering with 10 clusters
kmeans_result <- kmeans(km, centers = 10)
cluster_labels <- kmeans_result$cluster
similarity_matrix <- dist(kmeans_result$centers)
hc <- hclust(similarity_matrix)
reordered_labels <- cutree(hc, 10)
cluster_labels <- reordered_labels[cluster_labels]
cl <- raster(files_stack)
cl[] <- cluster_labels 

# Set cluster values on raster
r_cluster <- setValues(cl, cluster_labels)
r_cluster <- mask(r_cluster, shape)

# Plot dendrogram from hierarchical clustering
plot(hc)
# Extract most common cluster value for each district in Africa
cl2test <- data.frame(shape, extract(r_cluster, shape, fun = modal, na.rm = TRUE))
cl2test$zonaltest <- cl2test$extract.r_cluster..shape..fun...modal..na.rm...TRUE.

# Label zones based on hierarchical clustering
cl2test$zonalcat  <- with(cl2test, ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 1, 'Class 4', 
                                          ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 2, 'Class 3', 
                                                 ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE.== 3, 'Class 9',
                                                        ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 4, 'Class 6', 
                                                               ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 5, 'Class 1',
                                                                      ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==6 , 'Class 5',
                                                                             ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==7 , 'Class 8',
                                                                                    ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==8 , 'Class 7',
                                                                                           ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==9 , 'Class 2',
                                                                                                  ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==10 , 'Class 10',  0)))))))))))
# Remove NA values and islands
sum(is.na(cl2test$extract.r_cluster..shape..fun...modal..na.rm...TRUE.))
cl2test <- subset(cl2test, !COUNTRY %in% c("Cabo Verde", "Mauritius", "Seychelles", "São Tomé and Príncipe", "Comoros"))
sum(is.na(cl2test$extract.r_cluster..shape..fun...modal..na.rm...TRUE.))

# Merge shapefile and data to form logistic regression dataset for 10 clusters
cl2testraster <- merge(cl2test, shape, by = "GID_2")
cl2testraster <- st_as_sf(cl2testraster)
cl2testraster2 <- cl2testraster[, c('COUNTRY.x', 'NAME_1.x', 'GID_2', 'NAME_2.x', 'zonalcat')]
total3test <- cl2testraster2[!duplicated(cl2testraster2[, "GID_2"]), ]
st_write(total3test, "Absolutehumidity10clusters.shp", append = FALSE)

# Perform K-means clustering with 11 clusters
set.seed(4)
kmeans_result <- kmeans(km, centers = 11)
cluster_labels <- kmeans_result$cluster
similarity_matrix <- dist( kmeans_result$centers)
hc <- hclust(similarity_matrix)
reordered_labels <- cutree(hc,11) 
cluster_labels <- reordered_labels[cluster_labels]
cl = raster(files_stack)
cl[] = cluster_labels 

# Set cluster values on raster
r_cluster <- setValues(cl, cluster_labels)
r_cluster <- mask(r_cluster, shape)
plot(r_cluster)
r_cluster <- mask(r_cluster, shape)
# Plot dendrogram from hierarchical clustering
plot(hc)

# Plot the rasterized column with a color scale
library(RColorBrewer)
magma_like_palette <- brewer.pal(11, "Paired")

# Plot the rasterized column with a color scale
plot(r_cluster, col = magma_like_palette)

# Extract most common cluster value for each district in Africa
detach("package:R.utils", unload=TRUE)
cl2test<-data.frame(shape,extract(r_cluster, shape, fun=modal, na.rm = TRUE))
cl2test$zonaltest<-cl2test$extract.r_cluster..shape..fun...modal..na.rm...TRUE.

# Label zones based on hierarchical clustering
cl2test$zonalcat  <- with(cl2test, ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 1, 'Class 10', 
                                          ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 2, 'Class 6', 
                                                 ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE.== 3, 'Class 5',
                                                        ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 4, 'Class 1', 
                                                               ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 5, 'Class 8',
                                                                      ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==6 , 'Class 11',
                                                                             ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==7 , 'Class 3',
                                                                                    ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==8 , 'Class 4',
                                                                                           ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==9 , 'Class 7',
                                                                                                  ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==10 , 'Class 9',
                                                                                                         ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==11 , 'Class 2',0))))))))))))
# Remove NA values and islands
sum(is.na(cl2test$extract.r_cluster..shape..fun...modal..na.rm...TRUE.))
cl2test <- subset(cl2test, !COUNTRY %in% c("Cabo Verde", "Mauritius", "Seychelles", "São Tomé and Príncipe", "Comoros"))
sum(is.na(cl2test$extract.r_cluster..shape..fun...modal..na.rm...TRUE.))

# Merge shapefile and data to form logistic regression dataset for 11 clusters
cl2testraster <- merge(cl2test, shape, by = "GID_2")
cl2testraster <- st_as_sf(cl2testraster)
cl2testraster2 <- cl2testraster[, c('COUNTRY.x', 'NAME_1.x', 'GID_2', 'NAME_2.x', 'zonalcat')]

# Replace values in rows with specified class label
rows_to_replace <- cl2testraster2$zonalcat %in% c("Class 2")
cl2testraster2$zonalcat[rows_to_replace] <- "Class 1"

table(cl2testraster2$zonalcat)
st_write(cl2testraster2, "Absolutehumidity11clusters.shp", append = FALSE)
st_write(total3test, "Humidityabsolute11.shp",append=FALSE)


# Perform K-means clustering with 12 clusters
set.seed(12)
kmeans_result <- kmeans(km, centers = 12)
cluster_labels <- kmeans_result$cluster
similarity_matrix <- dist( kmeans_result$centers)
hc <- hclust(similarity_matrix)
reordered_labels <- cutree(hc,12) 
cluster_labels <- reordered_labels[cluster_labels]
cl = raster(files_stack)
cl[] = cluster_labels 
# Set cluster values on raster
r_cluster <- setValues(cl, cluster_labels)
r_cluster <- mask(r_cluster, shape)
plot(r_cluster)
r_cluster <- mask(r_cluster, shape)
# Plot dendrogram from hierarchical clustering
plot(hc)

# Extract most common cluster value for each district in Africa
detach("package:R.utils", unload=TRUE)
cl2test<-data.frame(shape,extract(r_cluster, shape, fun=modal, na.rm = TRUE))
cl2test$zonaltest<-cl2test$extract.r_cluster..shape..fun...modal..na.rm...TRUE.

# Label zones based on hierarchical clustering

cl2test$zonalcat  <- with(cl2test, ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 1, 'Class 6', 
                                          ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 2, 'Class 2', 
                                                 ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE.== 3, 'Class 3',
                                                        ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 4, 'Class 10', 
                                                               ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 5, 'Class 4',
                                                                      ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==6 , 'Class 8',
                                                                             ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==7 , 'Class 1',
                                                                                    ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==8 , 'Class 9',
                                                                                           ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==9 , 'Class 7',
                                                                                                  ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==10 , 'Class 11',
                                                                                                         ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==11 , 'Class 12',
                                                                                                                ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==12 , 'Class 5',0)))))))))))))
#why are there so many NAs? data quality? how to fill in
# Remove NA values and islands
sum(is.na(cl2test$extract.r_cluster..shape..fun...modal..na.rm...TRUE.))
cl2test <- subset(cl2test, !COUNTRY %in% c("Cabo Verde", "Mauritius", "Seychelles", "São Tomé and Príncipe", "Comoros"))
sum(is.na(cl2test$extract.r_cluster..shape..fun...modal..na.rm...TRUE.))

# Merge shapefile and data to form logistic regression dataset for 11 clusters
cl2testraster <- merge(cl2test, shape, by = "GID_2")
cl2testraster <- st_as_sf(cl2testraster)
cl2testraster2 <- cl2testraster[, c('COUNTRY.x', 'NAME_1.x', 'GID_2', 'NAME_2.x', 'zonalcat')]





#set seed makes this reproducible as kmeans clustering can vary
set.seed(12)
kmeans_result <- kmeans(km, centers = 12)
cluster_labels <- kmeans_result$cluster
similarity_matrix <- dist( kmeans_result$centers)
hc <- hclust(similarity_matrix)
reordered_labels <- cutree(hc,12) 
cluster_labels <- reordered_labels[cluster_labels]
cl = raster(files_stack)
cl[] = cluster_labels 

r_cluster <- setValues(cl, cluster_labels)
r_cluster <- mask(r_cluster, shape)
plot(r_cluster)
r_cluster <- mask(r_cluster, shape)
plot(hc)

cl2test$zonalcat  <- with(cl2test, ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 1, 'Class 6', 
                                          ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 2, 'Class 2', 
                                                 ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE.== 3, 'Class 3',
                                                        ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 4, 'Class 10', 
                                                               ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. == 5, 'Class 4',
                                                                      ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==6 , 'Class 8',
                                                                             ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==7 , 'Class 1',
                                                                                    ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==8 , 'Class 9',
                                                                                           ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==9 , 'Class 7',
                                                                                                  ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==10 , 'Class 11',
                                                                                                         ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==11 , 'Class 12',
                                                                                                                ifelse(extract.r_cluster..shape..fun...modal..na.rm...TRUE. ==12 , 'Class 5',0)))))))))))))
# Remove NA values and islands
sum(is.na(cl2test$extract.r_cluster..shape..fun...modal..na.rm...TRUE.))
cl2test <- subset(cl2test, !COUNTRY %in% c("Cabo Verde", "Mauritius", "Seychelles", "São Tomé and Príncipe", "Comoros"))
sum(is.na(cl2test$extract.r_cluster..shape..fun...modal..na.rm...TRUE.))

# Merge shapefile and data to form logistic regression dataset for 11 clusters
cl2testraster <- merge(cl2test, shape, by = "GID_2")
cl2testraster <- st_as_sf(cl2testraster)
cl2testraster2 <- cl2testraster[, c('COUNTRY.x', 'NAME_1.x', 'GID_2', 'NAME_2.x', 'zonalcat')]

# Replace values in rows with specified class label
rows_to_replace <- cl2testraster2$zonalcat %in% c("Class 12")

# Replace values in those rows with new values
cl2testraster2$zonalcat[rows_to_replace] <- "Class 1"


table(cl2testraster2$zonalcat)
st_write(cl2testraster2, "Absolutehumidity12clusters.shp", append = FALSE)


