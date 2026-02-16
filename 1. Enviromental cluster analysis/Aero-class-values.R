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


#reduction of dimensionality of data
rpc <- rasterPCA(allrasters)
summary(rpc$model)
raster_stack <- stack(rpc$map)

test <- raster_stack[[1]]
test2 <- raster_stack[[2]]
test3 <- raster_stack[[3]]
test4 <- raster_stack[[4]]
test5 <- raster_stack[[5]]
test6 <- raster_stack[[6]]
test7 <- raster_stack[[7]]
test8 <- raster_stack[[8]]
test9 <- raster_stack[[9]]


files_stack <- stack(test,test2,test3,test4,test5,test6,test7,test8,test9)
files_stack<-crop(files_stack, shape)
files_stack<-mask(files_stack, shape)

#extract k means for africa, to create 20 different clusters of rainfall

km<-as.matrix(files_stack)
num_na <- sum(is.na(km))
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

#------------------------------------------------------------
# 1. Prepare data
#------------------------------------------------------------
km <- as.matrix(files_stack)

km_mean <- mean(km, na.rm = TRUE)
km[is.na(km)] <- km_mean

#------------------------------------------------------------
# 2. Define range of k to test
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
# 3. Helper function to detect outline cluster (very high silhouette)
#------------------------------------------------------------
detect_outline_cluster <- function(sil_values) {
  # silhouette near 1 is suspicious in raster applications
  high_idx <- which(sil_values > 0.9)
  if (length(high_idx) == 0) return(NULL)
  return(high_idx)
}

#------------------------------------------------------------
# 4. Loop through all k-values
#------------------------------------------------------------
set.seed(41000000)

for (i in seq_along(k_range)) {
  
  k <- k_range[i]
  
  # Run k-means
  km_res <- kmeans(km, centers = k, nstart = 25, iter.max = 300)
  
  # Elbow method (total WSS)
  elbow_wss[i] <- sum(km_res$withinss)
  
  # Silhouette (sample if too large)
  # if (nrow(km) > 4000) {
  #   idx <- sample(nrow(km), 4000)
  #   sil <- silhouette(km_res$cluster[idx], dist(km[idx, ]))
  #  } else {
  #    sil <- silhouette(km_res$cluster, dist(km))
  #  }
  sil <- silhouette(km_res$cluster, dist(km))
  sil_vals <- sil[, 3]
  sil_mean[i]   <- mean(sil_vals)
  sil_median[i] <- median(sil_vals)
  
  # Detect outline cluster
  out_idx <- detect_outline_cluster(sil_vals)
  
  if (!is.null(out_idx)) {
    sil_no_out[i] <- mean(sil_vals[-out_idx], na.rm = TRUE)
  } else {
    sil_no_out[i] <- mean(sil_vals)
  }
  
  # Other cluster validity indices (using clusterCrit)
  crit <- intCriteria(as.matrix(km), km_res$cluster,
                      c("Calinski_Harabasz", "Davies_Bouldin", "Dunn"))
  
  CH_index[i]   <- crit$calinski_harabasz
  DB_index[i]   <- crit$davies_bouldin
  Dunn_index[i] <- crit$dunn
  
  cat("Finished k =", k, "\n")
}

#------------------------------------------------------------
# 5. Combine results into a clean table
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
# 6. Plot key indicators
#------------------------------------------------------------

par(mfrow=c(1,2))

# Elbow
plot(k_range, elbow_wss, type="b", pch=19,
     xlab="k", ylab="Total Within-Cluster Sum of Squares (WCSS)", main="Aerosol Optical Depth Elbow Curve")


# Silhouette without outline cluster
plot(k_range, sil_no_out, type="b", pch=19, 
     xlab="k", ylab="Mean Silhouette",
     main="Aerosol Optical Dept Silhouette Score")
par(mfrow=c(1,1))



set.seed(41000000)

# Perform K-means clustering with 6 centers

kmeans_result <- kmeans(km, centers =8,nstart = 25, iter.max = 300)
cluster_labels <- kmeans_result$cluster



similarity_matrix <- dist( kmeans_result$centers)
hc <- hclust(similarity_matrix)
reordered_labels <- cutree(hc,8) 
cluster_labels <- reordered_labels[cluster_labels]
cl = raster(files_stack)
cl[] = cluster_labels 

r_cluster <- setValues(cl, cluster_labels)
plot(r_cluster)
r_cluster <- mask(r_cluster, shape)
plot(hc)


cl2test<-data.frame(shape,raster::extract(r_cluster, shape, fun=modal, na.rm = TRUE))
table(cl2test$raster..extract.r_cluster..shape..fun...modal..na.rm...TRUE.)



cl2test<-data.frame(shape2, raster::extract(r_cluster, shape2, fun=modal, na.rm = TRUE))
cl2test$zonaltest<-cl2test$raster..extract.r_cluster..shape2..fun...modal..na.rm...TRUE.
cl2test$zonalcat <- with(cl2test, ifelse(zonaltest == 1, 'Class 1', 
                                         ifelse(zonaltest == 2, 'Class 7', 
                                                ifelse(zonaltest == 3, 'Class 4',
                                                       ifelse(zonaltest == 4, 'Class 6', 
                                                              ifelse(zonaltest == 5, 'Class 8',
                                                                     ifelse(zonaltest == 6, 'Class 2',
                                                                            ifelse(zonaltest == 7, 'Class 3',
                                                                                   ifelse(zonaltest == 8, 'Class 5',0)))))))))








cl2test <- subset(cl2test, COUNTRY != "Cabo Verde")
cl2test <- subset(cl2test, COUNTRY != "Mauritius")
cl2test <- subset(cl2test, COUNTRY != "Seychelles")
cl2test <- subset(cl2test, COUNTRY != "São Tomé and Príncipe")
cl2test <- subset(cl2test, COUNTRY != "Comoros")

cl2test_missing <- st_as_sf(cl2test)
sum(is.na(cl2test$extract.r_cluster..shape2..fun...modal..na.rm...TRUE.))
table(cl2test$zonalcat)

rows_to_replace <- cl2test$zonalcat %in% c("Class 8")

# Replace values in those rows with new values
cl2test$zonalcat[rows_to_replace] <- "Class 7"
table(cl2test$zonalcat)
cl2testraster <- merge(cl2test,shape2,by="GID_2")
cl2testraster <- st_as_sf(cl2testraster)
cl2testraster2<-cl2testraster[ , c('COUNTRY.x','NAME_1.x', 'GID_2','NAME_2.x','zonalcat')]
total3test<-cl2testraster2[!duplicated(cl2testraster2[ , c("GID_2")]),]

table(total3test$zonalcat)
st_write(total3test, "aerotest_8clusters7.shp",append=FALSE)


# Perform K-means clustering with 6 centers
set.seed(41000000)
kmeans_result <- kmeans(km, centers =9,nstart = 25, iter.max = 300)
cluster_labels <- kmeans_result$cluster



similarity_matrix <- dist( kmeans_result$centers)
hc <- hclust(similarity_matrix)
reordered_labels <- cutree(hc,9) 
cluster_labels <- reordered_labels[cluster_labels]
cl = raster(files_stack)
cl[] = cluster_labels 

r_cluster <- setValues(cl, cluster_labels)
plot(r_cluster)
r_cluster <- mask(r_cluster, shape)
plot(hc)


cl2test<-data.frame(shape,raster::extract(r_cluster, shape, fun=modal, na.rm = TRUE))
table(cl2test$raster..extract.r_cluster..shape..fun...modal..na.rm...TRUE.)






cl2test<-data.frame(shape2, raster::extract(r_cluster, shape2, fun=modal, na.rm = TRUE))
cl2test$zonaltest<-cl2test$raster..extract.r_cluster..shape2..fun...modal..na.rm...TRUE.
cl2test$zonalcat <- with(cl2test, ifelse(zonaltest == 1, 'Class 8', 
                                         ifelse(zonaltest == 2, 'Class 2', 
                                                ifelse(zonaltest == 3, 'Class 5',
                                                       ifelse(zonaltest == 4, 'Class 6', 
                                                              ifelse(zonaltest == 5, 'Class 3',
                                                                     ifelse(zonaltest == 6, 'Class 7',
                                                                            ifelse(zonaltest == 7, 'Class 9',
                                                                                   ifelse(zonaltest == 8, 'Class 1',
                                                                                          ifelse(zonaltest == 9, 'Class 4',0))))))))))








cl2test <- subset(cl2test, COUNTRY != "Cabo Verde")
cl2test <- subset(cl2test, COUNTRY != "Mauritius")
cl2test <- subset(cl2test, COUNTRY != "Seychelles")
cl2test <- subset(cl2test, COUNTRY != "São Tomé and Príncipe")
cl2test <- subset(cl2test, COUNTRY != "Comoros")






cl2test_missing <- st_as_sf(cl2test)
sum(is.na(cl2test$extract.r_cluster..shape2..fun...modal..na.rm...TRUE.))
table(cl2test$zonalcat)

rows_to_replace <- cl2test$zonalcat %in% c("Class 6")

# Replace values in those rows with new values
cl2test$zonalcat[rows_to_replace] <- "Class 7"
table(cl2test$zonalcat)
cl2testraster <- merge(cl2test,shape2,by="GID_2")
cl2testraster <- st_as_sf(cl2testraster)
cl2testraster2<-cl2testraster[ , c('COUNTRY.x','NAME_1.x', 'GID_2','NAME_2.x','zonalcat')]
total3test<-cl2testraster2[!duplicated(cl2testraster2[ , c("GID_2")]),]

table(total3test$zonalcat)
st_write(total3test, "aerotest_9clusters8.shp",append=FALSE)


# Perform K-means clustering with 6 centers
set.seed(41000000)
kmeans_result <- kmeans(km, centers =10,nstart = 25, iter.max = 300)
cluster_labels <- kmeans_result$cluster



similarity_matrix <- dist( kmeans_result$centers)
hc <- hclust(similarity_matrix)
reordered_labels <- cutree(hc,10) 
cluster_labels <- reordered_labels[cluster_labels]
cl = raster(files_stack)
cl[] = cluster_labels 

r_cluster <- setValues(cl, cluster_labels)
plot(r_cluster)
r_cluster <- mask(r_cluster, shape)
plot(hc)



cl2test<-data.frame(shape2, raster::extract(r_cluster, shape2, fun=modal, na.rm = TRUE))
cl2test$zonaltest<-cl2test$raster..extract.r_cluster..shape2..fun...modal..na.rm...TRUE.
cl2test$zonalcat <- with(cl2test, ifelse(zonaltest == 1, 'Class 3', 
                                         ifelse(zonaltest == 2, 'Class 9', 
                                                ifelse(zonaltest == 3, 'Class 6',
                                                       ifelse(zonaltest == 4, 'Class 5', 
                                                              ifelse(zonaltest == 5, 'Class 7',
                                                                     ifelse(zonaltest == 6, 'Class 8',
                                                                            ifelse(zonaltest == 7, 'Class 10',
                                                                                   ifelse(zonaltest == 8, 'Class 4',
                                                                                          ifelse(zonaltest == 9, 'Class 1',
                                                                                                 ifelse(zonaltest == 10, 'Class 2',0)))))))))))








cl2test <- subset(cl2test, COUNTRY != "Cabo Verde")
cl2test <- subset(cl2test, COUNTRY != "Mauritius")
cl2test <- subset(cl2test, COUNTRY != "Seychelles")
cl2test <- subset(cl2test, COUNTRY != "São Tomé and Príncipe")
cl2test <- subset(cl2test, COUNTRY != "Comoros")






cl2test_missing <- st_as_sf(cl2test)
sum(is.na(cl2test$extract.r_cluster..shape2..fun...modal..na.rm...TRUE.))
table(cl2test$zonalcat)

rows_to_replace <- cl2test$zonalcat %in% c("Class 8")

# Replace values in those rows with new values
cl2test$zonalcat[rows_to_replace] <- "Class 7"
table(cl2test$zonalcat)
cl2testraster <- merge(cl2test,shape2,by="GID_2")
cl2testraster <- st_as_sf(cl2testraster)
cl2testraster2<-cl2testraster[ , c('COUNTRY.x','NAME_1.x', 'GID_2','NAME_2.x','zonalcat')]
total3test<-cl2testraster2[!duplicated(cl2testraster2[ , c("GID_2")]),]

table(total3test$zonalcat)
st_write(total3test, "aerotest_10clusters9.shp",append=FALSE)





# Perform K-means clustering with 6 centers
set.seed(41000000)
kmeans_result <- kmeans(km, centers =11,nstart = 25, iter.max = 300)
cluster_labels <- kmeans_result$cluster



similarity_matrix <- dist( kmeans_result$centers)
hc <- hclust(similarity_matrix)
reordered_labels <- cutree(hc,10) 
cluster_labels <- reordered_labels[cluster_labels]
cl = raster(files_stack)
cl[] = cluster_labels 

r_cluster <- setValues(cl, cluster_labels)
plot(r_cluster)
r_cluster <- mask(r_cluster, shape)
plot(hc)



cl2test<-data.frame(shape2, raster::extract(r_cluster, shape2, fun=modal, na.rm = TRUE))
cl2test$zonaltest<-cl2test$raster..extract.r_cluster..shape2..fun...modal..na.rm...TRUE.
cl2test$zonalcat <- with(cl2test, ifelse(zonaltest == 1, 'Class 3', 
                                         ifelse(zonaltest == 2, 'Class 8', 
                                                ifelse(zonaltest == 3, 'Class 9',
                                                       ifelse(zonaltest == 4, 'Class 7', 
                                                              ifelse(zonaltest == 5, 'Class 10',
                                                                     ifelse(zonaltest == 6, 'Class 5',
                                                                            ifelse(zonaltest == 7, 'Class 1',
                                                                                   ifelse(zonaltest == 8, 'Class 11',
                                                                                          ifelse(zonaltest == 9, 'Class 6',
                                                                                                 ifelse(zonaltest == 10, 'Class 4',
                                                                                                        ifelse(zonaltest == 11, 'Class 2',0))))))))))))








cl2test <- subset(cl2test, COUNTRY != "Cabo Verde")
cl2test <- subset(cl2test, COUNTRY != "Mauritius")
cl2test <- subset(cl2test, COUNTRY != "Seychelles")
cl2test <- subset(cl2test, COUNTRY != "São Tomé and Príncipe")
cl2test <- subset(cl2test, COUNTRY != "Comoros")






cl2test_missing <- st_as_sf(cl2test)
sum(is.na(cl2test$extract.r_cluster..shape2..fun...modal..na.rm...TRUE.))
table(cl2test$zonalcat)

rows_to_replace <- cl2test$zonalcat %in% c("Class 8")

# Replace values in those rows with new values
cl2test$zonalcat[rows_to_replace] <- "Class 9"
table(cl2test$zonalcat)
cl2testraster <- merge(cl2test,shape2,by="GID_2")
cl2testraster <- st_as_sf(cl2testraster)
cl2testraster2<-cl2testraster[ , c('COUNTRY.x','NAME_1.x', 'GID_2','NAME_2.x','zonalcat')]
total3test<-cl2testraster2[!duplicated(cl2testraster2[ , c("GID_2")]),]

table(total3test$zonalcat)
table(total3test$zonalcat)
st_write(total3test, "aerotest_11clusters10.shp",append=FALSE)
