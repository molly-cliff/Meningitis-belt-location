
library(ggplot2)
library(mgcv)
library(reshape2)
library(caret)
library(sf)
library(readxl)
library(gridExtra)
library(haven)
library(tidyverse)
library(RStoolbox)
library(pROC)
library(raster)
library(terra)
library(stringr)
library(sf)
library(readxl)
library(lubridate)
library(dplyr)





path <- "~/INLA project/temperatuture"
files <- list.files(path, pattern = "nc4$", full.names = TRUE)

# Filter out duplicate files with (1), (2), etc. in their names
files <- files[!grepl("\\([0-9]+\\)", files)]

# Read as SpatRaster
r <- rast(files)
setwd("C:/Users/mvc32/OneDrive - University of Cambridge/Documents/Climate_meningitis_belt")
shape2 <- read_sf(dsn = ".", layer = "Shapefile_improved")
# Summary of raster layers
summary_df <- data.frame(
  Layer = names(r),
  Min = sapply(1:nlyr(r), function(i) min(minmax(r[[i]])[1], na.rm = TRUE)),
  Max = sapply(1:nlyr(r), function(i) max(minmax(r[[i]])[2], na.rm = TRUE))
)
print(summary_df)
print(names(r))

# Check and align CRS between raster and shapefile
if (!identical(crs(r), crs(vect(shape2)))) {
  message("CRS mismatch - Reprojecting raster to match vector")
  r <- project(r, crs(vect(shape2)))
} else {
  message("CRS aligned")
}
start_date <- ymd("2003-01-01") 
date_seq <- seq(start_date, by = "1 month", length.out = nlyr(r))
layer_names <- paste0("temp_", year(date_seq), "_", sprintf("%02d", month(date_seq)))
names(r) <- layer_names
print(names(r))
# Crop and mask
allrasters <- mask(crop(r, vect(shape2)), vect(shape2))




# Reduction of dimensionality of data using PCA
rpc <- rasterPCA(allrasters)
raster_stack <- stack(rpc$map)

summary(rpc$model)
raster_stack <- stack(rpc$map)
files_stack <- raster_stack[[1:3]]



library(cluster)
library(clusterCrit)

km <- as.matrix(files_stack)
km_mean <- mean(km, na.rm = TRUE)
km[is.na(km)] <- km_mean


#------------------------------------------------------------
# 1. Prepare data
#------------------------------------------------------------
km <- as.matrix(files_stack)

km_mean <- mean(km, na.rm = TRUE)
km[is.na(km)] <- 0

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
set.seed(42)

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
  # } else {
  #   sil <- silhouette(km_res$cluster, dist(km))
  # }
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
                      c("Calinski_Harabasz"))
  
  CH_index[i]   <- crit$calinski_harabasz
  
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
  CalinskiHarabasz = CH_index
)

print(results)

#------------------------------------------------------------
# 6. Plot key indicators
#------------------------------------------------------------



par(mfrow=c(1,2))

# Elbow
plot(k_range, elbow_wss, type="b", pch=19,
     xlab="k", ylab="Total Within-Cluster Sum of Squares (WCSS)", main="Temperature Elbow Curve")


# Silhouette without outline cluster
plot(k_range, sil_no_out, type="b", pch=19, 
     xlab="k", ylab="Mean Silhouette",
     main="Temperature Silhouette Score")

par(mfrow=c(1,1))




set.seed(42)

# Perform K-means clustering with 6 centers
kmeans_result <- kmeans(km, centers = 10, nstart = 25, iter.max = 300)
cluster_labels <- kmeans_result$cluster


# Calculate similarity matrix and perform hierarchical clustering
similarity_matrix <- dist(kmeans_result$centers)
hc <- hclust(similarity_matrix)
reordered_labels <- cutree(hc, 10) 
cluster_labels <- reordered_labels[cluster_labels]
plot(hc)
# Assign cluster labels to raster
cl <- raster(files_stack)
cl[] = cluster_labels 
r_cluster <- setValues(cl, cluster_labels)
r_cluster <- mask(r_cluster, shape2)
plot(r_cluster)
# Plot the clustered raster

cl2test<-data.frame(shape2, raster::extract(r_cluster, shape2, fun=modal, na.rm = TRUE))
cl2test$zonaltest<-cl2test$raster..extract.r_cluster..shape2..fun...modal..na.rm...TRUE.
cl2test$zonalcat <- with(cl2test, ifelse(zonaltest == 1, 'Class 3', 
                                         ifelse(zonaltest == 2, 'Class 9', 
                                                ifelse(zonaltest == 3, 'Class 8',
                                                       ifelse(zonaltest == 4, 'Class 7', 
                                                              ifelse(zonaltest == 5, 'Class 4',
                                                                     ifelse(zonaltest == 6, 'Class 1',
                                                                            ifelse(zonaltest == 7, 'Class 5',
                                                                                   ifelse(zonaltest == 8, 'Class 2',
                                                                                          ifelse(zonaltest == 9, 'Class 6',
                                                                                                 ifelse(zonaltest == 10, 'Class 10',0)))))))))))

cl2test <- subset(cl2test, COUNTRY != "Cabo Verde")
cl2test <- subset(cl2test, COUNTRY != "Mauritius")
cl2test <- subset(cl2test, COUNTRY != "Seychelles")
cl2test <- subset(cl2test, COUNTRY != "São Tomé and Príncipe")
cl2test <- subset(cl2test, COUNTRY != "Comoros")

cl2test_missing <- st_as_sf(cl2test)
sum(is.na(cl2test$extract.r_cluster..shape2..fun...modal..na.rm...TRUE.))
table(cl2test$zonalcat)
cl2testraster <- merge(cl2test,shape2,by="GID_2")
cl2testraster <- st_as_sf(cl2testraster)
cl2testraster2<-cl2testraster[ , c('COUNTRY.x','NAME_1.x', 'GID_2','NAME_2.x','zonalcat')]
total3test<-cl2testraster2[!duplicated(cl2testraster2[ , c("GID_2")]),]

table(total3test$zonalcat)
st_write(total3test, "temperature_10_clusters.shp",append=FALSE)
