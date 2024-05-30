# Load necessary libraries
library(dplyr)
library(RStoolbox)
library(writexl)
library(sf)
library(lwgeom)
library(stringi)
library(caret)
library(brglm)
library(brglm2)
library(pscl)
library(car)
library(brms)
library(pROC)
library(wesanderson)
library(InformationValue)
library(raster)

# Set working directory
setwd("C:/Users/mvc32/OneDrive - University of Cambridge/Documents/Climate_meningitis_belt")

# Read in all the environmental data
Pop_density <- read_sf(dsn = ".", layer = "Population_density")
windspeed <- read_sf(dsn = ".", layer = "windspeedagain")
enviromentalsurfaces <- read_sf(dsn = ".", layer = "Landcoverbilinear")
Rainfallcat <- read_sf(dsn = ".", layer = "rainfallbilinear")
Aerocat <- read_sf(dsn = ".", layer = "Aero9clusters")
Humiditycat <- read_sf(dsn = ".", layer = "Absolutehumidity11clusters")



# Correct names
Aerocat$aerozone <- Aerocat$zonalcat
Humiditycat$Humidityzone <- Humiditycat$zonalcat
Rainfallcat$rainfallzone <- Rainfallcat$zonalcat
windspeed$windspeedzone <- windspeed$zonalcat


# Disable s2 usage for spatial operations
sf::sf_use_s2(FALSE)

# Convert shapefiles to data frames
Rainfallcat <- as.data.frame(Rainfallcat)
Aerocat <- as.data.frame(Aerocat)
windspeed <- as.data.frame(windspeed)
Humiditycat <- as.data.frame(Humiditycat)

# Create a 'code' column for joining datasets
create_code <- function(df, name_col, gid_col) {
  df$code <- paste(df[[name_col]], df[[gid_col]])
  return(df)
}

Rainfallcat <- create_code(Rainfallcat, "NAME_2_x", "GID_2")
Aerocat <- create_code(Aerocat, "NAME_2_x", "GID_2")
Humiditycat <- create_code(Humiditycat, "NAME_2_x", "GID_2")
windspeed <- create_code(windspeed, "NAME_2_x", "GID_2")
Pop_density <- create_code(Pop_density, "NAME_2_", "GID_2")
enviromentalsurfaces <- create_code(enviromentalsurfaces, "NAME_2_", "GID_2")


#create code in r in which to join all the enviromental data together
Rainfallcat$code <- paste(Rainfallcat$NAME_2_x, Rainfallcat$GID_2)
Aerocat$code <- paste(Aerocat$NAME_2_x, Aerocat$GID_2)
Humiditycat$code <- paste(Humiditycat$NAME_2_x, Humiditycat$GID_2)
windspeed$code <- paste(windspeed$NAME_2_x, windspeed$GID_2)
Pop_density$code <- paste(Pop_density$NAME_2_, Pop_density$GID_2)
enviromentalsurfaces$code <- paste(enviromentalsurfaces$NAME_2_, enviromentalsurfaces$GID_2)

# Join based on matching 'code' column
total3test <- merge(Aerocat, Rainfallcat, by = c("code", "code"), all.x = TRUE)
total3test <- total3test[!duplicated(total3test$code), ]

total3test <- merge(total3test, Humiditycat, by = c("code", "code"), all.x = TRUE)
total3test <- total3test[!duplicated(total3test$code), ]

total3test <- merge(total3test, windspeed, by = c("code", "code"), all.x = TRUE)
total3test <- total3test[!duplicated(total3test$code), ]

total3test <- merge(total3test, Pop_density, by = c("code", "code"), all.x = TRUE)
total3test <- total3test[!duplicated(total3test$code), ]

total3test <- merge(total3test, enviromentalsurfaces, by = c("code", "code"), all.x = TRUE)
total_data <- total3test[!duplicated(total3test$code), ]
# Select final columns and remove duplicates
final_columns <- c("COUNTRY_x.x", "NAME_1_x.x", "GID_2.x", "NAME_2_x.x", "rainfallzone", 
                   "Humidityzone", "aerozone", "windspeedzone", "Pp_dnst", "nw_lnd_", 
                   "geometry.y", "code")
final_data <- total_data[ , final_columns]
final_data <- final_data[!duplicated(final_data$GID_2.x), ]

# Correct column name for windspeed
final_data$windspeed <- final_data$zonalcat

# Read in shapefile of Africa
shape2 <- st_read("Shapefile_improved.shp")
final_data$district_country <- paste(final_data$NAME_2_x.x, final_data$COUNTRY_x.x, sep = " ")
shape2$district_country <- paste(shape2$NAME_2, shape2$COUNTRY, sep = " ")

# Merge shapefile and environmental data
final_data <- final_data[!duplicated(final_data$district_country), ]
merged_data <- merge(shape2, final_data, by = "district_country")

# Plot one of the attributes and convert to data frame
merged_data <- as.data.frame(merged_data)
final_columns <- c("COUNTRY", "NAME_1", "GID_2", "NAME_2", "rainfallzone", 
                   "Humidityzone", "aerozone", "windspeedzone", "Pp_dnst", 
                   "nw_lnd_", "geometry.y", "code", "district_country")
final_data <- merged_data[ , final_columns]


# Exclude certain countries based on data quality
exclude_countries <- c("Cabo Verde", "Mauritius", "Seychelles", "São Tomé and Príncipe")
final_data <- subset(final_data, !(COUNTRY %in% exclude_countries))



sum(is.na(final_data$rainfallzone))
sum(is.na(final_data$Humidityzone))
sum(is.na(final_data$aerozone))
sum(is.na(final_data$windspeedzone))
sum(is.na(final_data$nw_lnd_))



#nearest neighbours function, for columns with nas, we use the modal value of districts with the same ADMN1 value as them
get_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Fill missing categorical values based on mode of matching "country" and "admn1"
for (i in 1:nrow(final_data)) {
  if (is.na(final_data$rainfallzone[i])) {
    mode_value <- with(final_data, rainfallzone[COUNTRY == COUNTRY[i] & NAME_1 == NAME_1[i]])
    final_data$rainfallzone[i] <- ifelse(length(mode_value) > 0, get_mode(mode_value), NA)
  }
}

for (i in 1:nrow(final_data)) {
  if (is.na(final_data$windspeedzone[i])) {
    mode_value <- with(final_data, windspeedzone[COUNTRY == COUNTRY[i] & NAME_1 == NAME_1[i]])
    final_data$windspeedzone[i] <- ifelse(length(mode_value) > 0, get_mode(mode_value), NA)
  }
}


for (i in 1:nrow(final_data)) {
  if (is.na(final_data$aerozone[i])) {
    mode_value <- with(final_data, aerozone[COUNTRY == COUNTRY[i] & NAME_1 == NAME_1[i]])
    final_data$aerozone[i] <- ifelse(length(mode_value) > 0, get_mode(mode_value), NA)
  }
}


for (i in 1:nrow(final_data)) {
  if (is.na(final_data$Humidityzone[i])) {
    mode_value <- with(final_data, Humidityzone[COUNTRY == COUNTRY[i] & NAME_1 == NAME_1[i]])
    final_data$Humidityzone[i] <- ifelse(length(mode_value) > 0, get_mode(mode_value), NA)
  }
}



for (i in 1:nrow(final_data)) {
  if (is.na(final_data$nw_lnd_[i])) {
    mode_value <- with(final_data, nw_lnd_[COUNTRY == COUNTRY[i] & NAME_1 == NAME_1[i]])
    final_data$nw_lnd_[i] <- ifelse(length(mode_value) > 0, get_mode(mode_value), NA)
  }
}
sum(is.na(final_data$rainfallzone))
sum(is.na(final_data$Humidityzone))
sum(is.na(final_data$aerozone))
sum(is.na(final_data$windspeedzone))
sum(is.na(final_data$nw_lnd_))

#here for those we are not able to match we match based on modal country
for (i in 1:nrow(final_data)) {
  if (is.na(final_data$Humidityzone[i])) {
    mode_value <- with(final_data, Humidityzone[COUNTRY == COUNTRY[i]])
    final_data$Humidityzone[i] <- ifelse(length(mode_value) > 0, get_mode(mode_value), NA)
  }
}


for (i in 1:nrow(final_data)) {
  if (is.na(final_data$nw_lnd_[i])) {
    mode_value <- with(final_data, nw_lnd_[COUNTRY == COUNTRY[i]])
    final_data$nw_lnd_[i] <- ifelse(length(mode_value) > 0, get_mode(mode_value), NA)
  }
}


for (i in 1:nrow(final_data)) {
  if (is.na(final_data$windspeedzone[i])) {
    mode_value <- with(final_data, windspeedzone[COUNTRY == COUNTRY[i]])
    final_data$windspeedzone[i] <- ifelse(length(mode_value) > 0, get_mode(mode_value), NA)
  }
}



for (i in 1:nrow(final_data)) {
  if (is.na(final_data$aerozone[i])) {
    mode_value <- with(final_data, aerozone[COUNTRY == COUNTRY[i]])
    final_data$aerozone[i] <- ifelse(length(mode_value) > 0, get_mode(mode_value), NA)
  }
}


sum(is.na(final_data$rainfallzone))
sum(is.na(final_data$Humidityzone))
sum(is.na(final_data$aerozone))
sum(is.na(final_data$windspeedzone))
sum(is.na(final_data$nw_lnd_))



# Load necessary libraries
library(dplyr)
library(readxl)

# Set working directory
setwd("C:/Users/mvc32/OneDrive - University of Cambridge/Documents/Climate_meningitis_belt/Disease_data")

# Define special countries
special_countries <- c("Libya", "Western Sahara", "Lesotho", "Comoros")

# Read in weekly epidemic data
file_path_weekly <- "totalweeklyepidemic.xlsx"
weeklyincidence_merge <- read_excel(file_path_weekly)

# Filter special rows for weekly data
special_rows_weekly <- weeklyincidence_merge %>% filter(COUNTRY_xx %in% special_countries)

# Remove duplicates for weekly data
weeklyincidence_merge <- weeklyincidence_merge %>%
  filter(!COUNTRY_xx %in% special_countries) %>%
  distinct(district_country, .keep_all = TRUE) %>%
  bind_rows(special_rows_weekly)

# Merge final dataset with weekly epidemic data
merged_data <- merge(final_data, weeklyincidence_merge, by = "code", all = TRUE)

# Read in annual epidemic data
file_path_annual <- "totalannualepidemic2.xlsx"
annualincidence_merge <- read_excel(file_path_annual)

# Filter special rows for annual data
special_rows_annual <- annualincidence_merge %>% filter(COUNTRY_xx %in% special_countries)

# Remove duplicates for annual data
annualincidence_merge <- annualincidence_merge %>%
  filter(!COUNTRY_xx %in% special_countries) %>%
  distinct(district_country, .keep_all = TRUE) %>%
  bind_rows(special_rows_annual)

# Merge final dataset with annual epidemic data
merged_data <- merge(merged_data, annualincidence_merge, by = "code", all = TRUE)
library(tidyverse)
# Create new epidemic column (binary) based on weekly and annual epidemic data
merged_data <- merged_data %>%
  mutate(epidemic = ifelse(epidemic_annual == 1 | epidemic_weekly == 1, 1, 0)) %>%
  replace_na(list(epidemic = 0))

# Select final columns and remove duplicates
final_columns <- c("COUNTRY.x", "code", "NAME_2.x", "rainfallzone", "Humidityzone", 
                   "aerozone", "windspeedzone", "Pp_dnst", "nw_lnd_", "epidemic", 
                   "district_country", "geometry.y")
final_data <- merged_data[ , final_columns]

# Check for missing values in final data (optional)
sum(is.na(final_data$rainfallzone))

# Exclude certain countries based on data quality (optional, if not done earlier)
exclude_countries <- c("Cabo Verde", "Mauritius", "Seychelles", "São Tomé and Príncipe")
final_data <- subset(final_data, !(COUNTRY.x %in% exclude_countries))


library(dplyr)
library(readxl)

# Clean data and set factors
merged_data$Aero3 <- as.factor(merged_data$aerozone)
merged_data$humidity3 <- as.factor(merged_data$Humidityzone)
merged_data$windspeed3 <- as.factor(merged_data$windspeedzone)
merged_data$rainfall3 <- as.factor(merged_data$rainfallzone)
merged_data$Land_category <- as.factor(merged_data$nw_lnd_)

# Remove duplicate rows based on 'code' column
dup_rows <- duplicated(merged_data$code)
dup_rows <- merged_data[duplicated(merged_data$code) | duplicated(merged_data$code, fromLast = TRUE), ]
dup_rows <- dup_rows[!(dup_rows$COUNTRY.x %in% c("Libya", "Lesotho", "Western Sahara")), ]
merged_data <- anti_join(merged_data, dup_rows, by = "code")
first_instance_indices <- !duplicated(dup_rows$code)
unique_df <- dup_rows[first_instance_indices, ]
merged_data <- rbind(unique_df, merged_data)

# Prepare data for analysis
merged_data[is.na(merged_data)] <- 0
merged_data <- merged_data[merged_data$COUNTRY.x != 0, ]
dataset_merge <- merged_data

# Calculate average latitude and cosine of latitude
#sf_object <- st_as_sf(merged_data)
#average_latitudes <- lapply(sf_object %>% {sapply(st_geometry(.), st_coordinates)} %>% {lapply(., as.data.frame)} %>% {lapply(., dplyr::select, Y)} %>% {lapply(., dplyr::rename, lat = Y)}, function(coords_df) {
#  mean(coords_df$lat)
#})

sf_object <- st_as_sf(merged_data)
average_latitudes <- st_geometry(sf_object) %>%
  map(~ st_coordinates(.x)[, 2]) %>%  # Extract the Y (latitude) coordinates
  map_dbl(mean)  # Calculate the mean latitude for each geometry

# Assign average latitudes back to merged_data
merged_data$average_latitude <- average_latitudes

# Calculate the cosine of the average latitudes in radians
weight <- cos(merged_data$average_latitude * (pi / 180))

# merged_data now has average_latitude and weight columns
merged_data$cosine_latitude <- weight
#merged_data$average_latitude <- unlist(average_latitudes)

#weight <- cos(merged_data$average_latitude*(pi/180))

#merged_data$cosine_latitude <- cos(merged_data$average_latitude)

# Select relevant columns for analysis
merged_data <- merged_data[, c("epidemic", "rainfall3", "Aero3", "Pp_dnst", "humidity3", "windspeed3", "Land_category", "cosine_latitude")]

# Update factor levels




sum(is.na(merged_data$rainfall3))
sum(is.na(merged_data$humidity3))
sum(is.na(merged_data$Aero3))
sum(is.na(merged_data$windspeed3))
sum(is.na(merged_data$Land_category))
sum(is.na(merged_data$epidemic))




merged_data$Aero3 <- gsub("Class 4", "Class 3", merged_data$Aero3)
merged_data$Aero3 <- gsub("Class 5", "Class 4", merged_data$Aero3)
merged_data$Aero3 <- gsub("Class 6", "Class 5", merged_data$Aero3)
merged_data$Aero3 <- gsub("Class 7", "Class 6", merged_data$Aero3)
merged_data$Aero3 <- gsub("Class 8", "Class 7", merged_data$Aero3)
merged_data$Aero3 <- gsub("Class 9", "Class 8", merged_data$Aero3)


merged_data$humidity3 <- gsub("Class 3", "Class 2", merged_data$humidity3)
merged_data$humidity3 <- gsub("Class 4", "Class 3", merged_data$humidity3)
merged_data$humidity3 <- gsub("Class 5", "Class 4", merged_data$humidity3)
merged_data$humidity3 <- gsub("Class 6", "Class 5", merged_data$humidity3)
merged_data$humidity3 <- gsub("Class 7", "Class 6", merged_data$humidity3)
merged_data$humidity3 <- gsub("Class 8", "Class 7", merged_data$humidity3)
merged_data$humidity3 <- gsub("Class 9", "Class 8", merged_data$humidity3)

merged_data$humidity3 <- gsub("Class 10", "Class 9", merged_data$humidity3)
merged_data$humidity3 <- gsub("Class 11", "Class 10", merged_data$humidity3)

sample_n(merged_data, 3)
table(merged_data$epidemic)

sum(is.na(merged_data$rainfallzone))
sum(is.na(merged_data$Humidityzone))
sum(is.na(merged_data$Aero3))
sum(is.na(merged_data$windspeedzone))
sum(is.na(merged_data$Land_category))
sum(is.na(merged_data$epidemic))

#min_cos <- min(merged_data$cosine_latitude)
#max_cos <- max(merged_data$cosine_latitude)

# Scale the cosine latitude to the range [0, 1]
#merged_data$scaled_cos_latitude <- (merged_data$cosine_latitude - min_cos) / (max_cos - min_cos)

#merged_data$scaled_cos_latitude <- merged_data$scaled_cos_latitude +1



variables <- c("Aero3", "humidity3", "rainfall3", "Land_category","windspeed3")

# Initialize a data frame to store model summaries
model_results <- data.frame(Variable = character(), Estimate = numeric(), StdError = numeric(), zValue = numeric(), PValue = numeric(), stringsAsFactors = FALSE)

# Fit separate logistic regression models for each variable
model2 <- glm(epidemic ~ humidity3,
              data = merged_data, 
              family = "binomial",
              weights = merged_data$cosine_latitude,method = "brglmFit")

summary(model2)

model2 <- glm(epidemic ~ Aero3,
              data = merged_data, 
              family = "binomial",
              weights = merged_data$cosine_latitude,method = "brglmFit")

summary(model2)

model2 <- glm(epidemic ~ rainfall3,
              data = merged_data, 
              family = "binomial",
              weights = merged_data$cosine_latitude,method = "brglmFit")

summary(model2)

model2 <- glm(epidemic ~ Aero3,
              data = merged_data, 
              family = "binomial",
              weights = merged_data$cosine_latitude,method = "brglmFit")

summary(model2)

model2 <- glm(epidemic ~ windspeed3,
              data = merged_data, 
              family = "binomial",
              weights = merged_data$cosine_latitude,method = "brglmFit")

summary(model2)


# Print the results
print(model_results)

categorical_variables <- c("Aero3", "humidity3", "rainfall3", "Land_category","windspeed3")
# Initialize a data frame to store chi-square test results
chi_square_results <- data.frame(Variable = character(), ChiSquare = numeric(), DF = numeric(), PValue = numeric(), stringsAsFactors = FALSE)

# Perform chi-square test for each variable
for (variable in categorical_variables) {
  contingency_table <- table(merged_data[[variable]], merged_data$epidemic)
  chi_test <- chisq.test(contingency_table)
  chi_square_results <- rbind(chi_square_results, data.frame(
    Variable = variable,
    ChiSquare = chi_test$statistic,
    DF = chi_test$parameter,
    PValue = round(chi_test$p.value, 40)
  ))
}

# Print the results
print(chi_square_results)



library(caret)
library(ggplot2)



library(caret)
library(ggplot2)
library(caret)
library(ggplot2)

# Function to calculate F1 score
F1_Score <- function(actual, predicted) {
  tp <- sum(actual == 1 & predicted == 1)
  fp <- sum(actual == 0 & predicted == 1)
  fn <- sum(actual == 1 & predicted == 0)
  
  precision <- tp / (tp + fp)
  recall <- tp / (tp + fn)
  
  f1_score <- 2 * precision * recall / (precision + recall)
  return(f1_score)
}

# Define a range of weights for the minority class (0 to 1)
weights <- seq(0, 1, length.out = 150)

# Initialize vectors to store F1 scores and weights
f1_scores <- numeric(length(weights))

# Iterate over each weight and train logistic regression model
for (i in seq_along(weights)) {
  # Fit logistic regression model with specified weight for minority class
  model <- glm(epidemic ~ Aero3 + humidity3 + rainfall3 , 
               data = merged_data, 
               family = "binomial",
               weights = ifelse(merged_data$epidemic == 1, weights[i], 1),
               method = "brglmFit")
  
  # Predict probabilities
  predicted_probs <- predict(model, merged_data, type = "response")
  
  # Convert probabilities to binary predictions
  predicted_classes <- ifelse(predicted_probs > 0.5, 1, 0)
  
  # Calculate F1 score
  f1_scores[i] <- F1_Score(merged_data$epidemic, predicted_classes)
}

# Find the optimum weight that maximizes the F1 score
optimum_weight <- weights[which.max(f1_scores)]
max_f1_score <- max(f1_scores)
optimum_weight <-0.8725
nonepidemicweight<-1-optimum_weight

# Plot F1 scores against weights
df <- data.frame(weights = weights, f1_scores = f1_scores)
ggplot(df, aes(x = weights, y = f1_scores)) +
  geom_line() +
  geom_point(data = data.frame(weights = optimum_weight, f1_scores = max_f1_score),
             aes(x = weights, y = f1_scores), color = "red", size = 3) +
  labs(x = "Weight for minority class", y = "F1 Score",
       title = "F1 Score vs. Weight for Minority Class") +
  theme_minimal()



#merged_data<-merged_data %>%
# filter(!( Land_category %in% c("Water bodies")))

# Check the length of the weights vector
length(ifelse(merged_data$epidemic == 1, optimum_weight, nonepidemicweight))

# Ensure that the weights vector has the same length as the number of observations
merged_data$weights <- ifelse(merged_data$epidemic == 1, optimum_weight, nonepidemicweight)


merged_data$weightswithcosine <- merged_data$weights * merged_data$cosine_latitude

merged_data$humidity3 <- as.factor(merged_data$humidity3)
merged_data$Aero3 <- as.factor(merged_data$Aero3)
merged_data$rainfall3 <- as.factor(merged_data$rainfall3)
# Check the levels of the factor to understand its current levels
levels(merged_data$humidity3)
merged_data$humidity3 <- relevel(merged_data$humidity3, ref = "Class 5")
merged_data$Aero3 <- relevel(merged_data$Aero3, ref = "Class 2")
merged_data$rainfall3 <- relevel(merged_data$rainfall3, ref = "Class 2")
#first model, weighted full regression
model <- glm(epidemic ~ ., data = merged_data, family = binomial, method = "brglmFit")
model <- glm(epidemic ~ Aero3 + humidity3 + rainfall3, data = merged_data, family = binomial, weights = merged_data$weightswithcosine, method = "brglmFit")




options(scipen=999)

summary(model)
library(pscl)
library(car)
# McFadden’s R2,In practice, values over 0.40 indicate that a model fits the data very well.
pscl::pR2(model)["McFadden"]
#Variable Importance
caret::varImp(model)
#VIF values of each variable in the model to see if multicollinearity is a problem:
car::vif(model)
library(brms)
vif_values <- vif(model)
#these are predictions to test if the model is ok
#predict on whole dataset
predicted<-predict(model, merged_data, type="response")

library(InformationValue)
#find optimal cutoff probability to use to maximize accuracy
optimal <- optimalCutoff(merged_data$epidemic, predicted)[1]
optimal
confusionMatrix(merged_data$epidemic, predicted)


predicted_classes <- ifelse(predicted > optimal, 1, 0)
conf_matrix_table <- table(Actual = merged_data$epidemic, Predicted = predicted_classes)
print(conf_matrix_table)



#calculate sensitivity
sensitivity(merged_data$epidemic, predicted)


#calculate specificity
specificity(merged_data$epidemic, predicted)


#calculate total misclassification error rate
misClassError(merged_data$epidemic, predicted, threshold=optimal)
#plot the ROC curve
plotROC(merged_data$epidemic, predicted)


#trying an unweighted model

test_merge <-merged_data [ , c("epidemic", "rainfall3"  ,"Aero3",    
                               "humidity3", "windspeed3", "Land_category")]

model <- glm(epidemic ~ ., data = test_merge, family = binomial, method = "brglmFit", weights= merged_data$weightswithcosine)
options(scipen=999)

summary(model)

# McFadden’s R2,
pscl::pR2(model)["McFadden"]
#Variable Importance
caret::varImp(model)
#VIF values of each variable in the model to see if multicollinearity is a problem:
car::vif(model)
library(brms)
vif_values <- vif(model)
#these are predictions to test if the model is ok
#predict on whole dataset
predicted<-predict(model, merged_data, type="response")

library(InformationValue)
#find optimal cutoff probability to use to maximize accuracy
optimal <- optimalCutoff(merged_data$epidemic, predicted)[1]
optimal
confusionMatrix(merged_data$epidemic, predicted)


predicted_classes <- ifelse(predicted > 0.4, 1, 0)
conf_matrix_table <- table(Actual = merged_data$epidemic, Predicted = predicted_classes)
print(conf_matrix_table)



#calculate sensitivity
sensitivity(merged_data$epidemic, predicted)


#calculate specificity
specificity(merged_data$epidemic, predicted)


#calculate total misclassification error rate
misClassError(merged_data$epidemic, predicted, threshold=optimal)
#plot the ROC curve
plotROC(merged_data$epidemic, predicted)



#now trying to do a backwards model, unweighted
#weighting seemed to increase all variable singificance

library(MASS)

set.seed(76)
backwards_test<-step(model,direction="backward",trace=FALSE)
options(scipen=999)

summary(backwards_test)
library(pscl)
library(car)
# McFadden’s R2,In practice, values over 0.40 indicate that a model fits the data very well.
pscl::pR2(backwards_test)["McFadden"]
#Variable Importance
caret::varImp(backwards_test)
#VIF values of each variable in the model to see if multicollinearity is a problem:
car::vif(backwards_test)
library(brms)
#vif_values <- vif(backwards_test)

#Step 4: Use the Model to Make Predictions
predicted<-predict(backwards_test, merged_data, type="response")

library(InformationValue)
#find optimal cutoff probability to use to maximize accuracy
optimal <- optimalCutoff(merged_data$epidemic, predicted)[1]
optimal
confusionMatrix(merged_data$epidemic, predicted)



predicted_classes <- ifelse(predicted > 0.4, 1, 0)
conf_matrix_table <- table(Actual = merged_data$epidemic, Predicted = predicted_classes)
print(conf_matrix_table)



#calculate sensitivity
sensitivity(merged_data$epidemic, predicted)


#calculate specificity
specificity(merged_data$epidemic, predicted)

#calculate total misclassification error rate
misClassError(merged_data$epidemic, predicted, threshold=optimal)
#plot the ROC curve
plotROC(merged_data$epidemic, predicted)

#now testing to see what the best model is using likelihood ratio test
library(lmtest)
lrt_result <- lrtest(model, backwards_test)

# Display the likelihood ratio test result
print(lrt_result)
glm(formula = epidemic ~ rainfall3 + Aero3 + humidity3 + windspeed3, 
    family = binomial, data = merged_data, method = "brglmFit")


model2 <- glm(epidemic ~ rainfall3 + Aero3  + windspeed3 + Land_category, data = merged_data, weights = merged_data$weightswithcosine, family = binomial, method = "brglmFit")
model3 <- glm(epidemic ~ rainfall3 + Aero3  + humidity3 + Land_category, data = merged_data, weights = merged_data$weightswithcosine, family = binomial, method = "brglmFit")
model4 <- glm(epidemic ~ rainfall3 + windspeed3  + humidity3 + Land_category, data = merged_data,  weights = merged_data$weightswithcosine,family = binomial, method = "brglmFit")
model5 <- glm(epidemic ~ windspeed3 + Aero3  + humidity3 + Land_category, data = merged_data, weights = merged_data$weightswithcosine, family = binomial, method = "brglmFit")
model6 <- glm(epidemic ~ windspeed3 + Aero3  + humidity3 + rainfall3, data = merged_data, weights = merged_data$weightswithcosine, family = binomial, method = "brglmFit")
library(lmtest)
lrt_result2 <- lrtest( backwards_test,model2)
print(lrt_result2)
lrt_result3 <- lrtest( backwards_test,model3)
print(lrt_result3)

lrt_result4 <- lrtest( backwards_test,model4)
print(lrt_result4)

lrt_result5 <- lrtest( backwards_test,model5)
print(lrt_result5)

lrt_result6 <- lrtest( backwards_test,model6)
print(lrt_result6)


model2 <- glm(epidemic ~ Aero3 +  rainfall3 + humidity3,
              data = merged_data, 
              family = "binomial",
              weights = merged_data$weightswithcosine,method = "brglmFit")



summary(model2)
# McFadden’s R2,In practice, values over 0.40 indicate that a model fits the data very well.

pscl::pR2(model2)["McFadden"]

LL_model <- logLik(model2)
LL_null <- logLik(update(model2, . ~ 1))  # Null model
McFaddens_R2 <- 1 - (LL_model / LL_null)

McFaddens_R2

#Variable Importance
caret::varImp(model2)
#VIF values of each variable in the model to see if multicollinearity is a problem:
car::vif(model2)

vif_values <- vif(model2)

#Step 4: Use the Model to Make Predictions
predicted<-predict(model2, merged_data, type="response")


#find optimal cutoff probability to use to maximize accuracy
optimal <- optimalCutoff(merged_data$epidemic, predicted)[1]
optimal


optimal<-optimalCutoff(merged_data$epidemic, predicted, optimiseFor = "misclasserror", returnDiagnostics = FALSE)

#confusionMatrix(merged_data$epidemic, predicted)

predicted_classes <- ifelse(predicted > 0.4, 1, 0)
conf_matrix <- table(Actual = merged_data$epidemic,Predicted = predicted_classes )
print(conf_matrix)


# Calculate sensitivity and specificity
sensitivity<- sensitivity(merged_data$epidemic, predicted_classes)


#calculate specificity
specificity<-  specificity(merged_data$epidemic, predicted_classes)

cat("Sensitivity:", sensitivity)
cat("Specificity:", specificity)



roc_curve <- roc(merged_data$epidemic, predicted)

# Choose colors from Wes Anderson palette
colors <- wes_palette("GrandBudapest2", n = 4)

plot(roc_curve, main = "Receiver Operating Characteristic (ROC) Curve",
     col = colors[4], lwd = 2, legacy.axes = TRUE, print.auc = TRUE,
     print.auc.x = 0.5, print.auc.y = 0.3, print.auc.cex = 1.5, # Increase AUC font size
     auc.polygon = TRUE, auc.polygon.col = colors[2], # Set color of the area under the curve
     grid = TRUE, grid.col = "lightgray", grid.lty = 3,
     xlab = "False Positive Rate", ylab = "True Positive Rate",
     cex.main = 1.5, cex.lab = 1.2, cex.axis = 1.2)

# Add a legend
legend("bottomright", legend = c("ROC Curve"), col = colors[2], lwd = 2, cex = 1.2)

cutoffs <- c(0, 0.2, 0.4,0.6, 0.8,1)
risk_categories <- c( "Very Low <0.15", "Low <0.35","Moderate <0.55", "High <0.75", "Very High <1")


merged_data2<-cbind(merged_data, predicted)

merged_data2$risk_category <- cut(merged_data2$predicted, breaks = cutoffs, labels = risk_categories, include.lowest = TRUE)
merged_data2<-cbind(merged_data2, dataset_merge)
testtable <- table(merged_data2$epidemic, merged_data2$COUNTRY.x)
non_meningitis_risk <-merged_data2[, c("COUNTRY.x", "risk_category")]

risk_table <- non_meningitis_risk %>%
  group_by(COUNTRY.x, risk_category)

table(risk_table$risk_category, risk_table$COUNTRY.x)

non_meningitis_risk <-merged_data2[, c("COUNTRY.x", "risk_category")]
country_list <- c(
  "Benin", "Burkina Faso", "Burundi", "Cameroon", "Central African Republic",
  "Chad", "Côte d'Ivoire", "Eritrea", "Ethiopia", "Democratic Republic of the Congo",
  "Gambia", "Ghana", "Guinea", "Guinea-Bissau", "Kenya", "Mali", "Mauritania",
  "Niger", "Nigeria", "Rwanda", "Senegal", "South Sudan", "Sudan", "Tanzania",
  "Togo", "Uganda"
)

non_meningitis_risk <- non_meningitis_risk  %>% 
  filter(!COUNTRY.x %in% country_list)
risk_table <- non_meningitis_risk %>%
  group_by(COUNTRY.x, risk_category)
table(risk_table$risk_category, risk_table$COUNTRY.x)



non_meningitis_risk <-merged_data2[, c("COUNTRY.x", "risk_category")]
country_list <- c(
  "Benin", "Burkina Faso", "Burundi", "Cameroon", "Central African Republic",
  "Chad", "Côte d'Ivoire", "Eritrea", "Ethiopia", "Democratic Republic of the Congo",
  "Gambia", "Ghana", "Guinea", "Guinea-Bissau", "Kenya", "Mali", "Mauritania",
  "Niger", "Nigeria", "Rwanda", "Senegal", "South Sudan", "Sudan", "Tanzania",
  "Togo", "Uganda"
)

non_meningitis_risk <- non_meningitis_risk  %>% 
  filter(COUNTRY.x %in% country_list)
risk_table <- non_meningitis_risk %>%
  group_by(COUNTRY.x, risk_category)
table(risk_table$risk_category, risk_table$COUNTRY.x)

# Convert proportions to percentage format (optional)
#proportions <- prop.table(proportions, margin = 1) * 100
# If you want to see the distribution of risk categories, you can use table()
table(merged_data2$risk_category)
merged_dataset2<-st_as_sf(merged_data2)
plot(merged_dataset2['risk_category']) 



merged_data2<-st_as_sf(merged_data2)

# Extract the column you want to convert to raster
column_to_raster <- merged_data2$risk_category

# Create a raster template from the shapefile
raster_template <- raster(extent(merged_data2), res = 0.1)  # You can adjust resolution as needed

# Convert the column to raster
rasterized_column <- rasterize(merged_data2, raster_template, field = column_to_raster)

# Plot the rasterized column
library(RColorBrewer)
magma_like_palette <- brewer.pal(5, "Blues")

plot(rasterized_column, col=magma_like_palette)



perform_analysis <- function(data) {
  # Set seed for reproducibility
  
  
  # Create training and test datasets
  training.samples <- data$epidemic %>% createDataPartition(p = 0.8, list = FALSE)
  train.data <- data[training.samples, ]
  test.data <- data[-training.samples, ]
  
  # Fit a logistic regression model
  #backwardsmodel <- glm(epidemic ~ rainfall3 + Aero3 + humidity3 + windspeed3 + Land_category,
  #family = binomial, data = train.data, method = "brglmFit")
  backwardsmodel <- glm(epidemic ~ Aero3 + humidity3 + rainfall3, 
                        data = merged_data, 
                        family = "binomial",
                        weights = merged_data$weightswithcosine,method = "brglmFit")
  options(scipen = 999)
  cat("Summary of the logistic regression model:\n")
  print(summary(backwardsmodel))
  caret::varImp(model2)
  # Predict on the test dataset
  predicted <- predict(backwardsmodel, test.data, type = "response")
  
  # Find optimal cutoff probability
  optimal <- optimalCutoff(test.data$epidemic, predicted)[1]
  cat("Optimal cutoff probability:", optimal, "\n")
  
  # Create confusion matrix
  predicted_classes <- ifelse(predicted > 0.4, 1, 0)
  conf_matrix <- table(Actual = test.data$epidemic, Predicted = predicted_classes)
  print("Confusion Matrix:")
  print(conf_matrix)
  
  # Calculate sensitivity and specificity
  sensitivity<- sensitivity(test.data$epidemic, predicted_classes)
  
  
  #calculate specificity
  specificity<-  specificity(test.data$epidemic, predicted_classes)
  
  # Return sensitivity and specificity
  return(c(sensitivity, specificity))
}


# Run the analysis multiple times and average the results
num_runs <- 10
sensitivity_values <- numeric(num_runs)
specificity_values <- numeric(num_runs)

for (i in 1:num_runs) {
  cat("\nRun:", i, "\n")
  results <- perform_analysis(merged_data)
  sensitivity_values[i] <- results[1]
  specificity_values[i] <- results[2]
}

# Calculate average sensitivity and specificity
average_sensitivity <- mean(sensitivity_values)
average_specificity <- mean(specificity_values)

# Calculate 95% confidence interval
confidence_intervalsensitivty <- t.test(sensitivity_values)$conf.int

# Calculate 95% confidence interval
confidence_intervalspecificty <- t.test(specificity_values)$conf.int


#

cat("Sensitivity:", average_sensitivity, "(", confidence_intervalsensitivty, ")\n")
cat("Specificity:", average_specificity, "(", confidence_intervalspecificty, ")\n")



# Load necessary packages
library(ggplot2)
library(reshape2) # For melt function
library(corrplot) # For correlation plot

test_daata<-merged_data
test_daata <- lapply(test_daata, as.factor)

# Convert factors to numeric
test_daata <- lapply(test_daata, as.numeric)
test_data <- as.data.frame(test_daata)
test_data <- test_data[, !colnames(test_data) %in% c("epidemic", "Pp_dnst")]
test_data$Rainfall<-test_data$rainfall3
test_data$AOD<-test_data$Aero3
test_data$Humidity<-test_data$humidity3
test_data$Windspeed<-test_data$windspeed3
test_data <- test_data[, !colnames(test_data) %in% c("windspeed3", "humidity3", "Aero3", "rainfall3","cosine_latitude"    
                                                     ,"scaled_cos_latitude","weights","weightswithcosine")]
# Compute correlation matrix
correlation_matrix <- cor(test_data)

# Visualize correlation matrix using a heatmap
# Using ggplot2
ggplot(melt(correlation_matrix), aes(Var1, Var2, fill=value)) +
  geom_tile(color="white") +
  scale_fill_gradient2(low="blue", mid="white", high="red", midpoint=0, limit=c(-1,1), space="Lab", name="Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,)) +
  coord_fixed()

corrplot(correlation_matrix, method = "color", type = "upper", 
         addCoef.col = "black", number.cex = 1.2, tl.cex = 1.2,
         tl.col = "black", col = colorRampPalette(c("#ff0000", "white", "#195696"))(10000))



