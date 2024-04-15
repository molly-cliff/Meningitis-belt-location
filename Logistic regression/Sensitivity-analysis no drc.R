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
setwd("C:/Users/mvc32/OneDrive - University of Cambridge/Documents/Climate_meningitis_belt")
#this is essentially reading in already created layers and joining together

library(raster)
#read in all the enviromental data
enviromentalsurfaces <-read_sf(dsn = ".", layer = "Landcoverreclassified2")
Pop_density <-read_sf(dsn = ".", layer = "Population_density")
windspeed <-read_sf(dsn = ".", layer = "windspeedfixed")
#Humiditycat <-read_sf(dsn = ".", layer = "Humidiityclusteragain")
Aerocat <-read_sf(dsn = ".", layer = "Aeroclustertesting")
#Humiditycat <-read_sf(dsn = ".", layer = "Humidityabsolute12")
#Humiditycat <-read_sf(dsn = ".", layer = "specifichumiditysallytest")
Humiditycat <-read_sf(dsn = ".", layer = "Humidityabsolute11")
#Humiditycat <-read_sf(dsn = ".", layer = "Humidiityclusteragain10")
Rainfallcat <-read_sf(dsn = ".", layer = "rainfalltestreducedclassesagainmissinggone")
Aerocat <-read_sf(dsn = ".", layer = "Aeroclustertestingfinal")
enviromentalsurfaces <-read_sf(dsn = ".", layer = "Landcovertest")
#Rainfallcat <-read_sf(dsn = ".", layer = "rainfall9clusters")
#correct names
Aerocat$aerozone<-Aerocat$zonalcat
Humiditycat$Humidityzone<-Humiditycat$zonalcat
Rainfallcat$rainfallzone<-Rainfallcat$zonalcat
windspeed$windspeedzone<-windspeed$zonalcat
plot(Rainfallcat['zonalcat'])
plot(Humiditycat['zonalcat'])
plot(Aerocat['zonalcat'])
plot(enviromentalsurfaces['nw_lnd_'])
#The sf_use_s2 function is used to control whether the sf package should use the s2 library for spatial operations.
sf::sf_use_s2(FALSE)
#change all of the shapefiles to data frames, essentially to be able to do analysis
Rainfallcat<-as.data.frame(Rainfallcat)
Aerocat<-as.data.frame(Aerocat)
windspeed<-as.data.frame(windspeed)
Humiditycat<-as.data.frame(Humiditycat)

#create code in r in which to join all the enviromental data together
Rainfallcat$code <- paste(Rainfallcat$NAME_2_x,Rainfallcat$GID_2)
Aerocat$code <- paste(Aerocat$NAME_2_x,Aerocat$GID_2)
Humiditycat$code <- paste(Humiditycat$NAME_2_x,Humiditycat$GID_2)
windspeed$code <- paste(windspeed$NAME_2_x,windspeed$GID_2)
Pop_density$code <- paste(Pop_density$NAME_2_x,Pop_density$GID_2)
enviromentalsurfaces$code <- paste(enviromentalsurfaces$NAME_2_,enviromentalsurfaces$GID_2)
#join together based on matching gid




#join together based on gid code

total3test <- merge(Aerocat,Rainfallcat, by=c("code", "code"))
total3test<-total3test[!duplicated(total3test[ , c("code")]),]
total3test <- merge(total3test,Humiditycat, by=c("code", "code"))
total3test<-total3test[!duplicated(total3test[ , c("code")]),]
total3test <- merge(total3test,windspeed, by=c("code", "code"))
total3test<-total3test[!duplicated(total3test[ , c("code")]),]
total3test <- merge(total3test,Pop_density, by=c("code", "code"))
total3test<-total3test[!duplicated(total3test[ , c("code")]),]
total3test <- merge(total3test,enviromentalsurfaces, by=c("code", "code"))
total4test<-total3test[!duplicated(total3test[ , c("code")]),]

finaldatasets<-total4test[ , c("COUNTRY_x.x", "NAME_1_x.x","GID_2.x","NAME_2_x.x",
                               "rainfallzone","Humidityzone", "aerozone","windspeedzone",
                               "Pp_dnst", "nw_lnd_", "geometry.y","code")]
finaldatasets<-finaldatasets[!duplicated(finaldatasets[ , c("GID_2.x")]),]
finaldatasets$windspeed<-finaldatasets$zonalcat



#read in shapefile of africa, this is to create the intersection weighting stuff
shape2 <-st_read("Shapefile_improved.shp")
finaldatasets$district_country <- paste(finaldatasets$NAME_2_x.x, finaldatasets$COUNTRY_x.x, sep = " ")
#create district intersection for weighting later
finaldatasets<-finaldatasets[!duplicated(finaldatasets[ , c("district_country")]),]
shape2$district_country <- paste(shape2$NAME_2, shape2$COUNTRY, sep = " ")
#merge together shapefile and enviromental data
test <- merge(shape2, finaldatasets, by = "district_country")



# plot one of the attributes
#we want this as a dataframe
test<-as.data.frame(test)
finaldatasets <-test [ , c("COUNTRY","NAME_1"  ,"GID_2","NAME_2",
                           "rainfallzone","Humidityzone", "aerozone","windspeedzone",
                           "Pp_dnst", "nw_lnd_","geometry.y","code" ,"district_country")]

sum(is.na(finaldatasets$rainfallzone))
#in this model, based on previous work we found that no including drc due to poor data quality significantly improved specificty and sensitivity
finaldatasets <- subset(finaldatasets, COUNTRY != "Cabo Verde")
finaldatasets <- subset(finaldatasets, COUNTRY != "Mauritius")
finaldatasets <- subset(finaldatasets, COUNTRY != "Seychelles")
finaldatasets <- subset(finaldatasets, COUNTRY != "São Tomé and Príncipe")




sum(is.na(finaldatasets$rainfallzone))
sum(is.na(finaldatasets$Humidityzone))
sum(is.na(finaldatasets$aerozone))
sum(is.na(finaldatasets$windspeedzone))
sum(is.na(finaldatasets$nw_lnd_))

#nearest neighbours function, for columns with nas, we use the modal value of districts with the same ADMN1 value as them
get_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Fill missing categorical values based on mode of matching "country" and "admn1"
for (i in 1:nrow(finaldatasets)) {
  if (is.na(finaldatasets$rainfallzone[i])) {
    mode_value <- with(finaldatasets, rainfallzone[COUNTRY == COUNTRY[i] & NAME_1 == NAME_1[i]])
    finaldatasets$rainfallzone[i] <- ifelse(length(mode_value) > 0, get_mode(mode_value), NA)
  }
}

for (i in 1:nrow(finaldatasets)) {
  if (is.na(finaldatasets$windspeedzone[i])) {
    mode_value <- with(finaldatasets, windspeedzone[COUNTRY == COUNTRY[i] & NAME_1 == NAME_1[i]])
    finaldatasets$windspeedzone[i] <- ifelse(length(mode_value) > 0, get_mode(mode_value), NA)
  }
}


for (i in 1:nrow(finaldatasets)) {
  if (is.na(finaldatasets$aerozone[i])) {
    mode_value <- with(finaldatasets, aerozone[COUNTRY == COUNTRY[i] & NAME_1 == NAME_1[i]])
    finaldatasets$aerozone[i] <- ifelse(length(mode_value) > 0, get_mode(mode_value), NA)
  }
}


for (i in 1:nrow(finaldatasets)) {
  if (is.na(finaldatasets$Humidityzone[i])) {
    mode_value <- with(finaldatasets, Humidityzone[COUNTRY == COUNTRY[i] & NAME_1 == NAME_1[i]])
    finaldatasets$Humidityzone[i] <- ifelse(length(mode_value) > 0, get_mode(mode_value), NA)
  }
}
sum(is.na(finaldatasets$rainfallzone))
sum(is.na(finaldatasets$Humidityzone))
sum(is.na(finaldatasets$aerozone))
sum(is.na(finaldatasets$windspeedzone))
sum(is.na(finaldatasets$nw_lnd_))

#here for those we are not able to match we match based on modal country
for (i in 1:nrow(finaldatasets)) {
  if (is.na(finaldatasets$Humidityzone[i])) {
    mode_value <- with(finaldatasets, Humidityzone[COUNTRY == COUNTRY[i]])
    finaldatasets$Humidityzone[i] <- ifelse(length(mode_value) > 0, get_mode(mode_value), NA)
  }
}
sum(is.na(finaldatasets$rainfallzone))
sum(is.na(finaldatasets$Humidityzone))
sum(is.na(finaldatasets$aerozone))
sum(is.na(finaldatasets$windspeedzone))
sum(is.na(finaldatasets$nw_lnd_))


#read in weekly and annual epidemic data, get rid of any duplicates
setwd("C:/Users/mvc32/OneDrive - University of Cambridge/Documents/Climate_meningitis_belt/Disease_data")
file_path <- "totalweeklyepidemic.xlsx"
weeklyincidence_merge<-readxl::read_excel(file_path)


special_countries <- c("Libya", "Western Sahara", "Lesotho", "Comoros")
special_rows <- weeklyincidence_merge$COUNTRY_xx %in% special_countries

special_rows <- weeklyincidence_merge %>%
  filter(COUNTRY_xx  %in% special_countries)

# Remove duplicates in the specified column, unless the country is "Libya" or "Egypt"
weeklyincidence_merge<- weeklyincidence_merge%>%
  filter(!COUNTRY_xx %in% special_countries) %>%
  distinct(district_country, .keep_all = TRUE) %>%
  bind_rows(special_rows)


#merge together final dataset and epidemic data
merged_data <- merge(finaldatasets, weeklyincidence_merge, by = "code", all = TRUE)
file_path <- "totalannualepidemic2.xlsx"
annualincidence_merge<-readxl::read_excel(file_path)



special_rows <- annualincidence_merge$COUNTRY_xx %in% special_countries

special_rows <- annualincidence_merge %>%
  filter(COUNTRY_xx  %in% special_countries)

# Remove duplicates in the specified column, unless the country is "Libya" or "Egypt"
annualincidence_merge<- annualincidence_merge%>%
  filter(!COUNTRY_xx %in% special_countries) %>%
  distinct(district_country, .keep_all = TRUE) %>%
  bind_rows(special_rows)




merged_data <- merge(merged_data, annualincidence_merge, by = "code", all = TRUE)



#create new epidemic column binary, based on weekly and monthly epidemic
merged_data$epidemic <- ifelse(merged_data$epidemic_annual == 1 | merged_data$epidemic_weekly == 1, 1, 0)
merged_data$epidemic[is.na(merged_data$epidemic)] <- 0


merged_data <-merged_data [ , c("COUNTRY.x","code","NAME_2.x",
                                "rainfallzone","Humidityzone", "aerozone","windspeedzone",
                                "Pp_dnst", "nw_lnd_", "epidemic", "district_country","geometry.y" )]
merged_data$Aero3<-merged_data$aerozone
merged_data$Aero3<- as.factor(merged_data$Aero3)


#data as factor for logistical analysis
#merged_data[is.na(merged_data)] <- 0
merged_data$humidity3<-merged_data$Humidityzone
merged_data$humidity3<-as.factor(merged_data$humidity3)
merged_data$windspeed3<-merged_data$windspeedzone
merged_data$windspeed3<-as.factor(merged_data$windspeed3)
merged_data$rainfall3<-merged_data$rainfallzone
merged_data$rainfall3<- as.factor(merged_data$rainfall3)

merged_data$Aero3<-merged_data$aerozone
merged_data$Aero3<-as.factor (merged_data$Aero3)
merged_data$Land_category<-as.factor (merged_data$nw_lnd_)
#crating district area weight




dup_rows <- duplicated(merged_data$code)
dup_rows <- merged_data[duplicated(merged_data$code) | duplicated(merged_data$code, fromLast = TRUE), ]
dup_rows <- dup_rows[!(dup_rows$COUNTRY.x %in% c("Libya", "Lesotho", "Western Sahara")), ]
merged_data <- anti_join(merged_data, dup_rows, by = "code")



first_instance_indices <- !duplicated(dup_rows$code)

# Filter the dataset to keep only the first instance of each duplicate row
unique_df <- dup_rows[first_instance_indices, ]

merged_data <- rbind(unique_df, merged_data)

# Print the filtered dataset A


#creating district ppdensity and area weight varialbe, first make them as units and then x them, this isn't
#used in the model

merged_data[is.na(merged_data)] <- 0
merged_data <- merged_data[merged_data$COUNTRY.x != 0, ]


merged_data <- subset(merged_data, COUNTRY.x != "Democratic Republic of the Congo")
dataset_merge<-merged_data

#final dataset! 

merged_data <-merged_data [ , c("epidemic", "rainfall3"  ,"Aero3",  "Pp_dnst",   
                                "humidity3", "windspeed3", "Land_category")]





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
#write_xlsx(merged_data, path = "Final_dataset2.xlsx")
#this part of data is read for analysis now
#Finaldataset <- "Final_dataset2.xlsx"
#Finaldataset<-readxl::read_excel(Finaldataset)
#merged_data<-Finaldataset
# Inspect the data
sample_n(merged_data, 3)


set.seed(111)


class_weights <- ifelse(merged_data$epidemic == 1, 6.07, 0.544)  # Weighting epidemics 3 times as much



model2 <- glm(epidemic ~ Aero3 + humidity3 + rainfall3, 
              data = merged_data, 
              family = "binomial",
              weights = ifelse(merged_data$epidemic == 1, 6.07, 0.5441),method = "brglmFit")

summary(model2)
# McFadden’s R2,In practice, values over 0.40 indicate that a model fits the data very well.
pscl::pR2(model2)["McFadden"]
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

predicted_classes <- ifelse(predicted > 0.5, 1, 0)
conf_matrix <- table(Actual = merged_data$epidemic,Predicted = predicted_classes )
print(conf_matrix)


# Calculate sensitivity and specificity
sensitivity<- sensitivity(merged_data$epidemic, predicted_classes)


#calculate specificity
specificity<-  specificity(merged_data$epidemic, predicted_classes)

cat("Sensitivity:", sensitivity)
cat("Specificity:", specificity)



cutoffs <- c(0, 0.1, 0.5,0.75,1)
risk_categories <- c( "Very Low <0.1", "Low <0.5","Moderate <0.75", "Very High <1")


merged_data2<-cbind(merged_data, predicted)

merged_data2$risk_category <- cut(merged_data2$predicted, breaks = cutoffs, labels = risk_categories, include.lowest = TRUE)
merged_data2<-cbind(merged_data2, dataset_merge)
testtable <- table(merged_data2$epidemic, merged_data2$COUNTRY.x)
non_meningitis_risk <-merged_data2[, c("COUNTRY.x", "risk_category")]

risk_table <- non_meningitis_risk %>%
  group_by(COUNTRY.x, risk_category)


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
magma_like_palette <- brewer.pal(5, "Paired")

plot(rasterized_column, col=magma_like_palette)

accuracy <- mean((predicted >= 0.5) == merged_data$epidemic)

plot(rasterized_column, col=magma_like_palette)

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

# Calculate precision, recall, and F1 score
conf_matrix <- table(Actual = merged_data$epidemic, Predicted = predicted_classes)
precision <- specificity(merged_data$epidemic, predicted_classes)
recall <- sensitivity(merged_data$epidemic, predicted_classes)
f1_score <- 2 * (precision * recall) / (precision + recall)

# Calculate AUC
roc_obj <- roc(merged_data$epidemic, predicted)
auc <- auc(roc_obj)
print(paste("Accuracy:", round(accuracy, 3)))
print(paste("Precision:", round(precision, 3)))
print(paste("Recall:", round(recall, 3)))
print(paste("F1 Score:", round(f1_score, 3)))
print(paste("AUC:", round(auc, 3)))



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
                        weights = ifelse(merged_data$epidemic == 1, 6.07, 0.5441),method = "brglmFit")
  options(scipen = 999)
  cat("Summary of the logistic regression model:\n")
  print(summary(backwardsmodel))
  
  # Predict on the test dataset
  predicted <- predict(backwardsmodel, test.data, type = "response")
  
  # Find optimal cutoff probability
  optimal <- optimalCutoff(test.data$epidemic, predicted)[1]
  cat("Optimal cutoff probability:", optimal, "\n")
  
  # Create confusion matrix
  predicted_classes <- ifelse(predicted > 0.5, 1, 0)
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


valid_indices <- is.finite(merged_data$epidemic) & is.finite(predicted)
epidemic <- merged_data$epidemic[valid_indices]
predicted <- predicted[valid_indices]

# Plot ROC curve with default settings

roc_curve <- roc(epidemic, predicted)

# Choose colors from Wes Anderson palette
colors <- wes_palette("GrandBudapest2", n = 4)
plot(roc_curve, main = "Receiver Operating Characteristic (ROC) Curve",
     col = colors[4], lwd = 2, legacy.axes = TRUE, print.auc = TRUE,
     print.auc.x = 0.5, print.auc.y = 0.3, auc.polygon = TRUE,
     auc.polygon.col = colors[2], # Set color of the area under the curve
     grid = TRUE, grid.col = "lightgray", grid.lty = 3,
     xlab = "False Positive Rate", ylab = "True Positive Rate")

# Add a legend
legend("bottomright", legend = c("ROC Curve"), col = colors[2], lwd = 2)

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
  model <- glm(epidemic ~ Aero3 + humidity3 + rainfall3, 
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

# Plot F1 scores against weights
df <- data.frame(weights = weights, f1_scores = f1_scores)
ggplot(df, aes(x = weights, y = f1_scores)) +
  geom_line() +
  geom_point(data = data.frame(weights = optimum_weight, f1_scores = max_f1_score),
             aes(x = weights, y = f1_scores), color = "red", size = 3) +
  labs(x = "Weight for minority class", y = "F1 Score",
       title = "F1 Score vs. Weight for Minority Class") +
  theme_minimal()





model2 <- glm(epidemic ~ Aero3 + humidity3 + rainfall3+ Land_category, 
              data = merged_data, 
              family = "binomial",
              weights = ifelse(merged_data$epidemic == 1, 0.805, 0.195),method = "brglmFit")



summary(model2)
# McFadden’s R2,In practice, values over 0.40 indicate that a model fits the data very well.
pscl::pR2(model2)["McFadden"]
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

predicted_classes <- ifelse(predicted > 0.3, 1, 0)
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
     print.auc.x = 0.5, print.auc.y = 0.3, auc.polygon = TRUE,
     auc.polygon.col = colors[2], # Set color of the area under the curve
     grid = TRUE, grid.col = "lightgray", grid.lty = 3,
     xlab = "False Positive Rate", ylab = "True Positive Rate")

# Add a legend
legend("bottomright", legend = c("ROC Curve"), col = colors[2], lwd = 2)


cutoffs <- c(0, 0.1, 0.3,0.5, 0.75,1)
risk_categories <- c( "Very Low <0.1", "Low <0.3","Moderate <0.5", "High <0.75", "Very High <1")


merged_data2<-cbind(merged_data, predicted)

merged_data2$risk_category <- cut(merged_data2$predicted, breaks = cutoffs, labels = risk_categories, include.lowest = TRUE)
merged_data2<-cbind(merged_data2, dataset_merge)
testtable <- table(merged_data2$epidemic, merged_data2$COUNTRY.x)
non_meningitis_risk <-merged_data2[, c("COUNTRY.x", "risk_category")]

risk_table <- non_meningitis_risk %>%
  group_by(COUNTRY.x, risk_category)

plot(rasterized_column, col=magma_like_palette)

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
magma_like_palette <- brewer.pal(5, "Paired")

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
  backwardsmodel <- glm(epidemic ~ Aero3 + humidity3 + rainfall3+  Land_category, 
                        data = merged_data, 
                        family = "binomial",
                        weights = ifelse(merged_data$epidemic == 1, 0.805, 0.195),method = "brglmFit")
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
  predicted_classes <- ifelse(predicted > 0.3, 1, 0)
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

