library(readr)
automobile = read_csv("/Users/yvette/Downloads/Automobile_data.csv")
library(dplyr)
attach(automobile)
View(automobile)
summary(automobile)
names(automobile)

###EDA################################
# Replace '?' with NA
automobile[automobile == "?"] <- NA
# Rename variables
automobile <- automobile %>%
  rename(
    normalized_losses = "normalized-losses",
    wheel_base = "wheel-base",
    engine_size = "engine-size",
    city_mpg = "city-mpg",
    highway_mpg = "highway-mpg",
    curb_weight="curb-weight",
    peak_rpm="peak-rpm",
    compression_ratio="compression-ratio",
    fuel_type="fuel-type",
    num_of_cylinders='num-of-cylinders',
    fuel_system='fuel-system'
  )

# Convert columns to numeric where appropriate
continuous_vars <- c("normalized_losses", "bore", "stroke", "horsepower", "peak_rpm", "price","symboling","wheel_base","engine_size","city_mpg","highway_mpg","length","width","height","curb_weight")
automobile[continuous_vars] <- lapply(automobile[continuous_vars], function(x) as.numeric(as.character(x)))

# Check for missing values
summary(automobile, function(x) sum(is.na(x)))

# Loop to create histograms
library(ggplot2)
for (var in continuous_vars) {
  p <- ggplot(automobile, aes_string(x = var)) +
    geom_histogram(bins = 30, fill = "blue", color = "black") + 
    theme_minimal() +
    labs(title = paste("Distribution of", var), x = var, y = "Frequency")
  
  print(p)
}


#############data preprocessing#########
## Replace NA with median in numeric columns
for (col in continuous_vars) {
  automobile[[col]][is.na(automobile[[col]])] <- median(automobile[[col]], na.rm = TRUE)
}
install.packages("psych") 
require(psych)
quantvars <- automobile[, continuous_vars]
corr_matrix=cor(quantvars)
round(corr_matrix,3)



# since high correlated, drop horsepower,curb_weight,city_mpg,length,width
# List of variables to be dropped
vars_to_drop <- c("horsepower", "curb_weight", "city_mpg", "length", "width")
automobile_reduced <- dplyr::select(automobile, -all_of(vars_to_drop))
continuous_vars1 <- c("normalized_losses", "bore", "stroke", "peak_rpm", "price","symboling","wheel_base","engine_size","highway_mpg","height","compression_ratio")
for (var in continuous_vars1) {
  plot <- ggplot(automobile_reduced, aes_string(x = var, y = "price")) +
    geom_point(color = "blue") +
    theme_minimal() +
    labs(title = paste("Price vs.", var), x = var, y = "Price")
  
  print(plot)
}



########Feature Engineering for LDA model##############
# Define market segments based on price and body-style
automobile_reduced <-automobile_reduced %>%
  rename(body_style='body-style',
         num_of_doors='num-of-doors',
         drive_wheels='drive-wheels',
         engine_location='engine-location',
         engine_type='engine-type'
  )
# Assuming k1, k2, and k3 are predefined thresholds
k1 <- quantile(automobile_reduced$price, 0.75, na.rm = TRUE)
k2 <- quantile(automobile_reduced$price, 0.25, na.rm = TRUE)
k3 <- median(automobile_reduced$engine_size, na.rm = TRUE)

automobile_reduced$segment <- 'mid-range'  # default category

automobile_reduced$segment[automobile_reduced$price >= k1] <- 'luxury'
automobile_reduced$segment[automobile_reduced$engine_size > k3 & 
                             automobile_reduced$segment != 'luxury'] <- 'sports'

automobile_reduced$segment[automobile_reduced$num_of_doors == 'four' & 
                             automobile_reduced$body_style %in% c('sedan', 'wagon') & 
                             automobile_reduced$segment != 'luxury' & 
                             automobile_reduced$segment != 'sports'] <- 'family'
automobile_reduced$segment[automobile_reduced$segment == 'mid-range'] <- 'economy'

automobile_reduced$segment <- as.factor(automobile_reduced$segment)

install.packages("MASS")
install.packages("klaR") 
library(MASS)
library(klaR)
install.packages("caret")
library(caret)

# Split the data into training and testing sets
set.seed(123)  # for reproducibility
index <- createDataPartition(automobile_reduced$segment, p = 0.8, list = FALSE)
train_data <- automobile_reduced[index, ]
test_data <- automobile_reduced[-index, ]

# Train the LDA model
mylda <- lda(segment ~ symboling + normalized_losses+wheel_base + height+highway_mpg+bore+ stroke+peak_rpm, data = train_data)
predictions <- predict(mylda, test_data)$class
confusionMatrix <- confusionMatrix(predictions, test_data$segment)
print(confusionMatrix)

#try different combination to get high related feature
mylda <- lda(segment ~ symboling + normalized_losses+wheel_base +highway_mpg+bore+ stroke+peak_rpm, data = train_data)
predictions <- predict(mylda, test_data)$class
confusionMatrix <- confusionMatrix(predictions, test_data$segment)
print(confusionMatrix)
#######best combination for lda
mylda <- lda(segment ~ symboling + normalized_losses+wheel_base+stroke+peak_rpm, data = train_data)
predictions <- predict(mylda, test_data)$class
confusionMatrix <- confusionMatrix(predictions, test_data$segment)
print(confusionMatrix)

mylda <- lda(segment ~ symboling + normalized_losses+wheel_base+stroke+peak_rpm,
             data = automobile_reduced)
mylda



# Define the features you want to include
features <- c("symboling", "normalized_losses", "wheel_base", "stroke", "peak_rpm")
library(klaR)

for(i in 1:(length(features) - 1)) {
  for(j in (i + 1):length(features)) {
    feature_pair <- paste(features[i], features[j], sep=" + ")
    formula <- as.formula(paste("segment ~", feature_pair))
    
    # Plot partimat for each pair directly
    partimat(formula, 
             data=automobile_reduced, 
             method="lda", 
             image.colors=c("light grey", "light green", "white", "orange"))

    Sys.sleep(time = 5) # Pause for 5 seconds
  }
}




#######Random forest############

library(randomForest)

# Function to calculate mode (most frequent value)
mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Replace missing values in factor columns with the mode
automobile_reduced <- automobile_reduced %>% 
  mutate(across(where(is.factor), ~ ifelse(is.na(.), mode(.), .)))

# Ensure that categorical variables are factors
automobile_reduced <- automobile_reduced %>% 
  mutate(across(where(is.character), as.factor))

# Check for any remaining missing values
sum(is.na(automobile_reduced))
colSums(is.na(automobile_reduced))

myforest <- randomForest(symboling ~ ., data = automobile_reduced,importance=TRUE, na.action = na.omit)
myforest
importance(myforest)
varImpPlot(myforest)

myforest <- randomForest(symboling ~ ., data = automobile_reduced,importance=TRUE, na.action = na.omit, do.trace=50)




