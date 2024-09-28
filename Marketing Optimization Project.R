# Load necessary libraries
library(dplyr)
library(ggplot2)
library(cluster)
library(tidyr)
library(caret)
library(reshape2)
library(factoextra)

# Data Import (simulating marketing data from Baidu's platform)
# Replace 'marketing_data.csv' with your actual file
data <- read.csv("data/marketing_data.csv")

# Data Cleaning: Remove NA values, handle outliers, and irrelevant features
cleaned_data <- data %>%
  filter(!is.na(Clicks), !is.na(Conversion_Volume), !is.na(Exposure), !is.na(Budget)) %>% # Drop NAs
  mutate(Clicks = ifelse(Clicks > quantile(Clicks, 0.99), quantile(Clicks, 0.99), Clicks), # Capping extreme outliers
         Conversion_Volume = ifelse(Conversion_Volume > quantile(Conversion_Volume, 0.99), 
                                    quantile(Conversion_Volume, 0.99), Conversion_Volume)) %>% 
  select(Industry, Budget, Exposure, Clicks, Conversion_Volume, Search_Index)

# Standardization of Features: Standardize to have a mean of 0 and SD of 1
scaler <- preProcess(cleaned_data[, c("Budget", "Exposure", "Clicks", "Conversion_Volume", "Search_Index")], method = c("center", "scale"))
standardized_data <- predict(scaler, cleaned_data)

# KMeans Clustering: Using 3 clusters (customizable)
set.seed(123)
kmeans_result <- kmeans(standardized_data[, c("Clicks", "Conversion_Volume", "Exposure", "Budget")], centers = 3)

# Add cluster labels to the dataset
cleaned_data$Cluster <- kmeans_result$cluster

# Visualize the KMeans clusters: Scatter Plot and Heatmap

# Scatter plot of Clicks vs. Conversion Volume colored by cluster
ggplot(cleaned_data, aes(x = Clicks, y = Conversion_Volume, color = as.factor(Cluster))) +
  geom_point(size = 3, alpha = 0.7) +
  labs(title = "KMeans Clustering of Marketing Data",
       x = "Clicks", y = "Conversion Volume", color = "Cluster") +
  theme_minimal()

# Heatmap visualization: showing Conversion Rate by Industry and Cluster
heatmap_data <- cleaned_data %>%
  group_by(Industry, Cluster) %>%
  summarise(Average_Conversion = mean(Conversion_Volume))

# Reshape data for heatmap
heatmap_data_wide <- dcast(heatmap_data, Industry ~ Cluster, value.var = "Average_Conversion")

# Create the heatmap
ggplot(melt(heatmap_data_wide), aes(x = as.factor(Cluster), y = Industry, fill = value)) +
  geom_tile() +
  labs(title = "Heatmap of Conversion Volume by Industry and Cluster", 
       x = "Cluster", y = "Industry", fill = "Average Conversion Volume") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  theme_minimal()

# Analyze advertising effectiveness across industries
industry_effectiveness <- cleaned_data %>%
  group_by(Industry) %>%
  summarise(Average_Clicks = mean(Clicks),
            Average_Exposure = mean(Exposure),
            Average_Conversion = mean(Conversion_Volume))

# Print effectiveness by industry
print(industry_effectiveness)

# Optimization Suggestions: Investigating differences between industries
# Based on conversion increases in education, tourism, and beauty industries
conversion_rates <- cleaned_data %>%
  group_by(Industry) %>%
  summarise(Average_Conversion_Rate = mean(Conversion_Volume / Exposure))

# Save cleaned and clustered data for future analysis
write.csv(cleaned_data, "data/cleaned_clustered_marketing_data.csv")
