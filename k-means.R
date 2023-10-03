#use this code to clean the global environment
rm(list=ls())
##load all the library needed
# install.packages("readr")
# install.packages("tidyverse")
# install.packages("tidyr")
# install.packages("ggplot2")
# install.packages("dylyer")
# install.packages("data.table")


###### call the library
library(readr)
library(tidyverse)
library(tidyr)
library(data.table)
library(dplyr) # for data manipulation
library(ggplot2) # for data visualization
# Modeling packages
library(cluster) # for general clustering algorithms
library(factoextra) # for visualizing cluster results
library(cluster)
library(readr)
library(plotly)
library(fpc)
mall <- read_csv("Data/Mall_Customers.csv")
View(mall)
#EDA
dim(mall)
##contains 200 observation and 5 variables
head(mall,10)
tail(mall,10)
sum(is.na(mall))
str(mall)
#remove white spaces from column names
colnames(mall) <- gsub(" ", "", colnames(mall))
##rename columns
str(mall)
mall <- mall %>%
  rename(Gender = Genre,
                   AnnualIncome = `AnnualIncome(k$)`,
                   SpendingScore = `SpendingScore(1-100)`)

#Get the summary of the data however we do not need customerid
mall = mall[-1]
summary(mall)
##check for categorical varaible
cat <- sapply(mall, is.character)
cat_name <- names(cat[cat])
#checking for outliers
numeric_data <- mall[,sapply(mall, is.numeric)]

boxplot(numeric_data, 
        main="Box Plot to Check for Outliers",
        ylab="Data Values")
##from the box plot we can see that there is an outlier in annualincome
#since,we have one outlier in annual income,we neglet it as it may be the person with higher annual income.
gender_counts <- table(mall$Gender)
barplot(gender_counts, 
        main = "Gender Distribution",
        xlab = "Gender",
        ylab = "Count",
        col = "skyblue",  # Bar color
        names.arg = names(gender_counts))  # Names for bars
###females are more than males
# Calculate the kernel density estimate
density_est <- density(mall$AnnualIncome)

# Create a KDE plot using the plot() function
plot(density_est, 
     main = "KDE Plot of Annual Income",
     xlab = "Annual Income",
     ylab = "Density")
#the data is most concentrated at 60 which is the peak, the tail is from 120 to 150 where data are less
#concentrated.
# Scatter plot for the AnnualIncome & SpendingScore by gender
# Create a scatter plot 
ggplot(mall, aes(x = AnnualIncome, y = SpendingScore, color = Gender)) +
  geom_point() +
  labs(title = "Income vs. Score by Gender", x = "Annual Income", y = "Spending Score") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(20, 100, by = 20)) +
  scale_y_continuous(breaks = seq(20, 100, by = 20))
# Scatter plot for the AnnualIncome & SpendingScore
#We can see that most of the customer data points lies at annual income(40-70) and spending score (40-60).
#scatter plot for age and spending score
ggplot(mall, aes(x= Age, y=SpendingScore,colour=Gender))+
  geom_point() +
  labs(title= "Age vs Score by Gender", x = "Age", y = "spending Score") +
  theme_minimal () +
  scale_x_continuous(breaks = seq(10, 100, by = 10)) +
  scale_y_continuous(breaks = seq(20, 100, by = 20))

#We interpret that the age (40-60) of having spending score around (20-60),
#the age (20-40) of having higher spending score around (40-100) and the age (60-70) 
#of having balanced spending score around (40-60) respectively .  

##data preprocessing
#cganging gender to 1 and 0 male = 1 female is 0
mall$Gender <- ifelse(mall$Gender =="Male", 1, 0)
#Variables often have different units and scales. For example, "AnnualIncome" might be in thousands of 
#dollars, while "SpendingScore" could be on a scale from 0 to 100. 
#Scaling ensures that variables are on a similar scale, making it easier to compare and analyze them.
scale_col <- c("AnnualIncome", "SpendingScore")
mall[scale_col] <- scale(mall[scale_col])
head(mall,5)

###exclude the dataset i  need for the k-means
new_mall = mall[3:4]

##model building and eveulation
##to get the optimal number for k-means, lets use the elbow method
#The Elbow Method involves running the K-means algorithm for a 
#range of cluster values and plotting the within-cluster sum of squares (WCSS) against the number of clusters.
#The "elbow point" on the plot is where the rate of decrease in WCSS starts to slow down. 
#This point suggests a good number of clusters.

wcss <- numeric(10)  # Initialize a vector to store WCSS values
for (i in 1:10) {
  kmeans_model <- kmeans(new_mall, centers = i)
  wcss[i] <- kmeans_model$tot.withinss
}
wcss
plot(1:10, wcss, type = "b", main = "Elbow Method", xlab = "Number of Clusters", ylab = "WCSS")

###another method to check the accuracy of the k-means
gap_stat <- clusGap(new_mall, FUN = kmeans, nstart = 25, K.max = 10)
plot(gap_stat, main = "Gap Statistics")

#We interpret that, i took the values for k is (1-20) & the line is steadily decreasing as k values increase.so considering elbow at k=5 clusters
#We interpret that, i took the values for k is (1-20) & the line is steadily decreasing as k values increase
#.so considering elbow at k=5 clusters
# Choose the optimal number of clusters (e.g., 3 based on the elbow curve)
optimal_clusters = 5
kmeans_model <- kmeans(new_mall, centers = optimal_clusters)
print(kmeans_model)
# Run K-means clustering with the chosen number of clusters
final_kmeans_model <- kmeans(new_mall, centers = optimal_clusters)
# Get cluster assignments for each data point
cluster_assignments <- final_kmeans_model$cluster
# View the cluster assignments
print(cluster_assignments)

##add the predicted values to the features
new_mall$cluster <- cluster_assignments 
 
###plot a 3d
# Create a scatter plot with colors representing clusters
ggplot(new_mall, aes(x = AnnualIncome, y = SpendingScore, color = factor(cluster))) +
  geom_point() +
  labs(title = "K-means Clustering", x ="AnnualIncome" , y = "SpendingScore") +
  theme_minimal()

# Create a 3D scatter plot with colors representing clusters
plot_ly(
  data = new_mall,
  x = ~AnnualIncome,
  y = ~SpendingScore,
  z = ~rep(0, nrow(new_mall)),  # Set a constant value for the z-axis (e.g., 0)
  color = ~factor(cluster),
  type = "scatter3d",
  mode = "markers"
) %>%
  layout(
    title = "Cluster Plot (3D with 2D Data)",
    scene = list(
      xaxis = list(title = "Annual Income"),
      yaxis = list(title = "Spending Score"),
      zaxis = list(title = "")  # No label for the z-axis
    )
  )

# Create a scatter plot of clusters with centroids
centroids <- aggregate(new_mall[c("SpendingScore", "AnnualIncome")], by = list(cluster = cluster_assignments), FUN = mean)
clr <- c("red", "green", "blue", "orange", "purple")
# Create a scatter plot of clusters with centroids
ggplot(new_mall, aes(x = AnnualIncome, y = SpendingScore, color = factor(cluster))) +
  geom_point(size = 3) +
  geom_point(data = centroids, aes(x = SpendingScore, y = AnnualIncome), color = "black", size = 5, shape = 4) +
  labs(title = "K-means Clustering", x = "AnnualIncome", y = "Spending Score") +
  scale_color_manual(values = clr) +  # Use the same cluster colors as needed
  theme_minimal()

##eveulate the model
#from the plot
cluster 1 (red) The data points in this cluster represents the 
customers with an high annual income tends to have high spending

