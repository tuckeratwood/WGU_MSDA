churn <- read.csv("C:/Users/atwoo/OneDrive/Desktop/MSDADirectory/churn_clean.csv")
# imports data

library(plyr) # using plyr package for necessary statistical tools such as mutate 
library(dplyr) # using dplyr package for necessary statistical tools such as count
library(purrr) # using purrr package to use map_dbl function and calculate total within cluster sum-of-squares
library(ggplot2) # using ggplot2 package to visualize data
library(cluster) # using cluster package to use silhouette function to analyze results

churn$Income <- scale(churn$Income)
churn$Bandwidth_GB_Year <- scale(churn$Bandwidth_GB_Year)
# scales necessary variables

churn_cln <- churn %>%
  select('Income','Bandwidth_GB_Year')
# selects variables relevant to k-means clustering

write.csv(churn_cln, "C:/Users/atwoo/OneDrive/Desktop/MSDADirectory/churn_cln_212T1.csv")
# creates CSV file for cleaned data set

set.seed(8)
# sets seed at specific value to ensure consistency in k-means analysis which uses random number generations

tot_withinss <- map_dbl(1:10, function(k){
  model <- kmeans(churn_cln[,1:2], centers = k)
  model$tot.withinss})
# calculate total within-cluster sum of squares for k-means analyses from 1 to 10 centers

elbow <- data.frame(
  k = 1:10,
  tot_withinss = tot_withinss)
# creates data frame to be used to create plot of total within-cluster sum of squares by number of centers

ggplot(elbow, aes(x = k, y = tot_withinss)) + geom_line(linewidth = 2) + scale_x_continuous(breaks = 1:10) +
  labs(title = "Total Within-Cluster Sum of Squares for Income and Bandwidth") +
  ylab("Total Within-Cluster Sum of Squares")
# creates plot of total within-cluster sum of squares by number of centers

set.seed(8)
# sets seed at specific value to ensure consistency in k-means analysis which uses random number generations

kmodel <- kmeans(churn_cln[,1:2], centers = 3)
# creates k-means model with number of centers determined by elbow method

churn_clust <- mutate(churn_cln, cluster = kmodel$cluster)
# creates new data frame with cluster designations appended to churn data used to generate k-means model

count(churn_clust, cluster)
# provides number of data points in each cluster

churn_clust %>%
  group_by(cluster) %>%
  summarize_all(list(mean))
# provides mean of relevant variables for k-means model and analysis

ggplot(churn_clust, aes(x=Income, y=Bandwidth_GB_Year, color = factor(cluster))) + geom_point(size = 2) +
  labs(title = "Income and Bandwidth, Scaled and Clustered") +
  ylab("Bandwidth GB Per Year")
# creates scatterplot of relevant variables, colored by cluster

SIL <- silhouette(churn_clust$cluster, dist(churn_clust[,1:2]))
summary(SIL)
# calculates average silhouette width of data points in k-means model
