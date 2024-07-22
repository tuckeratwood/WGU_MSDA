churn <- read.csv("C:/Users/atwoo/OneDrive/Desktop/MSDADirectory/churn_clean.csv")
# imports data

library(plyr) # using plyr package for necessary statistical tools such as revalue and mutate 
library(dplyr) # using dplyr package for necessary statistical tools such as count
library(ggplot2) # using ggplot2 package for visualizations such as scree plot
library(factoextra) # using factoextra package to summarize results of PCA

churn$Lat <- scale(churn$Lat)
churn$Lng <- scale(churn$Lng)
churn$Income <- scale(churn$Income)
churn$Outage_sec_perweek <- scale(churn$Outage_sec_perweek)
churn$Tenure <- scale(churn$Tenure)
churn$MonthlyCharge <- scale(churn$MonthlyCharge)
churn$Bandwidth_GB_Year <- scale(churn$Bandwidth_GB_Year)
# scales all continuous quantitative variables

churn_cln <- churn %>%
  select('Lat','Lng','Income','Outage_sec_perweek','Tenure','MonthlyCharge','Bandwidth_GB_Year')
# selects variables relevant to PCA (continuous quantitative)

write.csv(churn_cln, "C:/Users/atwoo/OneDrive/Desktop/MSDADirectory/churn_cln_212T2.csv")
# creates CSV file for prepared data set

pca <- prcomp(churn_cln)
# conducts Principal Component Analysis (PCA)

pca
# provides look at PCA info, including loading matrix

get_eig(pca)
# provides statistics on PCA including eigenvalues, individual variances, and cumulative variances

fviz_eig(pca, choice = "eigenvalue", addlabels = TRUE)
# creates scree plot for PCA
# PC1, PC2, and PC3 all have eigenvalues greater than or equal to 1, so they will be kept, per the Kaiser Rule