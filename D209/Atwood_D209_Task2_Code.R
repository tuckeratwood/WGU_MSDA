churn <- read.csv("C:/Users/atwoo/OneDrive/Desktop/MSDADirectory/churn_clean.csv")
# imports dataset

# CLEANING DATA:

sum(duplicated(churn)) # checks for full duplicates (0)

library(plyr) # using plyr package
library(dplyr) # using dplyr package

churn %>%
  count(Customer_id) %>%
  filter(n > 1) # checks for partial duplicates with matching Customer_id (0)

library(naniar) # using naniar package
n_miss(churn) # total missing values (0)

hist(churn$Children) # visualization of Children data; skewed right
boxplot(churn$Children) # 3 outlier values appear
chi_outliers <- churn %>%
  filter((Children < quantile(churn$Children, 0.25, na.rm = TRUE) - IQR(churn$Children, na.rm = TRUE) * 1.5) 
         | (Children > quantile(churn$Children, 0.75, na.rm = TRUE) + IQR(churn$Children, na.rm = TRUE) * 1.5))
# find outliers using IQR method
count(chi_outliers) # 401 outliers
summary(chi_outliers$Children) # outlier range is 8-10

hist(churn$Age) # visualization of Age data; relatively uniform
boxplot(churn$Age) # no outliers present
age_outliers <- churn %>%
  filter((Age < quantile(churn$Age, 0.25, na.rm = TRUE) - IQR(churn$Age, na.rm = TRUE) * 1.5) 
         | (Age > quantile(churn$Age, 0.75, na.rm = TRUE) + IQR(churn$Age, na.rm = TRUE) * 1.5))  
# find outliers using IQR method
count(age_outliers) # confirms zero Age outliers

hist(churn$Tenure) # visualization of Tenure data; bimodal
boxplot(churn$Tenure) # no outliers present
ten_outliers <- churn %>%
  filter((Tenure < quantile(churn$Tenure, 0.25, na.rm = TRUE) - IQR(churn$Tenure, na.rm = TRUE) * 1.5) 
         | (Tenure > quantile(churn$Tenure, 0.75, na.rm = TRUE) + IQR(churn$Tenure, na.rm = TRUE) * 1.5))
# find outliers using IQR method
count(ten_outliers) # confirms zero Tenure outliers

hist(churn$MonthlyCharge) # visualization of MonthlyCharge data; normal distribution
boxplot(churn$MonthlyCharge) # no outliers present
mon_outliers <- churn %>%
  mutate(mon_z = scale(churn$MonthlyCharge)) %>%
  filter(mon_z > 3 | mon_z < -3)  # find outliers using z-score method
count(mon_outliers) # confirms zero MonthlyCharge outliers

hist(churn$Bandwidth_GB_Year) # visualization of Bandwidth_GB_Year data; bimodal
boxplot(churn$Bandwidth_GB_Year) # no outliers present
bgy_outliers <- churn %>%
  filter((Bandwidth_GB_Year < quantile(churn$Bandwidth_GB_Year, 0.25, na.rm = TRUE) - IQR(churn$Bandwidth_GB_Year, na.rm = TRUE) * 1.5) 
         | (Bandwidth_GB_Year > quantile(churn$Bandwidth_GB_Year, 0.75, na.rm = TRUE) + IQR(churn$Bandwidth_GB_Year, na.rm = TRUE) * 1.5))
# find outliers using IQR method
count(bgy_outliers) # confirms zero Bandwidth_GB_Year outliers

# RE-EXPRESSION OF CATEGORICAL VARIABLES:

churn$Churn <- as.numeric(revalue(churn$Churn, replace = c("No" = 0, "Yes" = 1)))
churn$Techie <- as.numeric(revalue(churn$Techie, replace = c("No" = 0, "Yes" = 1)))
churn$Multiple <- as.numeric(revalue(churn$Multiple, replace = c("No" = 0, "Yes" = 1)))
churn$OnlineSecurity <- as.numeric(revalue(churn$OnlineSecurity, replace = c("No" = 0, "Yes" = 1)))
churn$TechSupport <- as.numeric(revalue(churn$TechSupport, replace = c("No" = 0, "Yes" = 1)))
churn$DeviceProtection <- as.numeric(revalue(churn$DeviceProtection, replace = c("No" = 0, "Yes" = 1)))
# re-expresses necessary categorical binary data as numeric, 0 for No, 1 for Yes

library(fastDummies) # using fastDummies package
churn <- churn %>%
  dummy_cols("Contract") %>%
  rename(Contract_One_Year = 'Contract_One year') %>%
  rename(Contract_Two_Year = 'Contract_Two Year') %>%
  rename(Contract_Month_to_Month = 'Contract_Month-to-month')

churn <- churn %>%
  dummy_cols("InternetService") %>%
  rename(InternetService_Fiber_Optic = 'InternetService_Fiber Optic')
# re-expresses necessary categorical non-binary data as numeric using one-hot encoding

# SELECTING RELEVANT VARIABLES:

churn <- churn %>%
  select(Churn, Tenure, Age, Children, MonthlyCharge, Bandwidth_GB_Year, Techie, Multiple, OnlineSecurity, TechSupport, DeviceProtection,
         Contract_One_Year, Contract_Two_Year, Contract_Month_to_Month,
         InternetService_DSL, InternetService_Fiber_Optic, InternetService_None)
# selecting only variables that will be used in the analysis

write.csv(churn, "C:/Users/atwoo/OneDrive/Desktop/MSDADirectory/churn_209_task2.csv")
# saving prepared dataset as csv

# SPLITTING THE DATA:

set.seed(2)
rand_churn <- churn[sample(10000),]
# randomizes churn rows

churn_train <- rand_churn[1:7000,]
churn_test <- rand_churn[7001:10000,]
# creates 70-30 split for training and test data

write.csv(churn_train, "C:/Users/atwoo/OneDrive/Desktop/MSDADirectory/train_209_task2.csv")
write.csv(churn_test, "C:/Users/atwoo/OneDrive/Desktop/MSDADirectory/test_209_task2.csv")
# saving training and test dataset as csv files

# RANDOM FOREST ANALYSIS:

library(ranger)

forest_model <- ranger(Tenure ~ ., churn_train)

forest_model

churn_test$pred <- predict(forest_model, churn_test)$predictions

churn_test %>%
  mutate(residual = pred - Tenure) %>%
  summarize(mse = mean(residual^2))
