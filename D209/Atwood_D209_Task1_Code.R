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

# RE-EXPRESSION OF CATEGORICAL VARIABLES:

churn$Techie <- as.numeric(revalue(churn$Techie, replace = c("No" = 0, "Yes" = 1)))
churn$Multiple <- as.numeric(revalue(churn$Multiple, replace = c("No" = 0, "Yes" = 1)))
churn$OnlineSecurity <- as.numeric(revalue(churn$OnlineSecurity, replace = c("No" = 0, "Yes" = 1)))
churn$TechSupport <- as.numeric(revalue(churn$TechSupport, replace = c("No" = 0, "Yes" = 1)))
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

# SCALING DATA:

normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x))) }
# creates normalizing function

churn$Tenure <- normalize(churn$Tenure)
churn$MonthlyCharge <- normalize(churn$MonthlyCharge)
# scales necessary variables

# SELECTING RELEVANT VARIABLES:

churn <- churn %>%
  select(Churn, Tenure, MonthlyCharge, Techie, Multiple, OnlineSecurity, TechSupport,
         Contract_One_Year, Contract_Two_Year, Contract_Month_to_Month,
         InternetService_DSL, InternetService_Fiber_Optic, InternetService_None)
# selecting only variables that will be used in the analysis

write.csv(churn, "C:/Users/atwoo/OneDrive/Desktop/MSDADirectory/churn_209_task1.csv")
# saving prepared dataset as csv

# SPLITTING THE DATA:

set.seed(1)
rand_churn <- churn[sample(10000),]
# randomizes churn rows

churn_train <- rand_churn[1:7000,]
churn_test <- rand_churn[7001:10000,]
# creates 70-30 split for training and test data

write.csv(churn_train, "C:/Users/atwoo/OneDrive/Desktop/MSDADirectory/train_209_task1.csv")
write.csv(churn_test, "C:/Users/atwoo/OneDrive/Desktop/MSDADirectory/test_209_task1.csv")

# K-NEAREST NEIGHBORS ANALYSIS:

library(class) # using class package

churn_actual <- rand_churn[7001:10000,]$Churn
churn_predicted <- knn(train = churn_train[-1], test = churn_test[-1], 
                       cl = rand_churn[1:7000,]$Churn, k = 85, prob = TRUE)
# creates actual and predicted churn values for test data

conf <- table(churn_actual, churn_predicted)
acc <- (conf[1,1] + conf[2,2])/(conf[1,1] + conf[1,2] + conf[2,1] + conf[2,2])
sens <- (conf[2,2]/(conf[2,2] + conf[1,2]))
spec <- (conf[1,1]/(conf[1,1] + conf[2,1]))
# creates table and calculates accuracy, sensitivity, and specificity for churn predictions using KNN

conf
acc
sens
spec
# displays the table and each of the calculated values

library(pROC) # using pROC package

churn_prob <- attr(churn_predicted, "prob")
ROC <- roc(churn_actual, churn_prob)
plot(ROC, col = 2)
auc(ROC)
# visualizes ROC curve and calculates area under the curve (AUC)
