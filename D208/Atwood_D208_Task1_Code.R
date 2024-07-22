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

library(ggplot2) # using ggplot2 package
churn %>%
  ggplot(aes(x=Lat, y=Lng)) + geom_point()
# scatterplot of Latitude and Longitude values; outliers present on each end of Latitude, low end of Longitude

churn %>%
  filter(Lng < -125) %>%
  count(State)
# 112 low Longitude values, 77 from Alaska, 35 from Hawaii

churn %>%
  filter(Lat < 24) %>%
  count(State)
# 75 low Latitude values, 35 from Hawaii, 40 from Puerto Rico

churn %>%
  filter(Lat > 50) %>%
  count(State)
# 77 high Latitude values, all from Alaska

item_outliers <- churn %>%
  filter(Item1 < 1 | Item1 > 8
         | Item2 < 1 | Item2 > 8
         | Item3 < 1 | Item3 > 8
         | Item4 < 1 | Item4 > 8
         | Item5 < 1 | Item5 > 8
         | Item6 < 1 | Item6 > 8
         | Item7 < 1 | Item7 > 8
         | Item8 < 1 | Item8 > 8)
# items 1-8 listed as scale from 1 to 8; finds values outside range
count(item_outliers) # confirms no values outside range

hist(churn$Population) # visualization of Population data; skewed right
boxplot(churn$Population) # many outliers present
pop_outliers <- churn %>%
  filter(Population < quantile(churn$Population, 0.25) - IQR(churn$Population) * 1.5
         | Population > (quantile(churn$Population, 0.75) + IQR(churn$Population) * 1.5))
# find outliers using IQR method
count(pop_outliers) # 937 outliers
summary(pop_outliers$Population) # outlier range is 31,816-111,850

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

hist(churn$Income) # visualization of Income data; skewed right
boxplot(churn$Income) # many outliers present
inc_outliers <- churn %>%
  filter((Income < quantile(churn$Income, 0.25, na.rm = TRUE) - IQR(churn$Income, na.rm = TRUE) * 1.5) 
         | (Income > quantile(churn$Income, 0.75, na.rm = TRUE) + IQR(churn$Income, na.rm = TRUE) * 1.5))
# find outliers using IQR method
count(inc_outliers) # 336 outliers
summary(inc_outliers$Income) # outlier range is 104,363-258,901

hist(churn$Outage_sec_perweek) # visualization of Outage_sec_perweek data; skewed right
boxplot(churn$Outage_sec_perweek) # many outliers present
outage_outliers <- churn %>%
  filter((Outage_sec_perweek < quantile(churn$Outage_sec_perweek, 0.25) - IQR(churn$Outage_sec_perweek) * 1.5) 
         | (Outage_sec_perweek > quantile(churn$Outage_sec_perweek, 0.75) + IQR(churn$Outage_sec_perweek) * 1.5))
# find outliers using IQR method
count(outage_outliers) # 76 outliers
summary(outage_outliers$Outage_sec_perweek) # outlier range is 0.1-21.2

hist(churn$Email) # visualization of Email data; normal distribution
boxplot(churn$Email) # 6 outlier values appear
email_outliers <- churn %>%
  mutate(email_z = scale(churn$Email)) %>%
  filter(email_z > 3 | email_z < -3)  # find outliers using z-score method
count(email_outliers) # 12 outliers
summary(email_outliers$Email) # outlier range is 1-23

hist(churn$Contacts) # visualization of Contacts data; skewed right
boxplot(churn$Contacts) # 2 outlier values appear
con_outliers <- churn %>%
  filter((Contacts < quantile(churn$Contacts, 0.25) - IQR(churn$Contacts) * 1.5) 
         | (Contacts > quantile(churn$Contacts, 0.75) + IQR(churn$Contacts) * 1.5))
# find outliers using IQR method
count(con_outliers) # 8 outliers
summary(con_outliers$Contacts) # outlier range is 6-7

hist(churn$Yearly_equip_failure) # visualization of Yearly_equip_failure data; skewed right
boxplot(churn$Yearly_equip_failure) # 3 outlier values appear
yef_outliers <- churn %>%
  filter((Yearly_equip_failure < quantile(churn$Yearly_equip_failure, 0.25) - IQR(churn$Yearly_equip_failure) * 1.5) 
         | (Yearly_equip_failure > quantile(churn$Yearly_equip_failure, 0.75) + IQR(churn$Yearly_equip_failure) * 1.5))
# find outliers using IQR method
count(yef_outliers) # 94 outliers
summary(yef_outliers$Yearly_equip_failure) # outlier range is 3-6

hist(churn$Tenure) # visualization of Tenure data; bimodal
boxplot(churn$Tenure) # no outliers present
ten_outliers <- churn %>%
  filter((Tenure < quantile(churn$Tenure, 0.25, na.rm = TRUE) - IQR(churn$Tenure, na.rm = TRUE) * 1.5) 
         | (Tenure > quantile(churn$Tenure, 0.75, na.rm = TRUE) + IQR(churn$Tenure, na.rm = TRUE) * 1.5))
# find outliers using IQR method
count(ten_outliers) # confirms zero Tenure outliers

hist(churn$MonthlyCharge) # visualization of MonthlyCharge data; normal distribution
boxplot(churn$MonthlyCharge) # 5 outlier values appear
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

# DATA EXPLORATION:

# summary statistics for response variable and all explanatory variables:
summary(churn$Tenure)
summary(churn$Population)
summary(churn$Children)
summary(churn$Age)
summary(churn$Income)
summary(churn$Outage_sec_perweek)
summary(churn$Email)
summary(churn$Yearly_equip_failure)
summary(churn$MonthlyCharge)
summary(churn$Bandwidth_GB_Year)
table(churn$Churn)
table(churn$Contract)
table(churn$DeviceProtection)
table(churn$TechSupport)
table(churn$InternetService)

# univariate visualizations of all variables:
hist(churn$Tenure, main = "Histogram of Tenure", xlab = "Tenure", col = 2, breaks = seq(0,80,5))
hist(churn$Population, main = "Histogram of Population", xlab = "Population", col = 3)
ggplot(churn, aes(Children)) + geom_bar(color = 9, fill = 4) + scale_x_continuous(breaks = seq(0,10,1)) +
  labs(title = "Bar Graph of Children")
ggplot(churn, aes(Age)) + geom_histogram(color = 9, fill = 5, breaks = seq(15,90,5)) + scale_x_continuous(breaks = seq(15,90,5)) +
  labs(title = "Histogram of Age")
hist(churn$Income, main = "Histogram of Income", xlab = "Income", col = 6)
hist(churn$Outage_sec_perweek, main = "Histogram of Outage Seconds Per Week", xlab = "Outage Seconds Per Week", col = 7)
ggplot(churn, aes(Email)) + geom_bar(color = 9, fill = 2) + scale_x_continuous(breaks = seq(0,24,2)) +
  labs(title = "Bar Graph of Emails Received")
ggplot(churn, aes(Yearly_equip_failure)) + geom_bar(color = 9, fill = 3) + scale_x_continuous(breaks = seq(0,6,1)) +
  labs(title = "Bar Graph of Yearly Equipment Failures") + xlab("Yearly Equipment Failures")
ggplot(churn, aes(MonthlyCharge)) + geom_histogram(color = 9, fill = 4, breaks = seq(60,300,20)) + scale_x_continuous(breaks = seq(60,300,20)) +
  labs(title = "Histogram of MonthlyCharge") + xlab("Monthly Charge")
ggplot(churn, aes(Bandwidth_GB_Year)) + geom_histogram(color = 9, fill = 5, breaks = seq(0,7500,500)) + scale_x_continuous(breaks = seq(0,7500,500)) +
  labs(title = "Histogram of Bandwidth Gigabytes Per Year") + xlab("Bandwidth Gigabytes Per Year")
ggplot(churn, aes(Churn)) + geom_bar(color = 9, fill = 6) + labs(title = "Bar Graph of Churn Rates")
ggplot(churn, aes(Contract)) + geom_bar(color = 9, fill = 7) + labs(title = "Bar Graph of Contracts")
ggplot(churn, aes(DeviceProtection)) + geom_bar(color = 9, fill = 2) + labs(title = "Bar Graph of Device Protection") +
  xlab("Device Protection")
ggplot(churn, aes(TechSupport)) + geom_bar(color = 9, fill = 3) + labs(title = "Bar Graph of Tech Support") +
  xlab("Tech Support")
ggplot(churn, aes(InternetService)) + geom_bar(color = 9, fill = 4) + labs(title = "Bar Graph of Internet Service") +
  xlab("Internet Service")

# bivariate visualizations of all variables:
ggplot(churn, aes(Population, Tenure)) + geom_point() + labs(title = "Scatterplot of Population vs. Tenure")
ggplot(churn, aes(Children, Tenure)) + geom_jitter()  + labs(title = "Jitterplot of Children vs. Tenure") +
  scale_x_continuous(breaks = seq(0,10,1))
ggplot(churn, aes(Age, Tenure)) + geom_jitter() + labs(title = "Jitterplot of Age vs. Tenure")
ggplot(churn, aes(Income, Tenure)) + geom_point() + labs(title = "Scatterplot of Income vs. Tenure")
ggplot(churn, aes(Outage_sec_perweek, Tenure)) + geom_point() + labs(title = "Scatterplot of Outage Seconds Per Week vs. Tenure") +
  xlab("Outage Seconds Per Week")
ggplot(churn, aes(Email, Tenure)) + geom_jitter() + labs(title = "Jitterplot of Email vs. Tenure") + 
  scale_x_continuous(breaks = seq(0,24,2))
ggplot(churn, aes(Yearly_equip_failure, Tenure)) + geom_jitter() + labs(title = "Jitterplot of Yearly Equipment Failure vs. Tenure") + 
  scale_x_continuous(breaks = seq(0,6,1)) + xlab("Yearly Equipment Failures")
ggplot(churn, aes(MonthlyCharge, Tenure)) + geom_jitter() + labs(title = "Jitterplot of Monthly Charge vs. Tenure") +
  xlab("Monthly Charge")
ggplot(churn, aes(Bandwidth_GB_Year, Tenure)) + geom_point() + labs(title = "Scatterplot of Bandwidth Gigabytes Per Year vs. Tenure") +
  xlab("Bandwidth Gigabytes Per Year")
ggplot(churn, aes(Churn, Tenure)) + geom_jitter() + labs(title = "Jitterplot of Churn vs. Tenure")
ggplot(churn, aes(Contract, Tenure)) + geom_jitter() + labs(title = "Jitterplot of Contract vs. Tenure")
ggplot(churn, aes(DeviceProtection, Tenure)) + geom_jitter() + labs(title = "Jitterplot of Device Protection vs. Tenure") +
  xlab("Device Protection")
ggplot(churn, aes(TechSupport, Tenure)) + geom_jitter() + labs(title = "Jitterplot of Tech Support vs. Tenure") +
  xlab("Tech Support")
ggplot(churn, aes(InternetService, Tenure)) + geom_jitter() + labs(title = "Jitterplot of Internet Service vs. Tenure") +
  xlab("Internet Service")

# RE-EXPRESSION OF CATEGORICAL VARIABLES:

churn$Churn <- as.numeric(revalue(churn$Churn, replace = c("No" = 0, "Yes" = 1)))
# converts Churn to numeric: 0 for No, 1 for Yes

churn$TechSupport <- as.numeric(revalue(churn$TechSupport, replace = c("No" = 0, "Yes" = 1)))
# converts TechSupport to numeric: 0 for No, 1 for Yes

churn$DeviceProtection <- as.numeric(revalue(churn$DeviceProtection, replace = c("No" = 0, "Yes" = 1)))
# converts DeviceProtection to numeric: 0 for No, 1 for Yes

library(fastDummies) # using fastDummies package
churn <- churn %>%
  dummy_cols("InternetService") %>%
  rename(InternetService_Fiber_Optic = 'InternetService_Fiber Optic') %>%
  select(-InternetService_None)
# re-expresses InternetService as numeric using one-hot encoding

churn <- churn %>%
  dummy_cols("Contract") %>%
  rename(Contract_One_Year = 'Contract_One year') %>%
  rename(Contract_Two_Year = 'Contract_Two Year') %>%
  select(-'Contract_Month-to-month')
# re-expresses Contract as numeric using one-hot encoding

churn <- churn %>%
  select(Tenure, Population, Children, Age, Income, Outage_sec_perweek, Email, Yearly_equip_failure, MonthlyCharge,
         Bandwidth_GB_Year, Churn, DeviceProtection, TechSupport, InternetService_DSL, InternetService_Fiber_Optic,
         Contract_One_Year, Contract_Two_Year)
# selecting only variables that will be used in the analysis

write.csv(churn, "C:/Users/atwoo/OneDrive/Desktop/MSDADirectory/churn_new.csv")

# CREATING INITIAL MODEL:

in_model <- lm(Tenure ~ Population + Children + Age + Income + Churn + Outage_sec_perweek + Email + Yearly_equip_failure +
              DeviceProtection + TechSupport + MonthlyCharge + Bandwidth_GB_Year + InternetService_Fiber_Optic + 
              InternetService_DSL + Contract_One_Year + Contract_Two_Year, churn)

summary(in_model)

# REDUCING MODEL:

re_model <- lm(Tenure ~ Population + Children + Age + Income + Churn + Email + Yearly_equip_failure +
                 DeviceProtection + TechSupport + MonthlyCharge + Bandwidth_GB_Year + InternetService_Fiber_Optic + 
                 InternetService_DSL + Contract_One_Year + Contract_Two_Year, churn)
# remove Outage_sec_perweek

summary(re_model)

re_model <- lm(Tenure ~ Population + Children + Age + Income + Churn + Yearly_equip_failure +
                 DeviceProtection + TechSupport + MonthlyCharge + Bandwidth_GB_Year + InternetService_Fiber_Optic + 
                 InternetService_DSL + Contract_One_Year + Contract_Two_Year, churn)
# remove Email

summary(re_model)

re_model <- lm(Tenure ~ Children + Age + Income + Churn + Yearly_equip_failure + DeviceProtection + TechSupport +
                 MonthlyCharge + Bandwidth_GB_Year + InternetService_Fiber_Optic + InternetService_DSL +
                 Contract_One_Year + Contract_Two_Year, churn)
# remove Population

summary(re_model)

re_model <- lm(Tenure ~ Children + Age + Income + Churn + DeviceProtection + TechSupport +
                 MonthlyCharge + Bandwidth_GB_Year + InternetService_Fiber_Optic + InternetService_DSL +
                 Contract_One_Year + Contract_Two_Year, churn)
# remove Yearly_equip_failure

summary(re_model)

re_model <- lm(Tenure ~ Children + Age + Income + Churn + DeviceProtection + TechSupport +
                 MonthlyCharge + Bandwidth_GB_Year + InternetService_Fiber_Optic + InternetService_DSL +
                 Contract_Two_Year, churn)
# remove Contract_One_Year

summary(re_model)

re_model <- lm(Tenure ~ Children + Age + Income + Churn + DeviceProtection + TechSupport +
                 MonthlyCharge + Bandwidth_GB_Year + InternetService_Fiber_Optic + InternetService_DSL, churn)
# remove Contract_Two_Year

summary(re_model)

re_model <- lm(Tenure ~ Children + Age + Churn + DeviceProtection + TechSupport + MonthlyCharge + 
                 Bandwidth_GB_Year + InternetService_Fiber_Optic + InternetService_DSL, churn)
# remove Income

summary(re_model)

library(car)
vif(re_model)

re_res <- resid(re_model)

qqnorm(re_res, main = "Q-Q Plot of Reduced Model")
qqline(re_res)

hist(re_res, main = "Histogram of Reduced Model Residuals", xlab = "Residuals", breaks = seq(-2,2,0.2))
