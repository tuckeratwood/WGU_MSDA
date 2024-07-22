churn <- read.csv("C:/Users/atwoo/OneDrive/Desktop/MSDADirectory/churn_clean.csv") 
# imports dataset

# DETECTING DUPLICATES:

sum(duplicated(churn)) # checks for full duplicates (0)

library(plyr) # using plyr package
library(dplyr) # using dplyr package
churn %>%
  count(Customer_id) %>%
  filter(n > 1) # checks for partial duplicates with matching Customer_id (0)

churn %>%
  filter(duplicated(churn[,c('State','City','Job','Gender','Employment','Education')]) | 
           duplicated(churn[,c('State','City','Job','Gender','Employment','Education')], fromLast = TRUE))
# partial duplicates based on specified columns
# 1 match, but other factors make this highly unlikely to be a duplicate

# DETECTING MISSING VALUES:

library(naniar) # using naniar package
n_miss(churn) # total missing values (13,906)
prop_miss(churn) # proportion of missing values (approx. 0.027)
miss_var_summary(churn) # gives total and proportion of missing values for each variable
# data is missing from 8 factors: Children, Age, Income, Techie, Phone, TechSupport, Tenure, Bandwidth_GB_Year

library(visdat) # using visdat package
vis_miss(churn, cluster = TRUE) # visualize missing data; no clear patterns between missingness

churn %>%
  filter(Population == 0) %>%
  count() # finds cases with population listed as 0 (97)

# DETECTING INCORRECT APPLICATION OF ZIP CODE:

str(churn$Zip) # checks data type of Zip

library(stringr) # using stringr package
churn %>%
  filter(str_length(Zip) != 5) %>%
  count(State)
# finds zip codes less than 5 digits, by state (verifies this only takes place in states with zip codes starting with 0)

# DETECTING OUTLIERS:

library(ggplot2) # using ggplot2 package
churn %>%
  ggplot(aes(x=Lat, y=Lng)) + geom_point()
# scatterplot of Latitude and Longitude values; outliers present on each end of Latitude, low end of Longitude

churn %>%
  filter(Lng < -125) %>%
  count()
# 112 low Longitude values

churn %>%
  filter(Lat < 24) %>%
  count()
# 75 low Latitude values

churn %>%
  filter(Lat > 50) %>%
  count()
# 77 high Latitude values

item_outliers <- churn %>%
  filter(item1 < 1 | item1 > 8
         | item2 < 1 | item2 > 8
         | item3 < 1 | item3 > 8
         | item4 < 1 | item4 > 8
         | item5 < 1 | item5 > 8
         | item6 < 1 | item6 > 8
         | item7 < 1 | item7 > 8
         | item8 < 1 | item8 > 8)
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
count(chi_outliers) # 302 outliers
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
count(inc_outliers) # 249 outliers
summary(inc_outliers$Income) # outlier range is 104,868-258,901

hist(churn$Outage_sec_perweek) # visualization of Outage_sec_perweek data; skewed right
boxplot(churn$Outage_sec_perweek) # many outliers present
outage_outliers <- churn %>%
  filter((Outage_sec_perweek < quantile(churn$Outage_sec_perweek, 0.25) - IQR(churn$Outage_sec_perweek) * 1.5) 
         | (Outage_sec_perweek > quantile(churn$Outage_sec_perweek, 0.75) + IQR(churn$Outage_sec_perweek) * 1.5))
# find outliers using IQR method
count(outage_outliers) # 539 outliers
summary(outage_outliers$Outage_sec_perweek) # outlier range is (-1.349)-47.049

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
count(mon_outliers) # 3 outliers
summary(mon_outliers$MonthlyCharge) # outlier range is 306.3-315.9

hist(churn$Bandwidth_GB_Year) # visualization of Bandwidth_GB_Year data; bimodal
boxplot(churn$Bandwidth_GB_Year) # no outliers present
bgy_outliers <- churn %>%
  filter((Bandwidth_GB_Year < quantile(churn$Bandwidth_GB_Year, 0.25, na.rm = TRUE) - IQR(churn$Bandwidth_GB_Year, na.rm = TRUE) * 1.5) 
         | (Bandwidth_GB_Year > quantile(churn$Bandwidth_GB_Year, 0.75, na.rm = TRUE) + IQR(churn$Bandwidth_GB_Year, na.rm = TRUE) * 1.5))
# find outliers using IQR method
count(bgy_outliers) # confirms zero Bandwidth_GB_Year outliers

# DETECTING CATEGORICAL VARIABLE DATA QUALITY ISSUES:

str(churn) # checks data type for all variables

unique(churn$Area) # 3 distinct answers
unique(churn$Timezone) # 25 answers, some redundant
# will be reduced to reflect only necessary amount of distinct time zones
unique(churn$Education) # 12 distinct answers
# will be re-expressed as numeric to allow regression calculations
unique(churn$Employment) # 5 distinct answers
unique(churn$Marital) # 5 distinct answers
unique(churn$Gender) # 3 distinct answers
unique(churn$Churn) # all values either "Yes" or "No"
# will be re-expressed as numeric to allow regression calculations
unique(churn$Techie) # all values either "Yes," "No," or "NA"
unique(churn$Contract) # 3 distinct answers
unique(churn$Port_modem) # all values either "Yes" or "No"
unique(churn$Tablet) # all values either "Yes" or "No"
unique(churn$InternetService) # 3 distinct answers
unique(churn$Phone) # all values either "Yes" or "No"
unique(churn$Multiple) # all values either "Yes" or "No"
unique(churn$OnlineSecurity) # all values either "Yes" or "No"
unique(churn$OnlineBackup) # all values either "Yes" or "No"
unique(churn$DeviceProtection) # all values either "Yes" or "No"
unique(churn$TechSupport) # all values either "Yes," "No," or "NA"
unique(churn$StreamingTV) # all values either "Yes" or "No"
unique(churn$StreamingMovies) # all values either "Yes" or "No"
unique(churn$PaperlessBilling) # all values either "Yes" or "No"
unique(churn$PaymentMethod) # 4 distinct answers

# TREATING MISSING VALUES:

churn_cln <- churn %>%
  mutate(Children = ifelse(is.na(Children), 0, Children)) %>%
  mutate(Techie = ifelse(is.na(Techie), "No", Techie)) %>%
  mutate(Phone = ifelse(is.na(Phone), "No", Phone)) %>%
  mutate(TechSupport = ifelse(is.na(TechSupport), "No", TechSupport))
# replaces missing values in Children with 0 and missing values in Techie, Phone, and TechSupport with “No”

gg_miss_span(churn_cln, Age, 1000)
gg_miss_span(churn_cln, Income, 1000)
gg_miss_span(churn_cln, Tenure, 1000)
gg_miss_span(churn_cln, Bandwidth_GB_Year, 1000)
# checks for clear patterns of missingness in remaining quantitative variables (none)

churn_cln <- churn_cln %>%
  group_by(Employment) %>%
  mutate(Age = ifelse(is.na(Age), as.integer(mean(Age, na.rm = TRUE)), Age)) %>%
  ungroup()
# replaces NA Age values with median age from that Employment type

churn_cln <- churn_cln %>%
  group_by(Job) %>%
  mutate(Income = ifelse(is.na(Income), as.integer(median(Income, na.rm = TRUE)), Income)) %>%
  ungroup()
# replaces NA Income values with median income from that Job

churn_cln <- churn_cln %>%
  mutate(Population = replace(Population, Population == 0, NA)) %>%
  group_by(State) %>%
  mutate(Population = ifelse(is.na(Population), as.integer(median(Population, na.rm = TRUE)), Population)) %>%
  ungroup()
# replaces Population values of 0 with median Population from that state

library(simputation) # using simputation package

churn_cln <- churn_cln %>%
  impute_lm(Tenure ~ Age)
# replaces NA Tenure values with linear model using Age factor

churn_cln <- churn_cln %>%
  impute_lm(Bandwidth_GB_Year ~ MonthlyCharge)
# replaces NA Bandwidth_GB_Year values with linear model using MonthlyCharge factor

n_miss(churn_cln) # confirm 0 missing values

# TREATING INCORRECT APPLICATION OF ZIP CODE:

churn_cln <- churn_cln %>%
  mutate(Zip = as.character(Zip)) %>%
  mutate(Zip = ifelse(str_length(Zip) == 3, paste0("00",Zip), Zip)) %>%
  mutate(Zip = ifelse(str_length(Zip) == 4, paste0("0",Zip), Zip))
# converts zip code to character, adds leading 00 to zip codes with 3 digits and 0 to zip codes with 4 digits

churn_cln %>%
  filter(str_length(Zip) != 5) %>%
  count() # confirms all zip codes are now 5 digits

# TREATING OUTLIERS:

churn_cln %>%
  filter(Lng < -125) %>%
  count(State)
# confirms all low Longitude values are from Alaska (77 values) and Hawaii (35 values)
# Longitude outliers will be retained

churn_cln %>%
  filter(Lat < 24) %>%
  count(State)
# confirms all low Latitude values are from Puerto Rico (40 values) and Hawaii (35 values)

churn_cln %>%
  filter(Lat > 50) %>%
  count(State)
# confirms all high Latitude values are from Alaska (77 values)
# Latitude outliers will be retained

summary(pop_outliers$Population)
# outlier range (31,816-111,850) and maximum value (111,850) are reasonable and acceptable
# Population outliers will be retained

summary(chi_outliers$Children)
# outlier range (8-10) and maximum value (10) are reasonable and acceptable
# Children outliers will be retained

summary(inc_outliers$Income)
# outlier range (104,868-258,901) and maximum value (258,901) are reasonable and acceptable
# Income outliers will be retained

summary(outage_outliers$Outage_sec_perweek)
# minimum value shows negative values, maximum value (47.049) is reasonable and acceptable
# Negative Outage_sec_perweek values will be imputed and other outliers will be retained

churn_cln <- churn_cln %>%
  mutate(Outage_sec_perweek = ifelse(Outage_sec_perweek < 0, Outage_sec_perweek == 0, Outage_sec_perweek))
# replaces negative Outage_sec_perweek values with 0

summary(churn_cln$Outage_sec_perweek)
# confirms minimum value is now 0

summary(email_outliers$Email)
# outlier range (1-23) and maximum value (23) are reasonable and acceptable
# Email outliers will be retained

summary(con_outliers$Contacts)
# outlier range (6-7) and maximum value (7) are reasonable and acceptable
# Contacts outliers will be retained

summary(yef_outliers$Yearly_equip_failure)
# outlier range (3-6) and maximum value (6) are reasonable and acceptable
# Yearly_equip_failure outliers will be retained

summary(mon_outliers$MonthlyCharge)
# outlier range (306.3-315.9) and maximum value (315.9) are reasonable and acceptable
# MonthlyCharge outliers will be retained

# RE-EXPRESSING CATEGORICAL VARIABLES:

churn_cln$Timezone <- revalue(churn_cln$Timezone, replace = c("America/Sitka" = "Alaska", "America/Detroit" = "Eastern", "America/Los_Angeles" = "Pacific",
                                                              "America/Chicago" = "Central", "America/New_York" = "Eastern", "America/Puerto_Rico" = "Atlantic",
                                                              "America/Denver" = "Mountain", "America/Menominee" = "Central", "America/Phoenix" = "Mountain",
                                                              "America/Indiana/Indianapolis" = "Eastern", "America/Boise" = "Mountain",
                                                              "America/Kentucky/Louisville" = "Eastern", "Pacific/Honolulu" = "Hawaii",
                                                              "America/Indiana/Petersburg" = "Eastern", "America/Nome" = "Alaska", "America/Anchorage" = "Alaska",
                                                              "America/Indiana/Knox" = "Central", "America/Juneau" = "Alaska", "America/Toronto" = "Eastern",
                                                              "America/Indiana/Winamac" = "Eastern", "America/Indiana/Vincennes" = "Eastern",
                                                              "America/North_Dakota/New_Salem" = "Central", "America/Indiana/Tell_City" = "Central",
                                                              "America/Indiana/Marengo" = "Eastern", "America/Ojinaga" = "Central"))
# consolidates redundant time zones into 7 distinct time zones

churn_cln$Education <- as.numeric(revalue(churn_cln$Education, replace = c("No Schooling Completed" = 1, "Nursery School to 8th Grade" = 2,
                                                        "9th Grade to 12th Grade, No Diploma" = 3, "GED or Alternative Credential" = 4,
                                                        "Regular High School Diploma" = 5, "Some College, Less than 1 Year" = 6,
                                                        "Some College, 1 or More Years, No Degree" = 7, "Associate's Degree" = 8,
                                                        "Bachelor's Degree" = 9, "Master's Degree" = 10, "Professional School Degree" = 11,
                                                        "Doctorate Degree" = 12)))
# converts Education to numeric

churn_cln$Churn <- as.numeric(revalue(churn_cln$Churn, replace = c("No" = 0, "Yes" = 1)))
# converts Churn to numeric

unique(churn_cln$Timezone) # confirms new time zones
unique(churn_cln$Education) # confirms new education values
unique(churn_cln$Churn) # confirms new churn values

# PRINCIPAL COMPONENT ANALYSIS:

vars <- churn_cln %>%
  select(Lat, Lng, Income, Outage_sec_perweek, Tenure, MonthlyCharge, Bandwidth_GB_Year)
# variables to be used for PCA: all continuous quantitative variables

vars_pca <- vars %>%
  prcomp(center = TRUE, scale = TRUE)
# conducts PCA, centering and scaling the data

vars_pca$rotation
# creates PCA loading matrix

library(factoextra) # using factoextra package

fviz_eig(vars_pca, choice = "eigenvalue", addlabels = TRUE)
# creates scree plot for PCA
# PC1, PC2, PC3, and PC4 all have eigenvalues greater than or equal to 1, so they will be kept, per the Kaiser Rule

write.csv(churn_cln, "C:/Users/atwoo/OneDrive/Desktop/206Directory/churn_cln.csv")
