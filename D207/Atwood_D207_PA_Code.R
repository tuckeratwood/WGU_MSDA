churn <- read.csv("C:/Users/atwoo/OneDrive/Desktop/207Directory/churn_clean.csv")
# imports dataset

library(ggplot2) # using ggplot2 package

ggplot(churn, aes(x = Bandwidth_GB_Year, fill = Churn)) +
  geom_density(alpha = 0.5) +
  labs(title = "Bandwidth and Churn Rate Density Plot") +
  xlab("Bandwidth Gigabytes Per Year") +
  ylab("Density")
# shows relationship between Bandwidth_GB_Year and Churn

churn_no <- churn[which(churn$Churn == "No"),]
churn_yes <- churn[which(churn$Churn == "Yes"),]
# split dataset into 2 groups based on Churn response

t.test(churn_no$Bandwidth_GB_Year, churn_yes$Bandwidth_GB_Year, alternative = "g", mu = 0, var.equal = FALSE)
# conducts two-sample t-test for difference in means between Churn No and Churn Yes groups

hist(churn$Outage_sec_perweek, main = "Customer Outage Seconds Per Week", xlab = "Outage Seconds Per Week", col = 2)
# univariate statistics on continuous variable
summary(churn$Outage_sec_perweek)

boxplot(churn$MonthlyCharge, main = "Customer Monthly Charge", xlab = "Charge Per Month in US Dollars", col = 3, horizontal = TRUE)
# univariate statistics on continuous variable
summary(churn$MonthlyCharge)

ggplot(churn, aes(x = PaymentMethod)) + 
  geom_bar(color = 9, fill = 4) +
  labs(title = "Customer Payment Methods") +
  xlab("Payment Method") +
  ylab("Number of Customers")
# univariate statistics on categorical variable ()

library(dplyr) # using dplyr package

churn %>%
  group_by(PaymentMethod) %>%
  count()

library(treemapify)

statedata <- churn %>%
  count(State)

ggplot(statedata, aes(fill = State, area = n, label = State)) + 
  geom_treemap() +
  geom_treemap_text(col = 9, place = "center") +
  labs(title = "Customers in Each State")
# univariate statistics on categorical variable (treemap)
# code structure from: https://rkabacoff.github.io/datavis/Univariate.html#tree-map

states <- churn %>%
  group_by(State) %>%
  count() %>%
  arrange(desc(n)) %>%
  print(n = 52)

ggplot(churn, aes(x = Tenure, y = Bandwidth_GB_Year)) +
  geom_point(color = 5) +
  labs(title = "Customer Tenure and Bandwidth") +
  ylab("Bandwidth Gigabytes Per Year")
# bivariate statistics on 2 continuous variables (scatterplot)
summary(churn$Tenure)
summary(churn$Bandwidth_GB_Year)
cor(churn$Tenure, churn$Bandwidth_GB_Year)

ggplot(churn, aes(x = Contract, fill = Churn)) +
  geom_bar(position = "dodge") +
  labs(title = "Churn Rate for Different Contracts") +
  ylab("Number of Customers")
# bivariate statistics on 2 categorical variables (grouped bar chart)

table(churn$Contract, churn$Churn)
