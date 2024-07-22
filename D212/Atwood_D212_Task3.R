mba <- read.csv("C:/Users/atwoo/OneDrive/Desktop/MSDADirectory/teleco_market_basket.csv")
# imports data set

library(tidyr) # using tidyr package to pivot data
library(arules) # using arules package to transactionalize data

mba <- mba[seq(2,nrow(mba),2),]
# removes all blank rows

mba$ID <- factor(seq(nrow(mba)))
# adds ID column

mba <- pivot_longer(mba, cols = 1:20, names_to = "Item #", values_to = "Product")
# pivots data frame

mba <- mba[,c(1,3)]
# removes Item # column

mba <- mba[!(mba$Product == ""),]
# removes empty rows

mba <- split(mba$Product, mba$ID)
# split data into products and transaction IDs

mba <- as(mba, "transactions")
# transactionalizes data set

mba <- as(mba, "matrix")
# converts data to matrix

write.csv(mba, "C:/Users/atwoo/OneDrive/Desktop/MSDADirectory/D212_Task3.csv")

arules <- apriori(mba, parameter = list(supp = 0.008, conf = 0.4))
# runs apriori function to produce association rules

inspect(sort(arules, by = "lift"))
# inspect top rules sorted by lift

summary(arules)
# summary statistics of the apriori rules