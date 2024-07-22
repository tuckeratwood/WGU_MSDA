# loads required packages
library(plyr) 
library(dplyr) # plyr and dplyr used for data frame manipulation
library(ggplot2) # ggplot2 used for data visualization
library(corrplot) # corrplot used to create correlation plots
library(gridExtra) # gridExtra used to display more than 1 graph at a time

# imports relevant data
teams <- read.csv(
  "C:/Users/atwoo/OneDrive/Desktop/MSDADirectory/lahman_1871-2023_csv/Teams.csv")
pitching <- read.csv(
  "C:/Users/atwoo/OneDrive/Desktop/MSDADirectory/lahman_1871-2023_csv/Pitching.csv")

# initial look at the teams and pitching dataframes
head(teams)
head(pitching)

# pulls statistics necessary to calculate more pitching values (HBP, SF, and BF)
pitching <- pitching %>%
  group_by(teamID, yearID) %>%
  mutate(pHBP = sum(HBP)) %>%
  mutate(pSF = sum(SF)) %>%
  mutate(BF = sum(BFP)) %>%
  distinct(yearID, teamID, pHBP, pSF, BF)

# a look at the transformed pitching dataframe
head(pitching)

# joins pitching stats from above into the teams data set
teams <- inner_join(teams, pitching)

# selects only stats to be used in analysis
teams <- teams %>%
  select(Year = yearID, Team = teamID, WSWin, R, G, AB, H, X2B, X3B,
         HR, BB, SO, SB, HBP, SF, RA, IPouts, HA, HRA, BBA, pHBP,
         SOA, pSF, BF, DivWin, WCWin, LgWin)

# a look at the teams dataframe with pitching joined to it
head(teams)

# filters out years irrelevant to analysis
teams <- teams %>%
  filter(Year > 1902, Year != 1904, Year != 1994, Year != 2020)

# some teams appear to have no value in the WSWin column
no_WS <- teams[teams$WSWin == "",]
no_WS

# removes these teams from the data
teams <- teams %>%
  anti_join(no_WS)
no_WS <- teams[teams$WSWin == "",]
no_WS

# imputes missing values with means (calculated by year when applicable)
teams <- teams %>%
  mutate(HBP = ifelse(is.na(HBP), as.integer(mean(HBP, na.rm = TRUE)), HBP)) %>%
  mutate(SF = ifelse(is.na(SF), as.integer(mean(SF, na.rm = TRUE)), SF)) %>%
  mutate(pSF = ifelse(is.na(pSF), as.integer(mean(pSF, na.rm = TRUE)), pSF)) %>%
  group_by(Year) %>%
  mutate(SO = ifelse(is.na(SO), as.integer(mean(SO, na.rm = TRUE)), SO)) %>%
  mutate(BF = ifelse(is.na(BF), as.integer(mean(BF, na.rm = TRUE)), BF)) %>%
  ungroup()

# calculates yearly league averages for relevant statistics
teams <- teams %>%
  group_by(Year) %>%
  mutate(PA = AB + BB + HBP + SF) %>%
  mutate(pAB = BF - BBA - pHBP - pSF) %>%
  mutate(lg_AVG = sum(H)/sum(AB)) %>%
  mutate(lg_OBP = (sum(H) + sum(BB) + sum(HBP))/(sum(PA))) %>%
  mutate(lg_SLG = ((sum(H)-sum(X2B)-sum(X3B)-sum(HR))+2*sum(X2B)+3*sum(X3B)+4*sum(HR))/sum(AB)) %>%
  mutate(lg_OPS = lg_OBP + lg_SLG) %>%
  mutate(lg_Rg = sum(R)/sum(G)) %>%
  mutate(lg_BBr = sum(BB)/sum(PA)) %>%
  mutate(lg_SOr = sum(SO)/sum(PA)) %>%
  mutate(lg_HRr = sum(HR)/sum(PA)) %>%
  mutate(lg_SBg = sum(SB)/sum(G)) %>%
  mutate(lg_RAA = sum(RA)*27/sum(IPouts)) %>%
  mutate(lg_WHIP = (sum(HA)+sum(BBA))/(sum(IPouts)/3)) %>%
  mutate(lg_pBBr = sum(BBA)/sum(BF)) %>%
  mutate(lg_pSOr = sum(SOA)/sum(BF)) %>%
  mutate(lg_pHRr = sum(HRA)/sum(BF)) %>%
  mutate(lg_DEF = 1-(sum(HA)-sum(HRA))/(sum(BF)-sum(BBA)-sum(pHBP)-sum(SOA)-sum(HRA))) %>%
  ungroup()

# transforms team statistics into year-adjusted commonly used statistics
teams <- teams %>%
  mutate(AVGb = H/AB) %>%
  mutate(OBPb = (H+BB+HBP)/(PA)) %>%
  mutate(SLGb = ((H-X2B-X3B-HR)+2*X2B+3*X3B+4*HR)/AB) %>%
  mutate(OPSb = OBPb + SLGb) %>%
  mutate(AVG = 100 * AVGb/lg_AVG) %>%
  mutate(OBP = 100 * OBPb/lg_OBP) %>%
  mutate(SLG = 100 * SLGb/lg_SLG) %>%
  mutate(OPS = 100 * OPSb/lg_OPS) %>%
  mutate(RPG = 100 * (R/G)/lg_Rg) %>%
  mutate(BBr = 100 * (BB/PA)/lg_BBr) %>%
  mutate(Kr = 100 * (SO/PA)/lg_SOr) %>%
  mutate(HRr = 100 * (HR/PA)/lg_HRr) %>%
  mutate(SBg = 100 * (SB/G)/lg_SBg) %>%
  mutate(RAA = 100 * (RA*27/IPouts)/lg_RAA) %>%
  mutate(WHIP = 100 * ((HA+BBA)/(IPouts/3))/lg_WHIP) %>%
  mutate(pBBr = 100 * (BBA/BF)/lg_pBBr) %>%
  mutate(pKr = 100 * (SOA/BF)/lg_pSOr) %>%
  mutate(pHRr = 100 * (HRA/BF)/lg_pHRr) %>%
  mutate(DEF = 100 * (1-(HA-HRA)/(BF-BBA-pHBP-SOA-HRA))/lg_DEF) %>%
  select(Year, Team, WSWin, DivWin, WCWin, LgWin, AVG, OBP, SLG, OPS,
         RPG, BBr, Kr, HRr, SBg, RAA, WHIP, pBBr, pKr, pHRr, DEF) %>%
  arrange(desc(Year))

# filters out all non-playoff teams from analysis
playoff_teams <- teams %>%
  filter(DivWin == "Y" | WCWin == "Y" | LgWin == "Y") %>%
  select(-c(DivWin,WCWin,LgWin))
head(playoff_teams)

# creates dataframe of World Series winners
winners <- playoff_teams %>%
  filter(WSWin == "Y")
head(winners)

# creates dataframe of teams who made the playoffs but did not win World Series
losers <- playoff_teams %>%
  filter(WSWin == "N")
head(losers)

# calculates summary statistics for WS winners and other playoff teams
summary(winners)
summary(losers)

# creates correlation heatmap for offensive statistics
offensive_cor <- cor(playoff_teams[,4:12])
corrplot(offensive_cor, method = "color", type = "upper", 
         addCoef.col = 1, tl.col = 1, tl.srt = 45, 
         title = "Correlation Heatmap of Offensive Statistics", mar = c(0,0,2,0))

# creates correlation heatmap for defensive statistics
defensive_cor <- cor(playoff_teams[,13:18])
corrplot(defensive_cor, method = "color", type = "upper", 
         addCoef.col = 1, tl.col = 1, tl.srt = 45, 
         title = "Correlation Heatmap of Defensive Statistics", mar = c(0,0,2,0))

# creates histograms for each factor
num_cols <- length(names(playoff_teams))
colors <- scales::hue_pal()(num_cols)
h_list <- list()
for (x in seq(names(playoff_teams))) {
  col <- names(playoff_teams)[x]
  if (is.numeric(playoff_teams[[col]])) {
    p <- ggplot(playoff_teams, aes_string(x = col)) +
      geom_histogram(fill = colors[x], color = "black") +
      ggtitle(paste("Histogram of", col)) +
      xlab(col) +
      ylab("Frequency") +
      geom_vline(xintercept = 100, linetype = "solid", color = "black", linewidth = 2)
    h_list[[col]] <- p}}

# offensive stats histograms
grid.arrange(h_list[[2]],h_list[[3]],h_list[[4]],h_list[[5]],
             h_list[[6]],h_list[[7]],h_list[[8]],h_list[[9]], h_list[[10]],
             ncol = 3, nrow = 3)

# defensive stats histograms
grid.arrange(h_list[[11]],h_list[[12]],h_list[[13]],h_list[[14]],
             h_list[[15]],h_list[[16]],
             ncol = 3, nrow = 3)

# creates side-by-side boxplots for each factor
b_list <- list()
for (col_name in names(playoff_teams)[!names(playoff_teams) %in% "WSWin"]) {
  p <- ggplot(playoff_teams, aes(x = factor(WSWin), y = .data[[col_name]], fill = factor(WSWin))) +
    geom_boxplot() +
    ggtitle(paste("Boxplot of", col_name, "by World Series Winner")) +
    xlab("World Series Winner") +
    ylab(col_name) +
    theme_minimal() +
    guides(fill = "none") 
  b_list[[col_name]] <- p}

# offensive stats boxplots
grid.arrange(b_list[[3]],b_list[[4]],b_list[[5]],
             ncol = 3, nrow = 1)
grid.arrange(b_list[[6]],b_list[[7]],b_list[[8]],
             ncol = 3, nrow = 1)
grid.arrange(b_list[[9]],b_list[[10]],b_list[[11]],
             ncol = 3, nrow = 1)

# defensive stats boxplots
grid.arrange(b_list[[12]],b_list[[13]],b_list[[14]],
             ncol = 3, nrow = 1)
grid.arrange(b_list[[15]],b_list[[16]],b_list[[17]],
             ncol = 3, nrow = 1)

# creates dataframe of p-values for two-sample t-tests on all remaining variables
# helps determine statistical significance of differences between WS winners and other playoff teams
tests <- data.frame(Stat = character(0), t_stat = numeric(0), P_Value = numeric(0))
for (i in 4:18) {
  result <- t.test(winners[,i], losers[,i], alternative = "g")
  column_name <- colnames(winners)[i]
  p_value <- ifelse(column_name == "RAA" | column_name == "Kr" | column_name == "WHIP" |
                      column_name == "pBBr" | column_name == "pHRr", 1 - result$p.value, result$p.value)
  tests <- rbind(tests, data.frame(Stat = column_name, t_stat = result$statistic, P_Value = p_value)) }
tests <- arrange(tests, P_Value)
tests

