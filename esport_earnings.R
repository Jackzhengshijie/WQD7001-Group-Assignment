library(tidyverse)
library(readr)
library(readxl)

# set working directory
setwd("C:/Users/aby_a/OneDrive/UM Master in Data Science/Principle of data science/WQD7001-Group-Assignment/")

# check working directory
getwd()

# import data and rename column names
names <- c('Year', 'PlayerID', 'PlayerName', 'TotalPrizeMoneyYear', 'OverallPrizeMoney', 'TotalPercentage')
esport_earnings_players <- read_excel("Datasets/eSports Earnings 1998-2020.xlsx", sheet = "Top 100 Players 1998-2020", col_names = names, skip = 1)
esport_earnings_players %>% View()
esport_earnings_players
summary(esport_earnings_players)

# convert variables from string to factor
esport_earnings <- esport_earnings_players %>%
  mutate(PlayerID=as.factor(PlayerID), PlayerName=as.factor(PlayerName), 
         TotalPrizeMoneyYear=as.numeric(TotalPrizeMoneyYear), OverallPrizeMoney=as.numeric(OverallPrizeMoney))
summary(esport_earnings)
View(esport_earnings)

# check if dataset contain NA values
colSums(is.na(esport_earnings))

# visualization
top10players <- esport_earnings %>%
  group_by(Year) %>%
  arrange(desc(TotalPrizeMoneyYear), .by_group = TRUE) %>%
  top_n(10) %>%
  filter(Year==2020)
top10players

# Overall players
total_players_overall = unique(esport_earnings$PlayerName)
total_players_overall

total_players_overall = length(total_players_overall)
total_players_overall

total_players <- esport_earnings %>% 
  select(Year, PlayerName) %>%
  filter(Year == 1998)

t = unique(total_players$PlayerName)
length(unique(total_players$PlayerName))

# Prize money
totalPrizeMoneyAll = sum(esport_earnings$TotalPrizeMoneyYear)
print(totalPrizeMoneyAll)

total = esport_earnings %>%
  group_by(Year) %>%
    summarise(total = sum(TotalPrizeMoneyYear)) %>%
      filter(Year==2020) %>%
        select(total)
print(total)


ggplot(data=top10players) +
  geom_col(mapping = aes(x=top10players$PlayerID, y=as.character(top10players$OverallPrizeMoney)))

# Average prize money from 1998-2020
esport_earnings %>%
  group_by(Year) %>%
  summarize(average_prizeMoney=mean(TotalPrizeMoneyYear)) %>%
  ggplot() +
  geom_col(mapping = aes(x=Year, y=average_prizeMoney))

esport_earnings %>%
  group_by(Year) %>%
  summarize(average_prizeMoney=mean(OverallPrizeMoney)) %>%
  ggplot() +
  geom_point(mapping = aes(x=Year, y=average_prizeMoney))

# TotalPrizeMoney and OverallPrizeMoney distribution
ggplot(esport_earnings,
       aes(TotalPrizeMoneyYear, OverallPrizeMoney, color = Year)) +
  geom_point(size = 3) +
  geom_smooth(method = lm) +
  geom_density2d(alpha = .5) +
  theme(legend.position = "bottom")

# Average players from 1998-2020
esport_earnings %>%
  group_by(Year) %>%
  summarize(average_players=mean(PlayerName)) %>%
  ggplot() +
  geom_col(mapping = aes(x=Year, y=average_players))
    
    


