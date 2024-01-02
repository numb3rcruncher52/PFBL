##############################################################
# 04_OptimalPlayTime.R
#
# Run this to combine relevant data and find optimal playtime
##############################################################

rm(list=ls())

library(tidyverse)

LATEST_SEASON <- 2024

## update MAPPING_DATA/dim_team.csv before beginning

source("Solution Scripts/z_CoefficientsLoad.R")
source("Solution Scripts/z_CalculateValue.R")
source("Solution Scripts/z_OptimalTeamLP.R")
##source("Solution Scripts/EvaluateResults.R")

# Load all season data ----------------------------------------------------
rosters <- read_csv("PROCESSED_DATA/pfbl_rosters.csv")
players <- read_csv("PROCESSED_DATA/pfbl_players.csv")
stats <- read_csv("PROCESSED_DATA/pfbl_splits_real.csv")

# Final Reporting Tables --------------------------------------------------

## TeamName Check
rosters %>%
  filter(!is.na(Team)) %>%
  distinct(Team, TeamName, season) %>%
  anti_join(dim_team, by = c("Team", "TeamName", "season"="Season"))

## Calculate Rookies
rookie_season <- rosters %>%
  group_by(ID) %>%
  arrange(season) %>%
  mutate(rookie_season = min(season))

## Join all relevant data together
all_ratings <- players %>%
  left_join(stats, by = c("ID", "Name", "season", "role")) %>%
  left_join(rookie_season, by = c("ID", "Name", "season")) %>%
  left_join(dim_team, by = c("Team", "TeamName", "season"="Season")) %>%
  calcPlaytimeLimits()

## Calculate Optimal Team
optimal_team <- all_ratings %>%
  optimalTeam()

# Write out Final Tables --------------------------------------------------

write_csv(all_ratings, "PROCESSED_DATA/pfbl_all_ratings.csv")
write_csv(optimal_team, "OUTPUT_NEW/optimal_value_2022.csv")

# Hypothetical trades -----------------------------------------------------

all_ratings <- read_csv("PROCESSED_DATA/pfbl_all_ratings.csv")

## baseline: 251
checkteam <- all_ratings %>%
  filter(season == 2024, TeamName == 'Frisco Beer Snobs')

checkteam <- all_ratings %>%
  mutate(TeamName = ifelse(Name %in% c('Albert Pujols', 'Brendan Rodgers', 'Paul Sewald', 'Daniel Bard'), 'Frisco Beer Snobs', TeamName)) %>%
  mutate(TeamName = ifelse(Name %in% c('Cavan Biggio'), 'Lake Mendota Muskies', TeamName)) %>%
  filter(season == 2023, TeamName == 'Frisco Beer Snobs')

## Stars Trade
checkteam <- all_ratings %>%
  mutate(TeamName = ifelse(Name %in% c('Cionel Perez', 'Joey Meneses', 'Jacob deGrom'), 'Frisco Beer Snobs', TeamName)) %>%
  #mutate(TeamName = ifelse(Name %in% c('Cavan Biggio'), 'Lake Mendota Muskies', TeamName)) %>%
  filter(season == 2023, TeamName == 'Frisco Beer Snobs')


checkteam <- all_ratings %>%
  mutate(TeamName = ifelse(Name %in% c('Andrew Knizner'), 'Frisco Beer Snobs', TeamName)) %>%
  mutate(TeamName = ifelse(Name %in% c('Luis Torrens'), 'Lake Mendota Muskies', TeamName)) %>%
  filter(season == 2023, TeamName == 'Frisco Beer Snobs')

checkteam %>%
  optimalTeam() %>%
  #select(Name, POS, split, value_PA, value_GS, value_INN, opt_pa, opt_starts, opt_inn)
  replace_na(list(opt_pa = 0, opt_inn = 0, opt_starts = 0, value_PA = 0, value_GS = 0, value_INN = 0)) %>%
  mutate(value = opt_pa * value_PA + opt_starts * value_GS + opt_inn * value_INN) %>%
  #group_by(POS,Name) %>%
  summarise(value = sum(value), opt_pa = sum(opt_pa), opt_inn = sum(opt_inn)) %>%
  filter(value != 0 )

View(check_opt <- checkteam %>%
       optimalTeam() %>%
       select(Name, POS, split, value_PA, value_GS, value_INN, opt_pa, opt_starts, opt_inn) %>%
       replace_na(list(opt_pa = 0, opt_inn = 0, opt_starts = 0, value_PA = 0, value_GS = 0, value_INN = 0)) %>%
       mutate(value = opt_pa * value_PA + opt_starts * value_GS + opt_inn * value_INN) %>%
       group_by(POS,Name) %>%
       summarise(value = sum(value), opt_pa = sum(opt_pa), opt_inn = sum(opt_inn)/2, opt_gs = sum(opt_starts)/2) %>%
       filter(value != 0 ))

View(check_opt <- checkteam %>%
       optimalTeam() %>%
       select(Name, POS, split, value_PA, value_GS, value_INN, opt_pa, opt_starts, opt_inn) %>%
       replace_na(list(opt_pa = 0, opt_inn = 0, opt_starts = 0, value_PA = 0, value_GS = 0, value_INN = 0)) %>%
       mutate(value = opt_pa * value_PA + opt_starts * value_GS + opt_inn * value_INN) %>%
       group_by(POS) %>%
       summarise(value = sum(value), opt_pa = sum(opt_pa), opt_inn = sum(opt_inn)/2, opt_gs = sum(opt_starts)/2) %>%
       filter(value != 0 ))

View(check_opt %>%
       replace_na(list(opt_pa = 0, opt_inn = 0, opt_starts = 0, value_PA = 0, value_GS = 0, value_INN = 0)) %>%
       mutate(value = opt_pa * value_PA + opt_starts * value_GS + opt_inn * value_INN) %>%
       arrange(-value) %>% 
       select(Name, POS, split, value, opt_pa, value_PA, opt_starts, value_GS, opt_inn, value_INN)) %>%
  summarise(value = sum(value))