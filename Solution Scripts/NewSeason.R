##############################################################
# NewSeason2.R
#
# Run this when a new season is starting
##############################################################

rm(list=ls())

library(tidyverse)

LATEST_SEASON <- 2022

source("Solution Scripts/CoefficientsLoad.R")
source("Solution Scripts/CalculateValue.R")
source("Solution Scripts/OptimalTeamLP.R")

### Find all Raw data files
RAW_DIR <- "RAW_DATA/PFBL"
output_files <- list.files(RAW_DIR, full.names = TRUE, recursive = TRUE)

roster_files <- output_files[grep("rosters", output_files)]
realstats_files <- output_files[grep("real_stats", output_files)]
dmbstats_files <- output_files[grep("dmb_stats", output_files)]
batter_files <- output_files[grep("batter_ratings", output_files)]
pitcher_files <- output_files[grep("pitcher_ratings", output_files)]

rosters <- map(roster_files, read_csv) %>% bind_rows() 
batter_ratings  <- map(batter_files, read_csv) %>% bind_rows()
pitcher_ratings <- map(pitcher_files, read_csv) %>% bind_rows()

real_stats <- map(realstats_files, read_csv) %>% bind_rows()
dmb_stats <- map(dmbstats_files, read_csv) %>% bind_rows()

players <- cleanPlayerRatings(batter_ratings, pitcher_ratings)
stats <- cleanPlayerStats(real_stats)
dmb_stats <- cleanPlayerStats(dmb_stats)

source("Solution Scripts/EvaluateResults.R")

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


# Hypothetical trades -----------------------------------------------------

checkteam <- all_ratings %>%
  mutate(TeamName = ifelse(Name %in% c('Lance Lynn', 'Marcus Semien', 'Jacob Stallings'), 'Frisco Beer Snobs', TeamName)) %>%
  filter(season == 2022, TeamName == 'Frisco Beer Snobs')

check_opt <- checkteam %>%
  optimalTeam()

check_opt %>%
  replace_na(list(opt_pa = 0, opt_inn = 0, opt_starts = 0, value_PA = 0, value_GS = 0, value_INN = 0)) %>%
  mutate(value = opt_pa * value_PA + opt_starts * value_GS + opt_inn * value_INN) %>%
  arrange(-value) %>% 
  select(Name, POS, split, value, opt_pa, value_PA, opt_starts, value_GS, opt_inn, value_INN) %>%
  summarise(value = sum(value))

View(all_ratings %>% filter(season == 2022, Name == 'Shohei Ohtani'))

# Write out Final Tables --------------------------------------------------

write_csv(optimal_team, paste0("OUTPUT_NEW/optimal_value_",LATEST_SEASON,".csv"))

write_csv(all_ratings, paste0("OUTPUT_NEW/player_value_",LATEST_SEASON,".csv"))
write_csv(final_optimal, paste0("OUTPUT_NEW/optimal_value_",LATEST_SEASON,".csv"))
