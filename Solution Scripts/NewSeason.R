##############################################################
# NewSeason2.R
#
# Run this when a new season is starting
##############################################################

rm(list=ls())

## Be sure to run rosters, Profiles, and Ratings reports from DMB and save
## in Onedrive before running this script

LATEST_SEASON <- 2022

source("Solution Scripts/CoefficientsLoad.R")
source("Solution Scripts/CalculateValue.R")

### Find all Raw data files
RAW_DIR <- "RAW_DATA/PFBL"
output_files <- list.files(RAW_DIR, full.names = TRUE, recursive = TRUE)

roster_files <- output_files[grep("rosters", output_files)]
realstats_files <- output_files[grep("real_stats", output_files)]
batter_files <- output_files[grep("batter_ratings", output_files)]
pitcher_files <- output_files[grep("pitcher_ratings", output_files)]

rosters <- map(roster_files, read_csv) %>% bind_rows() 
batter_ratings  <- map(batter_files, read_csv) %>% bind_rows()
pitcher_ratings <- map(pitcher_files, read_csv) %>% bind_rows()

real_stats <- map(realstats_files, read_csv) %>% bind_rows()

players <- cleanPlayerRatings(batter_ratings, pitcher_ratings)
stats <- cleanPlayerStats(real_stats)

# Final Reporting Tables --------------------------------------------------

## TeamName Check
teams <- rosters %>%
  distinct(Team, TeamName, season) %>%
  left_join(dim_team, by = c("Team", "TeamName", "season"="Season"))

## Calculate Rookies
rookie_season <- rosters %>%
  group_by(ID) %>%
  arrange(season) %>%
  mutate(rookie_season = min(season))

## Join all relevant data together
all_ratings <- players %>%
  left_join(stats, by = c("ID", "Name", "season")) %>%
  left_join(rookie_season, by = c("ID", "Name", "season")) %>%
  left_join(dim_team, by = c("Team", "TeamName", "season"="Season")) %>%
  calcPlaytimeLimits()

## Calculate Optimal Team
