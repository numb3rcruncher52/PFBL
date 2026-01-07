##############################################################
# NewSeason2.R
#
# Run this when a new season is starting
##############################################################

rm(list=ls())

library(tidyverse)

LATEST_SEASON <- 2026

## update MAPPING_DATA/dim_team.csv before beginning

source("Solution Scripts/z_CoefficientsLoad.R")
source("Solution Scripts/z_CalculateValue.R")

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

## Row for every player-position with defensive value
players <- cleanPlayerRatings(batter_ratings, pitcher_ratings) 
## Row for every player-handedness split with value/PA for real-life & DMB
stats <- cleanPlayerStats(real_stats) 
dmb_stats <- cleanPlayerStats(dmb_stats)

all_ratings <- players %>%
  left_join(stats, by = c("ID", "Name", "season", "role")) %>%
  calcPlaytimeLimits()

# Write consolidated data to PROCESSED_DATA -------------------------------
write_csv(rosters, "PROCESSED_DATA/pfbl_rosters.csv")
write_csv(players, "PROCESSED_DATA/pfbl_players.csv")
write_csv(stats, "PROCESSED_DATA/pfbl_splits_real.csv")
write_csv(dmb_stats, "PROCESSED_DATA/pfbl_splits_sim.csv")
write_csv(all_ratings, "PROCESSED_DATA/pfbl_ratings.csv")

#write_csv(all_ratings, paste0("OUTPUT_NEW/player_value_",LATEST_SEASON,".csv"))
#write_csv(final_optimal, paste0("OUTPUT_NEW/optimal_value_",LATEST_SEASON,".csv"))
