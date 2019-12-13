##############################################################
# NewSeason.R
#
# Run this when a new season is starting
##############################################################

rm(list=ls())

## Be sure to run rosters, Profiles, and Ratings reports from DMB and save
## in Onedrive before running this script

LATEST_SEASON <- 2020
REPORT_DIR <- paste0("~/OneDrive/PFBL/Reports - DMB/reports_",LATEST_SEASON,"/")

source("Solution Scripts/cleanFieldingData.R")
source("Solution Scripts/cleanStats.R")
source("BaseballCoefficientsLoad.R")

fielding <- cleanFieldingData(REPORT_DIR, LATEST_SEASON)
stats <- cleanPlayerStats(REPORT_DIR, LATEST_SEASON, type = "Profile")

write_csv(fielding, paste0("OUTPUT_NEW/final_players_",LATEST_SEASON,".csv"))
write_csv(stats, paste0("OUTPUT_NEW/player_splits_",LATEST_SEASON,".csv"))
