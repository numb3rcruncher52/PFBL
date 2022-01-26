##############################################################
# ProcessReports.R
#
# Run this anytime you want to reprocess reports exported from DMB
##############################################################

rm(list=ls())

library(tidyverse)
source("./DMBreportLoad.R")

## Define a directory to look for raw DMB reports
LATEST_SEASON <- 2021
## "01_New Season" "02_Pre Draft" "03_Post Draft" "04_Post Free Agency"
## "05_Season Start" "06_Midseason"
TIMEFRAME <- "01_New Season"
REPORT_DIR <- paste0("~/OneDrive/PFBL/Reports - DMB/reports_",LATEST_SEASON,"/")

## Check for and read in various types of reports
batter_ratings <- readBatterRatings(REPORT_DIR, LATEST_SEASON)
pitcher_ratings <- readPitcherRatings(REPORT_DIR, LATEST_SEASON)
real_stats <- readPlayerStats(REPORT_DIR, LATEST_SEASON, type='Profile')
dmb_stats <- readPlayerStats(REPORT_DIR, LATEST_SEASON, type='Results')
rosters <- readRosterStatus(REPORT_DIR, LATEST_SEASON)

## Write out any reports that were read in to the RAW_DATA folder
write_csv(batter_ratings
          , paste0("RAW_DATA/PFBL/BatterRatings/batter_ratings_",LATEST_SEASON))
write_csv(pitcher_ratings
          , paste0("RAW_DATA/PFBL/PitcherRatings/pitcher_ratings_",LATEST_SEASON))
write_csv(real_stats
          , paste0("RAW_DATA/PFBL/RealStats/real_stats_",LATEST_SEASON))
write_csv(rosters
          , paste0("RAW_DATA/PFBL/Rosters/rosters_",LATEST_SEASON,"_",TIMEFRAME))
write_csv(dmb_stats
          , paste0("RAW_DATA/PFBL/DMBStats/dmb_stats_",LATEST_SEASON,"_",TIMEFRAME))
