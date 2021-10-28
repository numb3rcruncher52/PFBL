##############################################################
# UpdateRosters.R
#
# Run this each time rosters fundamentally change
##############################################################

rm(list=ls())

LATEST_SEASON <- 2021
REPORT_DIR <- paste0("C:/Users/mwlyo/OneDrive/PFBL/Reports - DMB/reports_",LATEST_SEASON,"/")
MAPPINGS_DIR <- "C:/Users/mwlyo/OneDrive/PFBL/Reports - DMB/MAPPINGS/"

timeframe_list <- c("01_New Season"
                    , "02_Pre Draft"
                    , "03_Post Draft"
                    , "04_Post Free Agency"
                    , "05_Season Start"
                    , "06_Midseason")
CURRENT_TIMEFRAME <- timeframe_list[1]

library(tidyverse)

source("./DMBreportLoad.R")

dim_team <- read_csv(paste0(MAPPINGS_DIR, "dimTeam.csv"), col_types = "ccicc")

rosters <- readRosterStatus(REPORT_DIR, LATEST_SEASON) %>%
  select(ID, Name, Team, season) %>%
  left_join(dim_team, by = c('season' = 'Season', 'Team' = 'Team'))

head(rosters)

write_csv(rosters, paste0("OUTPUT_NEW/rosters_",LATEST_SEASON,"_",CURRENT_TIMEFRAME,".csv"))
