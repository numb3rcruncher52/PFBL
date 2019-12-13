##############################################################
# UpdateRosters.R
#
# Run this each time rosters fundamentally change
##############################################################

rm(list=ls())

LATEST_SEASON <- 2020
REPORT_DIR <- paste0("~/OneDrive/PFBL/Reports - DMB/reports_",LATEST_SEASON,"/")

timeframe_list <- c("01_New Season"
                    , "02_Pre Draft"
                    , "03_Post Draft"
                    , "04_Post Free Agency"
                    , "05_Season Start"
                    , "06_Midseason")
CURRENT_TIMEFRAME <- timeframe_list[1]

library(tidyverse)

source("./DMBreportLoad.R")

rosters <- readRosterStatus(REPORT_DIR, LATEST_SEASON)

write_csv(rosters, paste0("OUTPUT_NEW/rosters_",LATEST_SEASON,"_",CURRENT_TIMEFRAME,".csv"))
