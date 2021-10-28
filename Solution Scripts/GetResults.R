##############################################################
# GetResults.R
#
# Run this every time a new disk comes out
##############################################################

rm(list=ls())

LATEST_SEASON <- 2020
REPORT_DIR <- paste0("C:/Users/mwlyo/OneDrive/PFBL/Reports - DMB/reports_",LATEST_SEASON,"/")

months_list <- c("04_April"         ## 1
                    , "05_May"      ## 2
                    , "06_June"     ## 3
                    , "07_July"     ## 4
                    , "08_August"   ## 5
                    , "09_SeptOct") ## 6
CURRENT_MONTH <- months_list[6]

source("Solution Scripts/cleanStats.R")
source("BaseballCoefficientsLoad.R")

stats <- cleanPlayerStats(REPORT_DIR, LATEST_SEASON, type = "Results")

write_csv(stats, paste0("OUTPUT_NEW/player_results_",LATEST_SEASON,"_",CURRENT_MONTH,".csv"))
