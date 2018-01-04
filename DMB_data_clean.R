##############################################################
# DMB_data_clean.R
#
# Load all reports from DMB and return cleaned data sources
# * wOBA vs. LH and RH
# * Full season fielding values
##############################################################

rm(list=ls())

library(tidyverse)

## The following benchmarks are calculated from 2016 Total Reports
LH_PA_FULL <- 179
RH_PA_FULL <- 510
TOTAL_PA_FULL <- LH_PA_FULL + RH_PA_FULL
PA_INN <- 4.277
INN_START_MAX <- 7
PITCH_LH_SPLIT <- 0.4357984
LATEST_SEASON <- 2017

REPORT_DIR <- "C:\\Users\\mwlyo\\Dropbox\\PFBL\\Reports - DMB\\"
REPORT_DIR <- "C:\\Users\\maxl\\Dropbox (Personal)\\PFBL\\Reports - DMB\\"

# Source Helper functions -------------------------------------------------

source("DMBreportLoad.R")
source("Fielding_Value.R")
source("Calculate_Player_Stats.R")
source("functions.R")
source("BaseballCoefficientsLoad.R")

# Load data for all available seasons ----------------------------------------

seasons <- seq(2014, LATEST_SEASON, 1)
season_folders <- paste("reports", seasons + 1, sep = "_")

args2 <- list(directory = paste0(REPORT_DIR,season_folders,"\\"),
              season = seasons)

stats <- args2 %>% pmap(.f = readPlayerStats, type = 'Profile') %>% bind_rows()
batter_ratings <- args2 %>% pmap(readBatterRatings) %>% bind_rows()
pitcher_ratings <- args2 %>% pmap(readPitcherRatings) %>% bind_rows()

# Get Fielding Value ------------------------------------------------------

fielding <- fieldingValue(batter_ratings)

# Calculate Split Run Values ----------------------------------------------

stats_final <- calcPlayerStats(stats)
