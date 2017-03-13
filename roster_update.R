source("DMBreportLoad.R")
source("BaseballCoefficientsLoad.R")
source("Fielding_Value.R")
source("Calculate_Player_Stats.R")
source("functions.R")

library(tidyverse)

season_folders <- c("PFBL 2015", "PFBL 2016", "PFBL 2017")
seasons <- seq(2014, 2016, 1)

rosters <- readRosterStatus(directory = paste0("C:\\dmb11\\",season_folders[length(season_folders)],"\\reports\\"),
                            season = seasons[length(seasons)])

write_csv(rosters, "C:\\Users\\maxl\\Dropbox (Personal)\\PFBL\\Player Values\\rosters.csv")
