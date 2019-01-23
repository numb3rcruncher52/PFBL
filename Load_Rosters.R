##############################################################
# Load_Rosters.R
#
# Given a directory, load the most current roster information
##############################################################
library(tidyverse)
source("DMBreportLoad.R")

SEASON <- 2018

roster_directory <- "C:\\dmb11\\PFBL 2019\\reports\\"

rosters <- readRosterStatus(roster_directory, SEASON)

write_csv(rosters, "rosters_current.csv")
