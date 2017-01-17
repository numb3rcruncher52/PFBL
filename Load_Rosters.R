##############################################################
# Load_Rosters.R
#
# Given a directory, load the most current roster information
##############################################################

SEASON <- 2016

roster_directory <- "C:\\dmb11\\PFBL 2017\\reports\\"

rosters <- readRosterStatus(roster_directory, SEASON)

write_csv(rosters, "Output/rosters_current.csv")
