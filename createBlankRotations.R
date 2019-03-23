##############################################################
# CreateBlankRotation.R
#
# Take organization schedule and create team by team 
# schedules for rotation sheets
##############################################################

source("RotationSheetsCreate.R")

writeRotationSheet("OUTPUT/rotation_sheets_2019_NL.xlsx"
                   , nl_league
                   , schedule)

writeRotationSheet("OUTPUT/rotation_sheets_2019_AL.xlsx"
                   , al_league
                   , schedule)