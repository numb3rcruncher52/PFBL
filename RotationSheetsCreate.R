##############################################################
# RotationSheets.R
#
# Take organization schedule and create team by team 
# schedules for rotation sheets
##############################################################

source("DMBreportLoad.R")
library(tidyverse)
library(readxl)
library(openxlsx)

filepath <- "C:/dmb11/PFBL 2019/reports/OrgSchedule2019.xlsx"

schedule <- read_xlsx(filepath)

teams <- unique(schedule$Away)

al_league <- c("Highland Engineers"
               ,"Iowa Farmers"
               ,"Middleburg Mudcats"
               , "Winston-Salem Pirates"    
               ,"West Side Wizards"
               ,"Holland Mothmen"
               ,"Delaware Destroyers"
               ,"Lansing Aces"
               ,"Great Lakes Bay Grunts"
               ,"Portland Columbias"       
               ,"Essexville Eruption"
               ,"Houston Mockingbirds"
               ,"Newark Force" 
               ,"Tri City Tornados")

nl_leagues <- teams[!teams %in% al_league & !is.na(teams) & teams != 'Org schedule -']

teamSchedule <- function(TEAMNAME, data) {
  data %>%
    filter(Away == TEAMNAME | Home == TEAMNAME)
}

writeTeamSchedule <- function(TEAMNAME, data, workbook) {
  schedule <- teamSchedule(TEAMNAME, data)
  
  addWorksheet(workbook, sheetName = TEAMNAME)
  writeData(workbook, TEAMNAME, x = schedule)
}

filename <- "OUTPUT/rotation_sheets_2019_NL.xlsx"
wb <- createWorkbook("rotation")

map(nl_leagues, writeTeamSchedule, data = schedule, workbook = wb)

saveWorkbook(wb, file = filename)
