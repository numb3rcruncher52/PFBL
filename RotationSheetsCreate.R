##############################################################
# RotationSheets.R
#
# Take organization schedule and create team by team 
# schedules for rotation sheets
##############################################################

rm(list = ls())
source("DMBreportLoad.R")
library(tidyverse)
library(readxl)
library(openxlsx)


# Read in schedule information --------------------------------------------

sched_filepath <- "C:/dmb11/PFBL 2019/reports/OrgSchedule2019.xlsx"

schedule <- read_xlsx(sched_filepath)

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

nl_league <- teams[!teams %in% al_league & !is.na(teams) & teams != 'Org schedule -']

teamSchedule <- function(TEAMNAME, data) {
  data %>%
    filter(Away == TEAMNAME | Home == TEAMNAME)
}

writeTeamSchedule <- function(TEAMNAME, data, workbook) {
  schedule <- teamSchedule(TEAMNAME, data)
  
  addWorksheet(workbook, sheetName = TEAMNAME)
  writeData(workbook, TEAMNAME, x = schedule)
}

readFinalRotation <- function(filepath, sheet, include_teams = FALSE) {
  data <- read_excel(filepath
                     , col_names = c('Date', 'GameID', 'Away', 'AwayPitcher', 'Home', 'HomePitcher')
                     , sheet = sheet
                     , range = "A2:F163"
                     , col_types = c('date', 'numeric', 'text', 'text', 'text', 'text'))
  
  ## Check for option to include team names for error checking
  if (include_teams) {
    data <- data %>% select(GameID, Away, Home, AwayPitcher, HomePitcher) 
  }
  else {
    data <- data %>% select(GameID, AwayPitcher, HomePitcher)
  }
  
  data  %>% 
    gather('away_home', 'starter', AwayPitcher, HomePitcher) %>%
    filter(!is.na(starter))
}

writeRotationSheet <- function(filename, team_list, schedule_data) {
  wb <- createWorkbook("rotation")
  
  map(team_list, writeTeamSchedule, data = schedule_data, workbook = wb)
  
  saveWorkbook(wb, file = filename, overwrite = TRUE)
}
