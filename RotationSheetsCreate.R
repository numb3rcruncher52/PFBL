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

## Before starting you must prepare the OrgSchedule.txt output from Diamond
## mind. 

# Read in schedule information --------------------------------------------

sched_filepath <- "C:/dmb12/PFBL 2021/reports/OrgSchedule.xlsx"

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
              ## ,"Newark Force" 
               ,"Tri City Tornados"
               ,"Frisco Beer Snobs")

nl_league <- teams[!teams %in% al_league & !is.na(teams) & teams != 'Org schedule -']

teamSchedule <- function(TEAMNAME, data) {
  data %>%
    filter(Away == TEAMNAME | Home == TEAMNAME) %>%
    #mutate(AwayPitcher = NA
     #      , HomePitcher = NA) %>%
    select(Date, GameID, Away, AwayPitcher, Home, HomePitcher)
}

addOffDays <- function(data) {
  data %>%
    mutate(Date = as.Date(Date)) %>%
    complete(Date = seq.Date(min(Date), max(Date), by='day')) %>%
    mutate(Away = ifelse(is.na(Away), "OFF DAY", Away))
}

writeTeamSchedule <- function(TEAMNAME, data, workbook) {
  schedule <- teamSchedule(TEAMNAME, data) %>% 
    addOffDays()
  
  addWorksheet(workbook, sheetName = TEAMNAME)
  writeData(workbook, TEAMNAME, x = schedule)
}

readFinalRotation <- function(filepath, sheet, include_teams = FALSE) {
  data <- read_excel(filepath
                     , col_names = c('Date', 'GameID', 'Away', 'AwayPitcher', 'Home', 'HomePitcher')
                     , sheet = sheet
                     , range = "A2:F183"
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



# Write Rotation Sheets for each league -----------------------------------

writeRotationSheet("AL2021_RotationTemplate.xlsx", al_league, schedule)
writeRotationSheet("NL2021_RotationTemplate.xlsx", nl_league, schedule)
