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

LATEST_SEASON <- 2022

### Find all Raw data files
RAW_DIR <- "RAW_DATA/PFBL/Schedules/schedule_"
sched_filepath <- paste0(RAW_DIR, LATEST_SEASON)

schedule <- read_csv(sched_filepath, col_types = "cccc") %>%
  mutate(Away = str_sub(Away, 6, 100)
         , Home = str_sub(Home, 6, 100)
         , GameID = row_number()
         , Date = as.Date(Date, format = "%m/%d/%Y"))

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
    mutate(AwayPitcher = NA
           , HomePitcher = NA) %>%
    select(Date, GameID, Away, AwayPitcher, Home, HomePitcher)
}

addOffDays <- function(data) {
  data %>%
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

writeRotationSheet(paste0("OUTPUT_NEW/AL",LATEST_SEASON,"_RotationTemplate.xlsx"), al_league, schedule)
writeRotationSheet(paste0("OUTPUT_NEW/NL",LATEST_SEASON,"_RotationTemplate.xlsx"), nl_league, schedule)
