##############################################################
# 03_RotationSheets.R
#
# Run this to create blank rotation sheets
##############################################################

rm(list = ls())
source("DMBreportLoad.R")
library(tidyverse)
library(readxl)
library(openxlsx)

# Read in schedule information --------------------------------------------

LATEST_SEASON <- 2026

### Find all Raw data files
#RAW_DIR <- "RAW_DATA/PFBL/Schedules/schedule_"
#sched_filepath <- paste0(RAW_DIR, LATEST_SEASON)
sched_filepath <- paste0("schedule_",LATEST_SEASON,".csv")
col_names <- c("GameID", "Date", "double_header", "visit_team", "home_team"
               , "season_type", "played", "col_h", "col_i")

## Create teams to join the team IDs to
teams <- read_csv("/Users/maxlyons/R/PFBL/MAPPING_DATA/dim_team.csv") %>%
  filter(Season == LATEST_SEASON)

teams_a <- teams %>%
  select(Away = TeamName
         , visit_team = team_id)

teams_h <- teams %>%
  select(Home = TeamName
         , home_team = team_id)

schedule <- read_csv(sched_filepath, col_names = col_names) %>%
  left_join(teams_a, by= "visit_team") %>%
  left_join(teams_h, by = "home_team") %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%y")) %>% 
  select(GameID, Date, Away, Home)

teams <- sort(unique(schedule$Away))

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
                     , range = "A2:F193"
                     , col_types = c('date', 'numeric', 'text', 'text', 'text', 'text'))
  
  ## Check for option to include team names for error checking
  if (include_teams) {
    data <- data %>% select(Date, GameID, Away, Home, AwayPitcher, HomePitcher) 
  }
  else {
    data <- data %>% select(GameID, AwayPitcher, HomePitcher)
  }
  
  data  %>% 
    gather('away_home', 'starter', AwayPitcher, HomePitcher) %>%
    filter(!is.na(starter))
}

finalTeamSchedule <- function(TEAMNAME, data) {
  data %>%
    filter(Away == TEAMNAME | Home == TEAMNAME) %>%
    select(Date, GameID, Away, AwayPitcher, Home, HomePitcher)
}

writeFinalTeamSchedule <- function(TEAMNAME, data, workbook) {
  schedule <- finalTeamSchedule(TEAMNAME, data) %>% 
    addOffDays()
  
  addWorksheet(workbook, sheetName = TEAMNAME)
  writeData(workbook, TEAMNAME, x = schedule)
}

writeRotationSheet <- function(filename, team_list, schedule_data, final = FALSE) {
  wb <- createWorkbook("rotation")
  
  if (final) {
    map(team_list, writeFinalTeamSchedule, data = schedule_data, workbook = wb)
  }
  else {
    map(team_list, writeTeamSchedule, data = schedule_data, workbook = wb)
  }
  
  saveWorkbook(wb, file = filename, overwrite = TRUE)
}

# Write Rotation Sheets for each league -----------------------------------

writeRotationSheet(paste0("OUTPUT_NEW/PFBL",LATEST_SEASON,"_RotationTemplate.xlsx"), teams, schedule)

