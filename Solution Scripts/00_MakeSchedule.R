##############################################################
# MakeSchedule.R
#
# Run this to create schedules for a season based on template
##############################################################

rm(list=ls())

library(tidyverse)

template <- read_csv("/Users/maxlyons/R/PFBL/MAPPING_DATA/schedule_template.csv", col_types = "cccc") %>%
  filter(visitor != 'OFF')
teams <- read_csv("/Users/maxlyons/R/PFBL/MAPPING_DATA/dim_team.csv")

## Replace The divisions we have in PFBL and the letter assigned to that division 
#on this spreadsheet need to be flip flopped every year for one league only.  
#So, for example, in odd-numbered years, the NL West would be 'N' teams on the 
#spread sheet.  In even-numbered years, the NL West would be 'S' teams.   The AL 
#divisions would remain the same in this example.  This is important so that 
#specific interleague opponents alternate between home and away series each season.  
#As the spreadsheet is set up, N is always at E, E is always at S, S is always at W, and W is always at N.

LATEST_SEASON <- 2024

season_teams <- teams %>%
  filter(Season == LATEST_SEASON) %>%
  group_by(Division) %>%
  mutate(team_num = sample.int(7,7),
         team_code = paste0(Division, team_num)) %>%
  ungroup()

visitor <- season_teams %>%
  select(visitor = team_code,
         visit_team = team_id)

home <- season_teams %>%
  select(home = team_code,
         home_team = team_id,
         park_id)

final_schedule <- template %>%
  left_join(visitor) %>%
  left_join(home) %>% 
  mutate(game_num = sequence(nrow(template)),
         double_header = 0,
         season_type = 2,
         played = 0,
         col_h = -1,
         col_i = 9) %>%
  select(game_num 
         , date
         , double_header
         , visit_team
         , home_team
         , season_type
         , played
         , col_h
         , col_i)

## Write output without headers
write_csv(final_schedule, 
          "schedule_2024.csv",
          col_names = FALSE)
