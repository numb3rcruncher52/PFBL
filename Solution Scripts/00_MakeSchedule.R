##############################################################
# MakeSchedule.R
#
# Run this to create schedules for a season based on template
##############################################################

rm(list=ls())

library(tidyverse)

LATEST_SEASON <- 2025

template <- read_csv("/Users/maxlyons/R/PFBL/MAPPING_DATA/schedule_template_2025.csv", col_types = "cccc") %>%
  filter(visitor != 'OFF') %>%
  ## Replace year with current season 
  mutate(date = str_sub(date, 1, -3) %>% str_c(str_sub(LATEST_SEASON - 1, -2)))
teams <- read_csv("/Users/maxlyons/R/PFBL/MAPPING_DATA/dim_team.csv")

## Replace The divisions we have in PFBL and the letter assigned to that division 
#on this spreadsheet need to be flip flopped every year for one league only.  
#So, for example, in odd-numbered years, the NL West would be 'N' teams on the 
#spread sheet.  In even-numbered years, the NL West would be 'S' teams.   The AL 
#divisions would remain the same in this example.  This is important so that 
#specific interleague opponents alternate between home and away series each season.  
#As the spreadsheet is set up, N is always at E, E is always at S, S is always at W, and W is always at N.

season_teams <- teams %>%
  filter(Season == LATEST_SEASON) %>%
  ## If season is even, AL East = E and AL West = W
  ## If season is odd, AL East = W and AL West = E 
  mutate(Division = case_when(LATEST_SEASON %% 2 == 1 & Division == 'E' ~ 'W'
                              , LATEST_SEASON %% 2 == 1 & Division == 'W' ~ 'E'
                              , .default = Division)) %>%
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
          paste0("schedule_",LATEST_SEASON,".csv"),
          col_names = FALSE)
