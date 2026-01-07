##############################################################
# 04_OptimalPlayTime.R
#
# Run this to combine relevant data and find optimal playtime
##############################################################

rm(list=ls())

library(tidyverse)

LATEST_SEASON <- 2026

## update MAPPING_DATA/dim_team.csv before beginning

source("Solution Scripts/z_CoefficientsLoad.R")
source("Solution Scripts/z_CalculateValue.R")
source("Solution Scripts/z_OptimalTeamLP.R")
##source("Solution Scripts/EvaluateResults.R")

# Load all season data ----------------------------------------------------
rosters <- read_csv("PROCESSED_DATA/pfbl_rosters.csv") %>%
  mutate(TeamName = ifelse(TeamName == 'Tower Hall Nitgh Owlz', 'Tower Hall Night Owlz', TeamName))
players <- read_csv("PROCESSED_DATA/pfbl_players.csv")
stats <- read_csv("PROCESSED_DATA/pfbl_splits_real.csv")

# Final Reporting Tables --------------------------------------------------

## TeamName Check
rosters %>%
  filter(!is.na(Team)) %>%
  distinct(Team, TeamName, season) %>%
  anti_join(dim_team, by = c("Team", "TeamName", "season"="Season"))

## Calculate Rookies
rookie_season <- rosters %>%
  group_by(ID) %>%
  arrange(season) %>%
  mutate(rookie_season = min(season))

## Join all relevant data together
all_ratings <- players %>%
  left_join(stats, by = c("ID", "Name", "season", "role")) %>%
  left_join(rookie_season, by = c("ID", "Name", "season")) %>%
  left_join(dim_team, by = c("Team", "TeamName", "season"="Season")) %>%
  mutate(TeamName = ifelse(Name %in% c('Freddie Freeman', 'Blake Perkins', 'Nathan Eovaldi') & season == 2026, 'West Side Wizards', TeamName)) %>%
  mutate(TeamName = ifelse(Name %in% c('Connor Norby','Parker Meadows', 'Lance McCullers', 'Bailey Ober', 'Max Scherzer') & season == 2026, 'Tri City Tornados', TeamName)) %>%
  mutate(TeamName = ifelse(Name %in% c('Jose Berrios','Pete Fairbanks','Mike Yastrzemski','Gleyber Torres','Brendon Little') & season == 2026, 'Brandywine Boom',  TeamName)) %>%
  mutate(TeamName = ifelse(Name %in% c('Spencer Torkelson', 'Logan Allen') & season == 2026, 'Back Bay Blizzard',  TeamName)) %>%
  mutate(TeamName = ifelse(Name %in% c('Riley Greene') & season == 2026, 'Lake Mendota Muskies',TeamName)) %>%
  mutate(TeamName = ifelse(Name %in% c('Matt Olson') & season == 2026, 'Frisco Beer Snobs',TeamName)) %>%
  mutate(TeamName = ifelse(Name %in% c('Brayan Bello', 'Xander Bogaerts') & season == 2026, 'Alcatraz Stars',TeamName)) %>%
  mutate(TeamName = ifelse(Name %in% c('Tyler Ferguson') & season == 2026, 'San Diego Flash',TeamName)) %>%
  ## Scenario
  mutate(TeamName = ifelse(Name %in% c('Taylor Ward', 'Jacob deGrom', 'Jorge Polanco') & season == 2026, 'Frisco Beer Snobs', ifelse(Name %in% c('Reese Olson', 'Cristian Javier') & season == 2026, 'Middleburg Mudcats', TeamName))) %>% 
  calcPlaytimeLimits()

## Calculate Optimal Team
 optimal_team <- all_ratings %>%
   #mutate(TeamName = ifelse(Name %in% c('Chase Silseth', 'Chris Taylor', 'Dominic Canzone', 'Spencer Schwellenbach', 'Andy Pages', 'Hayden Birdsong', 'Jhonkensy Noel', 'Matt Thaiss') & season == 2025, 'Frisco Beer Snobs', TeamName)) %>%
   #mutate(TeamName = ifelse(Name %in% c('Jose Azocar') & season == 2025, 'FA', TeamName)) %>%
   #mutate(TeamName = ifelse(Name %in% c('Ian Happ') & season == 2025, 'Highland Engineers', TeamName)) %>%
   #mutate(TeamName = ifelse(Name %in% c('Brandon Lowe') & season == 2025, 'San Diego Flash', TeamName)) %>%
   #mutate(TeamName = ifelse(Name %in% c('Randal Grichuk', 'Jose Caballero', 'Jeff Hoffman') & season == 2025, 'Bennington Black Diamonds', TeamName)) %>%
   #mutate(TeamName = ifelse(Name %in% c('Bailey Ober', 'Miles Mikolas', 'Brendan Rodgers') & season == 2024, 'San Diego Flash', TeamName)) %>%
#   mutate(TeamName = ifelse(Name %in% c('Clayton Kershaw', 'Devin Williams', 'Patrick Corbin', 'David Robertson') & season == 2024, 'Galveston Whitecaps', TeamName)) %>%
   optimalTeam()

# Write out Final Tables --------------------------------------------------

write_csv(all_ratings, "PROCESSED_DATA/pfbl_all_ratings.csv")
write_csv(optimal_team, "OUTPUT_NEW/optimal_value_2022.csv")

# Hypothetical trades -----------------------------------------------------

all_ratings <- read_csv("PROCESSED_DATA/pfbl_all_ratings.csv")

team_run_value <- function(team_ratings) {
  team_ratings %>%
    optimalTeam() %>%
    replace_na(list(opt_pa = 0, opt_inn = 0, opt_starts = 0, value_PA = 0, value_GS = 0, value_INN = 0)) %>%
    mutate(value = opt_pa * value_PA + opt_starts * value_GS + opt_inn * value_INN) %>%
    summarise(value = sum(value)) %>%
    pull(value)
}

checkteam <- all_ratings %>%
  mutate(TeamName = ifelse(Name %in% c('Taylor Ward', 'Jacob deGrom', 'Jorge Polanco') & season == 2026, 'Frisco Beer Snobs', ifelse(Name %in% c('Reese Olson', 'Cristian Javier') & season == 2026, 'Middleburg Mudcats', TeamName))) %>%
  ##mutate(POS = ifelse(Name == 'Francisco Lindor' & POS == 'SS', '2B', POS)) %>%
filter(season == 2026, TeamName == "Frisco Beer Snobs")

## 158 
checkteam %>% team_run_value()

biggest_gain <- function(all_ratings, team) {
  ## for each player on the team, calculate the lost run value from losing that player
  # baseline value for team of interest
  current_team <- all_ratings %>%
    filter(TeamName == team)
  
  current_val <- current_team %>%
    team_run_value()
  
  ## Identify all players not on the team 
  all_players <- team_ratings %>%
    filter(TeamName != team) 
  
  add_player <- function(add_id) {
    added_player <- all_players %>%
      filter(ID == add_id)
    
    current_team %>%
      bind_rows(added_player) %>%
      team_run_value()
  }
  
  new_vals <- all_players %>%
    pull(ID) %>%
    map_dbl(rm_player)
  
  all_players %>% 
    mutate(new_val = new_vals,
           net_diff = new_val - current_val)
}

biggest_loss <- function(team_ratings) {
  ## for each player on the team, calculate the lost run value from losing that player
  current_val <- team_run_value(team_ratings)
  all_players <- team_ratings %>%
    distinct(ID, Name)
  
  rm_player <- function(rm_id) {
    team_ratings %>%
      filter(ID != rm_id) %>%
      team_run_value()
  }
  
  new_vals <- all_players %>%
    pull(ID) %>%
    map_dbl(rm_player)
  
  all_players %>% 
    mutate(new_val = new_vals,
           net_diff = new_val - current_val)
}

new_vals <- checkteam %>% biggest_gain()

## baseline: 251
checkteam <- all_ratings %>%
  mutate(TeamName = ifelse(Name %in% c('Jose E. Ramirez', 'George Springer', 'Brandon Nimmo') & season == 2026, 'Frisco Beer Snobs',
                          ifelse(Name %in% c('Reese Olson', 'Jhonkensy Noel'), 'Tri-City Tornadoes',TeamName))) %>%
  filter(season == 2026, TeamName == 'Frisco Beer Snobs')

checkteam <- all_ratings %>%
  #mutate(TeamName = ifelse(Name %in% c('Chase Silseth', 'Chris Taylor', 'Dominic Canzone', 'Spencer Schwellenbach', 'Andy Pages', 'Hayden Birdsong', 'Jhonkensy Noel', 'Bryan Ramos', 'Leo Jimenez', 'Joey Loperfido', 'Oliver Dunn', 'Joe Boyle', 'Kyle Leahy', 'Randy Rodriguez') & season == 2025, 'Frisco Beer Snobs', TeamName)) %>%
  mutate(TeamName = ifelse(Name %in% c('Jose Azocar', 'Chris Taylor', 'Nick Senzel') & season == 2025, 'FA', TeamName)) %>%
  mutate(TeamName = ifelse(Name %in% c('Matt Thaiss', 'Jared Triolo') & season == 2025, 'Frisco Beer Snobs', TeamName)) %>%
  mutate(TeamName = ifelse(Name %in% c('Brandon Lowe') & season == 2025, 'San Diego Flash', TeamName)) %>%
  mutate(TeamName = ifelse(Name %in% c('Randal Grichuk', 'Jose Caballero', 'Jeff Hoffman') & season == 2025, 'Bennington Black Diamonds', TeamName)) %>%
  ##mutate(TeamName = ifelse(Name %in% c('Bailey Ober', 'Miles Mikolas', 'Brendan Rodgers') & season == 2024, 'San Diego Flash', TeamName)) %>%
  ##mutate(TeamName = ifelse(Name %in% c('Clayton Kershaw', 'Devin Williams', 'Patrick Corbin', 'David Robertson') & season == 2024, 'Galveston Whitecaps', TeamName)) %>%
  
  filter(season == 2025, TeamName == 'Frisco Beer Snobs')

## Stars Trade
checkteam <- all_ratings %>%
  mutate(TeamName = ifelse(Name %in% c('Brandon Lowe', 'Ian Happ', 'Randal Grichuk'), 'FA', TeamName)) %>%
  #mutate(TeamName = ifelse(Name %in% c('Cavan Biggio'), 'Lake Mendota Muskies', TeamName)) %>%
  filter(season == 2025, TeamName == 'Frisco Beer Snobs')


checkteam <- all_ratings %>%
  mutate(TeamName = ifelse(Name %in% c('Andrew Knizner'), 'Frisco Beer Snobs', TeamName)) %>%
  mutate(TeamName = ifelse(Name %in% c('Luis Torrens'), 'Lake Mendota Muskies', TeamName)) %>%
  filter(season == 2023, TeamName == 'Frisco Beer Snobs')

checkteam %>%
  optimalTeam() %>%
  #select(Name, POS, split, value_PA, value_GS, value_INN, opt_pa, opt_starts, opt_inn)
  replace_na(list(opt_pa = 0, opt_inn = 0, opt_starts = 0, value_PA = 0, value_GS = 0, value_INN = 0)) %>%
  mutate(value = opt_pa * value_PA + opt_starts * value_GS + opt_inn * value_INN) %>%
  #group_by(POS,split) %>%
  summarise(value = sum(value), opt_pa = sum(opt_pa), opt_inn = sum(opt_inn)) %>%
  filter(value != 0 )
  #View()

View(check_opt <- checkteam %>%
       optimalTeam() %>%
       select(Name, POS, split, value_PA, value_GS, value_INN, opt_pa, opt_starts, opt_inn) %>%
       replace_na(list(opt_pa = 0, opt_inn = 0, opt_starts = 0, value_PA = 0, value_GS = 0, value_INN = 0)) %>%
       mutate(value = opt_pa * value_PA + opt_starts * value_GS + opt_inn * value_INN) %>%
       group_by(POS,Name) %>%
       summarise(value = sum(value), opt_pa = sum(opt_pa), opt_inn = sum(opt_inn)/2, opt_gs = sum(opt_starts)/2) %>%
       filter(value != 0 ))

View(check_opt <- checkteam %>%
       optimalTeam() %>%
       select(Name, POS, split, value_PA, value_GS, value_INN, opt_pa, opt_starts, opt_inn) %>%
       replace_na(list(opt_pa = 0, opt_inn = 0, opt_starts = 0, value_PA = 0, value_GS = 0, value_INN = 0)) %>%
       mutate(value = opt_pa * value_PA + opt_starts * value_GS + opt_inn * value_INN) %>%
       group_by(POS) %>%
       summarise(value = sum(value), opt_pa = sum(opt_pa), opt_inn = sum(opt_inn)/2, opt_gs = sum(opt_starts)/2) %>%
       filter(value != 0 ))

View(check_opt %>%
       replace_na(list(opt_pa = 0, opt_inn = 0, opt_starts = 0, value_PA = 0, value_GS = 0, value_INN = 0)) %>%
       mutate(value = opt_pa * value_PA + opt_starts * value_GS + opt_inn * value_INN) %>%
       arrange(-value) %>% 
       select(Name, POS, split, value, opt_pa, value_PA, opt_starts, value_GS, opt_inn, value_INN)) %>%
  summarise(value = sum(value))