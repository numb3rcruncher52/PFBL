###########################################################
# optimal_team.R
#
# A non-strict optimization function to understand 
# how good each team is/where they have extra playtime
###########################################################
source("Dmb_dw.R")

TOTAL_TEAMS <- 28

pos_hierarchy <- c('C'
                   , 'SS'
                   , '3B'
                   , 'CF'
                   , 'RF'
                   , 'LF'
                   , '2B'
                   , '1B'
                   #, 'DH'
)

## Only need half as much DH PA (for the AL)
## Need 162 starts for pitchers
## Need xx amount of innings from bullpen

## Setup needs player information, grouping information, and 

opt_team_base <- final_players %>%
  left_join(player_splits_final, by = c('season', 'Name', 'ID')) %>%
  left_join(rosters, by = c('season', 'Name', 'ID')) %>%
  filter(season == LATEST_SEASON
        ## , split == "RH"
         , POS %in% pos_hierarchy
  ) %>%
  mutate(pa_value = run_raa / TOTAL_PA_FULL + RAA_PA + wRAA) %>%
  select(ID, Name, season, TeamName, POS, split, max_PA, pa_value) 

opt_pitchers_base <- final_pitchers %>%
  left_join(stats_final, by = c("ID", "Name", "season")) %>%
  left_join(rosters, by = c('season', 'Name', 'ID')) %>%
  mutate(PA_wRAA = LH_wRAA * PITCH_LH_SPLIT + RH_wRAA * (1 - PITCH_LH_SPLIT)
         , POS = ifelse(P %in% c('sp', 'dh'), 'SP', 'RP') 
         , max_PA = ifelse(P == "sp", max_GS, INN * 1.33)
         , pa_value = PA_wRAA * (LH_PA + RH_PA) / max_PA
         , split = "BOTH") %>%
  filter(season == LATEST_SEASON) %>%
  select(ID, Name, season, TeamName, POS, split, max_PA, pa_value)

opt_team_base <- opt_team_base %>%
  bind_rows(opt_pitchers_base)

assignPA <- function(players, player_pa, pos_pa) {
  ## add current player and positional PA to player dataframe, and figure out 
  ## the next best player
  
  players %>% 
    left_join(player_pa, by = c("ID", "split")) %>%
    left_join(pos_pa, by = c("POS", "split")) %>%
    mutate(pa_used = ifelse(is.na(pa_used), 0, pa_used)
           , pa_remaining = pmax(max_PA - pa_used, 0)
           , player_pos_value = pa_remaining * pa_value * POS_PA) %>%
    filter(pa_remaining > 0
           , POS_PA > 0) %>%
    arrange(desc(player_pos_value)) %>%
    slice(1)
}

update_players <- function(final_players) {
  final_players %>%
    group_by(ID, split) %>%
    summarise(pa_used = sum(max_PA))
}

updatePositionPA <- function(final_players, pos_pa_needs) {
  pos_pa_used <- final_players %>%
    group_by(POS, split) %>%
    summarise(pa_used = sum(max_PA))
  
  pos_pa_needs %>%
    left_join(pos_pa_used, by = c("POS", "split")) %>%
    mutate(pa_used = ifelse(is.na(pa_used), 0, pa_used)
           , POS_PA = pmax(POS_PA - pa_used, 0)) %>%
    select(-pa_used)
}


# Establish Positional Baselines ------------------------------------------

pos_pa_needs_base <- expand.grid(POS = pos_hierarchy
                                 , split = c("LH", "RH")
                                 , stringsAsFactors = FALSE) %>%
  mutate(POS_PA = ifelse(split == "LH", LH_PA_FULL, RH_PA_FULL) * TOTAL_TEAMS)

pitch_pa_needs <- tribble(
  ~POS, ~split, ~POS_PA,
  'SP', 'BOTH', 162 * TOTAL_TEAMS,
  'RP', 'BOTH', TOTAL_TEAMS * 10 / 3 * 162
)

pos_pa_needs_base <- pos_pa_needs_base %>% 
  bind_rows(pitch_pa_needs)

player_pa <- opt_team_base %>%
  distinct(ID, split) %>%
  mutate(pa_used = 0)

final_player_pa <- assignPA(opt_team_base, player_pa, pos_pa_needs_base)
player_pa <- update_players(final_player_pa)
pos_pa_needs <- updatePositionPA(final_player_pa, pos_pa_needs_base)

while (sum(pos_pa_needs$POS_PA) > 0) {
  print(sum(pos_pa_needs$POS_PA))
  final_player_pa <- final_player_pa %>%
    bind_rows(assignPA(opt_team_base, player_pa, pos_pa_needs))
  player_pa <- update_players(final_player_pa)
  pos_pa_needs <- updatePositionPA(final_player_pa, pos_pa_needs_base)
}

final_pa_used <- final_player_pa %>%
  mutate(pa_used = pmin(max_PA, POS_PA)
         , value = pa_used * pa_value) 

final_players_used <- final_pa_used %>%
  group_by(ID, Name, season, TeamName, POS) %>%
  summarise(max_PA = sum(max_PA)
            , pa_used = sum(pa_used)
            , value = sum(value))

positional_replacement <- final_pa_used %>%
  group_by(POS, split) %>%
  arrange(value) %>%
  slice(3) %>%
  summarise(pa_value = sum(value) / sum(pa_used))
  select(POS, split, pa_value)

write_csv(positional_replacement, "OUTPUT_NEW/pos_replace_2018.csv")
write_csv(final_players_used, "OUTPUT_NEW/final_player_pos.csv")


# Try Optimizing Rosters --------------------------------------------------

TOTAL_TEAMS <- 1

pos_pa_needs_base <- expand.grid(POS = pos_hierarchy
                                 , split = c("LH", "RH")
                                 , stringsAsFactors = FALSE) %>%
  mutate(POS_PA = ifelse(split == "LH", LH_PA_FULL, RH_PA_FULL) * TOTAL_TEAMS)

pitch_pa_needs <- tribble(
  ~POS, ~split, ~POS_PA,
  'SP', 'BOTH', 162 * TOTAL_TEAMS,
  'RP', 'BOTH', TOTAL_TEAMS * 10 / 3 * 162
)

pos_pa_needs_base <- pos_pa_needs_base %>% 
  bind_rows(pitch_pa_needs)


optimizeTeam <- function(TEAM_NAME, pos_pa_needs_base, opt_team_base) {
  current_team <- opt_team_base %>%
    filter(TeamName == TEAM_NAME)
  
  player_pa <- current_team %>%
    distinct(ID, split) %>%
    mutate(pa_used = 0)
  
  final_player_pa <- assignPA(current_team, player_pa, pos_pa_needs_base)
  player_pa <- update_players(final_player_pa)
  pos_pa_needs <- updatePositionPA(final_player_pa, pos_pa_needs_base)
  
  PA_NEEDS <- sum(pos_pa_needs$POS_PA) + 1
  
  while (sum(pos_pa_needs$POS_PA) < PA_NEEDS) {
    print(sum(pos_pa_needs$POS_PA))
    PA_NEEDS <- sum(pos_pa_needs$POS_PA)
    final_player_pa <- final_player_pa %>%
      bind_rows(assignPA(current_team, player_pa, pos_pa_needs))
    player_pa <- update_players(final_player_pa)
    pos_pa_needs <- updatePositionPA(final_player_pa, pos_pa_needs_base)
  }
  
  return(final_player_pa)
}

teams <- unique(opt_team_base$TeamName)

opt_teams <- map(teams, optimizeTeam, pos_pa_needs_base = pos_pa_needs_base
                 , opt_team_base = opt_team_base)

final_opt_teams <- bind_rows(opt_teams)

write_excel_csv(final_opt_teams, "opt_teams_test.csv")
