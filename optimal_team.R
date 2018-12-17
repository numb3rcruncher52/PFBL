###########################################################
# optimal_team.R
#
# A non-strict optimization function to understand 
# how good each team is/where they have extra playtime
###########################################################
source("Dmb_dw.R")

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
  filter(season == 2018
        ## , split == "RH"
         , POS %in% pos_hierarchy
  ) %>%
  mutate(pa_value = run_raa / TOTAL_PA_FULL + RAA_PA + wRAA) %>%
  select(ID, Name, season, TeamName, POS, split, max_PA, pa_value) 

assignPA <- function(players, player_pa, pos_pa) {
  ## add current player and positional PA to player dataframe, and figure out 
  ## the next best player
  
  players %>% 
    left_join(player_pa, by = "ID") %>%
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
    group_by(ID) %>%
    summarise(pa_used = sum(max_PA))
}

updatePositionPA <- function(final_players, pos_pa_needs) {
  pos_pa_used <- final_players %>%
    group_by(POS) %>%
    summarise(pa_used = sum(max_PA))
  
  pos_pa_needs %>%
    left_join(pos_pa_used, by = "POS") %>%
    mutate(pa_used = ifelse(is.na(pa_used), 0, pa_used)
           , POS_PA = pmax(POS_PA - pa_used, 0)) %>%
    select(-pa_used)
}

pos_pa_needs_base <- expand.grid(POS = pos_hierarchy
                                 , split = c("LH", "RH")
                                 , stringsAsFactors = FALSE) %>%
  mutate(POS_PA = ifelse(split == "LH", LH_PA_FULL, RH_PA_FULL) * 28)

player_pa <- final_players %>%
  distinct(ID) %>%
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

View(final_pa_used %>% group_by(TeamName) %>% summarise(batting_value = sum(value)))

positional_replacement <- final_pa_used %>%
  group_by(POS) %>%
  summarise(replacement_level = min(value))


#    , split_max = ifelse(split == "RH"
#, RH_PA_FULL
#, LH_PA_FULL)



distPlayTime <- function(data, position) {
  data %>%
    filter(POS == position) %>%
    group_by(TeamName, split) %>%
    mutate(cum_pa = cumsum(max_PA)
           , use_pa = ifelse(cum_pa <= split_max
                             , max_PA
                             , pmax(0,split_max - (cum_pa - max_PA)))
           , value = use_pa * pa_value) %>%
    select(ID
           , Name
           , TeamName
           , season
           , POS_final = POS
           , split
           , use_pa
           , value)
}

raw_stats <- opt_team_base

for (i in seq_along(pos_hierarchy)) {
  ## Find playtime for one position
  positional <- distPlayTime(raw_stats, pos_hierarchy[i])
  
  ## subtract that play time from all instances of that player
  raw_stats <- raw_stats %>%
    left_join(positional, by = c('ID'
                                 , 'Name'
                                 , 'TeamName'
                                 , 'season'
                                 , 'split')) %>%
    mutate(max_PA = max_PA - ifelse(is.na(use_pa), 0, use_pa)) %>%
    select(-use_pa, -value, -POS_final)
  
  ## keep final playtime record
  if (i == 1) {
    final <- positional 
  } else {
    final <- bind_rows(final, positional)
  }
}

test_df <- final %>%
  group_by(TeamName
           , POS_final
           , split) %>%
  summarise(total_pa = sum(use_pa)
            , total_value = sum(value))

overall_team <- test_df %>%
  group_by(TeamName) %>%
  summarise(total_pa = sum(total_pa)
            , total_value = sum(total_value)) %>%
  arrange(desc(total_value))

## Thoughts::
# to more efficiently optimize we need:
# 1. to go from most useful numbers as opposed to by position
# 2. ensure out of position values are possible
# 3. when to incorporate DH? 

## Sort all players
## For first player that comes up:
## compute how much playing time is needed everywhere
## choose the pos that adds the most total value 

raw_stats <- opt_team_base

pt_df_init <- expand.grid(POS = pos_hierarchy
                     , split = c("LH", "RH")
                     , stringsAsFactors = FALSE) %>%
  mutate(assigned_pa = 0)
pt_df <- pt_df_init

for (i in 1:nrow(raw_stats)) {
  current_player <- raw_stats$ID[i]
  current_split <- raw_stats$split[i]
  
  ## grab all occurrences of this player split
  cur_player_split <- raw_stats[i:nrow(raw_stats),] %>%
    filter(ID == current_player
           , split == current_split) %>%
    select(ID
           , Name
           , TeamName
           , season
           , split
           , POS
           , max_PA
           , pa_value
           , split_max) %>%
    left_join(pt_df, by = c("POS", "split")) %>%
    mutate(pa_comp = pmax(split_max - assigned_pa, 0)
           , use_pa = pmin(pa_comp, max_PA)
           , value = use_pa * pa_value) %>%
    arrange(desc(value)) %>%
    mutate(cum_use_pa = cumsum(use_pa)
           , use_pa = ifelse(cum_use_pa <= max_PA
                             , cum_use_pa
                             , pmax(0, max_PA - (cum_use_pa - use_pa)))) %>%
    filter(use_pa != 0) %>%
    select(ID:split
           , POS_final = POS
           , use_pa
           , value)
  
  cur_player_pa <- cur_player_split %>%
    group_by(ID
             , Name
             , TeamName
             , season
             , split) %>%
    summarise(use_pa = sum(use_pa))
  
    
  ## subtract that play time from all instances of that player
  raw_stats <- raw_stats %>%
    left_join(cur_player_pa, by = c('ID'
                                 , 'Name'
                                 , 'TeamName'
                                 , 'season'
                                 , 'split')) %>%
    mutate(max_PA = max_PA - ifelse(is.na(use_pa), 0, use_pa)) %>%
    select(-use_pa)
  
  ## keep final playtime record
  if (i == 1) {
    final <- cur_player_split
  } else {
    final <- bind_rows(final, cur_player_split)
  }
  
  
  ## subtract play time from master position record
  pt_df <- final %>%
    rename(POS = POS_final
           , assigned_pa = use_pa) %>%
    bind_rows(pt_df_init) %>%
    group_by(POS, split) %>%
    summarise(assigned_pa = sum(assigned_pa))
  print(cur_player_split)
  print(final)
  print(pt_df)
  
  #print(final)
}



# 3rd try at simplifying optimization -------------------------------------

pt_df_init <- expand.grid(POS = pos_hierarchy
                          , split = c("LH", "RH")
                          , season = unique(opt_team_base$season)
                          , TeamName = unique(opt_team_base$TeamName)
                          , stringsAsFactors = FALSE) %>%
  mutate(remaining_pa = ifelse(split == "LH"
                              , LH_PA_FULL
                              , RH_PA_FULL))

raw_stats <- opt_team_base %>%
  left_join(pt_df_init, by = c("POS", "split", "season", "TeamName")) %>%
  mutate(assigned_pa = 0)

topRemainingValue <- function(players, pt_df) {
  players %>%
    select(-remaining_pa) %>%
    left_join(pt_df, by = c("POS", "split", "season", "TeamName")) %>%
    mutate(possible_pa = pmin(remaining_pa, max_PA - assigned_pa)
           , possible_value = possible_pa * pa_value) %>% 
    filter(possible_pa != 0) %>%
    arrange(desc(possible_value)) %>%
    slice(1) %>%
    select(ID
           , Name
           , season
           , TeamName
           , POS
           , split
           , assigned_pa = possible_pa
           , value = possible_value)
}


updatePTDF <- function(final_values, pt_df) {
  assigned_pa <- final_values %>%
    group_by(POS, split, season, TeamName) %>%
    summarise(assigned_pa = sum(assigned_pa))
  
  pt_df %>%
    left_join(assigned_pa, by = c("POS", "split", "season", "TeamName")) %>%
    mutate(remaining_pa = pmax(0, remaining_pa - ifelse(is.na(assigned_pa)
                                                , 0
                                                , assigned_pa))) %>%
    select(-assigned_pa)
}

updatePlayers <- function(raw_players, final_values) {
  total_players <- final_values %>%
    group_by(ID, Name, TeamName, season, split) %>%
    summarise(assigned_pa = sum(assigned_pa))
  
  raw_players %>%
    left_join(total_players, by = c('ID', 'Name', 'TeamName', 'season', 'split')) %>%
    mutate(assigned_pa = ifelse(is.na(assigned_pa), 0, assigned_pa))
}

final <- topRemainingValue(raw_stats, pt_df_init)

pt_df <- updatePTDF(final, pt_df_init)

players <- updatePlayers(raw_stats %>% select(-assigned_pa), final)

while (sum(pt_df$remaining_pa) > 0) {
  next_best_player <- topRemainingValue(players, pt_df)
  if (nrow(next_best_player) == 0) break
  #print(next_best_player)
  final <- bind_rows(final,next_best_player)
  
  pt_df <- updatePTDF(final, pt_df_init)
  
  players <- updatePlayers(players %>% select(-assigned_pa), final)
}

test_df <- final %>%
  group_by(TeamName
           , POS) %>%
  summarise(total_pa = sum(assigned_pa)
            , total_value = sum(value))

overall_team <- test_df %>%
  group_by(TeamName) %>%
  summarise(total_pa = sum(total_pa)
            , total_value = sum(total_value)) %>%
  arrange(desc(total_value))