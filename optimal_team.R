###########################################################
# optimal_team.R
#
# A non-strict optimization function to understand 
# how good each team is/where they have extra playtime
# based on all_ratings table
###########################################################

positionalNeeds <- function(ratings) {
  ## identify playtime needs for each position
  
  TOTAL_TEAMS <- ratings %>%
    filter(!is.na(Team)) %>%
    distinct(Team) %>%
    nrow()
  
  AL_TEAMS <- ratings %>%
    filter(Conference == 'AL') %>%
    distinct(Team) %>%
    nrow()
  
  pos_hierarchy <- c('C'
                     , 'SS'
                     , '3B'
                     , 'CF'
                     , 'RF'
                     , 'LF'
                     , '2B'
                     , '1B'
  )
  
  pos_pa_needs_base <- expand.grid(POS = pos_hierarchy
                                   , split = c("LH", "RH")
                                   , stringsAsFactors = FALSE) %>%
    mutate(POS_PA = ifelse(split == "LH", LH_PA_FULL, RH_PA_FULL) * TOTAL_TEAMS)
  
  pitch_pa_needs <- tribble(
    ~POS, ~split, ~POS_PA,
    'SP', 'BOTH', 162 * TOTAL_TEAMS,
    'RP', 'BOTH', TOTAL_TEAMS * 10 / 3 * 162, 
    ## Assuming 3.3 innings of relief per game average
    'DH', 'LH', AL_TEAMS * LH_PA_FULL,
    'DH', 'RH', AL_TEAMS * RH_PA_FULL
  )
  
  pos_pa_needs_base <- pos_pa_needs_base %>% 
    bind_rows(pitch_pa_needs)
  
  return(pos_pa_needs_base)
}

prepRatings <- function(ratings) {
  ## Clean up ratings so that there is only one line for pitchers
  ## showing a combined rate
  
  batters <- ratings %>%
    filter(role == 'batter'
           , POS != 'DH') %>%
    mutate(run_raa = ifelse(is.na(run_raa), 0, run_raa)
           , RAA_PA = ifelse(is.na(RAA_PA), 0, RAA_PA)
           , pa_value = run_raa / TOTAL_PA_FULL + RAA_PA + wRAA
           , max_playtime = max_PA) %>%
    select(ID, Name, season, TeamName, POS, split, max_playtime, pa_value) 
  
  pitchers <- ratings %>%
    filter(role == 'pitcher') %>%
    mutate(POS = ifelse(POS == 'sp', 'SP', 'RP') 
           , total_value = wRAA * max_PA) %>%
    group_by(ID, Name, season, POS, TeamName) %>%
    summarise(max_GS = min(max_GS)
              , max_INN = sum(max_INN)
              , total_value = sum(total_value)) %>%
    mutate(max_playtime = ifelse(POS == "SP", max_GS, max_INN)
           , pa_value = total_value / max_playtime
           , split = "BOTH") %>%
    select(ID, Name, season, TeamName, POS, split, max_playtime, pa_value)
  
  batters %>%
    bind_rows(pitchers) %>%
    mutate(pt_used = 0
           , pt_remaining = max_playtime)
}

prepDHRatings <- function(ratings, player_pt) {
  dh <- ratings %>%
    filter(role == 'batter') %>%
    mutate(pa_value = wRAA
           , max_playtime = max_PA
           , POS = 'DH') %>%
    select(ID, Name, season, TeamName, POS, split, max_playtime, pa_value) %>%
    mutate(pt_used = 0
           , pt_remaining = max_playtime)
  
  dh_update <- dh %>%
    left_join(player_pt, by = c("ID", "Name", "split")) %>%
    mutate(pt_remaining = pmax(pt_remaining - player_pt_used, 0)) %>%
    select(-player_pt_used)
}

getPlayerAssignedPT <- function(assigned_playtime) {
  assigned_playtime %>%
    group_by(ID, Name, split) %>%
    summarise(player_pt_used = sum(pt_used))
}

getNextPlayer <- function(opt_team, needs) {
  next_player <- opt_team %>%
    filter(pt_remaining > 0) %>%
    left_join(needs, by = c("POS", "split")) %>%
    mutate(pt_assigned = pmin(pt_remaining, POS_PA)
           ## calc how much pt can be assigned
           , most_value = pt_assigned * pa_value * (1 - pt_assigned / POS_PA)) %>%
           ## weight the POS that would close the gap the most for needs
    arrange(desc(most_value)) %>%
    slice(1) %>%
    select(ID, Name, season, TeamName, POS, split, pt_assigned)
  
  return(next_player)
}

updateOptTeam <- function(opt_team, next_player) {
  
  ## Add next player to opt_team and update pt_used
  opt_team_update <- opt_team %>%
    left_join(next_player, by = c("ID", "Name", "season", "TeamName", "POS", "split")) %>%
    mutate(pt_used = pt_used + ifelse(is.na(pt_assigned), 0, pt_assigned)) %>%
    select(-pt_assigned)
  
  ## calculate playtime remaining per player and update pt_remaining
  player_pt <- getPlayerAssignedPT(opt_team_update)
  
  opt_team_update <- opt_team_update %>%
    left_join(player_pt, by = c("ID", "Name", "split")) %>%
    mutate(pt_remaining = pmax(pt_remaining - player_pt_used, 0)) %>%
    select(-player_pt_used)
  
  return(opt_team_update)
}

updateNeeds <- function(needs, next_player) {
  ## Add next player pos_split to needs and calculate pt_remaining
  next_pos <- next_player %>%
    select(POS, split, pt_assigned)
  
  needs_update <- needs %>%
    left_join(next_pos, by = c("POS", "split")) %>%
    mutate(POS_PA = POS_PA - ifelse(is.na(pt_assigned), 0, pt_assigned)) %>%
    select(-pt_assigned)
  
  return(needs_update)
}

test_ratings <- all_ratings %>%
  filter(season == 2020)

opt_team <- prepRatings(test_ratings)
needs <- positionalNeeds(test_ratings) %>%
  filter(POS != 'DH')

while (sum(needs$POS_PA) > 0) {
  print(sum(needs$POS_PA))
  next_player <- getNextPlayer(opt_team, needs)
  #print(next_player)
  opt_team <- updateOptTeam(opt_team, next_player)
  needs <- updateNeeds(needs, next_player)
}

# Now finish off the optimization with DH ---------------------------------

player_pt <- getPlayerAssignedPT(opt_team)

dh_opt <- prepDHRatings(test_ratings, player_pt)

dh_needs <- positionalNeeds(test_ratings) %>%
  filter(POS == 'DH')

while (sum(dh_needs$POS_PA) > 0) {
  print(sum(dh_needs$POS_PA))
  next_player <- getNextPlayer(dh_opt, dh_needs)
  #print(next_player)
  dh_opt <- updateOptTeam(dh_opt, next_player)
  dh_needs <- updateNeeds(dh_needs, next_player)
}

total_opt_team <- opt_team %>%
  bind_rows(dh_opt)

write_csv(opt_team, "OUTPUT_NEW/opt_team_2020.csv")

# TOTAL_TEAMS <- 28
# 
# pos_hierarchy <- c('C'
#                    , 'SS'
#                    , '3B'
#                    , 'CF'
#                    , 'RF'
#                    , 'LF'
#                    , '2B'
#                    , '1B'
#                    , 'DH'
# )

## Only need half as much DH PA (for the AL)
## Need 162 starts for pitchers
## Need xx amount of innings from bullpen

## Setup needs player information, grouping information, and 

# opt_team_base <- final_players %>%
#   filter(season == LATEST_SEASON
#          ## , split == "RH"
#          , POS %in% pos_hierarchy
#   ) %>%
#   left_join(player_splits_final, by = c('season', 'Name', 'ID')) %>%
#   left_join(rosters, by = c('season', 'Name', 'ID')) %>%
#   mutate(pa_value = run_raa / TOTAL_PA_FULL + RAA_PA + wRAA) %>%
#   select(ID, Name, season, TeamName, POS, split, max_PA, pa_value) 
# 
# opt_pitchers_base <- final_pitchers %>%
#   left_join(stats_final, by = c("ID", "Name", "season")) %>%
#   left_join(rosters, by = c('season', 'Name', 'ID')) %>%
#   mutate(PA_wRAA = LH_wRAA * PITCH_LH_SPLIT + RH_wRAA * (1 - PITCH_LH_SPLIT)
#          , POS = ifelse(P %in% c('sp', 'dh'), 'SP', 'RP') 
#          , max_PA = ifelse(P == "sp", max_GS, INN * 1.33)
#          , pa_value = PA_wRAA * (LH_PA + RH_PA) / max_PA
#          , split = "BOTH") %>%
#   filter(season == LATEST_SEASON) %>%
#   select(ID, Name, season, TeamName, POS, split, max_PA, pa_value)
# 
# opt_team_base <- opt_team_base %>%
#   bind_rows(opt_pitchers_base)

# assignPA <- function(players, player_pa, pos_pa) {
#   ## add current player and positional PA to player dataframe, and figure out 
#   ## the next best player
#   
#   players %>% 
#     left_join(player_pa, by = c("ID", "split")) %>%
#     left_join(pos_pa, by = c("POS", "split")) %>%
#     mutate(pa_used = ifelse(is.na(pa_used), 0, pa_used)
#            , pa_remaining = pmax(max_PA - pa_used, 0)
#            , player_pos_value = pa_remaining * pa_value * POS_PA) %>%
#     filter(pa_remaining > 0
#            , POS_PA > 0) %>%
#     arrange(desc(player_pos_value)) %>%
#     slice(1)
# }

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

# pos_pa_needs_base <- expand.grid(POS = pos_hierarchy
#                                  , split = c("LH", "RH")
#                                  , stringsAsFactors = FALSE) %>%
#   mutate(POS_PA = ifelse(split == "LH", LH_PA_FULL, RH_PA_FULL) * TOTAL_TEAMS)
# 
# pitch_pa_needs <- tribble(
#   ~POS, ~split, ~POS_PA,
#   'SP', 'BOTH', 162 * TOTAL_TEAMS,
#   'RP', 'BOTH', TOTAL_TEAMS * 10 / 3 * 162
# )
# 
# pos_pa_needs_base <- pos_pa_needs_base %>% 
#   bind_rows(pitch_pa_needs)
# 
# player_pa <- opt_team_base %>%
#   distinct(ID, split) %>%
#   mutate(pa_used = 0)

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


# Combine with original team info to see who has excess value -------------

final_opt_teams_clean <- final_opt_teams %>%
  mutate(pa_used = pmin(POS_PA, pa_remaining)
         , opt_value = pa_used * pa_value) %>%
  select(ID:split, pa_used, opt_value)

player_pa_used_final <- final_opt_teams_clean %>%
  group_by(ID, Name, season, split) %>%
  summarise(total_pa_used = sum(pa_used))

overall_value <- opt_team_base %>%
  left_join(player_pa_used_final, by = c("ID", "Name", "season", "split")) %>%
  left_join(final_opt_teams_clean, by = c("ID", "Name", "season", "split", "POS", "TeamName")) %>%
  mutate(total_pa_used = ifelse(is.na(total_pa_used),0,total_pa_used)
         , pa_remaining = max_PA - total_pa_used
         , possible_value = pa_value * pa_remaining)


write_excel_csv(final_opt_teams, "opt_teams_test_postfa.csv")
write_excel_csv(overall_value, "overall_value.csv")
