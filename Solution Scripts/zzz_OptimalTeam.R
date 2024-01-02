###########################################################
# OptimalTeam.R
#
# A non-strict optimization function to understand 
# how good each team is/where they have extra playtime
# based on all_ratings table
###########################################################

options(dplyr.summarise.inform = FALSE)

findOptimalTeam <- function(test_ratings, LEVEL = 'Team') {
  ## Could also be LEVEL = 'League' 
  positionalNeeds <- function(ratings, level = 'Team') {
    ## identify playtime needs for each position
    
    TOTAL_TEAMS <- ratings %>%
      filter(!is.na(TeamName)) %>%
      distinct(TeamName) 
    
    AL_TEAMS <- ratings %>%
      filter(Conference == 'AL') %>%
      distinct(TeamName) 
    
    pos_hierarchy <- c('C'
                       , 'SS'
                       , '3B'
                       , 'CF'
                       , 'RF'
                       , 'LF'
                       , '2B'
                       , '1B'
                       , 'DH'
    )
    
    pos_pa_needs_base <- expand_grid(POS = pos_hierarchy
                                     , split = c("LH", "RH")
                                     , TeamName = TOTAL_TEAMS$TeamName) %>%
      mutate(POS_PA = ifelse(split == "LH", LH_PA_FULL, RH_PA_FULL)) %>%
      filter(!(!(TeamName %in% AL_TEAMS$TeamName) & POS == 'DH'))
    
    pitch_pa_needs <- tribble(
      ~POS, ~split, ~POS_PA,
      'SP', 'BOTH', 162,
      'RP', 'BOTH', 10 / 3 * 162, 
      ## Assuming 3.3 innings of relief per game average
      # 'DH', 'LH', LH_PA_FULL,
      # 'DH', 'RH', RH_PA_FULL
    )
    
    pos_pa_needs_base <- pitch_pa_needs %>%
      expand_grid(TOTAL_TEAMS) %>%
      bind_rows(pos_pa_needs_base)
    
    if (level == 'Team') {
      return(pos_pa_needs_base)
    }
    else {
      pos_pa_needs_league <- pos_pa_needs_base %>%
        group_by(POS, split) %>%
        summarise(POS_PA = sum(POS_PA))
      return(pos_pa_needs_league)
    }
    
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
             , pt_remaining = max_playtime
             , value_used = 0
             , value_remaining = pt_remaining * pa_value) %>%
      arrange(desc(value_remaining))
  }
  
  prepDHRatings <- function(ratings, player_pt) {
    dh <- ratings %>%
      filter(role == 'batter') %>%
      mutate(pa_value = wRAA
             , max_playtime = max_PA
             , POS = 'DH') %>%
      select(ID, Name, season, TeamName, POS, split, max_playtime, pa_value) %>%
      mutate(pt_used = 0
             , pt_remaining = max_playtime
             , value_used = 0)
    
    dh_update <- dh %>%
      left_join(player_pt, by = c("ID", "Name", "split")) %>%
      mutate(pt_remaining = pmax(pt_remaining - player_pt_used, 0)
             , value_remaining = pt_remaining * pa_value) %>%
      select(-player_pt_used)
    
    return(dh_update)
  }
  
  getPlayerAssignedPT <- function(assigned_playtime) {
    assigned_playtime %>%
      group_by(ID, Name, split) %>%
      summarise(player_pt_used = sum(pt_used))
  }
  
  getNextPlayer <- function(opt_team, needs, join_crit = c("POS", "split")) {
    next_player <- opt_team %>%
      left_join(needs, by = join_crit) %>%
      filter(pt_remaining > 0
             , POS_PA > 0) %>%
      mutate(pt_assigned = pmin(pt_remaining, POS_PA)
             ## calc how much pt can be assigned
             , most_value = pt_assigned * pa_value * POS_PA) %>%
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
      mutate(pt_used = pt_used + ifelse(is.na(pt_assigned), 0, pt_assigned)
             , value_used = pt_used * pa_value) %>%
      select(-pt_assigned)
    
    ## calculate playtime remaining per player and update pt_remaining
    player_pt <- getPlayerAssignedPT(opt_team_update)
    
    opt_team_update <- opt_team_update %>%
      left_join(player_pt, by = c("ID", "Name", "split")) %>%
      mutate(pt_remaining = pmax(pt_remaining - player_pt_used, 0)
             , value_remaining = pt_remaining * pa_value) %>%
      select(-player_pt_used)
    
    return(opt_team_update)
  }
  
  updateNeeds <- function(needs, next_player, join_crit = c("POS", "split")) {
    ## Add next player pos_split to needs and calculate pt_remaining
    next_pos <- next_player %>%
      select(join_crit, pt_assigned)
    
    needs_update <- needs %>%
      left_join(next_pos, by = join_crit) %>%
      mutate(POS_PA = POS_PA - ifelse(is.na(pt_assigned), 0, pt_assigned)) %>%
      select(-pt_assigned)
    
    return(needs_update)
  }
  
  test_ratings <- all_ratings %>%
    filter(season == 2022)
  
  #LEVEL = 'Team' ##League'    ## Team if you want to do it by team
  
  if (LEVEL == 'Team') {
    JOIN_CRIT = c("POS", "split", "TeamName")
  } else {
    JOIN_CRIT = c("POS", "split")
  }
  
  opt_team <- prepRatings(test_ratings)
  needs <- positionalNeeds(test_ratings, level = LEVEL) %>%
    filter(POS != 'DH')
  
  remaining_PA <- sum(needs$POS_PA)
  print_count <- 10
  
  while (sum(needs$POS_PA) > 0) {
    if (print_count == 0) {
      print(sum(needs$POS_PA))
      print_count <- 10
    }
    next_player <- getNextPlayer(opt_team, needs, JOIN_CRIT)
    #print(next_player)
    opt_team <- updateOptTeam(opt_team, next_player)
    needs <- updateNeeds(needs, next_player, JOIN_CRIT)
    if (sum(needs$POS_PA) == remaining_PA) {
      break
    }
    remaining_PA <- sum(needs$POS_PA)
    print_count <- print_count - 1
  }
  
  # Now finish off the optimization with DH ---------------------------------
  
  player_pt <- getPlayerAssignedPT(opt_team)
  
  dh_opt <- prepDHRatings(test_ratings, player_pt)
  
  dh_needs <- positionalNeeds(test_ratings, level = LEVEL) %>%
    filter(POS == 'DH')
  
  while (sum(dh_needs$POS_PA) > 0) {
    print(sum(dh_needs$POS_PA))
    next_player <- getNextPlayer(dh_opt, dh_needs, JOIN_CRIT)
    #print(next_player)
    dh_opt <- updateOptTeam(dh_opt, next_player)
    dh_needs <- updateNeeds(dh_needs, next_player, JOIN_CRIT)
  }
  
  total_opt_team <- opt_team %>%
    bind_rows(dh_opt) %>%
    distinct()
}