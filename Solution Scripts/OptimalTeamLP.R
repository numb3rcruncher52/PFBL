## Optimal usage test

library(lpSolve)

optimalBattersLP <- function(ratings) {
  
  test_rating <- ratings %>% 
    filter(!(Conference == 'NL' & POS == 'DH')) %>% ## remove DH from NL
    select(ID, Name, role, POS, split, RAA_PA, wRAA, min_PA, max_PA, value_PA) %>%
    ## Ensure all values are non-negative for valid optimization
    mutate(value_PA_pos = value_PA - min(value_PA))
  
  POS <- unique(test_rating$POS)
  SPLITS <- c("LH", "RH")
  
  pos_limits <- expand_grid(POS, SPLITS)
  
  # a = coefficient vector of obj function, run values/PA at each position 
  #     for every player and handedness, length n
  a <- test_rating$value_PA_pos
  # A1 = An m1 by n matrix of coefficients for the <= type of constraints.
  A1 <- matrix(nrow = nrow(test_rating), ncol = nrow(test_rating))
  for (i in 1:nrow(A1)) {
    A1[i,] <- test_rating$Name == test_rating$Name[i] & test_rating$split == test_rating$split[i]
  }
  # b1 = less-than-or-equal constraint; maximum PA for each player/position
  b1 <- test_rating$max_PA
  # A2 = An m2 by n matrix of coefficients for the >= type of constraints.
  # b2 = greater-than-or-equal constraint; minimum PA for each player/position
  b2 <- test_rating$min_PA
  # A3 = m3 x n matrix of positions & 
  A3 <- matrix(nrow = nrow(pos_limits), ncol = nrow(test_rating))
  for (i in 1:nrow(A3)) {
    A3[i,] <- (test_rating$POS == pos_limits$POS[i]) & (test_rating$split == pos_limits$SPLITS[i])
  }
  # b3 = equality constraint; total PA at each overall position
  b3 <- map_dbl(pos_limits$SPLITS, ~ ifelse(.x == 'LH', LH_PA_FULL, RH_PA_FULL))
  
  
  l <- lp (direction = "max"
           , objective.in = a
           , const.mat = rbind(A1, A3)
           , const.dir = rep("<=",nrow(A1)) %>% append(rep("<=",nrow(A3)))
           , const.rhs = append(b1,b3)
  )
  
  print(l$objval)
  
  test_rating <- test_rating %>%
    mutate(opt_pa = round(l$solution,0)) %>%
    select(ID, Name, role, POS, split, opt_pa)
}

optimalPitchers <- function(ratings) {
  ## Define Needs
  STARTS_NEEDED <- 162
  RELIEF_IP_NEEDED <- 10 / 3 * 162
  
  ## first collapse pitchers down to calculate value/start & per inning
  pitcher_value <- ratings %>%
    select(ID, Name, POS, max_PA, max_GS, split, value_INN, value_GS, PA_PER_START, PA_PER_INN) %>%
    group_by(ID, Name, POS) %>%
    summarise(max_GS = min(max_GS)
              , max_PA = min(max_PA)
              , value_GS = sum(value_GS)
              , value_INN = sum(value_INN)
              , PA_PER_START = min(PA_PER_START)
              , PA_PER_INN = min(PA_PER_INN)) %>%
    ungroup()

  opt_starts <- pitcher_value %>%
    filter(max_GS > 0) %>%
    arrange(-value_GS) %>%
    mutate(cum_starts = cumsum(max_GS)
           , opt_starts = ifelse(cum_starts <= STARTS_NEEDED, max_GS
                                 , pmax(STARTS_NEEDED - lag(cum_starts),0))
           , PA_used = opt_starts * PA_PER_START) %>%
    select(ID, opt_starts, PA_used)
  
  opt_relief <- pitcher_value %>%
    filter(POS != 'SP') %>%
    left_join(opt_starts, by = 'ID') %>%
    arrange(-value_INN) %>%
    mutate(PA_remaining = pmax(0, max_PA - ifelse(is.na(PA_used),0,PA_used))
           , INN_remaining = PA_remaining / PA_PER_INN
           , cum_inn = cumsum(INN_remaining)
           , opt_inn = ifelse(cum_inn <= RELIEF_IP_NEEDED, INN_remaining
                              , pmax(RELIEF_IP_NEEDED - lag(cum_inn),0))) %>%
    select(ID, opt_inn)
  
  optimal_pitchers <- pitcher_value %>%
    left_join(opt_starts, by = 'ID') %>%
    left_join(opt_relief, by = 'ID') %>%
    select(ID, Name, POS, opt_starts, opt_inn) %>%
    mutate(opt_starts = ifelse(is.na(opt_starts),0,opt_starts)
           , opt_inn = ifelse(is.na(opt_inn),0,opt_inn))
}

optimalTeam <- function(ratings) {
  ## 4 separate problems to solve: 
  # Optimal batting lineup 
  # Assigning DH PA
  # Games Started
  # Relief Innings
  
  optimal_batters <- ratings %>%
    filter(role == 'batter') %>%
    nest_by(TeamName, season) %>%
    mutate(opt_team = list(optimalBattersLP(data))) %>%
    unnest(opt_team) %>%
    select(-data)
  
  optimal_pitchers <- ratings %>%
    filter(role == 'pitcher') %>%
    nest_by(TeamName, season) %>%
    mutate(opt_team = list(optimalPitchers(data))) %>%
    unnest(opt_team) %>%
    select(-data)
  
  final_ratings <- ratings %>%
    left_join(optimal_batters, by = c("TeamName", "season", "ID", "Name", "role", "POS", "split" )) %>%
    left_join(optimal_pitchers, by = c("TeamName", "season", "ID", "Name", "POS")) %>%
    distinct()
  
}


# tr <- all_ratings %>% filter(season == 2022, TeamName == 'Frisco Beer Snobs', role == 'batter')

# 
# teams <- unique(dim_team$TeamName[dim_team$Season == LATEST_SEASON])
# 
# test_ratings <- all_ratings %>%
#   filter(season == max(all_ratings$season))
# 
# fbs <- test_ratings %>% filter(TeamName == 'Frisco Beer Snobs')
# 
# opt <- map_df(teams, optimalTeamLP, ratings = test_ratings) %>% bind_rows()
# 
# write_csv(opt_all, "OUTPUT_NEW/real_optimal_value_2022.csv")
# 
# result <- opt %>% group_by(TeamName, POS) %>% summarise(value_opt = sum(opt_pa * value_PA))
# result_all <- opt_all  %>% group_by(TeamName, POS) %>% summarise(value_opt_all = sum(opt_pa * value_PA))
# 
# view(result %>% left_join(result_all))
