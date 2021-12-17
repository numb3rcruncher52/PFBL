##############################################################
# cleanFieldingData.R
#
# Load in batter ratings info from a particular season,
# calculate, and return fielding value
##############################################################

library(tidyverse)

source("./DMBreportLoad.R")

cleanFieldingData <- function(directory, season) {
  
  batter_ratings <- readBatterRatings(directory, season)
  pitcher_ratings <- readPitcherRatings(directory, season)
  
  fieldingValue <- function(batter_ratings) {
    
    ## Reshape player ratings so we have a record for every player,position-rating
    player_pos_ratings <- batter_ratings %>%
      select(ID, Name, season, c:C) %>%
      gather(POS, Range, c:rf) %>%
      filter(Range != "") %>%
      separate(Range, c("Range", "Err"), convert = TRUE) %>%
      mutate(POS = toupper(POS), 
             RANGE = toupper(Range),
             Arm = ifelse(POS=='C',C,ifelse(POS %in% c('LF','CF','RF'),OF,NA))) %>%
      select(-Range, -OF, -C)
    
    ## Use linear defensive model to predict defensive value
    raa_values <- predict(def_model, newdata = player_pos_ratings)
    
    fielding <- player_pos_ratings %>%
      mutate(RAA = raa_values) %>%
      left_join(coef_arm) %>%
      mutate(RAA_throw = ifelse(is.na(RAA_throw),0,RAA_throw),
             RAA = RAA + 
               RAA_throw,
             RAA_PA = RAA / TOTAL_PA_FULL
             , rated = "YES") %>%
      select(ID, Name, season, POS, rated, RANGE, Err, Arm, RAA, RAA_PA) %>%
      arrange(ID, Name, season, POS, rated, desc(RAA)) %>%
      distinct(ID, Name, season, POS, .keep_all = TRUE) %>%
      filter(!is.na(POS)) %>%
      group_by(ID, season) %>%
      arrange(desc(RAA)) %>%
      mutate(pos_rank = row_number())
    
    return(fielding)
  }
  
  fielding <- fieldingValue(batter_ratings)
  
  final_batters <- batter_ratings %>%
    mutate(Run = toupper(Run)
           , Stl = toupper(Stl)
           , Jmp = toupper(Jmp)
           , Games = as.integer(G)) %>%
    left_join(coef_baserunning, by = c("Run" = "Run rating")) %>%
    select(ID
           , Name
           , season
           , handedness = Bats
           , birth_date = Birth
           , Run
           , Stl
           , Jmp
           , Games
           , run_raa = RAA)
  
  final_pitchers <- pitcher_ratings %>%
    mutate(max_GS = pmin(round(GS * 1.33,0), 37)) %>%
    filter(!is.na(INN),
           INN >= 20) %>%
    select(ID
           , Name
           , season
           , handedness = Throws
           , birth_date = Birth
           , P
           , Games = G
           , Starts = GS
           , max_GS
           , INN)
  
  final_players <- final_batters %>%
    bind_rows(final_pitchers) %>%
    left_join(fielding, by = c("ID", "Name", "season")) %>%
    #left_join(rookie_seasons, by = "ID") %>%
    mutate(POS = ifelse(is.na(POS) ## They have pitching stats or are a DH
                        , ifelse(is.na(INN) ## No innings pitched means DH
                                 , "DH"
                                 , ifelse(P == 'dh'
                                          , ifelse(Starts > 0, 'sp', 'mr'), P))
                        , POS)
           , pos_rank = ifelse(is.na(pos_rank), 1, pos_rank)) %>%
    select(-P)
}

