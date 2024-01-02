##############################################################
# CalculateValue.R
#
# Given player ratings, calculate offensive/defensive run values
##############################################################

library(tidyverse)

cleanPlayerRatings <- function(batter_ratings, pitcher_ratings) {
  cleanBatterRatings <- function(batter_ratings) {
    
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
    
    fielding <- fieldingValue(batter_ratings) %>% 
      bind_rows(batter_ratings %>% distinct(ID, Name, season) %>% mutate(POS = 'DH', pos_rank = 1)) %>%
      group_by(ID, Name, season) %>%
      mutate(pos_rank = ifelse(POS == 'DH', max(pos_rank, na.rm=TRUE) + 1, pos_rank))
    
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
             , run_raa = RAA) %>%
      left_join(fielding, by = c("ID", "Name", "season")) %>%
      mutate(POS = ifelse(is.na(POS), "DH", POS)
             , pos_rank = ifelse(is.na(pos_rank), 1, pos_rank)
             , RAA = ifelse(is.na(RAA),0,RAA)
             , RAA_PA = ifelse(is.na(RAA_PA),0,RAA_PA)
             , role = 'batter')
  }
  
  cleanPitcherRatings <- function(pitcher_ratings) {
    starters <- pitcher_ratings %>%
      filter(!is.na(INN),
             INN >= 20,
             GS > 0) %>%
      mutate(max_GS = pmin(round(GS * 1.33,0), 36)
             , POS = 'SP'
             , role = 'pitcher')
    
    relievers <- pitcher_ratings %>%
      filter(!is.na(INN),
             INN >= 20, 
             G != GS) %>%
      mutate(GS = 0
             , max_GS = 0
             , POS = 'RP'
             , role = 'pitcher')
      
    final_pitchers <- starters %>%
      bind_rows(relievers) %>%
      select(ID
             , Name
             , season
             , handedness = Throws
             , birth_date = Birth
             , POS
             , Games = G
             , Starts = GS
             , max_GS
             , INN
             , role) %>%
      group_by(ID, season) %>%
      arrange(desc(POS)) %>%
      mutate(pos_rank = row_number())
  }
  
  cleanPitcherRatings_old <- function(pitcher_ratings) {
    final_pitchers <- pitcher_ratings %>%
      mutate(max_GS = pmin(round(GS * 1.33,0), 36)
             , POS = ifelse(G == GS, 'SP', ifelse(GS == 0, 'RP', 'SW'))
             , role = 'pitcher') %>%
      filter(!is.na(INN),
             INN >= 20) %>%
      select(ID
             , Name
             , season
             , handedness = Throws
             , birth_date = Birth
             , POS
             , Games = G
             , Starts = GS
             , max_GS
             , INN
             , role)
  }
  
  batters <- cleanBatterRatings(batter_ratings)
  pitchers <- cleanPitcherRatings(pitcher_ratings)
  
  players <- batters %>%
    bind_rows(pitchers) %>%
    mutate(pos_rank = ifelse(is.na(pos_rank), 1, pos_rank))
}

cleanPlayerStats <- function(stats) {
  
  calcPA <- function(AB, UBB, HBP, SF) {
    ## Calculates PA based on individual components
    AB + UBB + HBP + SF
  }
  
  calcWOBA <- function(PA,SNG,DBL,TRI,HR,UBB,HBP,wBB,wHBP,w1B,w2B,w3B,wHR) {
    ## Takes counting statistics and league run values and returns a wOBA value
    
    wOBA <- ifelse(PA == 0, 
                   0, 
                   (wBB*UBB + wHBP*HBP + w1B*SNG + w2B*DBL + w3B*TRI + wHR*HR)/PA)
    round(wOBA,3)
  }
  
  calcWRAA <- function(role, wOBA, lg_wOBA, scale, PA) {
    wRAA = (wOBA - lg_wOBA)/scale*PA
    
    ifelse(role == "batter", wRAA, -wRAA)
  }
  
  calcPlayerStatsSplits <- function(stats) {
    stats %>%
      left_join(coef_wOBA, by = c('season'='Season')) %>%
      mutate(PA = calcPA(AB, UBB, HBP, SF),
             wOBA = calcWOBA(PA, SNG, DBL, TRI, HR, UBB, HBP, 
                             wBB, wHBP, w1B, w2B, w3B, wHR),
             wRAA = calcWRAA(role, wOBA, lg_wOBA, wOBAScale, 1)) %>%
      select(ID:season, PA, wOBA, wRAA) %>%
      group_by(ID, Name, season) %>%
      mutate(total_PA = sum(PA)) %>%
      ungroup()
  }
  
  return(calcPlayerStatsSplits(stats))
}

calcPlaytimeLimits <- function(ratings) {
  ratings %>%
    calcValue() %>%
    mutate(min_INN = round(ifelse(role == "batter", NA, INN * 0.7),0)
           , max_INN = round(ifelse(role == "batter", NA, INN * 1.33),0)
           , min_PA = ifelse(role == "batter"
                           ,ifelse(total_PA < 200, 0, round(0.7*PA,0))
                           , ifelse(INN < 50, 0, round(min_INN*PA_PER_INN,0)))
           , max_PA = ifelse(role == "batter"
                             ,round(ifelse(split == 'LH'
                                           , ifelse(total_PA >= 502 | 
                                                      OPS < .650
                                                    , LH_PA_FULL
                                                    , pmin(PA*1.33,LH_PA_FULL))
                                           , ifelse(total_PA >= 502 | 
                                                      OPS < .650
                                                    , RH_PA_FULL
                                                    , pmin(PA*1.33, RH_PA_FULL))),0)
                             ## Otherwise it's a pitcher
                             , round(ifelse(POS == 'SP'
                                            , max_GS * PA_PER_START
                                            , max_INN * PA_PER_INN
                                            )
                                            ))
           
    )
}

calcValue <- function(ratings) {
  ratings %>%
    left_join(coef_lh_perc, by = 'handedness') %>%
    mutate(value_PA = RAA_PA + wRAA + run_raa / TOTAL_PA_FULL
           , PA_PER_INN = total_PA / INN
           , PA_PER_START = ifelse(Starts == 0, NA,
                                   ifelse(POS == 'SW', INN_PER_START, INN / Starts) * PA_PER_INN)
           , lh_perc_RP = ifelse(split == 'LH', lh_perc_RP, 1 - lh_perc_RP)
           , lh_perc_SP = ifelse(split == 'LH', lh_perc_SP, 1 - lh_perc_SP)
           , value_INN = PA_PER_INN * lh_perc_RP * wRAA
           , value_GS = PA_PER_START * lh_perc_SP * wRAA) %>%
    select(-lh_perc_RP, -lh_perc_SP)
}