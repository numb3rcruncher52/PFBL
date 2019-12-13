##############################################################
# cleanStats.R
#
# Load in batter & pitcher stats from a particular season
# (either real life or simulated) and return wOBA/wRAA values
##############################################################

library(tidyverse)

source("./DMBreportLoad.R")

LH_PA_FULL <- 179
RH_PA_FULL <- 510
TOTAL_PA_FULL <- LH_PA_FULL + RH_PA_FULL
PA_INN <- 4.277
INN_START_MAX <- 7
PITCH_LH_SPLIT <- 0.4357984

cleanPlayerStats <- function(directory, season, type = 'Profile') {
  
  stats <- readPlayerStats(directory, season, type)
  
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

cleanSeasonResults <- function(directory, season) {
  
  batter_results <-readBatterSeasonResults(directory, season)
  pitcher_results <- readPitcherSeasonResults(directory, season)
  
  all_results <- batter_results %>%
    bind_rows(pitcher_results)
  
  return(all_results)
}

calcPlaytimeLimits <- function(ratings) {
  ratings %>%
    mutate(min_PA = ifelse(role == "batter"
                           ,ifelse(total_PA < 200, 0, round(0.7*PA,0))
                           , ifelse(INN < 50, 0, round(0.7*PA,0)))
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
                             , round(ifelse(split == 'LH'
                                            , ifelse(INN >= 162
                                                     , max_GS * INN_START_MAX * PITCH_LH_SPLIT * PA_INN
                                                     , PITCH_LH_SPLIT * total_PA * 1.33
                                            )
                                            , ifelse(INN >= 162
                                                     , max_GS * INN_START_MAX * (1 - PITCH_LH_SPLIT) * PA_INN
                                                     , (1 - PITCH_LH_SPLIT) * total_PA * 1.33
                                            ))))
           , min_INN = round(ifelse(role == "batter", NA, min_PA / PA_INN),0)
           , max_INN = round(ifelse(role == "batter", NA, max_PA / PA_INN),0)
    )
}