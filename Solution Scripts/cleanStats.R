##############################################################
# cleanStats.R
#
# Load in batter & pitcher stats from a particular season
# (either real life or simulated) and return wOBA/wRAA values
##############################################################

library(tidyverse)

source("./DMBreportLoad.R")

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
      select(ID:season, PA, wOBA, wRAA)
  }
  
  return(calcPlayerStatsSplits(stats))
}