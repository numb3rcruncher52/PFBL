##############################################################
# Calculate_Player_Stats.R
#
# Given a dataframe of raw batter/pitcher splits, calculate
# the corresponding wOBA and wRAA values
##############################################################


calcPlayerStats <- function(stats) {
  
  stats %>%
    mutate(Season = SEASON) %>%
    left_join(seasonal_constants) %>%
    mutate(PA = calcPA(AB, UBB, HBP, SF),
           wOBA = calcWOBA(PA, SNG, DBL, TRI, HR, UBB, HBP, 
                           wBB, wHBP, w1B, w2B, w3B, wHR),
           wRAA = calcWRAA(role, wOBA, lg_wOBA, wOBAScale, 1)) %>%
    select(ID, Name, role, Season, split, OPS, PA, wOBA, wRAA) %>%
    gather(stat_name,stat_value,OPS:wRAA) %>%
    mutate(stat_name = paste(split,"_",stat_name,sep="")) %>%
    select(-split) %>%
    spread(stat_name, stat_value)
}

