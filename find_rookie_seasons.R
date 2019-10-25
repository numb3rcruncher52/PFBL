##############################################################
# find_rookie_season.R
#
# Use data from DMB to determine the first season that a 
# player hit the 20 INN/30 PA threshold
##############################################################

getRookieSeasons <- function(pitcher_ratings, stats) {
  pitcher_rookies <- pitcher_ratings %>%
    filter(INN >= 20) %>%
    group_by(ID) %>%
    summarise(rookie_season = min(season))
  
  batter_rookies <- stats %>%
    filter(role == 'batter') %>%
    mutate(PA = calcPA(AB, UBB, HBP, SF)) %>%
    group_by(ID, Name, season) %>%
    summarise(total_PA = sum(PA)) %>%
    filter(total_PA >= 30) %>%
    group_by(ID) %>%
    summarise(rookie_season = min(season))
  
  rookie_seasons <- pitcher_rookies %>%
    bind_rows(batter_rookies)
  
  return(rookie_seasons)
}
