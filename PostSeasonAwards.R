##############################################################
# PostSeasonAwards.R
#
# Combine End of Season Results for PostSeason Awards
# - rookie status
# - season_results
# - results
##############################################################

source("DMB_data_clean.R")

all_season_results <- batter_season_results %>%
  bind_rows(pitcher_season_results)

## Roll up results by player id
season_results <- results %>%
  group_by(ID, Name, season, role) %>%
  summarise(AB = sum(AB)
            , SNG = sum(SNG)
            , DBL = sum(DBL)
            , TRI = sum(TRI)
            , HR = sum(HR)
            , UBB = sum(UBB)
            , HBP = sum(HBP)
            , SF = sum(SF)) %>%
  left_join(coef_wOBA, by = c('season'='Season')) %>%
  mutate(PA = calcPA(AB, UBB, HBP, SF),
       wOBA = calcWOBA(PA, SNG, DBL, TRI, HR, UBB, HBP, 
                       wBB, wHBP, w1B, w2B, w3B, wHR),
       wRAA = calcWRAA(role, wOBA, lg_wOBA, wOBAScale, 1)) %>%
  select(-(lg_wOBA:wHR)) %>%
  left_join(rookie_seasons, by = 'ID') %>%
  left_join(all_season_results, by = c('ID', 'Name', 'season')) %>%
  left_join(rosters, by = c('ID', 'Name', 'season')) %>%
  left_join(dim_team, by = c('season' = 'Season', 'TeamName'))

write_csv(season_results, "end_of_season_2018.csv")
