
source("DMB_data_clean.R")

# Combine results with rosters --------------------------------------------

rookies_batters <- stats %>%
  mutate(PA = calcPA(AB, UBB, HBP, SF)) %>%
  filter(role == "batter") %>%
  group_by(ID, Name, season) %>%
  summarise(PA = sum(PA)) %>%
  filter(PA >= 30) %>%
  distinct(ID, Name, .keep_all = TRUE) %>%
  filter(season == LATEST_SEASON)
## need to combine splits before calculating

rookies_pitchers <- pitcher_ratings %>%
  filter(INN >= 20) %>%
  distinct(ID, Name, .keep_all = TRUE) %>%
  filter(season == LATEST_SEASON)

rookies <- rookies_batters %>%
  bind_rows(rookies_pitchers) %>%
  select(ID, Name) %>%
  mutate(rookie = "YES")

final_2018 <- results %>%
  filter(season == LATEST_SEASON) %>%
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
         wRAA = calcWRAA(role, wOBA, lg_wOBA, wOBAScale, 1),
         offensive_runs = sum(wRAA * PA)) %>%
  select(ID:SF, PA:offensive_runs) %>%
  left_join(rosters, by = c("ID", "Name", "season")) %>%
  left_join(rookies, by = c("ID", "Name")) %>%
  left_join(select(pitcher_ratings, ID, Name, season, INN), by =c("ID", "Name","season"))
  
write_csv(final_2018, "ballot_data_2018.csv")
