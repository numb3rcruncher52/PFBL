##############################################################
# EvaluateResults.R
#
# Scratchpad for evaluating outcomes in DMB
##############################################################

### After all data is loaded in per New Season:

dmb <- dmb_stats %>% 
  left_join(rosters) %>%
  left_join(dim_team, by = c("Team", "TeamName", "season"="Season"))

## Breakdown of LH/RH pitcher splits by year
pitcher_details <- players %>% 
  filter(POS %in% c('SP', 'RP', 'SW'))

dmb_pitchers <- dmb %>%
  left_join(pitcher_details) 

## Breakdown of LH/RH pitcher splits by year
lh_perc <- dmb_pitchers %>%
  filter(season >= LATEST_SEASON - 4
         , POS %in% c('SP', 'RP')) %>%
  group_by(POS, handedness) %>%
  mutate(LH_PA = ifelse(split == 'LH', PA, 0)
         , RH_PA = ifelse(split == 'RH', PA, 0)) %>%
  summarise(LH_PA = sum(LH_PA)
            , RH_PA = sum(RH_PA)) %>%
  mutate(lh_perc = LH_PA / (LH_PA + RH_PA)) %>%
  select(-LH_PA, -RH_PA) %>%
  pivot_wider(id_cols = 'handedness'
              , names_from = 'POS'
              , values_from = 'lh_perc'
              , names_prefix = 'lh_perc_')

## Innings/Start
INN_PER_START <- pitcher_details %>%
  filter(POS == 'SP', season >= LATEST_SEASON - 4) %>%
  summarise(INN_START = sum(INN) / sum(Starts)) %>%
  pull()

rm(list = c('dmb', 'pitcher_details', 'dmb_pitchers'))

write_csv(lh_perc, "MAPPING_DATA/coef_lh_perc.csv")
