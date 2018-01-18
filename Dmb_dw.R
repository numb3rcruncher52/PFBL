##############################################################
# DMB_dw.R
#
# Take clean Diamond mind data and produce a data warehouse
# for analysis and reporting
##############################################################

source("DMB_data_clean.R")

dim_player <- batter_ratings %>%
  select(ID, Name, Birth) %>%
  bind_rows(select(pitcher_ratings, ID, Name, Birth)) %>%
  distinct()

pitchers <- stats_final %>%
  filter(role == "pitcher") %>%
  left_join(pitcher_ratings, by = c("ID", "Name", "season")) %>%
  filter(!is.na(INN),
         INN >= 20) %>%
  mutate(wRAA_INN = (PITCH_LH_SPLIT * LH_wRAA + 
                       (1 - PITCH_LH_SPLIT) * RH_wRAA) * PA_INN,
         min_INN = ifelse(INN >= 50, 0.7 * INN, 0),
         max_GS = pmin(round(GS * 1.33,0), 37),
         max_INN = ifelse(INN >= 162, max_GS * INN_START_MAX, INN * 1.33),
         realistic_GS = ifelse(max_GS * INN_START_MAX > max_INN, 
                               round(max_INN / INN_START_MAX, 0),
                               max_GS),
         value = round(max_INN * wRAA_INN,2)) %>%
  arrange(desc(value)) %>%
  select(ID
         , Name
         , season
         , POS = P
         , bats_pitches = Throws
         , birth_date = Birth
         , LH_wOBA
         , LH_wRAA
         , RH_wOBA
         , RH_wRAA
         , wRAA_INN
         , INN
         , min_INN
         , max_INN
         , max_GS
         , realistic_GS
         , value)

batters <- stats_final %>%
  filter(role == "batter") %>%
  left_join(batter_ratings, by = c("ID", "Name", "season")) %>%
#  mutate(Run = toupper(Run)) %>%
#  left_join(coef_baserunning, by = c("Run" = "Run rating")) %>%
  mutate(total_PA = LH_PA + RH_PA,
         min_PA=ifelse(total_PA < 200, 0, 0.7 * total_PA), 
         maxPAvsL=round(ifelse(total_PA >= 502 | 
                                 LH_OPS < .650,LH_PA_FULL,pmin(LH_PA*1.33,LH_PA_FULL)),0), 
         maxPAvsR=round(ifelse(total_PA >= 502 | 
                                 RH_OPS < .650, RH_PA_FULL, pmin(RH_PA*1.33,RH_PA_FULL)),0),
         max_PA = maxPAvsR + maxPAvsL,
         dh_LH_value = round(maxPAvsL * LH_wRAA,2),
         dh_RH_value = round(maxPAvsR * RH_wRAA,2),
         dh_value = dh_LH_value + dh_RH_value) %>%
  arrange(desc(dh_value)) %>%
  select(ID
         , Name
         , season
         , bats_pitches = Bats
         , birth_date = Birth
         , LH_wOBA
         , LH_wRAA
         , RH_wOBA
         , RH_wRAA
         , Games = G
         , min_PA
         , maxPAvsL
         , maxPAvsR
         , dh_LH_value
         , dh_RH_value
         , dh_value)

fielding_value <- batters %>%
  right_join(fielding) %>%
  mutate(LH_value = RAA_PA * maxPAvsL + dh_LH_value,
         RH_value = RAA_PA * maxPAvsR + dh_RH_value,
         value = LH_value + RH_value) %>%
  group_by(ID, Name)

best_position <- fielding_value %>%
  group_by(ID, season) %>%
  top_n(value, n = 1) %>%
  select(ID
         , season
         , POS
         , RANGE
         , Err
         , Arm
         , RAA
         , rated
         , LH_value = dh_LH_value
         , RH_value = dh_RH_value
         , value = dh_value)


final_values <- batters %>%
  left_join(best_position, by = c("ID", "season")) %>%
  bind_rows(pitchers) %>%
  mutate(POS = ifelse(is.na(POS), "DH", POS)
         , value = ifelse(is.na(value), dh_value, value)) %>%
  arrange(desc(value))


