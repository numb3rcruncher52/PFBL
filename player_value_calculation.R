source("DMBreportLoad.R")
source("BaseballCoefficientsLoad.R")
source("functions.R")

rosters <- readRosterStatus()
stats <- readPlayerStats()
batter_ratings <- readBatterRatings()
pitcher_ratings <- readPitcherRatings()

stats2 <- stats %>%
  mutate(Season = 2016) %>%
  add_PA(AB, UBB, HBP, SF) %>%
  add_wOBA(coef_wOBA)

batters2 <- batters %>% mutate(wOBA = calcWOBA(SEASON,AB,X1B,X2B,X3B,HR,UBB,HBP,SF), OPS = OBP + SPC) %>% 
  select(Name,Team,P,Age,Split,PA,AVG,OBP,SPC,OPS,wOBA) %>% 
  gather(stat_name,stat_value,PA:wOBA) %>% 
  mutate(Category = paste(Split,"_",stat_name,sep="")) %>% 
  select(-Split,-stat_name) %>% 
  spread(Category,stat_value) %>% 
  mutate(LHP_wRAA = calcWRAA(LHP_wOBA, LG_WOBA, WOBA_SCALE, 1), RHP_wRAA = calcWRAA(RHP_wOBA, LG_WOBA, WOBA_SCALE, 1),totPA = LHP_PA + RHP_PA) %>% 
  mutate(minPA=ifelse(totPA <200,0,0.7*totPA), maxPAvsL=ifelse(totPA >= 502 | LHP_OPS <= .65,160,pmin(LHP_PA*1.33,160)), maxPAvsR=ifelse(totPA >= 502 | RHP_OPS <= .65, 545, pmin(RHP_PA*1.33,545))) %>% 
  mutate(LHP_Runs = maxPAvsL*LHP_wRAA, RHP_Runs = maxPAvsR*RHP_wRAA, Bat_Runs = LHP_Runs + RHP_Runs) %>% 
  arrange(-Bat_Runs)
