source("DMBreportLoad.R")
source("BaseballCoefficientsLoad.R")
source("functions.R")

directory <- "C:\\dmb11\\sd2016_v11\\reports\\"
roster_directory <- "C:\\dmb11\\PFBL 2016\\reports\\"
SEASON <- 2016

rosters <- readRosterStatus(roster_directory)
stats <- readPlayerStats(directory)
batter_ratings <- readBatterRatings(directory)
pitcher_ratings <- readPitcherRatings(directory)

fieldingValue <- function(batter_ratings) {
  fielding <- batter_ratings %>%
    select(ID, Name, c:C) %>%
    gather(POS, Range, c:rf) %>%
    filter(Range != "") %>%
    separate(Range, c("Range", "Err"), convert = TRUE) %>%
    mutate(POS = toupper(POS), 
           RANGE = toupper(Range),
           Arm = ifelse(POS=='C',C,ifelse(POS %in% c('LF','CF','RF'),OF,NA))) %>%
    select(-Range, -OF, -C)
  
  ### Calculate actual fielding run values
  fielding <- fielding %>%
    mutate(Err_low = pmin(plyr::round_any(Err, 25, f = floor),175),
           Err_high = pmin(plyr::round_any(Err, 25, f = ceiling),200)) %>%
    left_join(coef_def, 
              by = c("POS"="POS", "RANGE"="RANGE", "Err_low"="Err")) %>%
    left_join(coef_def, 
              by = c("POS"="POS", "RANGE"="RANGE", "Err_high"="Err")) %>%
    rename(RAA_low = RAA.x, RAA_high = RAA.y) %>%
    left_join(coef_arm) %>%
    mutate(RAA_throw = ifelse(is.na(RAA_throw),0,RAA_throw),
           RAA = RAA_low + (Err - Err_low)*(RAA_high - RAA_low)/25 + RAA_throw)
  return(fielding)
}

fielding <- fieldingValue(batter_ratings)

stats2 <- stats %>%
  mutate(Season = SEASON) %>%
  left_join(seasonal_constants) %>%
  mutate(PA = calcPA(AB, UBB, HBP, SF),
         wOBA = calcWOBA(PA, SNG, DBL, TRI, HR, UBB, HBP, 
                         wBB, wHBP, w1B, w2B, w3B, wHR)) %>%
  select(ID:OPS, split:Season, PA, wOBA)

  # add_wOBA(coef_wOBA) %>%
  # gather(stat_name,stat_value,AVG:wRAA) %>% 
  # mutate(Category = paste(split,"_",stat_name,sep="")) %>% 
  # select(-stat_name)

batters <- stats2 %>%
  filter(split %in% c("LHP", "RHP")) %>%
  select(-split) %>%
  spread(Category,stat_value)

pitchers <- stats2 %>%
  filter(split %in% c("LHB", "RHB")) %>%
  select(-split) %>%
  spread(Category,stat_value)


batters2 <- batters %>% 
  mutate(LHP_wRAA = calcWRAA(LHP_wOBA, LG_WOBA, WOBA_SCALE, 1), RHP_wRAA = calcWRAA(RHP_wOBA, LG_WOBA, WOBA_SCALE, 1),totPA = LHP_PA + RHP_PA) %>% 
  mutate(minPA=ifelse(totPA <200,0,0.7*totPA), maxPAvsL=ifelse(totPA >= 502 | LHP_OPS <= .65,160,pmin(LHP_PA*1.33,160)), maxPAvsR=ifelse(totPA >= 502 | RHP_OPS <= .65, 545, pmin(RHP_PA*1.33,545))) %>% 
  mutate(LHP_Runs = maxPAvsL*LHP_wRAA, RHP_Runs = maxPAvsR*RHP_wRAA, Bat_Runs = LHP_Runs + RHP_Runs) %>% 
  arrange(-Bat_Runs)
