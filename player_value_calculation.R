source("DMBreportLoad.R")
source("BaseballCoefficientsLoad.R")
source("functions.R")

directory <- "C:\\dmb11\\PFBL 2017\\reports\\"
roster_directory <- "C:\\dmb11\\PFBL 2017\\reports\\"

SEASON <- 2016

## The following benchmarks are calculated from 2016 Total Reports
LH_PA_FULL <- 179
RH_PA_FULL <- 510
TOTAL_PA_FULL <- LH_PA_FULL + RH_PA_FULL
PA_INN <- 4.277
PITCH_LH_SPLIT <- 0.4357984 

# Load data for season in question ----------------------------------------

rosters <- readRosterStatus(roster_directory, SEASON)
stats <- readPlayerStats(directory, SEASON, 'Profile')
batter_ratings <- readBatterRatings(directory, SEASON)
pitcher_ratings <- readPitcherRatings(directory, SEASON)


# Get Fielding Value ------------------------------------------------------

## All below fielding value is full a full game schedule for any particular
## player. It also represents a specific era where a baseline team would
## score 782 runs in 6382 PA
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
           RAA = RAA_low + (Err - Err_low)*(RAA_high - RAA_low)/25 + 
             RAA_throw,
           RAA_PA = RAA / TOTAL_PA_FULL) %>%
    select(ID, Name, POS, RANGE, Err, Arm, RAA, RAA_PA)
  
  return(fielding)
}

fielding <- fieldingValue(batter_ratings)

# fielding_join <- fielding %>%
#   select(ID, Name, POS, RAA) %>%
#   mutate(RAA_PA = RAA / TOTAL_PA_FULL)


# Calculate Batting Value -------------------------------------------------

runValue <- function(stats) {
  stats %>%
    left_join(seasonal_constants) %>%
    mutate(PA = calcPA(AB, UBB, HBP, SF),
           wOBA = calcWOBA(PA, SNG, DBL, TRI, HR, UBB, HBP, 
                           wBB, wHBP, w1B, w2B, w3B, wHR),
           wRAA = calcWRAA(role, wOBA, lg_wOBA, wOBAScale, 1)) %>%
    select(ID, Name, role, Season, split, OPS, PA, wOBA, wRAA)
}


stats_final <- stats %>%
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

## Need to put all these on a common scale of one of the following:
# PA
# G
# INN


batters <- stats2 %>%
  filter(role == "batter") %>%
  spread(stat_name,stat_value) %>%
  left_join(coef_wOBA) %>%
  mutate(total_PA = LHP_PA + RHP_PA,
         min_PA=ifelse(total_PA < 200, 0, 0.7 * total_PA), 
         maxPAvsL=ifelse(total_PA >= 502 | 
                           LHP_OPS <= .650,LH_PA_FULL,pmin(LHP_PA*1.33,LH_PA_FULL)), 
         maxPAvsR=ifelse(total_PA >= 502 | 
                           RHP_OPS <= .650, RH_PA_FULL, pmin(RHP_PA*1.33,RH_PA_FULL)),
         max_PA = maxPAvsR + maxPAvsL,
         LHP_wRAA = calcWRAA(role, LHP_wOBA, lg_wOBA, wOBAScale, 1), 
         RHP_wRAA = calcWRAA(role, RHP_wOBA, lg_wOBA, wOBAScale, 1))


# Combine for Overall Value -----------------------------------------------

batters_full <- batters %>%
  left_join(fielding_join) %>%
  left_join(rosters) %>%
  mutate(LHP_Runs = calcRuns(maxPAvsL, LHP_wRAA, RAA_PA), 
         RHP_Runs = calcRuns(maxPAvsR, RHP_wRAA, RAA_PA), 
         Bat_Runs = LHP_Runs + RHP_Runs) %>%
  select(ID, Name, TeamName, Season, POS, total_PA, min_PA, maxPAvsL, maxPAvsR,
         LHP_wOBA, RHP_wOBA, RAA, RAA_PA, LHP_Runs, RHP_Runs, Bat_Runs) %>%
  arrange(desc(Bat_Runs))

write_csv(batters_full, "batters_2016.csv")
# 
# batters_full <- batters %>%
#   select(ID, Name, min_PA, max_PA) %>%
#   right_join(fielding_join) %>%
#   mutate(RAA_min = min_PA / (LH_PA_FULL + RH_PA_FULL) * RAA,
#          RAA_max = max_PA / (LH_PA_FULL + RH_PA_FULL) * RAA)
#   left_join(fielding_join) %>%
#   muta
# 
# pitchers <- stats2 %>%
#   filter(split %in% c("LHB", "RHB")) %>%
#   select(-split) %>%
#   spread(Category,stat_value)
# 


