source("DMBreportLoad.R")
source("BaseballCoefficientsLoad.R")
source("Fielding_Value.R")
source("Calculate_Player_Stats.R")
source("functions.R")

directory <- "C:\\dmb11\\PFBL 2017\\reports\\"
roster_directory <- "C:\\dmb11\\PFBL 2017\\reports\\"

SEASON <- 2016

## The following benchmarks are calculated from 2016 Total Reports
LH_PA_FULL <- 179
RH_PA_FULL <- 510
TOTAL_PA_FULL <- LH_PA_FULL + RH_PA_FULL
PA_INN <- 4.277
INN_START_MAX <- 7
PITCH_LH_SPLIT <- 0.4357984 

# Load data for season in question ----------------------------------------

rosters <- readRosterStatus(roster_directory, SEASON)
stats <- readPlayerStats(directory, SEASON, 'Profile')
batter_ratings <- readBatterRatings(directory, SEASON)
pitcher_ratings <- readPitcherRatings(directory, SEASON)


# Get Fielding Value ------------------------------------------------------

fielding <- fieldingValue(batter_ratings)

# Calculate Split Run Values ----------------------------------------------

stats_final <- calcPlayerStats(stats)

## Need to put all these on a common scale of one of the following:
# PA
# G
# INN


# Pitcher Value Calculation -----------------------------------------------

pitchers <- stats_final %>%
  filter(role == "pitcher") %>%
  left_join(pitcher_ratings, by = c("ID", "Name", "Season"="season")) %>%
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
  left_join(rosters, by = c("ID", "Name", "Season"="season"))

rotations <- pitchers %>%
  filter(realistic_GS > 0) %>%
  arrange(desc(wRAA_INN)) %>%
  group_by(TeamName) %>%
  mutate(cumulative_GS = cumsum(realistic_GS),
         make_starts = ifelse(cumulative_GS > 162, 
                              pmax(162 - (cumulative_GS - realistic_GS), 0),
                              realistic_GS),
         make_value = round(make_starts * INN_START_MAX * wRAA_INN,2),
         missing_starts = make_starts - realistic_GS)

pitchers_new <- pitchers %>%
  left_join(rotations %>% ungroup %>% select(ID, make_starts, missing_starts, make_value))

rotations_sum <- rotations %>%
  summarise(make_starts = sum(make_starts),
            missing_starts = sum(missing_starts),
            value = sum(make_value)) %>%
  arrange(desc(value))

## Still need ERA to get the accurate INN doubled and such


# Batter/Fielder Evaluation -----------------------------------------------

batters <- stats_final %>%
  filter(role == "batter") %>%
  mutate(total_PA = LH_PA + RH_PA,
         min_PA=ifelse(total_PA < 200, 0, 0.7 * total_PA), 
         maxPAvsL=round(ifelse(total_PA >= 502 | 
                                 LH_OPS <= .650,LH_PA_FULL,pmin(LH_PA*1.33,LH_PA_FULL)),0), 
         maxPAvsR=round(ifelse(total_PA >= 502 | 
                                 RH_OPS <= .650, RH_PA_FULL, pmin(RH_PA*1.33,RH_PA_FULL)),0),
         max_PA = maxPAvsR + maxPAvsL,
         LH_value = round(maxPAvsL * LH_wRAA,2),
         RH_value = round(maxPAvsR * RH_wRAA,2),
         value = LH_value + RH_value) %>%
  arrange(desc(value)) %>%
  left_join(rosters, by = c("ID", "Name", "Season" = "season"))

fielding_value <- batters %>%
  select(ID, Name, maxPAvsL, maxPAvsR, LH_value, RH_value) %>%
  right_join(fielding) %>%
  mutate(LH_value = RAA_PA * maxPAvsL + LH_value,
         RH_value = RAA_PA * maxPAvsR + RH_value,
         total_value = LH_value + RH_value) %>%
  group_by(ID, Name) %>%
  mutate(pos_rank = min_rank(-total_value))

fielding_dh <- batters %>%
  select(ID, Name, maxPAvsL, maxPAvsR, LH_value, RH_value, total_value = value) %>%
  mutate(POS = "DH") %>%
  bind_rows(fielding_value) %>%
  arrange(desc(total_value)) %>%
  group_by(ID, Name) %>%
  left_join(rosters, by = c("ID", "Name")) %>%
  group_by(POS, pos_rank) %>%
  mutate(cum_PA = cumsum(maxPAvsR + maxPAvsL),
         starter_PA = ifelse(cum_PA > (TOTAL_PA_FULL * 28), 
                             pmax(TOTAL_PA_FULL * 28 - (cum_PA - (maxPAvsL + maxPAvsR)), 0),
                             maxPAvsR + maxPAvsL),
         starter_value = ifelse(starter_PA == 0, NA, starter_PA / (maxPAvsR + maxPAvsL) * total_value),
         position_index = ifelse(pos_rank == 1, percent_rank(starter_value), NA))


write_csv(pitchers_new, "pitchers_2017.csv")
write_csv(fielding_dh, "batters_2017.csv")
