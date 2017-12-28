rm(list=ls())

library(tidyverse)

## The following benchmarks are calculated from 2016 Total Reports
LH_PA_FULL <- 179
RH_PA_FULL <- 510
TOTAL_PA_FULL <- LH_PA_FULL + RH_PA_FULL
PA_INN <- 4.277
INN_START_MAX <- 7
PITCH_LH_SPLIT <- 0.4357984

REPORT_DIR <- "C:\\Users\\mwlyo\\Dropbox\\PFBL\\Reports - DMB\\"
REPORT_DIR <- "C:\\Users\\maxl\\Dropbox (Personal)\\PFBL\\Reports - DMB\\"

# Source Helper functions -------------------------------------------------

source("DMBreportLoad.R")
source("BaseballCoefficientsLoad.R")
source("Fielding_Value.R")
source("Calculate_Player_Stats.R")
source("functions.R")

# Load data for season in question ----------------------------------------

season_folders <- c("reports_2015", "reports_2016", "reports_2017", "reports_2018")
seasons <- seq(2014, 2017, 1)

args2 <- list(directory = paste0(REPORT_DIR,season_folders,"\\"),
              season = seasons)

stats <- args2 %>% pmap(.f = readPlayerStats, type = 'Profile') %>% bind_rows()
batter_ratings <- args2 %>% pmap(readBatterRatings) %>% bind_rows()
pitcher_ratings <- args2 %>% pmap(readPitcherRatings) %>% bind_rows()

# Get Fielding Value ------------------------------------------------------

fielding <- fieldingValue(batter_ratings)

# Calculate Split Run Values ----------------------------------------------

stats_final <- calcPlayerStats(stats)

# Basic Player Value with Playtime Rules ----------------------------------

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
  select(ID, Name, season, POS = P, LH_wOBA, LH_wRAA, RH_wOBA, RH_wRAA,
         wRAA_INN, min_INN, max_INN, max_GS, realistic_GS, value)

batters <- stats_final %>%
  filter(role == "batter") %>%
  mutate(total_PA = LH_PA + RH_PA,
         min_PA=ifelse(total_PA < 200, 0, 0.7 * total_PA), 
         maxPAvsL=round(ifelse(total_PA >= 502 | 
                                 LH_OPS < .650,LH_PA_FULL,pmin(LH_PA*1.33,LH_PA_FULL)),0), 
         maxPAvsR=round(ifelse(total_PA >= 502 | 
                                 RH_OPS < .650, RH_PA_FULL, pmin(RH_PA*1.33,RH_PA_FULL)),0),
         max_PA = maxPAvsR + maxPAvsL,
         LH_value = round(maxPAvsL * LH_wRAA,2),
         RH_value = round(maxPAvsR * RH_wRAA,2),
         value = LH_value + RH_value) %>%
  arrange(desc(value)) %>%
  select(ID, Name, season, LH_wOBA, LH_wRAA, RH_wOBA, RH_wRAA,
         min_PA, maxPAvsL, maxPAvsR, LH_value, RH_value, value)

fielding_value <- batters %>%
  right_join(fielding) %>%
  mutate(LH_value = RAA_PA * maxPAvsL + LH_value,
         RH_value = RAA_PA * maxPAvsR + RH_value,
         value = LH_value + RH_value) %>%
  group_by(ID, Name) %>%
  bind_rows(batters %>% mutate(POS = "DH"))
  


# Write all cleaned data to output folder ---------------------------------

write_csv(pitchers, paste0(REPORT_DIR,"OUTPUTS/pitchers_clean.csv"))
write_csv(batters, paste0(REPORT_DIR,"OUTPUTS/batters_clean.csv"))
write_csv(fielding_value, paste0(REPORT_DIR,"OUTPUTS/fielding_clean.csv"))
write_csv(batter_ratings, paste0(REPORT_DIR,"OUTPUTS/batter_ratings_clean.csv"))
write_csv(pitcher_ratings, paste0(REPORT_DIR,"OUTPUTS/pitcher_ratings_clean.csv"))

## Final master spreadsheet
rosters <- readRosterStatus(directory = paste0(REPORT_DIR,season_folders[length(season_folders)],"\\"),
                            season = seasons[length(seasons)])

write_csv(rosters, paste0(REPORT_DIR,"OUTPUTS/rosters.csv"))

final_pitch <- pitcher_ratings %>%
  select(ID, Name, season, hand = Throws, Birth, INN) %>%
  left_join(pitchers) %>%
  filter(INN >= 20)

final_bat <- batter_ratings %>%
  select(ID, Name, season, hand = Bats, Birth, Run, Stl, Jmp) %>%
  left_join(fielding_value)

final <- final_pitch %>%
  bind_rows(final_bat) %>%
  left_join(rosters)

write_csv(final, "Output/final_values_2017.csv")


# Redefine value based on positional scarcity -----------------------------

POS_PA <- 19190

findStarters <- function(data, position) {
  
  data %>%
    arrange(desc(value)) %>%
    group_by(season, POS) %>%
    mutate(cum_value = cumsum(value),
           cum_pa = cumsum(maxPAvsL + maxPAvsR),
           prev_cum_pa = lag(cum_pa),
           starter = ifelse(is.na(prev_cum_pa) | prev_cum_pa < POS_PA, "YES", "NO")) %>%
    filter(POS == position,
           starter == "YES")
  
  
}

positional_hierarchy <- c('C'
                          ,'SS'
                          ,'CF'
                          ,'3B'
                          , 'RF'
                          , '2B'
                          , 'LF'
                          , '1B'
                          , 'DH'
                          )


removeStarters <- function(all_players, current_starters) {
  
  all_players %>%
    anti_join(current_starters, by = c('ID', 'Name', 'season'))
}


remaining_players <- fielding_value %>% 
  filter(is.na(rated) | rated == "YES")

starter_list <- vector(mode = 'list'
                       , length = length(positional_hierarchy))

for (i in seq_along(positional_hierarchy)) {
  starter_list[[i]] <- findStarters(remaining_players, positional_hierarchy[i])
  
  remaining_players <- removeStarters(remaining_players, starter_list[[i]])
}

starters <- bind_rows(starter_list)

View(
starters %>%
  group_by(season
           , POS) %>%
  summarise(min_value = min(value),
            max_value = max(value),
            total_value = sum(value),
            total_pa = sum(maxPAvsL + maxPAvsR))  %>%
  mutate(avg_value = total_value / total_pa * 685)
)

ss_starters <- findStarters(fielding_value, "SS")

remaining_players <- removeStarters(fielding_value, ss_starters)


## Additional information for evaluating teams
# 
# fielding_dh <- batters %>%
#   mutate(POS = "DH", total_value = value) %>%
#   bind_rows(fielding_value) %>%
#   arrange(desc(total_value)) %>%
#   group_by(ID, Name) %>%
#   left_join(rosters, by = c("ID", "Name")) %>%
#   group_by(POS, pos_rank) %>%
#   mutate(cum_PA = cumsum(maxPAvsR + maxPAvsL),
#          starter_PA = ifelse(cum_PA > (TOTAL_PA_FULL * 28), 
#                              pmax(TOTAL_PA_FULL * 28 - (cum_PA - (maxPAvsL + maxPAvsR)), 0),
#                              maxPAvsR + maxPAvsL),
#          starter_value = ifelse(starter_PA == 0, NA, starter_PA / (maxPAvsR + maxPAvsL) * total_value),
#          position_index = ifelse(pos_rank == 1, percent_rank(starter_value), NA))
# 
# rotations <- pitchers %>%
#   filter(realistic_GS > 0) %>%
#   arrange(desc(wRAA_INN)) %>%
#   group_by(TeamName) %>%
#   mutate(cumulative_GS = cumsum(realistic_GS),
#          make_starts = ifelse(cumulative_GS > 162, 
#                               pmax(162 - (cumulative_GS - realistic_GS), 0),
#                               realistic_GS),
#          make_value = round(make_starts * INN_START_MAX * wRAA_INN,2),
#          missing_starts = make_starts - realistic_GS)
# 
# pitchers_new <- pitchers %>%
#   left_join(rotations %>% ungroup %>% select(ID, make_starts, missing_starts, make_value))
# 
# rotations_sum <- rotations %>%
#   summarise(make_starts = sum(make_starts),
#             missing_starts = sum(missing_starts),
#             value = sum(make_value)) %>%
#   arrange(desc(value))
# 
# ## Still need ERA to get the accurate INN doubled and such
# 
# 
# # Batter/Fielder Evaluation -----------------------------------------------
# 
# 
# 
# 
# 
# 
# write_csv(pitchers_new, "pitchers_2017.csv")
# write_csv(fielding_dh, "batters_2017.csv")
