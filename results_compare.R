library(tidyverse)

source("DMBreportLoad.R")

season_folders <- c("PFBL 2015", "PFBL 2016", "PFBL 2017")
seasons <- seq(2014, 2016, 1)

args2 <- list(directory = paste0("C:\\dmb11\\",season_folders,"\\reports\\"),
              season = seasons)

real_stats <- args2 %>% 
  pmap(.f = readPlayerStats, type = 'Profile') %>% 
  bind_rows()

dmb_stats <- args2 %>% 
  pmap(.f = readPlayerStats, type = 'Results') %>% 
  bind_rows()

player_compare <- real_stats %>%
  left_join(dmb_stats
            , by = c("ID"
                     , "Name"
                     , "season"
                     , "role"
                     , "split")) %>%
  filter(!is.na(AVG.y)) %>%
  left_join(select(batter_ratings, season, Name, Bats), by = c("Name", "season")) %>%
  left_join(select(pitcher_ratings, season, Name, Throws, P), by = c("Name", "season")) %>%
  left_join(rosters2, by = c("season", "Name"))

player_type <- player_compare %>%
  select(Name, season, role, split, OPS.x) %>%
  distinct(Name, season, role, split, .keep_all = TRUE) %>%
  spread(split, OPS.x) %>% 
  mutate(rh_split = RH - LH)

player_compare_new <- player_compare %>%
  left_join(player_type, by = c("Name", "season", "role"))

write_csv(player_compare_new, "compared_players.csv")

player_compare <- real_stats %>%
  select(ID, Name, season, role, split, real_OPS = OPS) %>%
  left_join((dmb_stats %>%
               select(ID, Name, season, role, split, result_OPS = OPS)),
            by = c("ID"
                   , "Name"
                   , "season"
                   , "role"
                   , "split")) %>%
  mutate(OPS_diff = result_OPS - real_OPS)


# Add team ----------------------------------------------------------------

rosters <- args2 %>% 
  pmap(.f = readRosterStatus) %>% 
  bind_rows()

player_compare_team <- player_compare %>%
  left_join(rosters
            , by = c("ID"
                     , "Name"
                     , "season"))

player_compared_played <- player_compare_team %>%
  filter(!is.na(Team),
         Team != "")

overall_splits <- player_compared_played %>%
  group_by(season, split, role) %>%
  summarise(count = n()
            , avg_diff = mean(OPS_diff, na.rm = TRUE)
            , med_diff = median(OPS_diff, na.rm = TRUE)
            , sd_diff = sd(OPS_diff, na.rm = TRUE))

team_splits <- player_compared_played %>%
  group_by(season, TeamName, split, role) %>%
  summarise(count = n()
            , avg_diff = mean(OPS_diff, na.rm = TRUE)
            , med_diff = median(OPS_diff, na.rm = TRUE)
            , sd_diff = sd(OPS_diff, na.rm = TRUE))
