##############################################################
# DMB_googlesheets.R
#
# Take clean diamond mind data and write to googlesheets
##############################################################

rm(list=ls())

library(tidyverse)
library(googlesheets)

source("Solution Scripts/cleanStats.R")

### Combine all outputted & cleaned DMB data
OUTPUT_DIR <- "OUTPUT_NEW/"
output_files <- list.files(OUTPUT_DIR, full.names = TRUE)

roster_files <- output_files[grep("rosters", output_files)]
splits_files <- output_files[grep("player_splits", output_files)]
results_files <- output_files[grep("results", output_files)]
players_files <- output_files[grep("final_players", output_files)]

rosters <- map(roster_files, read_csv) %>% bind_rows()
player_splits <- map(splits_files, read_csv) %>% bind_rows()
results_splits <- map(results_files, read_csv) %>% bind_rows()
player_cols <- "icicDcccidiidcccicddi"
final_players <- map(players_files, read_csv, col_types = player_cols) %>% bind_rows()


# Create Complete Data sets -----------------------------------------------

all_ratings <- final_players %>%
  left_join(player_splits, by = c("ID", "Name", "season")) %>%
  left_join(rosters, by = c("ID", "Name", "season")) %>%
  calcPlaytimeLimits()

all_results <- final_players %>%
  left_join(results_splits, by = c("ID", "Name", "season")) %>%
  left_join(rosters, by = c("ID", "Name", "season"))

write_csv(all_ratings, "OUTPUT_FINAL/dmb_ratings.csv")
write_csv(all_results, "OUTPUT_FINAL/dmb_results.csv")

gs_upload("OUTPUT_FINAL/dmb_ratings.csv"
          , sheet_title = 'DMB Ratings'
          , overwrite = TRUE)

gs_upload("OUTPUT_FINAL/dmb_results.csv"
          , sheet_title = 'DMB Results'
          , overwrite = TRUE)

