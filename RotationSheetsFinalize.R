##############################################################
# FinalizeRotationSheets.R
#
# Take filled out team schedules and create final 
# file with home/away filled out
##############################################################

source("RotationSheetsCreate.R")

consolidateRotations <- function(file, include_teams = FALSE) {
  ## Given a filled out rotation, return all consolidated rotations
  sheets <- excel_sheets(file)
  sheets <- sheets[!grepl("Check", sheets)] ##|Essexville Eruption|Delaware Destroyers
  
  ## Read in all rotations and stack together
  all_rotations <- bind_rows(map(sheets
                                 , readFinalRotation
                                 , filepath = file
                                 , include_teams = include_teams))
}

al_filepath <- "C:/Users/mwlyo/OneDrive/PFBL/Rotation Sheets/2019 FINAL AL Rotation Sheets.xlsx"
nl_filepath <- "C:/Users/mwlyo/OneDrive/PFBL/Rotation Sheets/2019 FINAL NL Rotation Sheets.xlsx"

all_rotations <- consolidateRotations(al_filepath) %>%
  bind_rows(consolidateRotations(nl_filepath))

## Check for instances of a duplicated home/away pitcher indicating a mistake
## that will have to be corrected in the file
dup_games <- all_rotations %>% 
  group_by(GameID, away_home) %>% 
  summarise(starters = n()) %>% 
  filter(starters > 1)

View(all_rotations %>% filter(GameID %in% dup_games$GameID))

## spread stacked rotations by game ID for future joining with schedule
all_game_starters <- all_rotations %>%
  spread(away_home, starter)

## Create final rotation schedules and export to files
final_schedule <- schedule %>%
  left_join(all_game_starters, by = 'GameID') %>%
  select(Date, GameID, Away, AwayPitcher, Home, HomePitcher)

writeRotationSheet("OUTPUT/rotation_sheets_2019_AL_FINAL.xlsx"
                   , al_league
                   , final_schedule)

writeRotationSheet("OUTPUT/rotation_sheets_2019_NL_FINAL.xlsx"
                   , nl_league
                   , final_schedule)

