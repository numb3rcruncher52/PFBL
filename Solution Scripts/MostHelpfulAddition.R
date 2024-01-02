##############################################################
# MostHelpfulAddition.R
#
# For a current season find the most helpful player
##############################################################

IMPACTED_TEAM <- 'Frisco Beer Snobs'
TARGET_TEAM <- '_Rookie'
TGT_SEASON <- 2023

all_ratings <- read_csv("PROCESSED_DATA/pfbl_all_ratings.csv")

filterRatings <- function(ratings, tgt_season, impacted_team, target_team) {
  ratings %>%
    mutate(TeamName = ifelse(is.na(TeamName),
                             ifelse(season == rookie_season, '_Rookie', '_FA')
                             , TeamName)) %>%
    filter(season == tgt_season,
           TeamName %in% c(impacted_team, target_team))
}

filtered_ratings <- filterRatings(all_ratings, TGT_SEASON, IMPACTED_TEAM, TARGET_TEAM)

evaluateRoster <- function(ratings) {
  ratings %>%
    filter(TeamName == IMPACTED_TEAM) %>%
    optimalTeam() %>%
    select(Name, POS, split, value_PA, value_GS, value_INN, opt_pa, opt_starts, opt_inn) %>%
    replace_na(list(opt_pa = 0, opt_inn = 0, opt_starts = 0, value_PA = 0, value_GS = 0, value_INN = 0)) %>%
    mutate(value = opt_pa * value_PA + opt_starts * value_GS + opt_inn * value_INN) %>%
    group_by(POS,Name) %>%
    summarise(value = sum(value), opt_pa = sum(opt_pa), opt_inn = sum(opt_inn)/2, opt_gs = sum(opt_starts)/2)
}

addPlayerToTeam <- function(player_id, ratings) {
  ratings %>%
    filter(TeamName == IMPACTED_TEAM | ID == player_id) %>%
    mutate(TeamName = IMPACTED_TEAM) %>%
    evaluateRoster()
}

compareCases <- function(baseline, test_case) {
  baseline %>%
    ungroup() %>%
    mutate(value = -value
           , opt_pa = -opt_pa
           , opt_inn = -opt_inn
           , opt_gs = -opt_gs) %>%
    bind_rows(test_case) %>%
    group_by(POS, Name) %>%
    summarise(value = sum(value)
              , opt_pa = sum(opt_pa)
              , opt_inn = sum(opt_inn)
              , opt_gs = sum(opt_gs)) %>%
    filter(value != 0 | opt_pa != 0 | opt_inn != 0 | opt_gs != 0)
}

baseline <- evaluateRoster(filtered_ratings)

baseline_val <- sum(baseline$value)

testcase <- addPlayerToTeam(30795, filtered_ratings) 

compareCases(baseline, testcase)

players_to_eval <- filtered_ratings %>%
  filter(TeamName != IMPACTED_TEAM) %>%
  distinct(ID, Name) 

for (id in players_to_eval$ID) {
  
}

results <- map(players_to_eval$ID, addPlayerToTeam, filtered_ratings)

results2 <- map_dfr(results, ~ sum(.x$value), .id=value)

  group_by(ID) %>%
  group_modify(~ addPlayerToTeam(player_id = .y, ratings = filtered_ratings))




