##############################################################
# Fielding_Value.R
#
# Given a dataframe of batter fielding ratings, determine how
# much defensive value each player is worth in and out of 
# their rated positions
##############################################################


## All below fielding value is full a full game schedule for any particular
## player. It also represents a specific era where a baseline team would
## score 782 runs in 6382 PA

fieldingValue <- function(batter_ratings) {
  
  ## Find the upper and lower rating benchmarks for all rated
  ## defensive positions
  fielding <- batter_ratings %>%
    select(ID, Name, c:C) %>%
    gather(POS, Range, c:rf) %>%
    filter(Range != "") %>%
    separate(Range, c("Range", "Err"), convert = TRUE) %>%
    mutate(POS = toupper(POS), 
           RANGE = toupper(Range),
           Arm = ifelse(POS=='C',C,ifelse(POS %in% c('LF','CF','RF'),OF,NA))) %>%
    select(-Range, -OF, -C) %>%
    mutate(Err_low = pmin(plyr::round_any(Err, 25, f = floor),175),
           Err_high = pmin(plyr::round_any(Err, 25, f = ceiling),200)) %>%
    left_join(coef_arm)
  
  ## Join rated position information with out of position coefficients
  fielding_unrated <- fielding %>%
    left_join(coef_oop_def, 
              by = c("POS"="OPOS", "RANGE"="RANGE", "Err_low"="Err")) %>%
    left_join(coef_oop_def, 
              by = c("POS"="OPOS", "RANGE"="RANGE", "Err_high"="Err")) %>%
    mutate(POS = NPOS.x, rated = "unrated") %>%
    select(-NPOS.x, -NPOS.y)
  
  ### Join rated position information with in-position coefficients
  fielding_rated <- fielding %>%
    left_join(coef_def, 
              by = c("POS"="POS", "RANGE"="RANGE", "Err_low"="Err")) %>%
    left_join(coef_def, 
              by = c("POS"="POS", "RANGE"="RANGE", "Err_high"="Err")) %>%
    mutate(rated = "rated")
  
  ## Combine both rated and unrated raw coefficients to calculate
  ## defensive value, and keep the player-position combos that are
  ## either rated or have the highest defensive value
  fielding_value <- fielding_rated %>%
    bind_rows(fielding_unrated) %>%
    rename(RAA_low = RAA.x, RAA_high = RAA.y)  %>%
    mutate(RAA_throw = ifelse(is.na(RAA_throw),0,RAA_throw),
           RAA = RAA_low + (Err - Err_low)*(RAA_high - RAA_low)/25 + 
             RAA_throw,
           RAA_PA = RAA / TOTAL_PA_FULL) %>%
    select(ID, Name, POS, rated, RANGE, Err, Arm, RAA, RAA_PA) %>%
    arrange(ID, Name, POS, rated, desc(RAA)) %>%
    distinct(ID, Name, POS, .keep_all = TRUE) %>%
    filter(!is.na(POS))
  
  return(fielding_value)
}