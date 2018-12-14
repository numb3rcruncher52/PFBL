### DMBreportLoad.R

## Takes any .txt file report from DMB and returns a clean dataframe
readDMBfile <- function(file_path) {
  ## Read in file and separate header information from data
  file <- readLines(file_path, warn = FALSE)
  file <- file[5:length(file) - 1]
  headers <- file[1]
  data <- file[file != headers]
  data <- data[-grep("\f",data)]
  
  ## Figure out how to split the rest of the lines
  max_length <- nchar(headers)
  field_name_matches <- gregexpr(" [A-Za-z0-9]",headers)[[1]]
  field_lengths <- append(field_name_matches - 1, max_length)
  field_lengths <- field_lengths - 
    append(0, field_lengths[-length(field_lengths)])
  
  ## Create temporary file for ease in reading back in as fixed-width
  file_con <- file("temp_output.txt")
  writeLines(append(headers,data), file_con)
  close(file_con)
  
  ## Read in final data frame
  output <- read.fwf("temp_output.txt", 
                     widths = field_lengths, 
                     stringsAsFactors = FALSE, 
                     strip.white = T)
  output <- output[!((is.na(output$V1) | (output$V1 == ""))),]
  names(output) <- trimws(output[1,])
  output <- output[-1,]
  
  ## Clean up unnecessary file
  file.remove("temp_output.txt")
  return(output)
}

readRosterStatus <- function(directory, season) {
  batter_roster <- paste(directory, "BatterRoster.txt", sep = "")
  pitcher_roster <- paste(directory, "PitcherRoster.txt", sep = "")
  
  batter_roster <- readDMBfile(batter_roster)
  pitcher_roster <- readDMBfile(pitcher_roster)
  
  final_roster <- rbind(batter_roster, pitcher_roster) %>%
    mutate(season = season
           , ID = as.numeric(ID)) %>%
    arrange(ID, desc(Team)) %>%
    distinct(ID, .keep_all = TRUE)
  
  return(final_roster)
}

readPlayerStatsSplit <- function(directory, season, type = 'Profile', split) {
  
  ## Define file names
  batter <- paste0(directory, "Batter",type,"Vs",split,"P.txt")
  pitcher <- paste0(directory, "Pitcher",type,"Vs",split,"B.txt")
  
  batter <- readDMBfile(batter) %>%
    mutate(role = "batter")
  pitcher <- readDMBfile(pitcher) %>%
    mutate(role = "pitcher")
  
  final_stats <- bind_rows(batter, pitcher) %>%
    mutate(split = split,
           season = season) %>%
    arrange(ID, desc(AB)) %>%
    distinct(ID, .keep_all = TRUE)
  
  # Change the type of fields as necessary
  cols.num <- c("ID", "AVG", "OBP", "SLG", "OPS", "AB", "SNG", "DBL", "TRI",
                "HR", "UBB", "HBP", "SF")
  final_stats[cols.num] <- sapply(final_stats[cols.num],as.numeric)
  
  return(final_stats)
}

readPlayerStats <- function(directory, season, type = 'Profile') {
  lh <- readPlayerStatsSplit(directory, season, type, "LH")
  rh <- readPlayerStatsSplit(directory, season, type, "RH")
  
  bind_rows(lh, rh)
}

readBatterRatings <- function(directory, season) {
  batter_ratings <- paste0(directory, "BatterProfileRatings.txt")
  batter_ratings <- readDMBfile(batter_ratings) %>%
    mutate(season = season
           , G = as.numeric(G)) %>%
    arrange(ID, desc(G)) %>%
    distinct(ID, .keep_all = TRUE)

  # Change the type of fields as necessary
  batter_ratings$ID <- as.numeric(batter_ratings$ID)
  batter_ratings$Birth <- as.Date(batter_ratings$Birth,
                                   format = "%m/%d/%Y")
  
  return(batter_ratings)
}

readPitcherRatings <- function(directory, season) {
  pitcher_ratings <- paste0(directory, "PitcherProfileRatings.txt")
  pitcher_ratings <- readDMBfile(pitcher_ratings) %>%
    mutate(season = season
           , BF = as.numeric(BF)) %>%
    arrange(ID, desc(BF)) %>%
    distinct(ID, .keep_all = TRUE)
  
  # Change the type of fields as necessary
  cols.num <- c("ID", "G", "GS", "INN")
  pitcher_ratings[cols.num] <- sapply(pitcher_ratings[cols.num],as.numeric)
  pitcher_ratings$Birth <- as.Date(pitcher_ratings$Birth,
                                   format = "%m/%d/%Y")
  
  return(pitcher_ratings)
}