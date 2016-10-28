### DMBreportLoad.R

library(dplyr)
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
  field_name_matches <- gregexpr(" [A-Z]",headers)[[1]]
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

readRosterStatus <- function(directory = "C:\\dmb11\\PFBL 2016\\reports\\") {
  batter_roster <- paste(directory, "BatterRoster.txt", sep = "")
  pitcher_roster <- paste(directory, "PitcherRoster.txt", sep = "")
  
  batter_roster <- readDMBfile(batter_roster)
  pitcher_roster <- readDMBfile(pitcher_roster)
  
  final_roster <- rbind(batter_roster, pitcher_roster)
  # Change the type of fields as necessary
  final_roster$ID <- as.numeric(final_roster$ID)
  
  return(final_roster)
}

readPlayerStats <- function(directory = "C:\\dmb11\\PFBL 2016\\reports\\") {
  batter_lhp <- paste0(directory, "BatterProfileVsLHP.txt")
  batter_rhp <- paste0(directory, "BatterProfileVsRHP.txt")
  
  pitcher_lhb <- paste0(directory, "PitcherProfileVsLHB.txt")
  pitcher_rhb <- paste0(directory, "PitcherProfileVsRHB.txt")
  
  batter_lhp <- readDMBfile(batter_lhp) %>%
    mutate(split = "LHP")
  batter_rhp <- readDMBfile(batter_rhp) %>%
    mutate(split = "RHP")
  pitcher_lhb <- readDMBfile(pitcher_lhb) %>%
    mutate(split = "LHB")
  pitcher_rhb <- readDMBfile(pitcher_rhb) %>%
    mutate(split = "RHB")
  
  final_stats <- bind_rows(batter_lhp, batter_rhp,
                       pitcher_lhb, pitcher_rhb)
  
  # Change the type of fields as necessary
  cols.num <- c("ID", "AVG", "OBP", "SLG", "OPS", "AB", "SNG", "DBL", "TRI",
               "HR", "UBB", "HBP", "SF")
  final_stats[cols.num] <- sapply(final_stats[cols.num],as.numeric)
  
  return(final_stats)
}

readBatterRatings <- function(directory = "C:\\dmb11\\PFBL 2016\\reports\\") {
  batter_ratings <- paste0(directory, "BatterProfileRatings.txt")
  batter_ratings <- readDMBfile(batter_ratings)
  
  return(batter_ratings)
}

readPitcherRatings <- function(directory = "C:\\dmb11\\PFBL 2016\\reports\\") {
  pitcher_ratings <- paste0(directory, "PitcherProfileRatings.txt")
  pitcher_ratings <- readDMBfile(pitcher_ratings)
  
  # Change the type of fields as necessary
  cols.num <- c("ID", "G", "GS", "INN", "BF")
  pitcher_ratings[cols.num] <- sapply(pitcher_ratings[cols.num],as.numeric)
  pitcher_ratings$Birth <- as.Date(pitcher_ratings$Birth,
                                   format = "%m/%d/%Y")
  
  return(pitcher_ratings)
}