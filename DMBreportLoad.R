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
  field_name_matches <- gregexpr(" [A-Z]",headers)[[1]]
  field_lengths <- append(field_name_matches[-1] - 1, max_length)
  field_lengths <- field_lengths - append(0, field_lengths[-length(field_lengths)])
  
  ## Create temporary file for ease in reading back in as fixed-width
  file_con <- file("temp_output.txt")
  writeLines(append(headers,data), file_con)
  close(file_con)
  
  ## Read in final data frame
  output <- read.fwf("temp_output.txt", widths = field_lengths, stringsAsFactors = FALSE, strip.white = T)
  output <- output[!((is.na(output$V1) | (output$V1 == ""))),]
  names(output) <- trimws(output[1,])
  output <- output[-1,]
  
  ## Clean up unnecessary file
  file.remove("temp_output.txt")
  return(output)
}

readRosterStatus <- function(directory_location = "C:\\dmb11\\PFBL 2016\\reports\\") {
  batter_roster <- paste(directory_location, "BatterRoster.txt", sep = "")
  pitcher_roster <- paste(directory_location, "PitcherRoster.txt", sep = "")
  
  batter_roster <- readDMBfile(batter_roster)
  pitcher_roster <- readDMBfile(pitcher_roster)
  
  final_roster <- rbind(batter_roster, pitcher_roster)
  # Change the type of fields as necessary
  final_roster$ID <- as.numeric(final_roster$ID)
  
  return(final_roster)
}