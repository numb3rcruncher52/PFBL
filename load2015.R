### Load PFBL Seasonal data
# Takes a season and excel sheet path and returns:
# - batters dataframe
# - pitchers dataframe
# - fielders dataframe
# - wOBA coefficients dataframe

rm(list=ls())
source('functions.R')

library(readxl)
library(dplyr)
library(tidyr)
#library(reshape2)

season = 2016
sheet_path <- "~/Dropbox/PFBL/Reports - DMB/2016_Player_Data.xlsx"

# Load Batter Data
batters_L <- read_excel(sheet_path,sheet='vs LHP')
batters_L$Split <- "LHP"
batters_L$PA <- batters_L$AB + batters_L$UBB + batters_L$HBP + batters_L$SF
batters_R <- read_excel(sheet_path,sheet='vs RHP')
batters_R$Split <- "RHP"
batters_R$PA <- batters_R$AB + batters_R$UBB + batters_R$HBP + batters_R$SF

batters <- rbind(batters_L,batters_R)
batters$Season = season
batters$Team[is.na(batters$Team)] <- "FA"
rm(batters_L,batters_R)

# Load Pitcher Data
pitchers_L <- read_excel(sheet_path,sheet='vs LHB')
pitchers_L$Split <- "LHB"
pitchers_L$PA <- pitchers_L$AB + pitchers_L$UBB + pitchers_L$HBP + pitchers_L$SF
pitchers_R <- read_excel(sheet_path,sheet='vs RHB')
pitchers_R$Split <- "RHB"
pitchers_R$PA <- pitchers_R$AB + pitchers_R$UBB + pitchers_R$HBP + pitchers_R$SF

pitchers <-rbind(pitchers_L,pitchers_R)
pitchers$Season = season
pitchers$Team[is.na(pitchers$Team)] <- "FA"
rm(pitchers_L,pitchers_R)

# Load Fielding Data
fielders <- read_excel(sheet_path,sheet='Defense')

### Load in coefficients for defensive analysis, Fangraphs

coef_def <- read.csv("data/DefCoef.csv",header=T,stringsAsFactors=F)
coef_OFarm <- read.csv("data/OFArm.csv",header=T,stringsAsFactors=F)
coef_Carm <- read.csv("data/CArm.csv",header=T,stringsAsFactors=F)
names(coef_Carm) <- c("Arm","POS","RAA_throw")
coef_wOBA <- read.csv("data/wOBACoef.csv",header=T,stringsAsFactors=F)
coef_oopDef <- read.csv("data/DMBOOPCoef.csv",header=T,stringsAsFactors = F)

### Clean up fielding data for analysis

## Gather for player_position units of data, separate error and range ratings
fielders <- fielders %>% gather(Position,Rating,c:rf) %>% filter(!(is.na(Rating))) %>% separate(Rating,c('RANGE','Err'),sep='/',convert = TRUE) %>% mutate(POS = toupper(Position), RANGE = toupper(RANGE)) %>% mutate(Arm = ifelse(POS=='C',C,ifelse(POS %in% c('LF','CF','RF'),OF,NA))) %>% select(-Position,-C,-OF)

### Clean up the defensive coefficients
coef_def <- coef_def %>% gather(Err,RAA,X0:X200) %>% mutate(Err = as.numeric(substr(Err,2,4)))

## Clean up out of position coefficients
#coef_oopDef2 <- coef_oopDef %>% gather(Err,RAA,'0':'200',convert = TRUE)

### Clean up the outfield arm coefficients
coef_arm <- coef_OFarm %>% gather(POS,RAA_throw,-Arm) %>% rbind(coef_Carm)

### Calculate actual fielding run values
fielders$Err_real <- fielders$Err
errRatings <- seq(0,200,by=25)

# Add the low error rating number to fielders
fielders$Err <- as.numeric(lapply(fielders$Err_real,FUN=function(x) errRatings[max(which(x-errRatings >= 0))]))
fielders <- merge(fielders, coef_def, by=c('POS','RANGE','Err'),all.x=T)
names(fielders)[c(which(names(fielders)=="Err"),ncol(fielders))] <- c('Err_low',"RAA_low")

# Repeat for high number
fielders$Err <- as.numeric(lapply(fielders$Err_real,FUN=function(x) errRatings[min(which(min(x,200)-errRatings <= 0))]))
fielders <- merge(fielders, coef_def, by=c('POS','RANGE','Err'),all.x=T)
names(fielders)[c(which(names(fielders)=="Err"),ncol(fielders))] <- c('Err_high',"RAA_high")

# Add in run values for arm
fielders <- merge(fielders, coef_arm,all.x=T)
fielders$RAA_throw[is.na(fielders$RAA_throw)] <- 0

## Calculate final values
fielders$RAA <- fielders$RAA_low + (fielders$Err_real - fielders$Err_low)*(fielders$RAA_high - fielders$RAA_low)/25 + fielders$RAA_throw

fielders <- select(fielders,Name, Team, Age, POS, RANGE, Err_real, Arm, RAA) %>% arrange(-RAA)
rm(coef_arm, coef_Carm, coef_OFarm, coef_def)
