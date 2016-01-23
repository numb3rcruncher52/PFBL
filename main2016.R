### Main PFBL Analysis Script

source('load2015.R')

SEASON <- 2016

# Grab relevant wOBA data based on season
LG_WOBA <- coef_wOBA %>% filter(Season == SEASON) %>% select(wOBA)
LG_WOBA <- LG_WOBA[[1]][1]
WOBA_SCALE <- coef_wOBA %>% filter(Season == SEASON) %>% select(wOBAScale)
WOBA_SCALE <- WOBA_SCALE[[1]][1]

library(plyr)

## Calculate the wOBA, runs added for batters, pitchers
names(batters)[9:11] <- c("X1B","X2B","X3B")
batters2 <- batters %>% mutate(wOBA = calcWOBA(SEASON,AB,X1B,X2B,X3B,HR,UBB,HBP,SF), OPS = OBP + SPC) %>% 
  select(Name,Team,P,Age,Split,PA,AVG,OBP,SPC,OPS,wOBA) %>% 
  gather(stat_name,stat_value,PA:wOBA) %>% 
  mutate(Category = paste(Split,"_",stat_name,sep="")) %>% 
  select(-Split,-stat_name) %>% 
  spread(Category,stat_value) %>% 
  mutate(LHP_wRAA = calcWRAA(LHP_wOBA, LG_WOBA, WOBA_SCALE, 1), RHP_wRAA = calcWRAA(RHP_wOBA, LG_WOBA, WOBA_SCALE, 1),totPA = LHP_PA + RHP_PA) %>% 
  mutate(minPA=ifelse(totPA <200,0,0.7*totPA), maxPAvsL=ifelse(totPA >= 502 | LHP_OPS <= .65,160,pmin(LHP_PA*1.33,160)), maxPAvsR=ifelse(totPA >= 502 | RHP_OPS <= .65, 545, pmin(RHP_PA*1.33,545))) %>% 
  mutate(LHP_Runs = maxPAvsL*LHP_wRAA, RHP_Runs = maxPAvsR*RHP_wRAA, Total_Runs = LHP_Runs + RHP_Runs) %>% 
  arrange(-Total_Runs)

## Merge cleaned fielding data
# Clean names in both data frames for merging
fielders2 <- fielders %>% separate(Name, c("LastName","extraName"), sep=",") %>% mutate(Team = ifelse(is.na(Team),"FA",Team))
batters3 <- batters2 %>% separate(Name, c("FirstName","LastName"), sep=" ",extra = "merge") %>% transform(LastName = sub("[^A-Za-z]","",LastName))

# Fix any discrepancies here
fielders2$LastName[fielders2$LastName=="d'Arnaud"] <- "dArnaud"
fielders2$LastName[fielders2$LastName=="de Aza"] <- "deAza*"
fielders2$LastName[fielders2$LastName=="De Jesus"] <- "DeJesus"
fielders2$LastName[fielders2$LastName=="den Dekker"] <- "denDekker*"
fielders2$LastName[fielders2$LastName=="Kang"] <- "HoKang"
fielders2$LastName[fielders2$LastName=="La Stella"] <- "LaStella*"
fielders2$LastName[fielders2$LastName=="Murphy" & fielders2$Team == "GCJ"] <- "RyanMurphy"
fielders2$LastName[fielders2$LastName=="O'Malley"] <- "OMalley#"
fielders2$LastName[fielders2$LastName=="Van Slyke"] <- "VanSlyke"
  
# Complete merge and calculate runs
batters_final <- fielders %>% left_join(select(batters2,Name,Team,Age,minPA,maxPAvsL,maxPAvsR,LHP_wOBA,RHP_wOBA, LHP_Runs,RHP_Runs,Total_Runs)) %>% 
  mutate(bestRuns = pmax(RAA/705*maxPAvsL + LHP_Runs,RAA/705*maxPAvsR + RHP_Runs,RAA/705*(maxPAvsL+maxPAvsR) + Total_Runs),fullRuns = RAA/705*(maxPAvsL+maxPAvsR) + Total_Runs) %>% 
  arrange(-bestRuns)
rm(batters,batters2,fielders)

# Calculate runs for pitchers
names(pitchers)[9:11] <- c("X1B","X2B","X3B")
pitchers_final <- pitchers %>% mutate(wOBA = calcWOBA(SEASON,AB,X1B,X2B,X3B,HR,UBB,HBP,SF)) %>% select(Name,Team,P,Age,Split,PA,AVG,OBP,SPC,wOBA) %>% gather(stat_name,stat_value,PA:wOBA) %>% mutate(Category = paste(Split,"_",stat_name,sep="")) %>% select(-Split,-stat_name) %>% spread(Category,stat_value) %>% mutate(LHB_wRAA = -1*calcWRAA(LHB_wOBA, LG_WOBA, WOBA_SCALE, 1), RHB_wRAA = -1*calcWRAA(RHB_wOBA, LG_WOBA, WOBA_SCALE, 1),totPA = LHB_PA + RHB_PA) %>% mutate(minPA=ifelse(totPA <200,0,0.7*totPA), maxPAvsL=ifelse(totPA >= 502,160,pmin(LHB_PA*1.25,160)), maxPAvsR=ifelse(totPA >= 502, 545, pmin(RHB_PA*1.25,545))) %>% mutate(LHB_Runs = maxPAvsL*LHB_wRAA, RHB_Runs = maxPAvsR*RHB_wRAA, Total_Runs = LHB_Runs + RHB_Runs) %>% arrange(-Total_Runs)

pitchers_final2 <- pitchers_final %>% select(Name, Team, P, Age, LHB_wOBA, RHB_wOBA, totPA, minPA, maxPAvsL, maxPAvsR, LHB_Runs, RHB_Runs, Total_Runs)
write.csv(pitchers_final2, paste(sheet_path,"pitchers_2016.csv",sep=""),row.names = FALSE)
# Combine pitchers and batters for complete player comparison