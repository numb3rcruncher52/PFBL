#######################################
## BaseballCoefficientsLoad.R
#
# File to load data necessary for analyzing players
#
#######################################

library(readr)
library(rvest)
library(dplyr)
library(tidyr)

# Load in Coefficients ----------------------------------------------------

coef_def <- read_csv("DATA/DefCoef.csv", col_types = "cciiiiiiiii")

coef_OFarm <- read_csv("DATA/OFArm.csv", col_types = "ciii")

#coef_Carm <- read_csv("DATA/CArm.csv", col_types = "")
#names(coef_Carm) <- c("Arm","POS","RAA_throw")

#coef_oopDef <- read.csv("data/DMBOOPCoef.csv",header=T,stringsAsFactors = F)

## Get baserunning values in there as well

## Pull seasonal constants from fangraphs
fg_guts <- read_html("http://www.fangraphs.com/guts.aspx?type=cn")
seasonal_constants <- fg_guts %>% 
  html_node(css = "#GutsBoard1_dg1_ctl00") %>%
  html_table()


# Tidy coefficient data ---------------------------------------------------

coef_def <- coef_def %>% 
  gather(Err,RAA,`0`:`200`) %>% 
  mutate(Err = as.numeric(Err))

## Clean up out of position coefficients
#coef_oopDef2 <- coef_oopDef %>% gather(Err,RAA,'0':'200',convert = TRUE)

### Clean up the outfield arm coefficients
coef_arm <- coef_OFarm %>% 
  gather(POS,RAA_throw,-Arm) 
  #rbind(coef_Carm)

coef_wOBA <- seasonal_constants %>%
  select(Season:wHR) %>%
  rename(lg_wOBA = wOBA)




