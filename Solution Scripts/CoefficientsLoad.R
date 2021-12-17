#######################################
## BaseballCoefficientsLoad.R
#
# File to load data necessary for analyzing players
#######################################

library(rvest)

## The following benchmarks are calculated from 2016 Total Reports
LH_PA_FULL <- 179
RH_PA_FULL <- 510
TOTAL_PA_FULL <- LH_PA_FULL + RH_PA_FULL
PA_INN <- 4.277
INN_START_MAX <- 7
PITCH_LH_SPLIT <- 0.4357984

# Load in Coefficients ----------------------------------------------------

coef_def <- read_csv("MAPPING_DATA/coef_def.csv", col_types = "ccii")

coef_OFarm <- read_csv("MAPPING_DATA/coef_OFarm.csv", col_types = "ciii")

coef_Carm <- read_csv("MAPPING_DATA/coef_Carm.csv", col_types = "cci")
names(coef_Carm) <- c("Arm","POS","RAA_throw")

coef_oop_def <- read_csv("MAPPING_DATA/coef_oop_def.csv", col_types = "cccii")

coef_baserunning <- read_csv("MAPPING_DATA/coef_baserunning.csv", col_types = "ci")

coef_wOBA <- read_csv("MAPPING_DATA/coef_wOBA.csv", col_types = "idddddddd")

dim_team <- read_csv("MAPPING_DATA/dim_team.csv", col_types = "ccicc")

## Pull seasonal constants from fangraphs
if (max(coef_wOBA$Season) < LATEST_SEASON) {
  fg_guts <- read_html("http://www.fangraphs.com/guts.aspx?type=cn")
  coef_wOBA <- fg_guts %>% 
    html_node(css = "#GutsBoard1_dg1_ctl00") %>%
    html_table() %>%
    select(Season, lg_wOBA = wOBA, wOBAScale:wHR) %>%
    mutate(Season = Season + 1)# previously
  write_csv(coef_wOBA, paste0(MAPPINGS_DIR, "wOBACoef.csv"))
}

# Tidy coefficient data ---------------------------------------------------

### Clean up the outfield arm coefficients
coef_arm <- coef_OFarm %>% 
  gather(POS,RAA_throw,-Arm) %>%
  rbind(coef_Carm)
rm(coef_OFarm, coef_Carm)

# Model Defensive Data ----------------------------------------------------

#Make a model for every Position/Range combination based on Error as the 
#X variable and ensuring the model goes through the origin
#def_model <- lm(RAA ~ POS:RANGE + Err - 1, data = coef_def)

def_model <- lm(RAA ~ (POS:RANGE)*Err - 1, data = coef_def)

def_oop_model <- lm(RAA ~ (OPOS:NPOS:RANGE)*Err - 1, data = coef_oop_def)
