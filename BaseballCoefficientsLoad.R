#######################################
## BaseballCoefficientsLoad.R
#
# File to load data necessary for analyzing players
#
#######################################

library(rvest)

## The following benchmarks are calculated from 2016 Total Reports
LH_PA_FULL <- 179
RH_PA_FULL <- 510
TOTAL_PA_FULL <- LH_PA_FULL + RH_PA_FULL

# Load in Coefficients ----------------------------------------------------

#MAPPINGS_DIR <- paste0(REPORT_DIR, "MAPPINGS", SEPARATOR)
#MAPPINGS_DIR <- "C:/Users/mwlyo/OneDrive/PFBL/Reports - DMB/MAPPINGS/"
MAPPINGS_DIR <- "~/OneDrive/PFBL/Reports - DMB/MAPPINGS/"

coef_def <- read_csv(paste0(MAPPINGS_DIR, "DefCoef.csv"), col_types = "ccii")
#coef_def <- read_csv("DATA/DefCoef.csv", col_types = "cciiiiiiiii")

coef_OFarm <- read_csv(paste0(MAPPINGS_DIR, "OFArm.csv"), col_types = "ciii")

coef_Carm <- read_csv(paste0(MAPPINGS_DIR, "CArm.csv"), col_types = "cci")
names(coef_Carm) <- c("Arm","POS","RAA_throw")

coef_oop_def <- read_csv(paste0(MAPPINGS_DIR, "DMBOOPCoef.csv"), col_types = "cccii")

coef_baserunning <- read_csv(paste0(MAPPINGS_DIR, "RunCoef.csv"), col_types = "ci")

coef_wOBA <- read_csv(paste0(MAPPINGS_DIR, "wOBACoef.csv"), col_types = "idddddddd")

dim_team <- read_csv(paste0(MAPPINGS_DIR, "dimTeam.csv"), col_types = "ccicc")

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

# coef_def <- coef_def %>% 
#   gather(Err,RAA,`0`:`200`) %>% 
#   mutate(Err = as.numeric(Err))

## Clean up out of position coefficients
# coef_oop_def <- coef_oop_def %>% 
#   gather(Err,RAA,`0`:`200`,convert = TRUE)

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
