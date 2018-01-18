##############################################################
# DMB_googlesheets.R
#
# Take clean diamond mind data and write to googlesheets
##############################################################

library(googlesheets)

dmb_gs <- gs_new(title = "DMB Data"
                 , ws_title = "player_values"
                 , input = final_values)

## Fielding Value worksheet
gs_ws_new(dmb_gs
          , ws_title = "fielding_value")

## Pitching Worksheet
gs_ws_new(dmb_gs
          , ws_title = "pitching_value")

## Roster worksheet
gs_ws_new(dmb_gs
          , ws_title = "rosters")
