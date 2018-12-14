
# Find Rookies using fangraphs Leaderboards -------------------------------
library(baseballr)
library(tidyverse)
library(rvest)


seasons <- seq(2002, 2007)

batter_leaderboards <- fg_bat_leaders(x = 2012
                                      , y = 2017
                                      , qual = 30
                                      , ind = 1)

rookie_year <- batter_leaderboards %>%
  group_by(Name) %>%
  summarise(rookie_year = min(as.numeric(levels(Season)))
   
         
fg_leaderboard_url <- paste0("http://www.fangraphs.com/leaders.aspx?pos=all&stats=bat&lg=all&qual=30&type=8&season=2017&month=0&season1=2012&ind=1&team=&rost=&age=&filter=&players="
                             , "&page=1_200")

fg_html <- read_html(fg_leaderboard_url)

fg_data <- fg_html %>%
  html_node(css = "#LeaderBoard1_dg1_ctl00") %>%
  html_table()
