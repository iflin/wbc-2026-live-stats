library(dplyr)
library(baseballr)

today <- as.Date("2026-03-05")
# 試試看抓取整個 2026 的 WBC 賽程 (如果有的話)
schedule <- mlb_schedule(season = 2026, level_ids = 50)
print(head(schedule %>% select(game_pk, game_date, teams_away_team_name, teams_home_team_name, status_detailed_state)))

if(!is.null(schedule) && nrow(schedule) > 0) {
  today_games <- schedule %>% filter(as.Date(game_date) == today)
  print("今天的比賽:")
  print(today_games %>% select(game_pk, teams_away_team_name, teams_home_team_name, status_detailed_state))
}
