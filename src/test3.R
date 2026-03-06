library(dplyr)
library(baseballr)

date <- "2026-03-05"
pks <- mlb_game_pks(date = date)
print("Games on this date:")
if (!is.null(pks) && nrow(pks) > 0) {
  print(pks %>% select(game_pk, teams.away.team.name, teams.home.team.name, status.detailedState))
} else {
  print("No games found today")
}

# Also try mlb_schedule with sport_id=51
schedule <- mlb_schedule(season = 2026, sport_id = 51)
if(!is.null(schedule) && nrow(schedule) > 0) {
  today_games <- schedule %>% filter(as.Date(game_date) == as.Date(date))
  print("Today's WBC games via schedule:")
  print(today_games %>% select(starts_with("game_pk"), starts_with("teams")))
} else {
  print("No schedule data for sport_id 51")
}
