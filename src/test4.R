library(dplyr)
library(baseballr)

date <- "2026-03-05"
pks_int <- mlb_game_pks(date = date, level_ids = 51)
print("INT Games on this date:")
if (!is.null(pks_int) && nrow(pks_int) > 0) {
  print(pks_int %>% select(game_pk, teams.away.team.name, teams.home.team.name, status.detailedState))
} else {
  print("No INT games found today")
}
