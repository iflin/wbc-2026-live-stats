library(dplyr)
library(baseballr)
schedule <- mlb_schedule(season = 2026, level_ids = 50)
print(dim(schedule))
if(!is.null(schedule) && nrow(schedule) > 0) {
  print(colnames(schedule))
} else {
  print("No data returned")
}
