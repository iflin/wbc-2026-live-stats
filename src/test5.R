library(dplyr)
library(baseballr)

# 抓取 788120 的 play-by-play
cat("Fetching pbp for 788120...\n")
pbp <- mlb_pbp(game_pk = 788120)

cat("Columns in pbp:\n")
print(colnames(pbp))

if (nrow(pbp) > 0) {
  # 檢查我們需要的欄位：
  # 擊球初速: hitData.launchSpeed
  # 擊球仰角: hitData.launchAngle
  # 打者名稱: matchup.batter.fullName
  # 隊伍: 基本上需要知道是 away 還是 home, batting_team?
  
  cat("\nSample of hitData:\n")
  hit_events <- pbp %>% 
    filter(!is.na(hitData.launchSpeed)) %>%
    select(matchup.batter.fullName, result.event, hitData.launchSpeed, hitData.launchAngle) %>%
    head(10)
  print(hit_events)
  
  cat("\nCheck team columns:\n")
  print(pbp %>% select(starts_with("team"), starts_with("batting"), starts_with("matchup")) %>% colnames())
} else {
  cat("No pbp data found.\n")
}
