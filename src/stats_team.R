# 團隊層級統計模組

# 4. 各隊三振與四壞數
calc_team_k_bb <- function(ab_results) {
    ab_results %>%
        filter(!is.na(batting_team)) %>%
        group_by(batting_team) %>%
        summarise(
            三振_K = sum(grepl("Strikeout", result.event, ignore.case = TRUE), na.rm = TRUE),
            四壞_BB = sum(grepl("Walk", result.event, ignore.case = TRUE), na.rm = TRUE),
            .groups = "drop"
        ) %>%
        rename(隊伍 = batting_team)
}

# 5-1. 各隊團隊打擊結果總和統計
calc_team_outcomes <- function(ab_results) {
    ab_results %>%
        filter(!is.na(batting_team)) %>%
        group_by(batting_team, result.event) %>%
        summarise(總次數 = n(), .groups = "drop") %>%
        arrange(batting_team, desc(總次數)) %>%
        rename(隊伍 = batting_team, 結果 = result.event)
}
