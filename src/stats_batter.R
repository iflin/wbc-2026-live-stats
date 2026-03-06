# 打擊相關統計模組

# 1. 各隊最快擊球初速 (Max Exit Velocity) - 精簡版
calc_max_ev <- function(hit_data) {
    hit_data %>%
        filter(!is.na(batting_team)) %>%
        group_by(batting_team) %>%
        summarise(
            最快擊球初速_mph = max(hitData.launchSpeed, na.rm = TRUE),
            打者 = matchup.batter.fullName[which.max(hitData.launchSpeed)],
            結果 = result.event[which.max(hitData.launchSpeed)],
            .groups = "drop"
        )
}

# 1-1. 各隊最快擊球初速 - 詳細版 (含局數、投手)
calc_detailed_max_ev <- function(pbp) {
    pbp %>%
        filter(!is.na(hitData.launchSpeed), isPitch == TRUE) %>%
        filter(!is.na(batting_team)) %>%
        group_by(batting_team) %>%
        slice_max(order_by = hitData.launchSpeed, n = 1, with_ties = FALSE) %>%
        select(
            隊伍 = batting_team,
            打者 = matchup.batter.fullName,
            投手 = matchup.pitcher.fullName,
            局數 = about.inning,
            結果 = result.event,
            最快初速_mph = hitData.launchSpeed
        ) %>%
        ungroup()
}

# 2. 整合性打者統計：平均初速、仰角與場內擊球表現 (Combined Avg & HIP Stats)
calc_batter_combined_stats <- function(pbp, hit_data) {
    # 基礎平均數據 (含所有 Pitch)
    avg_stats <- hit_data %>%
        filter(!is.na(matchup.batter.fullName)) %>%
        group_by(batting_team, matchup.batter.fullName) %>%
        summarise(
            擊球數 = n(),
            平均初速_mph = round(mean(hitData.launchSpeed, na.rm = TRUE), 1),
            平均仰角_deg = round(mean(hitData.launchAngle, na.rm = TRUE), 1),
            .groups = "drop"
        )

    # 場內擊球數據 (In Play)
    hip_stats <- pbp %>%
        filter(isPitch == TRUE, !is.na(hitData.launchSpeed)) %>%
        filter(grepl("In play", details.description, ignore.case = TRUE)) %>%
        filter(!is.na(matchup.batter.fullName)) %>%
        group_by(matchup.batter.fullName) %>%
        summarise(
            場內擊球數 = n(),
            最快初速_mph = max(hitData.launchSpeed, na.rm = TRUE),
            最快初速仰角_deg = hitData.launchAngle[which.max(hitData.launchSpeed)],
            .groups = "drop"
        )

    # 整合兩者
    avg_stats %>%
        left_join(hip_stats, by = c("matchup.batter.fullName")) %>%
        mutate(
            場內擊球數 = ifelse(is.na(場內擊球數), 0, 場內擊球數),
            最快初速_mph = ifelse(is.na(最快初速_mph), 0, 最快初速_mph),
            最快初速仰角_deg = ifelse(is.na(最快初速仰角_deg), 0, 最快初速仰角_deg)
        ) %>%
        arrange(desc(平均初速_mph)) %>%
        rename(隊伍 = batting_team, 打者 = matchup.batter.fullName)
}

# 2-1. 各隊平均擊球初速最高的球員
calc_top_avg_ev_player <- function(hit_data) {
    hit_data %>%
        filter(!is.na(matchup.batter.fullName)) %>%
        group_by(batting_team, matchup.batter.fullName) %>%
        summarise(
            平均初速_mph = round(mean(hitData.launchSpeed, na.rm = TRUE), 1),
            擊球數 = n(),
            .groups = "drop"
        ) %>%
        group_by(batting_team) %>%
        slice_max(order_by = 平均初速_mph, n = 1, with_ties = FALSE) %>%
        ungroup() %>%
        rename(隊伍 = batting_team, 打者 = matchup.batter.fullName)
}

# 3. 上壘與全壘打的擊球數據
calc_hits_and_hrs <- function(pbp) {
    on_base_events <- c("Single", "Double", "Triple", "Home Run")
    pbp %>%
        filter(result.event %in% on_base_events, isPitch == TRUE) %>%
        filter(!is.na(hitData.launchSpeed)) %>%
        select(batting_team, matchup.batter.fullName, result.event, hitData.launchSpeed, hitData.launchAngle) %>%
        arrange(desc(hitData.launchSpeed)) %>%
        rename(
            隊伍 = batting_team,
            打者 = matchup.batter.fullName,
            結果 = result.event,
            擊球初速_mph = hitData.launchSpeed,
            擊球仰角_deg = hitData.launchAngle
        )
}

# 5. 各隊打者的打擊結果統計
calc_batter_outcomes <- function(ab_results) {
    ab_results %>%
        filter(!is.na(matchup.batter.fullName), !is.na(batting_team)) %>%
        group_by(batting_team, matchup.batter.fullName, result.event) %>%
        summarise(次數 = n(), .groups = "drop") %>%
        arrange(batting_team, matchup.batter.fullName, desc(次數)) %>%
        rename(隊伍 = batting_team, 打者 = matchup.batter.fullName, 結果 = result.event)
}
