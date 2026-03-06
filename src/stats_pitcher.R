# 投球相關統計模組

# 6. 各投手的投球內容統計
calc_pitcher_stats <- function(pbp) {
    pbp %>%
        filter(isPitch == TRUE, !is.na(matchup.pitcher.fullName), !is.na(details.type.description)) %>%
        mutate(防守方 = ifelse(!is.na(fielding_team), fielding_team, ifelse(batting_team == away_team, home_team, away_team))) %>%
        group_by(防守方, matchup.pitcher.fullName, details.type.description) %>%
        summarise(
            用球數 = n(),
            平均球速_mph = round(mean(pitchData.startSpeed, na.rm = TRUE), 1),
            平均轉速_rpm = round(mean(pitchData.breaks.spinRate, na.rm = TRUE), 0),
            .groups = "drop"
        ) %>%
        arrange(防守方, matchup.pitcher.fullName, desc(用球數)) %>%
        rename(投手 = matchup.pitcher.fullName, 球種 = details.type.description)
}

# 6-1. 各投手總用球數
calc_pitcher_total <- function(pbp) {
    pbp %>%
        filter(isPitch == TRUE, !is.na(matchup.pitcher.fullName)) %>%
        mutate(防守方 = ifelse(!is.na(fielding_team), fielding_team, ifelse(batting_team == away_team, home_team, away_team))) %>%
        group_by(防守方, matchup.pitcher.fullName) %>%
        summarise(總用球數 = n(), .groups = "drop") %>%
        arrange(防守方, desc(總用球數)) %>%
        rename(投手 = matchup.pitcher.fullName)
}

# 6-2. 各投手的平均球速
calc_pitcher_avg_speed <- function(pbp) {
    pbp %>%
        filter(isPitch == TRUE, !is.na(matchup.pitcher.fullName), !is.na(pitchData.startSpeed)) %>%
        mutate(防守方 = ifelse(!is.na(fielding_team), fielding_team, ifelse(batting_team == away_team, home_team, away_team))) %>%
        group_by(防守方, matchup.pitcher.fullName) %>%
        summarise(平均球速_mph = round(mean(pitchData.startSpeed, na.rm = TRUE), 1), .groups = "drop") %>%
        arrange(防守方, desc(平均球速_mph)) %>%
        rename(投手 = matchup.pitcher.fullName)
}

# 6-3. 各投手最快球速、轉速與球種 - 精簡版
calc_pitcher_max_speed <- function(pbp) {
    pbp %>%
        filter(isPitch == TRUE, !is.na(matchup.pitcher.fullName), !is.na(pitchData.startSpeed)) %>%
        mutate(防守方 = ifelse(!is.na(fielding_team), fielding_team, ifelse(batting_team == away_team, home_team, away_team))) %>%
        group_by(防守方, matchup.pitcher.fullName) %>%
        slice_max(order_by = pitchData.startSpeed, n = 1, with_ties = FALSE) %>%
        select(防守方, 投手 = matchup.pitcher.fullName, 最快球速_mph = pitchData.startSpeed, 轉速_rpm = pitchData.breaks.spinRate, 球種 = details.type.description) %>%
        arrange(防守方, desc(最快球速_mph))
}

# 6-5. 各隊最快球速 - 詳細版 (含局數、打者)
calc_detailed_pitcher_max_speed <- function(pbp) {
    away_team <- unique(pbp$away_team)[1]
    home_team <- unique(pbp$home_team)[1]

    pbp %>%
        filter(isPitch == TRUE, !is.na(matchup.pitcher.fullName), !is.na(pitchData.startSpeed)) %>%
        mutate(防守方 = ifelse(!is.na(fielding_team), fielding_team, ifelse(batting_team == away_team, home_team, away_team))) %>%
        group_by(防守方) %>%
        slice_max(order_by = pitchData.startSpeed, n = 1, with_ties = FALSE) %>%
        select(
            隊伍 = 防守方,
            投手 = matchup.pitcher.fullName,
            打者 = matchup.batter.fullName,
            局數 = about.inning,
            結果 = result.event,
            球種 = details.type.description,
            最快球速_mph = pitchData.startSpeed
        ) %>%
        ungroup()
}

# 6-6. 各隊平均投球速度最高的球員
calc_top_avg_pitch_speed_player <- function(pbp) {
    calc_pitcher_avg_speed(pbp) %>%
        group_by(防守方) %>%
        slice_max(order_by = 平均球速_mph, n = 1, with_ties = FALSE) %>%
        ungroup() %>%
        rename(隊伍 = 防守方)
}

# 6-4. 各投手好壞球比例 (Strike/Ball Ratio)
calc_pitcher_sb_ratio <- function(pbp) {
    pbp %>%
        filter(isPitch == TRUE, !is.na(matchup.pitcher.fullName), !is.na(details.description)) %>%
        mutate(
            防守方 = ifelse(!is.na(fielding_team), fielding_team, ifelse(batting_team == away_team, home_team, away_team)),
            is_strike = grepl("Strike|Foul|In play", details.description, ignore.case = TRUE),
            is_ball = grepl("Ball", details.description, ignore.case = TRUE)
        ) %>%
        group_by(防守方, matchup.pitcher.fullName) %>%
        summarise(
            總球數 = n(),
            好球數 = sum(is_strike, na.rm = TRUE),
            壞球數 = sum(is_ball, na.rm = TRUE),
            好球率 = ifelse(總球數 > 0, sprintf("%.1f%%", (好球數 / 總球數) * 100), "0.0%"),
            好壞比 = ifelse(壞球數 > 0, round(好球數 / 壞球數, 2), 好球數),
            .groups = "drop"
        ) %>%
        arrange(防守方, desc(總球數)) %>%
        rename(投手 = matchup.pitcher.fullName)
}

# 7-1. 全場變速球的揮空率 (Overall Changeup Whiff Rate)
calc_changeup_whiff_all <- function(changeup_data) {
    changeup_data %>%
        summarise(
            總變速球數 = n(),
            揮棒次數 = sum(is_swing, na.rm = TRUE),
            揮空次數 = sum(is_whiff, na.rm = TRUE),
            揮空率 = ifelse(揮棒次數 > 0, sprintf("%.1f%%", (揮空次數 / 揮棒次數) * 100), "0.0%")
        )
}

# 7-2. 各隊打者對變速球的揮空率 (Batter Changeup Whiff Rate)
calc_batter_changeup_whiff <- function(changeup_data) {
    changeup_data %>%
        filter(!is.na(batting_team), !is.na(matchup.batter.fullName)) %>%
        group_by(batting_team, matchup.batter.fullName) %>%
        summarise(
            遭遇變速球數 = n(),
            揮棒次數 = sum(is_swing, na.rm = TRUE),
            揮空次數 = sum(is_whiff, na.rm = TRUE),
            揮空率 = ifelse(揮棒次數 > 0, sprintf("%.1f%%", (揮空次數 / 揮棒次數) * 100), "0.0%"),
            .groups = "drop"
        ) %>%
        arrange(batting_team, desc(揮空次數), desc(遭遇變速球數)) %>%
        rename(隊伍 = batting_team, 打者 = matchup.batter.fullName)
}

# 8-1. 各投手總揮空率 (Pitcher Total Whiff Rate)
calc_pitcher_total_whiff <- function(pitch_whiff_data) {
    pitch_whiff_data %>%
        group_by(防守方, matchup.pitcher.fullName) %>%
        summarise(
            總球數 = n(),
            揮棒次數 = sum(is_swing, na.rm = TRUE),
            揮空次數 = sum(is_whiff, na.rm = TRUE),
            揮空率 = ifelse(揮棒次數 > 0, sprintf("%.1f%%", (揮空次數 / 揮棒次數) * 100), "0.0%"),
            .groups = "drop"
        ) %>%
        arrange(防守方, desc(揮空次數), desc(總球數)) %>%
        rename(投手 = matchup.pitcher.fullName)
}

# 8-2. 各投手各球種揮空率 (Pitcher Whiff Rate by Pitch Type)
calc_pitcher_type_whiff <- function(pitch_whiff_data) {
    pitch_whiff_data %>%
        filter(!is.na(details.type.description)) %>%
        group_by(防守方, matchup.pitcher.fullName, details.type.description) %>%
        summarise(
            該球種球數 = n(),
            揮棒次數 = sum(is_swing, na.rm = TRUE),
            揮空次數 = sum(is_whiff, na.rm = TRUE),
            揮空率 = ifelse(揮棒次數 > 0, sprintf("%.1f%%", (揮空次數 / 揮棒次數) * 100), "0.0%"),
            .groups = "drop"
        ) %>%
        arrange(防守方, matchup.pitcher.fullName, desc(揮空次數), desc(該球種球數)) %>%
        rename(投手 = matchup.pitcher.fullName, 球種 = details.type.description)
}
