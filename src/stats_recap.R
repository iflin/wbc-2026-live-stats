# 賽後/賽中新聞稿懶人包模組 (Game Recap / Notes)
# 用於快速生成記者可用的重點結論

generate_game_recap <- function(stats_list, away_team, home_team) {
    recap_lines <- c()
    recap_lines <- c(recap_lines, sprintf("### ⚾ %s vs %s 賽事數據亮點總結\n", away_team, home_team))

    # 1. 最快擊球 (Max EV) - 兩隊各自最快一球與平均最快球員
    recap_lines <- c(recap_lines, "#### 🔥 擊球火力戰 (Exit Velocity)")

    det_ev <- stats_list$DetailedMaxEV
    avg_ev <- stats_list$TopAvgEV

    if (!is.null(det_ev) && nrow(det_ev) > 0) {
        for (i in 1:nrow(det_ev)) {
            row <- det_ev[i, ]
            recap_lines <- c(recap_lines, sprintf(
                "- **%s 最快擊球**：由 **%s** 於第 %s 局對決 %s 時擊出，初速達 **%.1f mph**（結果：%s）。",
                row$隊伍, row$打者, row$局數, row$投手, row$最快初速_mph, row$結果
            ))
        }
    }

    if (!is.null(avg_ev) && nrow(avg_ev) > 0) {
        for (i in 1:nrow(avg_ev)) {
            row <- avg_ev[i, ]
            recap_lines <- c(recap_lines, sprintf(
                "- **%s 穩定重砲**：**%s** 今日平均擊球初速達 **%.1f mph** (共 %d 次擊球)。",
                row$隊伍, row$打者, row$平均初速_mph, row$擊球數
            ))
        }
    }
    recap_lines <- c(recap_lines, "")

    # 2. 最速球投手 (Fastest Pitcher) - 兩隊各自最快一球與平均最快球員
    recap_lines <- c(recap_lines, "#### ⚡ 投球速度戰 (Pitch Velocity)")

    det_pitch <- stats_list$DetailedMaxPitch
    avg_pitch <- stats_list$TopAvgPitch

    if (!is.null(det_pitch) && nrow(det_pitch) > 0) {
        for (i in 1:nrow(det_pitch)) {
            row <- det_pitch[i, ]
            recap_lines <- c(recap_lines, sprintf(
                "- **%s 最速球**：由 **%s** 於第 %s 局對決 %s 時投出，球速達 **%.1f mph** 之 %s（Play 結果：%s）。",
                row$隊伍, row$投手, row$局數, row$打者, row$最快球速_mph, row$球種, row$結果
            ))
        }
    }

    if (!is.null(avg_pitch) && nrow(avg_pitch) > 0) {
        for (i in 1:nrow(avg_pitch)) {
            row <- avg_pitch[i, ]
            recap_lines <- c(recap_lines, sprintf(
                "- **%s 火球司令**：**%s** 今日投球平均速度達 **%.1f mph**。",
                row$隊伍, row$投手, row$平均球速_mph
            ))
        }
    }
    recap_lines <- c(recap_lines, "")

    # 3. 關鍵打席 (Key At-Bats)
    recap_lines <- c(recap_lines, "#### 🏆 關鍵打席 (Key At-Bats)")
    key_abs <- stats_list$KeyAtBats
    if (!is.null(key_abs) && nrow(key_abs) > 0) {
        for (i in 1:nrow(key_abs)) {
            row <- key_abs[i, ]
            recap_lines <- c(recap_lines, sprintf(
                "- **%s**：**%s** 於第 %s 局擊出 **%s**（擊球初速 %.1f mph）。",
                row$隊伍, row$打者, row$局數, row$結果, row$擊球初速_mph
            ))
        }
    } else {
        recap_lines <- c(recap_lines, "- 暫無特別關鍵的得分或高初速安打紀錄。")
    }
    recap_lines <- c(recap_lines, "")

    # 4. 關鍵投球 (Key Pitches)
    recap_lines <- c(recap_lines, "#### 🎯 關鍵投球 (Key Pitches)")
    key_pitches <- stats_list$KeyPitches
    if (!is.null(key_pitches) && nrow(key_pitches) > 0) {
        for (i in 1:nrow(key_pitches)) {
            row <- key_pitches[i, ]
            recap_lines <- c(recap_lines, sprintf(
                "- **%s**：**%s** 於第 %s 局以 **%.1f mph** %s 三振 %s。",
                row$隊伍, row$投手, row$局數, row$球速_mph, row$球種, row$打者
            ))
        }
    } else {
        recap_lines <- c(recap_lines, "- 暫無高張力或高球速的三振紀錄。")
    }
    recap_lines <- c(recap_lines, "")

    # 5. 關鍵守備 (Key Defensive Plays)
    recap_lines <- c(recap_lines, "#### 🛡️ 關鍵守備 (Key Defensive Plays)")
    key_defense <- stats_list$KeyDefense
    if (!is.null(key_defense) && nrow(key_defense) > 0) {
        for (i in 1:nrow(key_defense)) {
            row <- key_defense[i, ]
            recap_lines <- c(recap_lines, sprintf(
                "- **%s**：守備方成功攔截了 %s 擊出、初速達 **%.1f mph** 的強勁球，將其接殺/封殺出局 (第 %s 局)。",
                row$隊伍, row$打者, row$擊球初速_mph, row$局數
            ))
        }
    } else {
        recap_lines <- c(recap_lines, "- 暫無攔截高初速強勁球的精彩紀錄。")
    }

    if (length(recap_lines) <= 10) { # 標題佔了幾行
        recap_lines <- c(recap_lines, "\n- 目前比賽數據尚在累積中暫無極端亮點。")
    }

    recap_lines <- c(recap_lines, "\n*(本懶人包由系統自動抓取 Statcast 數據生成，提供即時賽況速寫)*\n")

    return(paste(recap_lines, collapse = "\n"))
}

# 輔助計算函數：關鍵打席
calc_key_at_bats <- function(pbp) {
    pbp %>%
        filter(isPitch == TRUE, !is.na(hitData.launchSpeed)) %>%
        filter(result.event %in% c("Home Run", "Single", "Double", "Triple")) %>%
        # 篩選條件：全壘打，或是 RBI > 0，或是初速 > 105
        filter(result.event == "Home Run" | result.rbi > 0 | hitData.launchSpeed > 105) %>%
        group_by(batting_team) %>%
        slice_max(order_by = hitData.launchSpeed, n = 2, with_ties = FALSE) %>%
        select(
            隊伍 = batting_team,
            打者 = matchup.batter.fullName,
            局數 = about.inning,
            結果 = result.event,
            擊球初速_mph = hitData.launchSpeed
        ) %>%
        ungroup() %>%
        arrange(局數)
}

# 輔助計算函數：關鍵投球
calc_key_pitches <- function(pbp) {
    # 這裡需要傳進 home_team 和 away_team 或者是 pbp 已經有團隊資訊
    away_team <- unique(pbp$away_team)[1]
    home_team <- unique(pbp$home_team)[1]

    pbp %>%
        filter(isPitch == TRUE, !is.na(pitchData.startSpeed)) %>%
        # 篩選條件：三振
        filter(grepl("Strikeout", result.event, ignore.case = TRUE)) %>%
        # 次要條件：球速 > 95 或轉速極高
        filter(pitchData.startSpeed > 95 | pitchData.breaks.spinRate > 2600) %>%
        # 決定防守隊伍
        mutate(隊伍 = ifelse(!is.na(fielding_team), fielding_team, ifelse(batting_team == away_team, home_team, away_team))) %>%
        group_by(隊伍) %>%
        slice_max(order_by = pitchData.startSpeed, n = 2, with_ties = FALSE) %>%
        select(
            隊伍,
            投手 = matchup.pitcher.fullName,
            打者 = matchup.batter.fullName,
            局數 = about.inning,
            球速_mph = pitchData.startSpeed,
            球種 = details.type.description
        ) %>%
        ungroup() %>%
        arrange(局數)
}

# 輔助計算函數：關鍵守備 (高初速但被出局)
calc_key_defense <- function(pbp) {
    away_team <- unique(pbp$away_team)[1]
    home_team <- unique(pbp$home_team)[1]

    pbp %>%
        filter(isPitch == TRUE, !is.na(hitData.launchSpeed)) %>%
        # 擊球初速很高 (>100) 但結果是出局
        filter(hitData.launchSpeed > 100) %>%
        # 過濾出局結果
        filter(grepl("Flyout|Lineout|Groundout|Pop Out|Forceout", result.event, ignore.case = TRUE)) %>%
        mutate(隊伍 = ifelse(!is.na(fielding_team), fielding_team, ifelse(batting_team == away_team, home_team, away_team))) %>%
        group_by(隊伍) %>%
        slice_max(order_by = hitData.launchSpeed, n = 2, with_ties = FALSE) %>%
        select(
            隊伍,
            打者 = matchup.batter.fullName,
            局數 = about.inning,
            擊球初速_mph = hitData.launchSpeed
        ) %>%
        ungroup() %>%
        arrange(局數)
}
