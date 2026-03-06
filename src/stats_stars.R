# 焦點追蹤球星數據模組 (Star Tracker)
# 此模組專門處理大谷翔平(打者)與山本由伸(投手)的對應個人成績計算

suppressPackageStartupMessages({
    library(dplyr)
    library(tidyr)
})

# === 大谷翔平打擊追蹤 ===
# 1. 大谷翔平 - 打擊逐席結果
calc_ohtani_at_bats <- function(ab_results) {
    df <- ab_results %>%
        filter(grepl("Shohei Ohtani", .data$matchup.batter.fullName, ignore.case = TRUE))

    if (nrow(df) == 0) {
        return(NULL)
    }

    df %>%
        arrange(.data$atBatIndex) %>%
        mutate(
            打席 = row_number(),
            打者 = "大谷翔平 (Shohei Ohtani)",
            對手投手 = .data$matchup.pitcher.fullName,
            結果 = .data$result.event,
            打次描述 = .data$result.description
        ) %>%
        select(打席, 打者, 對手投手, 結果, 打次描述)
}

# 2. 大谷翔平 - 擊球初速與距離
calc_ohtani_hit_metrics <- function(hit_data) {
    df <- hit_data %>%
        filter(grepl("Shohei Ohtani", .data$matchup.batter.fullName, ignore.case = TRUE)) %>%
        filter(!is.na(.data$hitData.launchSpeed))

    if (nrow(df) == 0) {
        return(NULL)
    }

    df %>%
        mutate(
            打者 = "大谷翔平 (Shohei Ohtani)",
            結果 = .data$result.event,
            擊球初速_mph = .data$hitData.launchSpeed,
            擊球仰角 = .data$hitData.launchAngle,
            飛行距離_ft = .data$hitData.totalDistance
        ) %>%
        select(打者, 結果, 擊球初速_mph, 擊球仰角, 飛行距離_ft) %>%
        arrange(desc(.data$擊球初速_mph))
}


# === 山本由伸投球追蹤 ===
# 1. 山本由伸 - 各球種最快球速與總用球
calc_yamamoto_pitch_stats <- function(pbp) {
    df <- pbp %>%
        filter(grepl("Yoshinobu Yamamoto", .data$matchup.pitcher.fullName, ignore.case = TRUE)) %>%
        filter(.data$isPitch == TRUE, !is.na(.data$details.type.description))

    if (nrow(df) == 0) {
        return(NULL)
    }

    # 總用球數
    total_pitches <- nrow(df)

    # 各球種分析
    df %>%
        group_by(.data$details.type.description) %>%
        summarise(
            用球數 = n(),
            最快球速_mph = max(.data$pitchData.startSpeed, na.rm = TRUE),
            平均轉速_rpm = round(mean(.data$pitchData.breaks.spinRate, na.rm = TRUE), 0),
            .groups = "drop"
        ) %>%
        mutate(
            投手 = "山本由伸 (Yoshinobu Yamamoto)",
            總用球數 = total_pitches,
            球種 = .data$details.type.description
        ) %>%
        select(投手, 總用球數, 球種, 用球數, 最快球速_mph, 平均轉速_rpm) %>%
        arrange(desc(用球數))
}

# 2. 山本由伸 - 揮空率與好壞球比例
calc_yamamoto_advanced_stats <- function(pbp) {
    df <- pbp %>%
        filter(grepl("Yoshinobu Yamamoto", .data$matchup.pitcher.fullName, ignore.case = TRUE)) %>%
        filter(.data$isPitch == TRUE)

    if (nrow(df) == 0) {
        return(NULL)
    }

    # 揮空率計算
    swings <- sum(grepl("Swing|Foul|In play", df$details.description, ignore.case = TRUE), na.rm = TRUE)
    whiffs <- sum(grepl("Swinging Strike", df$details.description, ignore.case = TRUE), na.rm = TRUE)
    whiff_rate <- if (swings > 0) round((whiffs / swings) * 100, 1) else 0

    # 好壞球比例計算
    strikes <- sum(df$details.isStrike, na.rm = TRUE)
    balls <- sum(df$details.isBall, na.rm = TRUE)

    tibble(
        投手 = "山本由伸 (Yoshinobu Yamamoto)",
        總打席 = length(unique(df$atBatIndex)),
        總投球 = nrow(df),
        好球 = strikes,
        壞球 = balls,
        好球率_pct = if (nrow(df) > 0) round((strikes / nrow(df)) * 100, 1) else 0,
        製造揮棒 = swings,
        製造揮空 = whiffs,
        總揮空率_pct = whiff_rate
    )
}

# === 歷史基準 (2023 WBC & 2025 MLB) 比較 ===
calc_historical_comp <- function(pbp_2025_path, wbc_2023_path = NULL, today_stats = NULL) {
    # 如果目前賽事既沒大谷也沒山本，則直接跳過歷史資料對比，避免無謂載入 2.7 GB 的大檔案
    if (is.null(today_stats) || (is.null(today_stats$Ohtani) && is.null(today_stats$Yamamoto))) {
        return(NULL)
    }

    # 讀取資料
    pbp_2025 <- tryCatch(readr::read_csv(pbp_2025_path, show_col_types = FALSE), error = function(e) NULL)

    # 建立容器
    ohtani_comp <- tibble::tibble(
        比較基準 = character(),
        平均初速_mph = numeric(),
        最快初速_mph = numeric(),
        硬擊球率_pct = numeric(), # 擊球初速 >= 95 mph
        最遠距離_ft = numeric(),
        打擊結果_Top3 = character()
    )
    yamamoto_comp <- tibble::tibble(
        比較基準 = character(),
        平均球速_mph = numeric(),
        最快球速_mph = numeric(),
        製造揮空率_pct = numeric(),
        平均轉速_rpm = numeric(),
        主要球種_Top3 = character()
    )

    # 1. 處理 2025 MLB 資料
    if (file.exists(pbp_2025_csv_path)) {
        headers <- readLines(pbp_2025_csv_path, n = 1)

        # 大谷 (660271)
        tmp_o <- tempfile(fileext = ".csv")
        system(sprintf("grep \",660271,\" \"%s\" > %s", pbp_2025_csv_path, tmp_o))
        if (file.size(tmp_o) > 0) {
            o_data <- readr::read_csv(I(c(headers, readLines(tmp_o))), show_col_types = FALSE) %>%
                filter(.data$game_year == 2025, .data$batter == 660271)

            if (nrow(o_data) > 0) {
                o_opening <- o_data %>% filter(.data$game_date == min(.data$game_date))

                calc_o_row <- function(df, label) {
                    list(
                        比較基準 = label,
                        平均初速_mph = round(mean(df$launch_speed, na.rm = TRUE), 1),
                        最快初速_mph = round(max(df$launch_speed, na.rm = TRUE), 1),
                        硬擊球率_pct = round(sum(df$launch_speed >= 95, na.rm = TRUE) / sum(!is.na(df$launch_speed)) * 100, 1),
                        最遠距離_ft = round(max(df$hit_distance_sc, na.rm = TRUE), 0),
                        打擊結果_Top3 = paste(names(sort(table(df$events), decreasing = TRUE))[1:min(3, length(unique(df$events)))], collapse = ", ")
                    )
                }

                ohtani_comp <- ohtani_comp %>%
                    tibble::add_case(!!!calc_o_row(o_opening, "2025 開幕戰 (Opening Day)")) %>%
                    tibble::add_case(!!!calc_o_row(o_data, "2025 全季平均 (Full Season)"))
            }
        }

        # 山本 (808967)
        tmp_y <- tempfile(fileext = ".csv")
        system(sprintf("grep \",808967,\" \"%s\" > %s", pbp_2025_csv_path, tmp_y))
        if (file.size(tmp_y) > 0) {
            y_data <- readr::read_csv(I(c(headers, readLines(tmp_y))), show_col_types = FALSE) %>%
                filter(.data$game_year == 2025, .data$pitcher == 808967)

            if (nrow(y_data) > 0) {
                y_opening <- y_data %>% filter(.data$game_date == min(.data$game_date))

                calc_y_row <- function(df, label) {
                    swings <- sum(df$description %in% c("swinging_strike", "foul", "foul_tip", "hit_into_play"), na.rm = TRUE)
                    whiffs <- sum(df$description %in% c("swinging_strike", "foul_tip"), na.rm = TRUE)
                    list(
                        比較基準 = label,
                        平均球速_mph = round(mean(df$release_speed, na.rm = TRUE), 1),
                        最快球速_mph = round(max(df$release_speed, na.rm = TRUE), 1),
                        製造揮空率_pct = if (swings > 0) round((whiffs / swings) * 100, 1) else 0,
                        平均轉速_rpm = round(mean(df$release_spin_rate, na.rm = TRUE), 0),
                        主要球種_Top3 = paste(names(sort(table(df$pitch_type), decreasing = TRUE))[1:min(3, length(unique(df$pitch_type)))], collapse = ", ")
                    )
                }

                yamamoto_comp <- yamamoto_comp %>%
                    tibble::add_case(!!!calc_y_row(y_opening, "2025 開幕戰 (Opening Day)")) %>%
                    tibble::add_case(!!!calc_y_row(y_data, "2025 全季平均 (Full Season)"))
            }
        }
        unlink(c(tmp_o, tmp_y))
    }

    # 2. 處理 2023 WBC 資料 (大谷)
    if (!is.null(wbc_2023_csv_path) && file.exists(wbc_2023_csv_path)) {
        o_wbc <- readr::read_csv(wbc_2023_csv_path, show_col_types = FALSE) %>%
            filter(grepl("Shohei Ohtani", .data$matchup.batter.fullName, ignore.case = TRUE))

        if (nrow(o_wbc) > 0) {
            ohtani_comp <- ohtani_comp %>% tibble::add_case(
                比較基準 = "2023 WBC (大賽平均)",
                平均初速_mph = round(mean(o_wbc$hitData.launchSpeed, na.rm = TRUE), 1),
                最快初速_mph = round(max(o_wbc$hitData.launchSpeed, na.rm = TRUE), 1),
                硬擊球率_pct = round(sum(o_wbc$hitData.launchSpeed >= 95, na.rm = TRUE) / sum(!is.na(o_wbc$hitData.launchSpeed)) * 100, 1),
                最遠距離_ft = round(max(o_wbc$hitData.totalDistance, na.rm = TRUE), 0),
                打擊結果_Top3 = paste(names(sort(table(o_wbc$result.event), decreasing = TRUE))[1:min(3, length(unique(o_wbc$result.event)))], collapse = ", ")
            )
        }
    }

    # 3. 注入「今日表現」 (如果有傳入數據)
    if (!is.null(today_stats)) {
        if (!is.null(today_stats$Ohtani)) {
            ohtani_comp <- dplyr::bind_rows(tibble::tibble(比較基準 = "⭐️ 今日表現 (Today)", !!!today_stats$Ohtani), ohtani_comp)
        }
        if (!is.null(today_stats$Yamamoto)) {
            yamamoto_comp <- dplyr::bind_rows(tibble::tibble(比較基準 = "⭐️ 今日表現 (Today)", !!!today_stats$Yamamoto), yamamoto_comp)
        }
    }

    return(list(Ohtani = ohtani_comp, Yamamoto = yamamoto_comp))
}
