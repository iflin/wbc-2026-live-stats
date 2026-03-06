#!/usr/bin/env Rscript
library(dplyr)
library(baseballr)
library(tidyr)
options(dplyr.summarise.inform = FALSE)

# 載入當前目錄下的統計與抓取模組
script_dir <- dirname(sub("--file=", "", commandArgs(trailingOnly = FALSE)[grep("--file=", commandArgs(trailingOnly = FALSE))]))
if (length(script_dir) == 0 || script_dir == "") script_dir <- "src"

source(file.path(script_dir, "utils_fetch.R"))
source(file.path(script_dir, "utils_translation.R"))
source(file.path(script_dir, "stats_batter.R"))
source(file.path(script_dir, "stats_team.R"))
source(file.path(script_dir, "stats_pitcher.R"))
source(file.path(script_dir, "stats_recap.R"))
source(file.path(script_dir, "stats_stars.R"))

args <- commandArgs(trailingOnly = TRUE)
target_date <- if (length(args) > 0) args[1] else as.character(Sys.Date())

cat(sprintf("抓取日期: %s 國際賽程 (Module Framework V2)\n", target_date))
cat("==========================================\n")

valid_games <- get_live_wbc_games(target_date)

if (is.null(valid_games) || nrow(valid_games) == 0) {
  cat("目前無進行中或已完賽的比賽。\n")
  quit(status = 0)
}

for (i in seq_len(nrow(valid_games))) {
  game_info <- valid_games[i, ]
  g_pk <- game_info$game_pk
  away_team <- game_info$teams.away.team.name
  home_team <- game_info$teams.home.team.name
  state <- game_info$status.detailedState

  cat(sprintf(
    "\n>>> 正在分析 Game PK: %s (%s vs %s) [%s] <<<\n",
    g_pk, away_team, home_team, state
  ))

  cat("  - 抓取逐球資料中...\n")

  Sys.sleep(1)
  pbp <- fetch_game_pbp(g_pk)

  if (is.null(pbp)) {
    cat("  尚無逐球資料或資料擷取異常。\n")
    next
  }

  # === 資料模組化前處理 ===
  hit_data <- pbp %>% filter(!is.na(hitData.launchSpeed), isPitch == TRUE)
  ab_results <- pbp %>%
    filter(!is.na(atBatIndex), !is.na(result.event)) %>%
    distinct(atBatIndex, .keep_all = TRUE)
  changeup_data <- pbp %>%
    filter(isPitch == TRUE, details.type.description == "Changeup", !is.na(details.description)) %>%
    mutate(is_swing = grepl("Swing|Foul|In play", details.description, ignore.case = TRUE), is_whiff = grepl("Swinging Strike", details.description, ignore.case = TRUE))
  pitch_whiff_data <- pbp %>%
    filter(isPitch == TRUE, !is.na(matchup.pitcher.fullName), !is.na(details.description)) %>%
    mutate(防守方 = ifelse(!is.na(fielding_team), fielding_team, ifelse(batting_team == away_team, home_team, away_team)), is_swing = grepl("Swing|Foul|In play", details.description, ignore.case = TRUE), is_whiff = grepl("Swinging Strike", details.description, ignore.case = TRUE))

  # === List-Based 統一收集計算結果 ===
  stats_list <- list()

  # 安全包裝執行函數 (tryCatch 保護)
  safe_run <- function(metric_name, fn, ...) {
    tryCatch(
      {
        res <- fn(...)
        if (!is.null(res) && nrow(res) > 0) {
          return(res)
        }
        return(NULL)
      },
      error = function(e) {
        cat(sprintf("  [%s] 模組運算錯誤: %s\n", metric_name, e$message))
        return(NULL)
      }
    )
  }

  # Batter Stats
  stats_list[["MaxEV"]] <- safe_run("MaxEV", calc_max_ev, hit_data)
  stats_list[["BatterCombined"]] <- safe_run("BatterCombined", calc_batter_combined_stats, pbp, hit_data)
  stats_list[["Hits"]] <- safe_run("Hits", calc_hits_and_hrs, pbp)
  stats_list[["BatterOutcomes"]] <- safe_run("BatterOutcomes", calc_batter_outcomes, ab_results)

  # Team Stats
  stats_list[["TeamKBB"]] <- safe_run("TeamKBB", calc_team_k_bb, ab_results)
  stats_list[["TeamOutcomes"]] <- safe_run("TeamOutcomes", calc_team_outcomes, ab_results)

  # Pitcher Stats
  stats_list[["PitcherStats"]] <- safe_run("PitcherStats", calc_pitcher_stats, pbp)
  stats_list[["PitcherTotal"]] <- safe_run("PitcherTotal", calc_pitcher_total, pbp)
  stats_list[["PitcherAvgSpeed"]] <- safe_run("PitcherAvgSpeed", calc_pitcher_avg_speed, pbp)
  stats_list[["PitcherMaxSpeed"]] <- safe_run("PitcherMaxSpeed", calc_pitcher_max_speed, pbp)
  stats_list[["PitcherSBRatio"]] <- safe_run("PitcherSBRatio", calc_pitcher_sb_ratio, pbp)
  stats_list[["ChangeupWhiffBatter"]] <- safe_run("ChangeupWhiffBatter", calc_batter_changeup_whiff, changeup_data)
  stats_list[["PitcherTotalWhiff"]] <- safe_run("PitcherTotalWhiff", calc_pitcher_total_whiff, pitch_whiff_data)
  stats_list[["PitcherTypeWhiff"]] <- safe_run("PitcherTypeWhiff", calc_pitcher_type_whiff, pitch_whiff_data)

  # Recap 專用詳細統計 (不一定顯示在表格，但供 Recap 使用)
  stats_list[["DetailedMaxEV"]] <- safe_run("DetailedMaxEV", calc_detailed_max_ev, pbp)
  stats_list[["TopAvgEV"]] <- safe_run("TopAvgEV", calc_top_avg_ev_player, hit_data)
  stats_list[["DetailedMaxPitch"]] <- safe_run("DetailedMaxPitch", calc_detailed_pitcher_max_speed, pbp)
  stats_list[["TopAvgPitch"]] <- safe_run("TopAvgPitch", calc_top_avg_pitch_speed_player, pbp)
  stats_list[["KeyAtBats"]] <- safe_run("KeyAtBats", calc_key_at_bats, pbp)
  stats_list[["KeyPitches"]] <- safe_run("KeyPitches", calc_key_pitches, pbp)
  stats_list[["KeyDefense"]] <- safe_run("KeyDefense", calc_key_defense, pbp)

  # Star Player Stats
  ohtani_ab <- safe_run("StarOhtaniAB", calc_ohtani_at_bats, ab_results)
  ohtani_hit <- safe_run("StarOhtaniHit", calc_ohtani_hit_metrics, hit_data)
  yamamoto_stats <- safe_run("StarYamamotoStats", calc_yamamoto_pitch_stats, pbp)
  yamamoto_adv <- safe_run("StarYamamotoAdv", calc_yamamoto_advanced_stats, pbp)

  # 建立今日數據供歷史對比使用
  today_ohtani <- if (!is.null(ohtani_hit)) {
    list(
      平均初速_mph = round(mean(ohtani_hit$擊球初速_mph, na.rm = TRUE), 1),
      最快初速_mph = round(max(ohtani_hit$擊球初速_mph, na.rm = TRUE), 1),
      硬擊球率_pct = round(sum(ohtani_hit$擊球初速_mph >= 95, na.rm = TRUE) / nrow(ohtani_hit) * 100, 1),
      最遠距離_ft = round(max(ohtani_hit$飛行距離_ft, na.rm = TRUE), 0),
      打擊結果_Top3 = paste(names(sort(table(ohtani_hit$結果), decreasing = TRUE))[1:min(3, length(unique(ohtani_hit$結果)))], collapse = ", ")
    )
  } else {
    NULL
  }

  today_yamamoto <- if (!is.null(yamamoto_adv)) {
    list(
      平均球速_mph = if (!is.null(pbp)) round(mean(pbp$pitchData.startSpeed[grepl("Yoshinobu Yamamoto", pbp$matchup.pitcher.fullName, ignore.case = TRUE)], na.rm = T), 1) else 0,
      最快球速_mph = if (!is.null(yamamoto_stats)) max(yamamoto_stats$最快球速_mph, na.rm = T) else 0,
      製造揮空率_pct = yamamoto_adv$總揮空率_pct,
      平均轉速_rpm = if (!is.null(yamamoto_stats)) round(mean(yamamoto_stats$平均轉速_rpm, na.rm = T), 0) else 0,
      主要球種_Top3 = if (!is.null(yamamoto_stats)) paste(yamamoto_stats$球種[1:min(3, nrow(yamamoto_stats))], collapse = ", ") else ""
    )
  } else {
    NULL
  }

  # 讀取歷史比較數據 (2025 MLB & 2023 WBC)
  pbp_2025_path <- "/Users/iflin_m4/Desktop/2026棒球專題-MLB數據-捕手/data/processed/pbp_2020_2025_full.csv"
  wbc_2023_path <- "/Users/iflin_m4/Desktop/2026棒球專題-WBC數據/data/raw/pbp/wbc_pbp_2023.csv"
  historical_comps <- safe_run("HistoricalComps", calc_historical_comp, pbp_2025_path, wbc_2023_path, today_stats = list(Ohtani = today_ohtani, Yamamoto = today_yamamoto))

  # 整合所有報表 (加上翻譯)
  stats_list <- lapply(stats_list, enrich_stats_with_translation)
  if (!is.null(ohtani_ab)) stats_list[["StarOhtaniAB"]] <- enrich_stats_with_translation(ohtani_ab)
  if (!is.null(ohtani_hit)) stats_list[["StarOhtaniHit"]] <- enrich_stats_with_translation(ohtani_hit)
  if (!is.null(yamamoto_stats)) stats_list[["StarYamamotoStats"]] <- enrich_stats_with_translation(yamamoto_stats)
  if (!is.null(yamamoto_adv)) stats_list[["StarYamamotoAdv"]] <- enrich_stats_with_translation(yamamoto_adv)
  if (!is.null(historical_comps$Ohtani)) stats_list[["StarOhtaniComp"]] <- enrich_stats_with_translation(historical_comps$Ohtani)
  if (!is.null(historical_comps$Yamamoto)) stats_list[["StarYamamotoComp"]] <- enrich_stats_with_translation(historical_comps$Yamamoto)

  # === 匯出邏輯 (Markdown & CSV) ===
  timestamp_str <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  timestamp_file <- format(Sys.time(), "%Y%m%d_%H%M%S")
  game_prefix <- sprintf("%s_%s_vs_%s", g_pk, gsub(" ", "", away_team), gsub(" ", "", home_team))

  # 定義輸出目錄，依據目標日期產生
  report_dir <- file.path("reports", target_date)
  if (!dir.exists(report_dir)) dir.create(report_dir, recursive = TRUE)
  if (!dir.exists(file.path(report_dir, "data"))) dir.create(file.path(report_dir, "data"), recursive = TRUE)

  md_filename <- file.path(report_dir, sprintf("%s_LiveStats.md", game_prefix))

  # 定義標題對應 (順序也由此控制)
  titles <- c(
    MaxEV = "各隊最快擊球初速 (Max Exit Velocity)",
    BatterCombined = "打者打擊表現綜覽 (Batter Combined Stats)",
    Hits = "上壘與全壘打的擊球數據 (Hits & HRs EV & LA)",
    TeamKBB = "各隊三振與四壞數 (Team K & BB)",
    BatterOutcomes = "各隊打者的打擊結果統計 (Batter Outcomes)",
    TeamOutcomes = "各隊團隊打擊結果總和統計 (Team Outcomes)",
    ChangeupWhiffBatter = "各隊打者對變速球的揮空率 (Batter Changeup Whiff Rate)",
    PitcherAvgSpeed = "各投手的平均球速 (Pitcher Avg Speed)",
    PitcherTotal = "各投手總用球數 (Pitcher Total Pitches)",
    PitcherMaxSpeed = "各投手最快球速、轉速與球種 (Pitcher Max Speed)",
    PitcherSBRatio = "各投手好壞球比例 (Strike/Ball Ratio)",
    PitcherTotalWhiff = "各投手總揮空率 (Pitcher Total Whiff Rate)",
    PitcherTypeWhiff = "各投手各球種揮空率 (Pitcher Whiff Rate by Pitch Type)",
    PitcherStats = "各投手的投球內容統計 (Pitcher Stats)",
    # 焦點球星數據
    StarOhtaniAB = "大谷翔平打席數據 (Shohei Ohtani At-Bat Stats)",
    StarOhtaniHit = "大谷翔平擊球數據 (Shohei Ohtani Hit Metrics)",
    StarOhtaniComp = "大谷翔平 2023 WBC & 2025 MLB 歷史比較 (Shohei Ohtani Historical Comp)",
    StarYamamotoStats = "山本由伸投球數據 (Yoshinobu Yamamoto Pitch Stats)",
    StarYamamotoAdv = "山本由伸進階投球數據 (Yoshinobu Yamamoto Advanced Pitch Stats)",
    StarYamamotoComp = "山本由伸 2025 MLB 歷史比較 (Yoshinobu Yamamoto 2025 Historical Comp)"
  )

  md_content <- c(sprintf("## 更新時間: %s\n", timestamp_str))

  for (k in names(titles)) {
    cat(sprintf("\n【 %s 】\n", titles[k]))
    md_content <- c(md_content, sprintf("### %s", titles[k]))
    df <- stats_list[[k]]

    if (!is.null(df)) {
      md_content <- c(md_content, paste(knitr::kable(df, format = "markdown"), collapse = "\n"))
      print(as.data.frame(df), row.names = FALSE)
    } else {
      md_content <- c(md_content, "尚無紀錄。")
      cat("  尚無紀錄。\n")
    }
    md_content <- c(md_content, "")
  }

  md_content <- c(md_content, "\n---\n")
  cat(paste(md_content, collapse = "\n"), file = md_filename, append = FALSE)

  # 自動化遍歷輸出所有 CSV
  for (k in names(stats_list)) {
    df <- stats_list[[k]]
    if (!is.null(df)) {
      csv_out <- file.path(report_dir, "data", sprintf("%s_%s_%s.csv", game_prefix, k, timestamp_file))
      write.csv(df, file = csv_out, row.names = FALSE, fileEncoding = "UTF-8")
    }
  }

  cat(sprintf("\n  [重構版架構產出成功]\n   - MD 報告: %s\n   - CSV 資料集儲存於 %s/data/ 目錄下\n", md_filename, report_dir))

  # === 生成一頁式懶人包與互動式 HTML 報表 ===
  tryCatch(
    {
      recap_text <- generate_game_recap(stats_list, away_team, home_team)
      rmd_template <- file.path(script_dir, "report_template.Rmd")
      html_filename <- file.path(report_dir, sprintf("%s_LiveStats.html", game_prefix))

      rmarkdown::render(
        input = rmd_template,
        output_file = basename(html_filename),
        output_dir = report_dir,
        params = list(
          stats_list = stats_list,
          game_label = sprintf("%s vs %s", away_team, home_team),
          timestamp_str = timestamp_str,
          recap_text = recap_text
        ),
        quiet = TRUE
      )
      cat(sprintf("   - HTML 互動報表: %s\n", html_filename))
    },
    error = function(e) {
      cat(sprintf("   - [警告] HTML 報表生成失敗 (請確認已安裝 rmarkdown 與 DT): %s\n", e$message))
    }
  )

  # 原始 pbp 快照儲存
  csv_filename <- file.path(report_dir, sprintf("%s_%s_vs_%s_pbp.csv", g_pk, gsub(" ", "", away_team), gsub(" ", "", home_team)))
  pbp_export <- pbp %>% mutate(across(where(is.list), ~ sapply(., toString)))
  write.csv(pbp_export, file = csv_filename, row.names = FALSE, fileEncoding = "UTF-8")

  cat("------------------------------------------\n")
}
