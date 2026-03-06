#!/usr/bin/env Rscript
# src/umpire_heatmap_wbc.R
# 目的：從 WBC 即時 pbp 資料中，讀取主審「好球 (Called Strike)」與「壞球 (Ball)」判決紀錄，
#       並以六邊形分箱熱區圖呈現主審的好球帶判決分佈。
# 作者：Antigravity Agent
#
# 使用方式：
#   Rscript src/umpire_heatmap_wbc.R            ← 抓今日比賽，輸出 SVG 至 assets/umpire_heatmaps/
#   Rscript src/umpire_heatmap_wbc.R 2026-03-05 ← 指定特定日期
#
# 欄位對應：
#   pitchData.coordinates.pX  → 水平進壘點 (MLB 標準，單位 ft，0 為本壘板正中央)
#   pitchData.coordinates.pZ  → 垂直進壘點 (離地高度，單位 ft)
#   details.description       → 判決描述 (e.g. "Called Strike", "Ball", "Ball In Dirt")

options(repos = c(CRAN = "https://cloud.r-project.org"))

# ── 載入必要套件 ────────────────────────────────────────────────────────────────
suppressPackageStartupMessages({
  library(dplyr) # 資料操作
  library(ggplot2) # 繪圖
  library(scales) # 百分比格式化
})

# 偵測 svglite 是否已安裝，未安裝則自動安裝
if (!requireNamespace("svglite", quietly = TRUE)) {
  install.packages("svglite")
}
library(svglite)

# ── 讀取命令列參數 ───────────────────────────────────────────────────────────────
args <- commandArgs(trailingOnly = TRUE)
target_date <- if (length(args) > 0) args[1] else as.character(Sys.Date())
cat(sprintf("\n=== 主審判決熱區圖 | 日期：%s ===\n", target_date))

# ── 設定路徑 ─────────────────────────────────────────────────────────────────────
# 使用相對路徑，以當前工作目錄（專案根目錄）為基準
report_dir <- file.path("reports", target_date)
assets_dir <- file.path(report_dir, "assets", "umpire_heatmaps")
if (!dir.exists(assets_dir)) dir.create(assets_dir, recursive = TRUE)

# ── 自動偵測該日期的 pbp CSV 檔案 ────────────────────────────────────────────────
# pbp 檔名格式：{game_pk}_{AwayTeam}_vs_{HomeTeam}_pbp.csv
pbp_files <- list.files(report_dir, pattern = "_pbp\\.csv$", full.names = TRUE)

if (length(pbp_files) == 0) {
  cat("未找到任何 *_pbp.csv 檔案，請先執行 fetch_wbc_live.R。\n")
  quit(status = 0)
}

cat(sprintf("共找到 %d 個 pbp 檔案：\n", length(pbp_files)))
cat(paste(" -", pbp_files, collapse = "\n"), "\n\n")

# ── 定義好球帶框線（MLB 標準，平均值） ──────────────────────────────────────────
# 寬度為 17 英吋（約 ±0.708 ft），高度約 1.5–3.5 ft
sz_poly <- data.frame(
  x = c(-0.708, 0.708, 0.708, -0.708, -0.708),
  z = c(1.5, 1.5, 3.5, 3.5, 1.5)
)

# ── 個別處理每一個比賽的 pbp 檔案 ───────────────────────────────────────────────
for (pbp_path in pbp_files) {
  # 從檔名解析隊伍名稱（例：788120_ChineseTaipei_vs_Australia_pbp.csv → 788120 / ChineseTaipei / Australia）
  file_base <- basename(pbp_path)
  game_info_match <- regmatches(file_base, regexpr("^(\\d+)_(.+)_vs_(.+)_pbp", file_base))
  if (length(game_info_match) == 0) {
    cat(sprintf("  無法解析檔名格式：%s，跳過。\n", file_base))
    next
  }
  parts <- strsplit(game_info_match, "_vs_")[[1]]
  game_id <- sub("^(\\d+)_.*", "\\1", parts[1]) # 比賽 PK
  away_part <- sub("^\\d+_", "", parts[1]) # 客隊名
  home_part <- sub("_pbp$", "", parts[2]) # 主隊名
  game_label <- sprintf("%s vs %s", away_part, home_part)

  cat(sprintf("──────────────────────────────────────────\n"))
  cat(sprintf("處理比賽：%s（PK: %s）\n", game_label, game_id))

  # 讀取 pbp 資料
  pbp <- tryCatch(
    read.csv(pbp_path, check.names = FALSE, stringsAsFactors = FALSE),
    error = function(e) {
      cat("  讀取失敗：", e$message, "\n")
      NULL
    }
  )
  if (is.null(pbp)) next

  # 確認必要欄位是否存在
  required_cols <- c("pitchData.coordinates.pX", "pitchData.coordinates.pZ", "details.description")
  missing_cols <- setdiff(required_cols, colnames(pbp))
  if (length(missing_cols) > 0) {
    cat(sprintf("  缺少必要欄位：%s，跳過。\n", paste(missing_cols, collapse = ", ")))
    next
  }

  # ── 篩選主審有主觀判決的球 ────────────────────────────────────────────────
  # "Called Strike" = 裁判判定好球（打者未揮棒）
  # "Ball" / "Ball In Dirt" = 裁判判定壞球（打者未揮棒）
  ump_df <- pbp %>%
    filter(details.description %in% c("Called Strike", "Ball", "Ball In Dirt")) %>%
    filter(!is.na(pitchData.coordinates.pX), !is.na(pitchData.coordinates.pZ)) %>%
    mutate(
      # 轉換成數值（CSV 讀取時可能為字串）
      pX = as.numeric(pitchData.coordinates.pX),
      pZ = as.numeric(pitchData.coordinates.pZ),
      # 1 = 好球判決，0 = 壞球判決
      is_strike = as.integer(details.description == "Called Strike"),
      # 判決文字標籤（用於副標題統計）
      call_label = ifelse(details.description == "Called Strike", "Called Strike", "Ball")
    ) %>%
    # 僅保留進壘點在好球帶附近的球（過濾極端暴投）
    filter(pX >= -2 & pX <= 2) %>%
    filter(pZ >= 0.5 & pZ <= 5.5)

  if (nrow(ump_df) == 0) {
    cat("  本場比賽尚無有效的主審判決紀錄（Ball / Called Strike）。\n")
    next
  }

  # 統計判決數量
  n_strikes <- sum(ump_df$is_strike == 1)
  n_balls <- sum(ump_df$is_strike == 0)
  n_total <- nrow(ump_df)
  called_k_rate <- round(n_strikes / n_total * 100, 1)

  cat(sprintf(
    "  有效判決紀錄：共 %d 球（Called Strike: %d，Ball: %d，好球判決率: %.1f%%）\n",
    n_total, n_strikes, n_balls, called_k_rate
  ))

  # ── 計算完整的數據篩選資訊（用於圖表 caption） ───────────────────────────────
  # 全場投球數
  n_total_pitches <- sum(pbp[["isPitch"]] == TRUE, na.rm = TRUE)
  # 主審判決球（含 Ball In Dirt）
  n_ump_decisions <- sum(
    pbp[["details.description"]] %in% c("Called Strike", "Ball", "Ball In Dirt") &
      pbp[["isPitch"]] == TRUE,
    na.rm = TRUE
  )
  # 排除在視覺化範圍外的球
  n_excluded <- n_ump_decisions - n_total
  filter_caption <- sprintf(
    "資料說明：全場 %d 球中，%d 球為主審主觀判決（打者未揮棒的好球/壞球），其中 %d 球超出座標顯示範圍（±2ft / 0.5~5.5ft）後排除，最終以 %d 球繪製。",
    n_total_pitches, n_ump_decisions, n_excluded, n_total
  )
  cat(sprintf("  資料篩選：%s\n", filter_caption))

  # ── 繪製六邊形分箱熱區圖 ────────────────────────────────────────────────────
  # 使用自訂顏色漸層：
  #   低機率（壞球多）→ #0D6CF2（藍）
  #   中間（50%）     → #FFFFFF（白）
  #   高機率（好球多）→ #28BD73（綠）
  p <- ggplot(ump_df, aes(x = pX, y = pZ)) +
    # 六邊形熱區圖：每個 hex 格計算「平均好球判決機率」
    stat_summary_hex(
      aes(z = is_strike),
      fun   = mean,
      bins  = 18, # 格子數，資料少時設小一點，資料多時可以設大到 25
      alpha = 0.85
    ) +
    # 色階
    scale_fill_gradient2(
      low      = "#0D6CF2", # 低機率：藍色（壞球傾向）
      mid      = "#FFFFFF", # 五五波：白色
      high     = "#28BD73", # 高機率：綠色（好球傾向）
      midpoint = 0.5,
      limits   = c(0, 1),
      labels   = scales::percent,
      name     = "好球率"
    ) +
    # 好球帶標準框線（MLB 平均用作視覺參考）
    annotate("path",
      x         = sz_poly$x,
      y         = sz_poly$z,
      linewidth = 0.9,
      color     = "black"
    ) +
    # 鎖定 1:1 等比例
    coord_fixed(xlim = c(-2, 2), ylim = c(0.5, 5.5)) +
    # 標題與軸標
    labs(
      title = sprintf("%s 主審判決熱區圖", game_label),
      subtitle = sprintf(
        "判決總數: %d 球（好球 %d / 壞球 %d，好球判決率 %.1f%%）",
        n_total, n_strikes, n_balls, called_k_rate
      ),
      caption = filter_caption,
      x = "水平進壘點 (ft)，正值偏打者外角",
      y = "離地高度 (ft)"
    ) +
    # 外觀主題
    theme_minimal(base_family = "PingFang TC") +
    theme(
      legend.position    = "right",
      panel.grid.minor   = element_blank(),
      plot.title         = element_text(face = "bold", size = 14),
      plot.subtitle      = element_text(color = "gray40", size = 10),
      axis.title         = element_text(size = 10)
    )

  # ── 輸出 SVG 檔案 ────────────────────────────────────────────────────────────
  safe_game_label <- gsub(" ", "_", game_label)
  svg_filename <- sprintf("UmpireHeatmap_%s.svg", safe_game_label)
  svg_path <- file.path(assets_dir, svg_filename)

  ggsave(svg_path, p,
    device = svglite::svglite,
    width  = 8,
    height = 7
  )

  cat(sprintf("  ✅ [全場] 熱區圖已儲存：%s\n", svg_path))

  # ── 同時輸出各好球帶區域的好球率統計 CSV（全場） ─────────────────────────────────────
  zone_summary <- ump_df %>%
    mutate(
      # 依水平位置分成 3 區：外角、中間、內角（以打者右打視角）
      zone_h = case_when(
        pX < -0.5 ~ "內角",
        pX > 0.5 ~ "外角",
        TRUE ~ "中間"
      ),
      # 依垂直位置分成 3 區：低、中、高
      zone_v = case_when(
        pZ < 2.0 ~ "低區",
        pZ > 3.0 ~ "高區",
        TRUE ~ "中區"
      )
    ) %>%
    group_by(zone_h, zone_v) %>%
    summarise(
      判決總數 = n(),
      好球判決數 = sum(is_strike),
      好球率 = sprintf("%.1f%%", mean(is_strike) * 100),
      .groups = "drop"
    )

  zone_csv_path <- file.path(assets_dir, sprintf("UmpireZoneStats_%s.csv", safe_game_label))
  write.csv(zone_summary, zone_csv_path, row.names = FALSE, fileEncoding = "UTF-8")
  cat(sprintf("  📊 [全場] 區域統計 CSV：%s\n", zone_csv_path))
  print(as.data.frame(zone_summary), row.names = FALSE)

  # ── 依照「打擊隊伍」分別繪製熱區圖 ─────────────────────────────────────────────
  # 意義：主審面對不同隊伍打者時的判決習慣可能有所不同，
  #       因此將客隊與主隊打擊情況個別呈現，提供更細緻的分析。
  batting_teams <- unique(ump_df$batting_team)
  batting_teams <- batting_teams[!is.na(batting_teams)]

  if (length(batting_teams) > 0) {
    cat(sprintf("\n  ── 依打擊隊伍分別產圖（共 %d 支隊伍） ──\n", length(batting_teams)))
  }

  for (team in batting_teams) {
    team_df <- ump_df %>% filter(batting_team == team)

    if (nrow(team_df) < 5) {
      cat(sprintf("  [%s] 判決資料不足（%d 球），跳過。\n", team, nrow(team_df)))
      next
    }

    # 統計
    t_strikes <- sum(team_df$is_strike == 1)
    t_balls <- sum(team_df$is_strike == 0)
    t_total <- nrow(team_df)
    t_k_rate <- round(t_strikes / t_total * 100, 1)

    cat(sprintf(
      "  [%s] 有效判決：共 %d 球（好球 %d / 壞球 %d，好球率 %.1f%%）\n",
      team, t_total, t_strikes, t_balls, t_k_rate
    ))

    # 繪圖
    p_team <- ggplot(team_df, aes(x = pX, y = pZ)) +
      stat_summary_hex(
        aes(z = is_strike),
        fun   = mean,
        bins  = 15, # 各隊資料量約為全場一半，格子略少一點
        alpha = 0.85
      ) +
      scale_fill_gradient2(
        low      = "#0D6CF2",
        mid      = "#FFFFFF",
        high     = "#28BD73",
        midpoint = 0.5,
        limits   = c(0, 1),
        labels   = scales::percent,
        name     = "好球率"
      ) +
      annotate("path",
        x         = sz_poly$x,
        y         = sz_poly$z,
        linewidth = 0.9,
        color     = "black"
      ) +
      coord_fixed(xlim = c(-2, 2), ylim = c(0.5, 5.5)) +
      labs(
        title = sprintf("%s 打擊 — 主審判決熱區圖", team),
        subtitle = sprintf(
          "比賽：%s ｜ 判決總數: %d（好球 %d / 壞球 %d，好球率 %.1f%%）",
          game_label, t_total, t_strikes, t_balls, t_k_rate
        ),
        caption = filter_caption,
        x = "水平進壘點 (ft)，正值偏打者外角",
        y = "離地高度 (ft)"
      ) +
      theme_minimal(base_family = "PingFang TC") +
      theme(
        legend.position    = "right",
        panel.grid.minor   = element_blank(),
        plot.title         = element_text(face = "bold", size = 14),
        plot.subtitle      = element_text(color = "gray40", size = 10),
        plot.caption       = element_text(color = "gray50", size = 7.5, hjust = 0),
        axis.title         = element_text(size = 10)
      )

    # 輸出 SVG（隊伍專屬）
    safe_team <- gsub(" ", "_", team)
    team_svg <- file.path(assets_dir, sprintf("UmpireHeatmap_%s_Batting_%s.svg", safe_game_label, safe_team))
    ggsave(team_svg, p_team, device = svglite::svglite, width = 8, height = 7)
    cat(sprintf("  ✅ [%s] 熱區圖已儲存：%s\n", team, team_svg))

    # 輸出區域統計 CSV（隊伍專屬）
    team_zone <- team_df %>%
      mutate(
        zone_h = case_when(pX < -0.5 ~ "內角", pX > 0.5 ~ "外角", TRUE ~ "中間"),
        zone_v = case_when(pZ < 2.0 ~ "低區", pZ > 3.0 ~ "高區", TRUE ~ "中區")
      ) %>%
      group_by(zone_h, zone_v) %>%
      summarise(
        判決總數 = n(),
        好球判決數 = sum(is_strike),
        好球率 = sprintf("%.1f%%", mean(is_strike) * 100),
        .groups = "drop"
      )

    team_csv <- file.path(assets_dir, sprintf("UmpireZoneStats_%s_Batting_%s.csv", safe_game_label, safe_team))
    write.csv(team_zone, team_csv, row.names = FALSE, fileEncoding = "UTF-8")
    cat(sprintf("  📊 [%s] 區域統計 CSV：%s\n", team, team_csv))
    print(as.data.frame(team_zone), row.names = FALSE)
    cat("\n")
  }
}

cat("\n==========================================\n")
cat("所有比賽的主審判決熱區圖分析完成！\n")
cat(sprintf("SVG 與統計 CSV 存放於：%s/\n", assets_dir))
