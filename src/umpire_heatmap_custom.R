#!/usr/bin/env Rscript
# src/umpire_heatmap_custom.R
# 目的：嚴格繼承 src/umpire_heatmap_wbc.R 的排版與統計邏輯，
#       僅修改 100% 機率顏色為 #FF4713，及 Y 軸上限為 6.0。

options(repos = c(CRAN = "https://cloud.r-project.org"))

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(scales)
  library(svglite)
})

cat(sprintf("\n=== 中澳大戰 主審判決熱區圖 (自定樣式: #FF4713, Y max=6.0) ===\n"))

# 設定路徑 (根據先前的報告路徑)
target_date <- "2026-03-05" # 中澳大戰日期
report_dir <- file.path("reports", target_date)
assets_dir <- file.path("reports") # 輸出回使用者要求的 /reports/ 下

pbp_files <- list.files(report_dir, pattern = "ChineseTaipei_vs_Australia_pbp\\.csv$", full.names = TRUE)

if (length(pbp_files) == 0) {
  cat("未找到 ChineseTaipei_vs_Australia_pbp.csv 檔案。\n")
  quit(status = 0)
}

pbp_path <- pbp_files[1]
cat(sprintf("處理檔案：%s\n", pbp_path))

# 讀取 pbp 資料
pbp <- read.csv(pbp_path, check.names = FALSE, stringsAsFactors = FALSE)

# 篩選主審判決
ump_df <- pbp %>%
  filter(details.description %in% c("Called Strike", "Ball", "Ball In Dirt")) %>%
  filter(!is.na(pitchData.coordinates.pX), !is.na(pitchData.coordinates.pZ)) %>%
  mutate(
    pX = as.numeric(pitchData.coordinates.pX),
    pZ = as.numeric(pitchData.coordinates.pZ),
    is_strike = as.integer(details.description == "Called Strike")
  ) %>%
  filter(pX >= -2 & pX <= 2) %>%
  filter(pZ >= 0.5 & pZ <= 6.0) # ★ 修改點 1：Y 軸過濾上限至 6.0

# 統計判決數量
n_strikes <- sum(ump_df$is_strike == 1)
n_balls <- sum(ump_df$is_strike == 0)
n_total <- nrow(ump_df)
called_k_rate <- round(n_strikes / n_total * 100, 1)

# 計算完整的資料篩選說明
n_total_pitches <- sum(pbp[["isPitch"]] == TRUE, na.rm = TRUE)
n_ump_decisions <- sum(
  pbp[["details.description"]] %in% c("Called Strike", "Ball", "Ball In Dirt") &
    pbp[["isPitch"]] == TRUE,
  na.rm = TRUE
)
n_excluded <- n_ump_decisions - n_total
filter_caption <- sprintf(
  "資料說明：全場 %d 球中，%d 球為主審主觀判決（打者未揮棒的好球/壞球），其中 %d 球超出座標顯示範圍（±2ft / 0.5~6.0ft）後排除，最終以 %d 球繪製。",
  n_total_pitches, n_ump_decisions, n_excluded, n_total
)

# 好球帶標準框線
sz_poly <- data.frame(
  x = c(-0.708, 0.708, 0.708, -0.708, -0.708),
  z = c(1.5, 1.5, 3.5, 3.5, 1.5)
)

# 繪製六邊形分箱熱區圖
p <- ggplot(ump_df, aes(x = pX, y = pZ)) +
  stat_summary_hex(
    aes(z = is_strike),
    fun   = mean,
    bins  = 18, # ★ 保留原本的 18 個分箱以維持顆粒度
    alpha = 0.85
  ) +
  scale_fill_gradient2(
    low      = "#0D6CF2",
    mid      = "#FFFFFF",
    high     = "#FF4713", # ★ 修改點 2：高機率為 #FF4713
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
  coord_fixed(xlim = c(-2, 2), ylim = c(0.5, 6.0)) + # ★ 修改點 3：Y 軸比例上限至 6.0
  labs(
    title = "ChineseTaipei vs Australia 主審判決熱區圖",
    subtitle = sprintf(
      "判決總數: %d 球（好球 %d / 壞球 %d，好球判決率 %.1f%%）",
      n_total, n_strikes, n_balls, called_k_rate
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
    axis.title         = element_text(size = 10)
  )

# 輸出 SVG 檔案
svg_path <- file.path(assets_dir, "UmpireHeatmap_ChineseTaipei_vs_Australia.svg")
ggsave(svg_path, p,
  device = svglite::svglite,
  width  = 8,
  height = 7.7 # ★ 小幅增加高度以比例對齊擴大的 Y 軸 (原始為 7)
)

cat(sprintf("✅ 修正過排版對齊版的熱區圖已儲存：%s\n", svg_path))
