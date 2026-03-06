# 資料抓取與預處理模組
library(dplyr)
library(baseballr)
library(tidyr)

# 取得當日有效賽程
get_live_wbc_games <- function(target_date) {
    pks <- mlb_game_pks(date = target_date, level_ids = 51)

    if (is.null(pks) || nrow(pks) == 0) {
        return(NULL)
    }

    valid_games <- pks %>%
        filter(status.detailedState %in% c("In Progress", "Final", "Game Over", "Completed Early"))

    return(valid_games)
}

# 抓取並基本過濾單場逐球資料
fetch_game_pbp <- function(g_pk) {
    pbp <- tryCatch(
        {
            mlb_pbp(game_pk = g_pk)
        },
        error = function(e) {
            cat(sprintf("  抓取 PBP 時發生錯誤: %s\n", e$message))
            return(NULL)
        }
    )

    if (is.null(pbp) || nrow(pbp) == 0) {
        return(NULL)
    }

    # 確認有 hitData
    if (!"hitData.launchSpeed" %in% colnames(pbp)) {
        return(NULL)
    }

    return(pbp)
}
