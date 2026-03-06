#!/bin/bash
# ============================================================
# live_dashboard.sh
# 功能：WBC 即時賽況看板主程式
#   - 每隔 INTERVAL 秒執行一次資料抓取
#   - 每隔 DEPLOY_EVERY 輪次自動推送 HTML 至 GitHub Pages
# 使用方法：
#   ./live_dashboard.sh [更新秒數] [目標日期]
#   ./live_dashboard.sh 2026-03-06         # 僅指定日期，使用預設秒數
#   ./live_dashboard.sh 30 2026-03-06      # 指定秒數與日期
# ============================================================

# === 設定更新頻率 (秒) 與 目標日期 ===
INTERVAL=${1:-10}
TARGET_DATE=${2:-$(date +%Y-%m-%d)}

# 如果第一個參數不是數字，假設它是日期
if [[ ! $INTERVAL =~ ^[0-9]+$ ]]; then
    TEMP_DATE=$INTERVAL
    INTERVAL=${2:-10}
    TARGET_DATE=$TEMP_DATE
fi

# === GitHub Pages 部署設定 ===
# 每隔幾輪自動推送一次（設為 1 = 每輪都推，3 = 每三輪推一次）
DEPLOY_EVERY=3
CYCLE_COUNT=0

echo "=========================================="
echo "🏟️  啟動 WBC 即時看板"
echo "   📅 日期: ${TARGET_DATE}"
echo "   🔄 更新頻率: ${INTERVAL} 秒"
echo "   🚀 GitHub 部署: 每 ${DEPLOY_EVERY} 輪自動上傳"
echo "   按 Ctrl+C 可以停止看板程式"
echo "=========================================="
echo ""

while true; do
    # 清除畫面
    clear

    # 計數器遞增
    CYCLE_COUNT=$((CYCLE_COUNT + 1))
    echo "[第 ${CYCLE_COUNT} 輪] 抓取資料中... ($(date '+%H:%M:%S'))"

    # 執行 R 腳本獲取最新資料，並傳入日期參數
    Rscript src/fetch_wbc_live.R "$TARGET_DATE"

    echo ""

    # === 自動推送至 GitHub Pages ===
    if (( CYCLE_COUNT % DEPLOY_EVERY == 0 )); then
        echo "[GitHub Pages] 🚀 第 ${CYCLE_COUNT} 輪，自動部署中..."
        bash deploy_to_github.sh 2>&1 | tail -3
    fi

    echo ""
    echo "[即時看板] 下次更新將於 ${INTERVAL} 秒後..."

    # 等待指定秒數
    sleep "$INTERVAL"
done
