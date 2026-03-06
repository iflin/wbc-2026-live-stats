#!/bin/bash
# ============================================================
# deploy_to_github.sh
# 功能：將最新的 WBC 即時報表 HTML 自動推送至 GitHub Pages
# 使用方法：./deploy_to_github.sh [html_file_path]
#   - html_file_path：可選，指定要部署的 HTML 檔案路徑
#                     若不指定，則自動搜尋 reports/ 下最新的 HTML
# 執行前提：已初始化 Git Repo 並設定 Remote origin
# ============================================================

set -e  # 任何指令失敗即中止

# === 設定區 ===
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPORTS_DIR="$REPO_ROOT/reports"
DEPLOY_BRANCH="gh-pages"

# === 尋找最新的 HTML 報表 ===
if [ -n "$1" ] && [ -f "$1" ]; then
  # 如果有指定 HTML 路徑且檔案存在
  HTML_FILE="$1"
else
  # 自動搜尋 reports/ 下最新修改的 HTML 檔案
  HTML_FILE=$(find "$REPORTS_DIR" -name "*.html" -not -path "*/assets/*" | sort -t/ -k4 -r | head -1)
fi

if [ -z "$HTML_FILE" ]; then
  echo "[部署腳本] ❌ 找不到任何 HTML 報表，跳過部署。"
  exit 1
fi

echo "[部署腳本] 📄 準備部署：$HTML_FILE"
HTML_BASENAME=$(basename "$HTML_FILE")
ASSETS_DIR="$(dirname "$HTML_FILE")/assets"

# === 將目前工作狀態儲存（避免影響 main 的工作中檔案）===
cd "$REPO_ROOT"

# 暫存所有工作中的變更（stash）
git stash --include-untracked --quiet 2>/dev/null || true

# === 切換或建立 gh-pages 分支 ===
if git show-ref --verify --quiet refs/heads/$DEPLOY_BRANCH 2>/dev/null; then
  # gh-pages 分支已存在，切換過去
  git checkout $DEPLOY_BRANCH --quiet
else
  # 建立全新的孤立 gh-pages 分支（第一次部署）
  echo "[部署腳本] 🌱 初次部署：建立 gh-pages 分支..."
  git checkout --orphan $DEPLOY_BRANCH --quiet
  # 移除所有暫存檔案（確保乾淨起點）
  git rm -rf . --quiet 2>/dev/null || true
fi

# === 複製 HTML 與 assets 至分支根目錄 ===
# 清除舊有 HTML 與 assets（保留乾淨狀態）
rm -f ./*.html 2>/dev/null || true
rm -rf ./assets 2>/dev/null || true

# 複製最新 HTML 至根目錄
cp "$HTML_FILE" ./index.html           # 主頁面（訪問根 URL 就能看到）
cp "$HTML_FILE" "./$HTML_BASENAME"     # 保留完整檔名版本（方便深度連結）

# 複製 assets 資料夾（圖片、CSS 等）
if [ -d "$ASSETS_DIR" ]; then
  cp -r "$ASSETS_DIR" ./assets
fi

# 建立 README（讓 Repo 首頁有說明）
cat > README.md << EOF
# WBC 2026 賽事即時數據報表

> 本頁面由自動化腳本每 10 秒即時更新，資料來源為 MLB Statcast API。

## 查看即時報表

🔗 [點此開啟即時數據報表](https://iflin.github.io/wbc-2026-live-stats/)

## 技術資訊

- 資料抓取：R \`baseballr\` 套件
- 更新頻率：每 10 秒
- 最後更新：$(date '+%Y-%m-%d %H:%M:%S %Z')
EOF

# === 提交並推送 ===
COMMIT_MSG="🔄 自動部署：$(date '+%Y/%m/%d %H:%M:%S') - $HTML_BASENAME"
git add -A
git commit -m "$COMMIT_MSG" --quiet

echo "[部署腳本] ⬆️  推送至 GitHub Pages..."
git push origin $DEPLOY_BRANCH --quiet

echo "[部署腳本] ✅ 部署成功！"
echo "           🌐 網址：https://iflin.github.io/wbc-2026-live-stats/"

# === 切回 main 分支並還原工作狀態 ===
git checkout main --quiet 2>/dev/null || git checkout -b main --quiet
git stash pop --quiet 2>/dev/null || true
