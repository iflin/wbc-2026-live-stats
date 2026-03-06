#!/bin/bash
# ============================================================
# deploy_to_github.sh
# 功能：將最新的 WBC 即時報表 HTML 自動推送至 GitHub Pages
# 策略：使用 git worktree，在不切換當前分支的情況下操作 gh-pages
# 使用方法：
#   ./deploy_to_github.sh                  # 自動搜尋最新 HTML
#   ./deploy_to_github.sh <html_file_path> # 指定 HTML 路徑（支援相對或絕對路徑）
# ============================================================

set -e

# === 設定區（全部使用絕對路徑）===
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$SCRIPT_DIR"
REPORTS_DIR="$REPO_ROOT/reports"
DEPLOY_BRANCH="gh-pages"
WORKTREE_DIR="/tmp/wbc_gh_pages_$$"

# === 尋找最新的 Taiwan vs Japan HTML 報表（優先，其次才搜尋最新）===
if [ -n "$1" ]; then
  if [[ "$1" = /* ]]; then
    HTML_FILE="$1"
  else
    HTML_FILE="$REPO_ROOT/$1"
  fi
else
  # 優先選 Japan vs ChineseTaipei 報表
  HTML_FILE=$(ls -t "$REPORTS_DIR"/*/*Japan*ChineseTaipei*.html 2>/dev/null | head -1 || true)
  if [ -z "$HTML_FILE" ]; then
    # 若找不到，選最新的 HTML
    HTML_FILE=$(ls -t "$REPORTS_DIR"/*/*.html 2>/dev/null | head -1 || true)
  fi
fi

if [ -z "$HTML_FILE" ] || [ ! -f "$HTML_FILE" ]; then
  echo "[部署腳本] ❌ 找不到 HTML 報表，跳過部署。"
  exit 0
fi

echo "[部署腳本] 📄 準備部署：$(basename "$HTML_FILE")"
HTML_BASENAME=$(basename "$HTML_FILE")
ASSETS_DIR="$(dirname "$HTML_FILE")/assets"

# === 確保本地有 gh-pages 分支 ===
# 我們之前已經在遠端建立過。如果本地沒有，先建立追蹤。
if ! git show-ref --verify --quiet "refs/heads/$DEPLOY_BRANCH" 2>/dev/null; then
  git branch "$DEPLOY_BRANCH" "origin/$DEPLOY_BRANCH" 2>/dev/null || true
fi

# === 使用 git worktree 操作 ===
cd "$REPO_ROOT"

# 清理可能殘留的 worktree
git worktree remove "$WORKTREE_DIR" --force 2>/dev/null || true
rm -rf "$WORKTREE_DIR" 2>/dev/null || true

# 建立 worktree，把 gh-pages 掛載到暫存目錄
git worktree add "$WORKTREE_DIR" "$DEPLOY_BRANCH" --quiet

# === 複製 HTML 與相關資源至 worktree ===
find "$WORKTREE_DIR" -maxdepth 1 -name "*.html" -delete 2>/dev/null || true
rm -rf "$WORKTREE_DIR/assets" 2>/dev/null || true

cp "$HTML_FILE" "$WORKTREE_DIR/index.html"
cp "$HTML_FILE" "$WORKTREE_DIR/$HTML_BASENAME"

if [ -d "$ASSETS_DIR" ]; then
  cp -r "$ASSETS_DIR" "$WORKTREE_DIR/assets"
fi

cat > "$WORKTREE_DIR/README.md" << EOF
# WBC 2026 賽事即時數據報表

> 本頁面由自動化腳本即時更新，資料來源為 MLB Statcast API。

## 查看即時報表

🔗 [點此開啟即時數據報表](https://iflin.github.io/wbc-2026-live-stats/)

## 技術資訊

- 資料抓取：R \`baseballr\` 套件
- 更新頻率：每 10 秒 (每次抓取即時上傳)
- 最後更新：$(date '+%Y-%m-%d %H:%M:%S %Z')
EOF

# === 在 worktree 中提交並推送 ===
cd "$WORKTREE_DIR"
COMMIT_MSG="🔄 $(date '+%Y/%m/%d %H:%M:%S') - 更新 $HTML_BASENAME"
git add -A
if git diff --staged --quiet; then
  echo "[部署腳本] ℹ️  內容無變動，跳過 commit。"
else
  git commit -m "$COMMIT_MSG" --quiet
  echo "[部署腳本] ⬆️  推送至 GitHub Pages..."
  git push origin "$DEPLOY_BRANCH" --quiet
  echo "[部署腳本] ✅ 部署成功！"
  echo "           🌐 https://iflin.github.io/wbc-2026-live-stats/"
fi

# === 半清除 worktree（回到主目錄）===
cd "$REPO_ROOT"
git worktree remove "$WORKTREE_DIR" --force 2>/dev/null || true
rm -rf "$WORKTREE_DIR" 2>/dev/null || true
