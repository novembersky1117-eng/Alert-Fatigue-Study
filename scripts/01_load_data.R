# =============================================================================
# Phase 1: CSVの読み込みと日時型（POSIXct）への変換
# =============================================================================

library(readr)
library(dplyr)
library(lubridate)

# -----------------------------------------------------------------------------
# 1. CSV読み込み
# -----------------------------------------------------------------------------

raw <- read_csv(
  "data/proceeded/alarm_combined_20250715_20250909.csv",
  locale = locale(encoding = "UTF-8"),
  col_types = cols(.default = col_character()),
  show_col_types = FALSE
)

cat("読み込み行数:", nrow(raw), "\n")
cat("列名:", paste(names(raw), collapse = ", "), "\n\n")

# -----------------------------------------------------------------------------
# 2. 日時型への変換
# -----------------------------------------------------------------------------

# hh:mm:ss または h:mm:ss を秒数（numeric）に変換するヘルパー
hms_to_sec <- function(x) {
  # NA や空文字はそのままNA
  ifelse(
    is.na(x) | x == "",
    NA_real_,
    sapply(x, function(v) {
      parts <- as.integer(strsplit(v, ":")[[1]])
      if (length(parts) == 3) parts[1]*3600 + parts[2]*60 + parts[3]
      else NA_real_
    })
  )
}

df <- raw |>
  mutate(
    # イベント発生日時：日付 + 時刻 を結合してPOSIXct
    datetime = ymd_hms(paste(日付, 時刻), tz = "Asia/Tokyo"),

    # アラーム開始・終了時刻
    alarm_start = ymd_hm(開始時刻, tz = "Asia/Tokyo"),
    alarm_end   = ymd_hm(終了時刻,   tz = "Asia/Tokyo"),

    # 持続時間・反応時間を秒数（numeric）に変換
    duration_sec  = hms_to_sec(継続時間),
    response_sec  = hms_to_sec(反応時間),

    # 優先度をfactor（順序付き）
    優先度 = factor(優先度, levels = c("ADVISORY", "WARNING", "CRISIS")),

    # sourceをfactor
    source = factor(source)
  )

# -----------------------------------------------------------------------------
# 3. 変換結果の確認
# -----------------------------------------------------------------------------

cat("=== datetime変換の確認 ===\n")
cat("NA数:", sum(is.na(df$datetime)), "/", nrow(df), "\n")
cat("範囲:", format(min(df$datetime, na.rm=TRUE)), "~",
               format(max(df$datetime, na.rm=TRUE)), "\n\n")

cat("=== alarm_start変換の確認 ===\n")
cat("NA数:", sum(is.na(df$alarm_start)), "/", nrow(df), "\n\n")

cat("=== duration_sec（秒）の確認 ===\n")
cat("NA数:", sum(is.na(df$duration_sec)), "\n")
cat("中央値:", median(df$duration_sec, na.rm=TRUE), "秒\n")
cat("最大値:", max(df$duration_sec, na.rm=TRUE), "秒\n\n")

cat("=== 優先度の内訳 ===\n")
print(table(df$優先度, useNA="ifany"))

cat("\n=== source別行数 ===\n")
print(table(df$source))

# -----------------------------------------------------------------------------
# 4. 保存
# -----------------------------------------------------------------------------

saveRDS(df, "data/proceeded/01_loaded.rds")
cat("\n保存完了: data/proceeded/01_loaded.rds\n")
