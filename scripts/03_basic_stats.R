# =============================================================================
# Phase 1: 基本統計量の算出
# 総数、1日/1時間あたり件数、優先度別割合等
# =============================================================================

library(dplyr)
library(readr)
library(lubridate)

df <- readRDS("data/proceeded/02_cleaned.rds")

# 観察期間
period_start <- as.Date("2025-07-15")
period_end   <- as.Date("2025-09-09")
n_days <- as.numeric(period_end - period_start) + 1

cat("観察期間:", format(period_start), "~", format(period_end),
    "(", n_days, "日間 )\n")
cat("総アラーム件数:", nrow(df), "\n\n")

# -----------------------------------------------------------------------------
# 1. 優先度別の件数・割合
# -----------------------------------------------------------------------------

priority_stats <- df |>
  count(優先度, name = "件数") |>
  mutate(割合_pct = round(件数 / sum(件数) * 100, 1))

cat("=== 優先度別 ===\n")
print(priority_stats)
write_excel_csv(priority_stats, "outputs/stats/stats_priority.csv")

# -----------------------------------------------------------------------------
# 2. 1日あたり件数
# -----------------------------------------------------------------------------

daily <- df |>
  mutate(date = as.Date(datetime)) |>
  count(date, name = "件数") |>
  mutate(
    曜日 = wday(date, label = TRUE, abbr = FALSE, locale = "ja_JP.UTF-8"),
    曜日番号 = wday(date, week_start = 1)  # 1=月曜〜7=日曜
  )

daily_stats <- daily |>
  summarise(
    中央値 = median(件数),
    IQR下限 = quantile(件数, 0.25),
    IQR上限 = quantile(件数, 0.75),
    最小値 = min(件数),
    最大値 = max(件数),
    平均値 = round(mean(件数), 1)
  )

cat("\n=== 1日あたり件数 ===\n")
print(daily_stats)
write_excel_csv(daily, "outputs/stats/stats_daily_count.csv")

# 曜日別の集計
dow_stats <- daily |>
  group_by(曜日番号, 曜日) |>
  summarise(
    日数     = n(),
    中央値   = median(件数),
    IQR下限  = quantile(件数, 0.25),
    IQR上限  = quantile(件数, 0.75),
    平均値   = round(mean(件数), 1),
    .groups  = "drop"
  ) |>
  arrange(曜日番号)

cat("\n=== 曜日別アラーム件数 ===\n")
print(dow_stats)
write_excel_csv(dow_stats, "outputs/stats/stats_dow.csv")

# -----------------------------------------------------------------------------
# 3. 1時間あたり件数（時間帯別）
# -----------------------------------------------------------------------------

hourly <- df |>
  mutate(hour = hour(datetime)) |>
  count(hour, name = "件数") |>
  mutate(per_day_avg = round(件数 / n_days, 1))

cat("\n=== 時間帯別アラーム件数（1日平均）===\n")
print(hourly, n = 24)
write_excel_csv(hourly, "outputs/stats/stats_hourly_count.csv")

# -----------------------------------------------------------------------------
# 4. 内容別（アラーム種別）上位
# -----------------------------------------------------------------------------

content_stats <- df |>
  count(内容１, 優先度, name = "件数") |>
  arrange(desc(件数)) |>
  mutate(割合_pct = round(件数 / nrow(df) * 100, 1))

cat("\n=== アラーム種別 上位15件 ===\n")
print(head(content_stats, 15))
write_excel_csv(content_stats, "outputs/stats/stats_alarm_content.csv")

# -----------------------------------------------------------------------------
# 5. duration_zero / duration_long の内訳
# -----------------------------------------------------------------------------

cat("\n=== フラグ別内訳 ===\n")
cat("duration_zero（0秒）:", sum(df$duration_zero), "件 (",
    round(mean(df$duration_zero)*100,1), "%)\n")
cat("duration_long（1時間超）:", sum(df$duration_long), "件 (",
    round(mean(df$duration_long)*100,1), "%)\n")

# -----------------------------------------------------------------------------
# 6. サマリーテーブルとして保存
# -----------------------------------------------------------------------------

summary_tbl <- tibble(
  項目   = c("観察期間（日数）", "総アラーム件数",
             "1日あたり中央値 [IQR]",
             "ADVISORY件数（割合）", "WARNING件数（割合）", "CRISIS件数（割合）",
             "duration_zero件数（割合）", "duration_long件数（割合）"),
  値     = c(
    paste0(n_days, "日"),
    format(nrow(df), big.mark = ","),
    paste0(daily_stats$中央値, " [", daily_stats$IQR下限, "–", daily_stats$IQR上限, "]"),
    paste0(filter(priority_stats, 優先度=="ADVISORY")$件数, " (",
           filter(priority_stats, 優先度=="ADVISORY")$割合_pct, "%)"),
    paste0(filter(priority_stats, 優先度=="WARNING")$件数, " (",
           filter(priority_stats, 優先度=="WARNING")$割合_pct, "%)"),
    paste0(filter(priority_stats, 優先度=="CRISIS")$件数, " (",
           filter(priority_stats, 優先度=="CRISIS")$割合_pct, "%)"),
    paste0(sum(df$duration_zero), " (", round(mean(df$duration_zero)*100,1), "%)"),
    paste0(sum(df$duration_long), " (", round(mean(df$duration_long)*100,1), "%)")
  )
)

write_excel_csv(summary_tbl, "outputs/stats/stats_summary.csv")
cat("\n保存完了: outputs/stats/stats_summary.csv 他\n")
print(summary_tbl)
