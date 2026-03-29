# =============================================================================
# 20_recurrence_analysis.R
# 再発アラーム分析（Proxy for appropriateness）
#
# 【目的】
#   消音後5分以内に同一ベッド・同一パラメータのアラームが再発したかを検証。
#   - 再発なし → 介入が奏効した可能性（真の対応）
#   - 再発あり → 消音が一時しのぎだった可能性
#
# 【方法】
#   消音アラームの消音時刻（datetime + response_sec）から5分以内に
#   同一ベッド・同一内容１のアラームが存在するかを判定。
# =============================================================================

library(dplyr)
library(lubridate)
library(readr)

set.seed(42)

# -----------------------------------------------------------------------------
# 0. データ読み込み
# -----------------------------------------------------------------------------

df <- readRDS("data/proceeded/02_cleaned.rds")

clinical <- df |>
  filter(alarm_class == "clinical") |>
  mutate(row_id = row_number())

# -----------------------------------------------------------------------------
# 1. 消音アラームの消音時刻を計算
# -----------------------------------------------------------------------------

silenced_df <- clinical |>
  filter(silenced == TRUE) |>
  mutate(
    silence_time      = datetime + seconds(response_sec),
    recur_window_end  = silence_time + minutes(5)
  )

cat(sprintf("消音臨床アラーム: %d件\n", nrow(silenced_df)))

# -----------------------------------------------------------------------------
# 2. 再発判定（同一ベッド × 同一パラメータ × 消音後5分以内）
# -----------------------------------------------------------------------------

# 全臨床アラームを参照用テーブルとして準備
all_ref <- clinical |>
  select(ref_id = row_id, ベッド名, 内容１, ref_time = datetime)

# 消音アラームと結合して再発を検索
recur_result <- silenced_df |>
  select(row_id, ベッド名, 内容１, 優先度, silence_time, recur_window_end) |>
  left_join(all_ref, by = c("ベッド名", "内容１"), relationship = "many-to-many") |>
  filter(
    ref_time > silence_time,
    ref_time <= recur_window_end,
    ref_id != row_id          # 自分自身を除く
  ) |>
  group_by(row_id) |>
  summarise(n_recur = n(), .groups = "drop")

# 消音アラームに再発フラグを付与
silenced_flagged <- silenced_df |>
  left_join(recur_result, by = "row_id") |>
  mutate(
    n_recur   = coalesce(n_recur, 0L),
    recurred  = n_recur > 0
  )

# -----------------------------------------------------------------------------
# 3. 集計
# -----------------------------------------------------------------------------

cat("\n=== 優先度別 再発率（消音後5分以内） ===\n")

summary_tbl <- silenced_flagged |>
  group_by(優先度) |>
  summarise(
    n_silenced  = n(),
    n_recurred  = sum(recurred),
    recur_rate  = mean(recurred) * 100,
    .groups     = "drop"
  ) |>
  arrange(factor(優先度, levels = c("ADVISORY", "WARNING", "CRISIS")))

print(summary_tbl)

cat("\n=== 全体 ===\n")
cat(sprintf("消音件数: %d\n再発あり: %d (%.1f%%)\n再発なし: %d (%.1f%%)\n",
    nrow(silenced_flagged),
    sum(silenced_flagged$recurred),
    mean(silenced_flagged$recurred) * 100,
    sum(!silenced_flagged$recurred),
    mean(!silenced_flagged$recurred) * 100))

# -----------------------------------------------------------------------------
# 4. CRISISの詳細（パラメータ別）
# -----------------------------------------------------------------------------

cat("\n=== CRISISアラームの再発詳細（パラメータ別） ===\n")
crisis_detail <- silenced_flagged |>
  filter(優先度 == "CRISIS") |>
  group_by(内容１) |>
  summarise(
    n_silenced = n(),
    n_recurred = sum(recurred),
    recur_rate = mean(recurred) * 100,
    .groups    = "drop"
  )
print(crisis_detail)

# -----------------------------------------------------------------------------
# 5. 時間窓の感度分析（2分・5分・10分）
# -----------------------------------------------------------------------------

cat("\n=== 時間窓別 再発率（感度分析） ===\n")

for (window_min in c(2, 5, 10)) {
  tmp <- silenced_df |>
    mutate(recur_window_end = silence_time + minutes(window_min)) |>
    select(row_id, ベッド名, 内容１, 優先度, silence_time, recur_window_end) |>
    left_join(all_ref, by = c("ベッド名", "内容１"), relationship = "many-to-many") |>
    filter(ref_time > silence_time, ref_time <= recur_window_end, ref_id != row_id) |>
    group_by(row_id) |>
    summarise(recurred = TRUE, .groups = "drop")

  tmp_flagged <- silenced_df |>
    left_join(tmp, by = "row_id") |>
    mutate(recurred = coalesce(recurred, FALSE))

  res <- tmp_flagged |>
    group_by(優先度) |>
    summarise(
      n = n(),
      recur_rate = mean(recurred) * 100,
      .groups = "drop"
    ) |>
    arrange(factor(優先度, levels = c("ADVISORY", "WARNING", "CRISIS")))

  cat(sprintf("\n[%d分窓]\n", window_min))
  print(res)
}

# -----------------------------------------------------------------------------
# 6. 保存
# -----------------------------------------------------------------------------

write_excel_csv(summary_tbl, "outputs/stats/recurrence_summary.csv")
cat("\n保存: outputs/stats/recurrence_summary.csv\n")
