# =============================================================================
# 13_observed_silencing_table.R
# Priority × Burden Quartile 別 観察消音率表（Table 2候補）
#
# 【目的】
#   混合ロジスティック回帰（Table 3）の解釈補助として、
#   各 priority × alarm_burden quartile セルの観察消音率と
#   95% Wilson信頼区間を集計する。
#
# 【出力】
#   outputs/tables/13_observed_silencing_long.csv  （縦持ち・詳細）
#   outputs/tables/13_observed_silencing_table.csv （横持ち・論文用）
# =============================================================================

library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(binom)   # Wilson CI: install.packages("binom")

# -----------------------------------------------------------------------------
# 1. データ読み込み
# -----------------------------------------------------------------------------

df <- readRDS("data/proceeded/03_analysis_ready.rds")

cat("読み込み完了:", nrow(df), "件\n")
cat("優先度分布:\n"); print(table(df$priority_fct))
cat("四分位分布:\n"); print(table(df$burden_quartile))

# -----------------------------------------------------------------------------
# 2. Priority × Burden Quartile 別 消音率集計
# -----------------------------------------------------------------------------

# Quartile ラベル（Q1〜Q4の件数範囲）を取得
q_labels <- df |>
  group_by(burden_quartile) |>
  summarise(
    min_b = min(alarm_burden),
    max_b = max(alarm_burden),
    .groups = "drop"
  ) |>
  mutate(q_label = paste0("Q", burden_quartile, " (", min_b, "\u2013", max_b, ")"))

# 集計
summary_long <- df |>
  group_by(priority_fct, burden_quartile) |>
  summarise(
    n          = n(),
    n_silenced = sum(silenced, na.rm = TRUE),
    .groups    = "drop"
  ) |>
  mutate(
    # Wilson 信頼区間
    ci        = map2(n_silenced, n, ~ binom.confint(.x, .y, methods = "wilson")),
    rate_pct  = map_dbl(ci, ~ .x$mean  * 100),
    ci_lower  = map_dbl(ci, ~ .x$lower * 100),
    ci_upper  = map_dbl(ci, ~ .x$upper * 100),
    # 表示用セル文字列: "8.1% [5.9–10.9] (n=1,234)"
    cell = sprintf("%.1f%% [%.1f\u2013%.1f]\n(n=%s)",
                   rate_pct, ci_lower, ci_upper,
                   format(n, big.mark = ","))
  ) |>
  left_join(q_labels |> select(burden_quartile, q_label), by = "burden_quartile") |>
  select(priority_fct, q_label, rate_pct, ci_lower, ci_upper, n, n_silenced, cell)

# -----------------------------------------------------------------------------
# 3. 横持ち整形（論文テーブル用）
# -----------------------------------------------------------------------------

table_wide <- summary_long |>
  select(priority_fct, q_label, cell) |>
  pivot_wider(names_from = q_label, values_from = cell) |>
  rename(Priority = priority_fct) |>
  mutate(Priority = factor(Priority, levels = c("ADVISORY", "WARNING", "CRISIS"))) |>
  arrange(Priority)

cat("\n=== 観察消音率表（横持ち） ===\n")
print(table_wide)

# -----------------------------------------------------------------------------
# 4. CSV 保存
# -----------------------------------------------------------------------------

write_excel_csv(
  summary_long |> select(-cell),
  "outputs/tables/13_observed_silencing_long.csv"
)

write_excel_csv(
  table_wide,
  "outputs/tables/13_observed_silencing_table.csv"
)

cat("\n保存完了:\n")
cat("  outputs/tables/13_observed_silencing_long.csv\n")
cat("  outputs/tables/13_observed_silencing_table.csv\n")
