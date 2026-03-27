# =============================================================================
# 12d_table_lagged.R
# 探索的感度分析: lagged burden モデル（Model B）の結果テーブル整形
#
# 【目的】
#   12b で推定した Model B（alarm_burden + burden_lag）の結果を
#   既存の tableS1 と同形式で出力する。
#
# 【出力】
#   outputs/stats/tableS_lagged_EN.csv
#   outputs/stats/tableS_lagged_JA.csv
# =============================================================================

library(dplyr)
library(tidyr)
library(readr)

# -----------------------------------------------------------------------------
# 1. データ読み込み
# -----------------------------------------------------------------------------

res <- read_csv("outputs/tables/12b_lagged_model_results.csv", show_col_types = FALSE)
aic_B <- read_csv("outputs/tables/12b_model_comparison.csv", show_col_types = FALSE)

# -----------------------------------------------------------------------------
# 2. OR (95%CI); p値 の整形関数
# -----------------------------------------------------------------------------

fmt_or_ci_p <- function(or, ci_low, ci_high, p) {
  paste0(
    round(or, 3), " (", round(ci_low, 3), "\u2013", round(ci_high, 3), ")",
    "; p", ifelse(p < 0.001, "<0.001", paste0("=", round(p, 3)))
  )
}

res_fmt <- res |>
  filter(term != "(Intercept)") |>
  mutate(
    label = recode(term,
      "alarm_burden"                        = "  Per alarm increase, immediate (ADVISORY)",
      "priority_fctWARNING"                 = "  WARNING",
      "priority_fctCRISIS"                  = "  CRISIS",
      "alarm_burden:priority_fctWARNING"    = "  Per alarm increase, immediate \u00d7 WARNING",
      "alarm_burden:priority_fctCRISIS"     = "  Per alarm increase, immediate \u00d7 CRISIS",
      "burden_lag"                          = "  Per alarm increase, lagged (ADVISORY)",
      "zone_fct高重症"                       = "  Zone: high-acuity (ref: general)",
      "shiftevening"                        = "  Shift: evening (ref: day)",
      "shiftnight"                          = "  Shift: night (ref: day)",
      "weekendTRUE"                         = "  Weekend (ref: weekday)",
      "wardWest 11"                         = "  Ward: West 11 (ref: West 10)"
    ),
    result = fmt_or_ci_p(OR, CI_low, CI_high, p_value)
  ) |>
  select(label, result)

# -----------------------------------------------------------------------------
# 3. セクションヘッダーを加えた Wide 形式
# -----------------------------------------------------------------------------

section_headers <- tibble(
  label = c(
    "Alarm priority (ref: ADVISORY)",
    "  WARNING",
    "  CRISIS",
    "Immediate alarm burden \u00d7 priority interaction",
    "  Per alarm increase, immediate (ADVISORY)",
    "  Per alarm increase, immediate \u00d7 WARNING",
    "  Per alarm increase, immediate \u00d7 CRISIS",
    "Lagged alarm burden (1\u20132 hours prior)",
    "  Per alarm increase, lagged (ADVISORY)",
    "Covariates",
    "  Zone: high-acuity (ref: general)",
    "  Shift: evening (ref: day)",
    "  Shift: night (ref: day)",
    "  Weekend (ref: weekday)",
    "  Ward: West 11 (ref: West 10)"
  )
)

table_lag <- section_headers |>
  left_join(res_fmt, by = "label") |>
  rename(Variable = label, `OR (95% CI); p-value` = result)

# AIC行追記
aic_val <- aic_df <- aic_B |>
  filter(grepl("lag", model)) |>
  pull(AIC) |>
  round(1)

table_lag <- bind_rows(
  table_lag,
  tibble(Variable = "AIC", `OR (95% CI); p-value` = as.character(aic_val))
)

# -----------------------------------------------------------------------------
# 4. 保存
# -----------------------------------------------------------------------------

write_excel_csv(table_lag, "outputs/stats/tableS_lagged_EN.csv")

# 日本語版
table_lag_JA <- table_lag |>
  mutate(Variable = recode(Variable,
    "Alarm priority (ref: ADVISORY)"                    = "アラーム優先度（参照: ADVISORY）",
    "  WARNING"                                         = "  WARNING",
    "  CRISIS"                                          = "  CRISIS",
    "Immediate alarm burden \u00d7 priority interaction" = "即時アラーム負荷 × 優先度 交互作用",
    "  Per alarm increase, immediate (ADVISORY)"        = "  即時1件増加あたり（ADVISORY）",
    "  Per alarm increase, immediate \u00d7 WARNING"    = "  即時1件増加 × WARNING",
    "  Per alarm increase, immediate \u00d7 CRISIS"     = "  即時1件増加 × CRISIS",
    "Lagged alarm burden (1\u20132 hours prior)"        = "遅延アラーム負荷（1〜2時間前）",
    "  Per alarm increase, lagged (ADVISORY)"           = "  遅延1件増加あたり",
    "Covariates"                                        = "共変量",
    "  Zone: high-acuity (ref: general)"                = "  ゾーン: 高重症（参照: 一般）",
    "  Shift: evening (ref: day)"                       = "  シフト: 夕方（参照: 日勤）",
    "  Shift: night (ref: day)"                         = "  シフト: 夜間（参照: 日勤）",
    "  Weekend (ref: weekday)"                          = "  週末（参照: 平日）",
    "  Ward: West 11 (ref: West 10)"                    = "  病棟: 西11（参照: 西10）",
    "AIC"                                               = "AIC"
  )) |>
  rename("項目" = Variable, "OR（95%CI）; p値" = `OR (95% CI); p-value`)

write_excel_csv(table_lag_JA, "outputs/stats/tableS_lagged_JA.csv")

cat("保存完了:\n")
cat("  outputs/stats/tableS_lagged_EN.csv\n")
cat("  outputs/stats/tableS_lagged_JA.csv\n")

# -----------------------------------------------------------------------------
# 5. コンソール確認
# -----------------------------------------------------------------------------

cat("\n========== Table: Lagged Burden Analysis (EN) ==========\n")
print(table_lag, n = Inf, width = Inf)
