# =============================================================================
# 14_table_s1_sensitivity.R
# Supplementary Table S1: Sensitivity analysis across alarm burden time windows
#
# 【目的】
#   主解析で採用した30分窓の頑健性を示すため、5分・10分・30分の3窓で
#   混合ロジスティック回帰結果を横並びに比較するSupplementary Tableを作成する。
#
# 【選択根拠】
#   - AICは30分窓が最小（31,653.3）→ 統計的に最適
#   - 3窓すべてでCRISIS交互作用が非有意、WARNING交互作用が有意に負
#     → 時間窓の選択によらず主要結論は変わらない（robustness確認）
#   - 臨床的にも30分は看護ケアサイクルとして自然な時間単位
#
# 【出力】
#   outputs/stats/tableS1_sensitivity_EN.csv  論文用（UTF-8）
#   outputs/stats/tableS1_sensitivity_JA.csv  発表用（UTF-8 BOM）
# =============================================================================

library(dplyr)
library(tidyr)
library(readr)
library(stringr)

# -----------------------------------------------------------------------------
# 1. データ読み込み
# -----------------------------------------------------------------------------

res_5m  <- read_csv("outputs/tables/11_results_5m.csv",  show_col_types = FALSE)
res_10m <- read_csv("outputs/tables/11_results_10m.csv", show_col_types = FALSE)
res_30m <- read_csv("outputs/tables/11_results_30m.csv", show_col_types = FALSE)
aic_df  <- read_csv("outputs/tables/11_model_comparison.csv", show_col_types = FALSE)

# -----------------------------------------------------------------------------
# 2. term名 → 論文用ラベルのマッピング
# -----------------------------------------------------------------------------

term_labels <- c(
  # 優先度主効果
  "priority_fctWARNING"                  = "  WARNING",
  "priority_fctCRISIS"                   = "  CRISIS",
  # 負荷主効果（ADVISORY基準 = 交互作用なし）
  "burden_5m"                            = "  Per alarm increase (ADVISORY)",
  "burden_10m"                           = "  Per alarm increase (ADVISORY)",
  "alarm_burden"                         = "  Per alarm increase (ADVISORY)",
  # 交互作用項
  "burden_5m:priority_fctWARNING"        = "  Per alarm increase \u00d7 WARNING",
  "burden_5m:priority_fctCRISIS"         = "  Per alarm increase \u00d7 CRISIS",
  "burden_10m:priority_fctWARNING"       = "  Per alarm increase \u00d7 WARNING",
  "burden_10m:priority_fctCRISIS"        = "  Per alarm increase \u00d7 CRISIS",
  "alarm_burden:priority_fctWARNING"     = "  Per alarm increase \u00d7 WARNING",
  "alarm_burden:priority_fctCRISIS"      = "  Per alarm increase \u00d7 CRISIS",
  # 共変量
  "zone_fct高重症"                        = "  Zone: high-acuity (ref: general)",
  "shiftevening"                         = "  Shift: evening (ref: day)",
  "shiftnight"                           = "  Shift: night (ref: day)",
  "weekendTRUE"                          = "  Weekend (ref: weekday)",
  "wardWest 11"                          = "  Ward: West 11 (ref: West 10)"
)

# 表示する行の順序（Interceptは除外）
row_order <- c(
  "  WARNING",
  "  CRISIS",
  "  Per alarm increase (ADVISORY)",
  "  Per alarm increase \u00d7 WARNING",
  "  Per alarm increase \u00d7 CRISIS",
  "  Zone: high-acuity (ref: general)",
  "  Shift: evening (ref: day)",
  "  Shift: night (ref: day)",
  "  Weekend (ref: weekday)",
  "  Ward: West 11 (ref: West 10)"
)

# セクションヘッダー行
section_headers <- tibble(
  label = c(
    "Alarm priority (ref: ADVISORY)",
    "  WARNING",
    "  CRISIS",
    "Alarm burden \u00d7 priority interaction",
    "  Per alarm increase (ADVISORY)",
    "  Per alarm increase \u00d7 WARNING",
    "  Per alarm increase \u00d7 CRISIS",
    "Covariates",
    "  Zone: high-acuity (ref: general)",
    "  Shift: evening (ref: day)",
    "  Shift: night (ref: day)",
    "  Weekend (ref: weekday)",
    "  Ward: West 11 (ref: West 10)"
  )
)

# -----------------------------------------------------------------------------
# 3. OR (95% CI), p値を整形する関数
# -----------------------------------------------------------------------------

fmt_p <- function(p) {
  case_when(
    p < 0.001 ~ "<0.001",
    TRUE      ~ as.character(round(p, 3))
  )
}

fmt_or_ci_p <- function(or, ci_low, ci_high, p) {
  paste0(
    round(or, 3), " (", round(ci_low, 3), "\u2013", round(ci_high, 3), ")",
    "; p", ifelse(p < 0.001, "<0.001", paste0("=", round(p, 3)))
  )
}

format_window <- function(df) {
  df |>
    filter(term != "(Intercept)") |>
    mutate(
      label  = recode(term, !!!term_labels),
      result = fmt_or_ci_p(OR, CI_low, CI_high, p_value)
    ) |>
    select(label, result)
}

fmt_5m  <- format_window(res_5m)
fmt_10m <- format_window(res_10m)
fmt_30m <- format_window(res_30m)

# -----------------------------------------------------------------------------
# 4. Wide形式に結合
# -----------------------------------------------------------------------------

table_s1 <- section_headers |>
  left_join(fmt_5m,  by = "label") |> rename(`5-min window`  = result) |>
  left_join(fmt_10m, by = "label") |> rename(`10-min window` = result) |>
  left_join(fmt_30m, by = "label") |> rename(`30-min window` = result) |>
  rename(Variable = label)

# AIC行を追記
aic_row <- tibble(
  Variable        = "AIC",
  `5-min window`  = as.character(round(aic_df$AIC[aic_df$window == "5min"],  1)),
  `10-min window` = as.character(round(aic_df$AIC[aic_df$window == "10min"], 1)),
  `30-min window` = as.character(round(aic_df$AIC[aic_df$window == "30min"], 1))
)

table_s1 <- bind_rows(table_s1, aic_row)

# -----------------------------------------------------------------------------
# 5. 保存
# -----------------------------------------------------------------------------

write_excel_csv(table_s1, "outputs/stats/tableS1_sensitivity_EN.csv")

# 日本語版: Variable列のみ翻訳
table_s1_JA <- table_s1 |>
  mutate(Variable = recode(Variable,
    "Alarm priority (ref: ADVISORY)"          = "アラーム優先度（参照: ADVISORY）",
    "  WARNING"                               = "  WARNING",
    "  CRISIS"                                = "  CRISIS",
    "Alarm burden \u00d7 priority interaction" = "アラーム負荷 × 優先度 交互作用",
    "  Per alarm increase (ADVISORY)"         = "  1件増加あたり（ADVISORY）",
    "  Per alarm increase \u00d7 WARNING"      = "  1件増加 × WARNING",
    "  Per alarm increase \u00d7 CRISIS"       = "  1件増加 × CRISIS",
    "Covariates"                              = "共変量",
    "  Zone: high-acuity (ref: general)"      = "  ゾーン: 高重症（参照: 一般）",
    "  Shift: evening (ref: day)"             = "  シフト: 夕方（参照: 日勤）",
    "  Shift: night (ref: day)"               = "  シフト: 夜間（参照: 日勤）",
    "  Weekend (ref: weekday)"                = "  週末（参照: 平日）",
    "  Ward: West 11 (ref: West 10)"          = "  病棟: 西11（参照: 西10）",
    "AIC"                                     = "AIC"
  )) |>
  rename("項目" = Variable)

write_excel_csv(table_s1_JA, "outputs/stats/tableS1_sensitivity_JA.csv")

cat("保存完了:\n")
cat("  outputs/stats/tableS1_sensitivity_EN.csv\n")
cat("  outputs/stats/tableS1_sensitivity_JA.csv\n")

# -----------------------------------------------------------------------------
# 6. コンソール確認
# -----------------------------------------------------------------------------

cat("\n========== Table S1 (EN) ==========\n")
print(table_s1, n = Inf, width = Inf)
