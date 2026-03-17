# =============================================================================
# 15_table_s2_total_burden.R
# Supplementary Table S2: Sensitivity analysis using total alarm burden
#                         (clinical + technical alarms combined)
#
# 【目的】
#   主解析では alarm_burden（臨床アラームのみの前30分負荷）を使用したが、
#   看護師が実際に晒されるノイズは技術的アラームを含む。
#   alarm_burden_total（技術的アラーム含む総負荷）を用いた場合でも
#   主要結論が変わらないことを示す感度分析。
#
# 【比較】
#   Primary   : alarm_burden       臨床アラームのみ（前30分）← 主解析
#   Sensitivity: alarm_burden_total 技術的＋臨床アラーム合計（前30分）
#
# 【出力】
#   outputs/stats/tableS2_total_burden_EN.csv  論文用（UTF-8 BOM）
#   outputs/stats/tableS2_total_burden_JA.csv  発表用（UTF-8 BOM）
# =============================================================================

library(dplyr)
library(readr)
library(lme4)
library(broom.mixed)

# -----------------------------------------------------------------------------
# 1. データ読み込み
# -----------------------------------------------------------------------------

df <- readRDS("data/proceeded/03_analysis_ready.rds")

cat("N =", nrow(df), "\n")
cat("alarm_burden_total 要約:\n")
print(summary(df$alarm_burden_total))

# スケーリング: 主解析と単位を合わせるため10件単位に変換
# alarm_burden は中央値18件、alarm_burden_total は中央値38件と異なるため
# 比較のため両変数とも生の値（per alarm）でモデルに投入する
df <- df |>
  mutate(
    shift = case_when(
      hour >= 8  & hour < 17 ~ "day",
      hour >= 17 & hour < 22 ~ "evening",
      TRUE                   ~ "night"
    ),
    shift   = factor(shift, levels = c("day", "evening", "night")),
    weekend = dow %in% c(1, 7)  # 1=日, 7=土
  )

# -----------------------------------------------------------------------------
# 2. 感度分析モデル（alarm_burden_total 使用）
# -----------------------------------------------------------------------------

# alarm_burden_totalを10件単位にスケール（係数を見やすくするため）
# → 主解析と同じ1件単位で比較できるよう生値のまま投入
model_total <- glmer(
  silenced ~ alarm_burden_total * priority_fct +
    zone_fct + shift + weekend + ward +
    (1 | ベッド名),
  data   = df,
  family = binomial(link = "logit"),
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

cat("\nモデル収束確認:\n")
print(summary(model_total)$optinfo$conv)

# -----------------------------------------------------------------------------
# 3. 結果を整形
# -----------------------------------------------------------------------------

fmt_p <- function(p) {
  case_when(p < 0.001 ~ "<0.001", TRUE ~ as.character(round(p, 3)))
}

fmt_or_ci_p <- function(or, ci_low, ci_high, p) {
  paste0(
    round(or, 3), " (", round(ci_low, 3), "\u2013", round(ci_high, 3), ")",
    "; p", ifelse(p < 0.001, "<0.001", paste0("=", round(p, 3)))
  )
}

# 主解析結果（既存CSV）
res_primary <- read_csv("outputs/tables/11_results_30m.csv", show_col_types = FALSE) |>
  filter(term != "(Intercept)") |>
  mutate(result_primary = fmt_or_ci_p(OR, CI_low, CI_high, p_value)) |>
  select(term, result_primary)

# 感度分析結果
res_total <- tidy(model_total, effects = "fixed", conf.int = TRUE, exponentiate = TRUE) |>
  filter(term != "(Intercept)") |>
  mutate(result_total = fmt_or_ci_p(estimate, conf.low, conf.high, p.value)) |>
  select(term, result_total)

# -----------------------------------------------------------------------------
# 4. term名 → ラベルマッピング
# -----------------------------------------------------------------------------

term_labels <- c(
  "priority_fctWARNING"                         = "  WARNING",
  "priority_fctCRISIS"                          = "  CRISIS",
  "alarm_burden"                                = "  Per alarm increase (ADVISORY)",
  "alarm_burden_total"                          = "  Per alarm increase (ADVISORY)",
  "alarm_burden:priority_fctWARNING"            = "  Per alarm increase \u00d7 WARNING",
  "alarm_burden:priority_fctCRISIS"             = "  Per alarm increase \u00d7 CRISIS",
  "alarm_burden_total:priority_fctWARNING"      = "  Per alarm increase \u00d7 WARNING",
  "alarm_burden_total:priority_fctCRISIS"       = "  Per alarm increase \u00d7 CRISIS",
  "zone_fct高重症"                               = "  Zone: high-acuity (ref: general)",
  "shiftevening"                                = "  Shift: evening (ref: day)",
  "shiftnight"                                  = "  Shift: night (ref: day)",
  "weekendTRUE"                                 = "  Weekend (ref: weekday)",
  "wardWest 11"                                 = "  Ward: West 11 (ref: West 10)"
)

res_primary <- res_primary |>
  mutate(label = recode(term, !!!term_labels)) |>
  select(label, result_primary)

res_total <- res_total |>
  mutate(label = recode(term, !!!term_labels)) |>
  select(label, result_total)

# セクションヘッダー
section_headers <- tibble(label = c(
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
))

# -----------------------------------------------------------------------------
# 5. Wide形式に結合
# -----------------------------------------------------------------------------

table_s2 <- section_headers |>
  left_join(res_primary, by = "label") |>
  left_join(res_total,   by = "label") |>
  rename(
    Variable                                 = label,
    `Primary (clinical burden, 30-min)`      = result_primary,
    `Sensitivity (total burden, 30-min)`     = result_total
  )

# AIC行追記
aic_primary <- round(AIC(model_total) * NA + 31653.3, 1)  # 既存値
aic_total   <- round(AIC(model_total), 1)

aic_row <- tibble(
  Variable                             = "AIC",
  `Primary (clinical burden, 30-min)`  = "31,653.3",
  `Sensitivity (total burden, 30-min)` = format(aic_total, big.mark = ",")
)

table_s2 <- bind_rows(table_s2, aic_row)

# -----------------------------------------------------------------------------
# 6. 保存
# -----------------------------------------------------------------------------

write_excel_csv(table_s2, "outputs/stats/tableS2_total_burden_EN.csv")

table_s2_JA <- table_s2 |>
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
  rename(
    "項目"                   = Variable,
    "主解析（臨床負荷・30分）" = `Primary (clinical burden, 30-min)`,
    "感度分析（総負荷・30分）" = `Sensitivity (total burden, 30-min)`
  )

write_excel_csv(table_s2_JA, "outputs/stats/tableS2_total_burden_JA.csv")

cat("\n保存完了:\n")
cat("  outputs/stats/tableS2_total_burden_EN.csv\n")
cat("  outputs/stats/tableS2_total_burden_JA.csv\n")

# -----------------------------------------------------------------------------
# 7. コンソール確認
# -----------------------------------------------------------------------------

cat("\n========== Table S2 (EN) ==========\n")
print(table_s2, n = Inf, width = Inf)
