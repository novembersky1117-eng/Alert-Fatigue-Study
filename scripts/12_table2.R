# =============================================================================
# 12_table2.R
# Table 2: 混合ロジスティック回帰の結果テーブル（論文投稿用・英語）
#
# 【対象モデル】
#   11_main_analysis.R で推定した30分窓モデル（AIC最良）
#   silenced ~ alarm_burden * priority_fct +
#              zone_fct + shift + weekend + ward + (1 | ベッド名)
#
# 【出力】
#   outputs/tables/12_table2.csv   （UTF-8 BOM）
# =============================================================================

library(dplyr)
library(lubridate)
library(lme4)
library(broom.mixed)
library(readr)

# -----------------------------------------------------------------------------
# 0. データ読み込み・モデル再推定
# -----------------------------------------------------------------------------

df <- readRDS("data/proceeded/03_analysis_ready.rds")

df_model <- df |>
  mutate(
    minute_of_day = hour(datetime) * 60L + minute(datetime),
    shift = case_when(
      minute_of_day >= 510L  & minute_of_day < 1050L ~ "day",
      minute_of_day >= 1050L & minute_of_day < 1290L ~ "evening",
      TRUE                                            ~ "night"
    ) |> factor(levels = c("day", "evening", "night")),
    weekend = wday(datetime, week_start = 1) >= 6L
  )

cat("モデル推定中（30分窓）...\n")

fit_30m <- glmer(
  silenced ~ alarm_burden * priority_fct +
             zone_fct + shift + weekend + ward +
             (1 | ベッド名),
  data    = df_model,
  family  = binomial,
  control = glmerControl(optimizer = "bobyqa",
                         optCtrl   = list(maxfun = 2e5))
)

cat("収束確認 - isSingular:", isSingular(fit_30m), "\n")

# -----------------------------------------------------------------------------
# 1. p値フォーマット関数
# -----------------------------------------------------------------------------

fmt_p <- function(p) {
  case_when(
    p < 0.001 ~ "<0.001",
    TRUE      ~ sprintf("%.3f", p)
  )
}

# -----------------------------------------------------------------------------
# 2. 固定効果の整形
# -----------------------------------------------------------------------------

fixed <- tidy(fit_30m, effects = "fixed", conf.int = TRUE, exponentiate = TRUE) |>
  mutate(
    OR     = sprintf("%.3f", estimate),
    CI     = sprintf("%.3f – %.3f", conf.low, conf.high),
    p_fmt  = fmt_p(p.value)
  ) |>
  select(term, OR, CI, p_fmt)

# -----------------------------------------------------------------------------
# 3. ランダム効果の分散取得
# -----------------------------------------------------------------------------

re_var <- as.data.frame(VarCorr(fit_30m))$vcov[1]  # ベッドレベルの分散

# -----------------------------------------------------------------------------
# 4. 行ラベルの定義（論文用・英語）
# -----------------------------------------------------------------------------

row_labels <- tribble(
  ~term,                                      ~label,                                        ~section,
  "priority_fctWARNING",                      "  WARNING",                                   "Alarm priority (ref: ADVISORY)",
  "priority_fctCRISIS",                       "  CRISIS",                                    "Alarm priority (ref: ADVISORY)",
  "alarm_burden",                             "  Per alarm increase (ADVISORY)",              "Alarm burden × priority interaction",
  "alarm_burden:priority_fctWARNING",         "  Per alarm increase × WARNING",              "Alarm burden × priority interaction",
  "alarm_burden:priority_fctCRISIS",          "  Per alarm increase × CRISIS",               "Alarm burden × priority interaction",
  "zone_fct高重症",                           "  Zone: high-acuity (ref: general)",          "Covariates",
  "shiftevening",                             "  Shift: evening (ref: day)",                 "Covariates",
  "shiftnight",                               "  Shift: night (ref: day)",                   "Covariates",
  "weekendTRUE",                              "  Weekend (ref: weekday)",                    "Covariates",
  "wardWest 11",                              "  Ward: West 11 (ref: West 10)",              "Covariates"
)

# -----------------------------------------------------------------------------
# 5. テーブル組み立て
# -----------------------------------------------------------------------------

table2_body <- row_labels |>
  left_join(fixed, by = "term") |>
  select(section, label, OR, CI, p_fmt) |>
  rename(
    Section   = section,
    Variable  = label,
    `OR`      = OR,
    `95% CI`  = CI,
    `p-value` = p_fmt
  )

# セクションヘッダー行を挿入
sections <- unique(table2_body$Section)

table2_final <- bind_rows(
  lapply(sections, function(s) {
    header <- tibble(
      Variable  = s,
      OR        = NA_character_,
      `95% CI`  = NA_character_,
      `p-value` = NA_character_
    )
    body <- table2_body |> filter(Section == s) |> select(-Section)
    bind_rows(header, body)
  })
) |>
  # ランダム効果行を末尾に追加
  bind_rows(
    tibble(
      Variable  = "Random effect",
      OR        = NA_character_,
      `95% CI`  = NA_character_,
      `p-value` = NA_character_
    ),
    tibble(
      Variable  = "  Bed-level variance",
      OR        = sprintf("%.3f", re_var),
      `95% CI`  = NA_character_,
      `p-value` = NA_character_
    )
  )

# フッター情報
footer <- tibble(
  Variable  = paste0("Model: 30-min burden window; N = ",
                     format(nrow(df_model), big.mark = ","),
                     "; AIC = ", format(round(AIC(fit_30m), 1), big.mark = ",")),
  OR        = NA_character_,
  `95% CI`  = NA_character_,
  `p-value` = NA_character_
)

table2_out <- bind_rows(table2_final, footer)

# -----------------------------------------------------------------------------
# 6. 確認・保存
# -----------------------------------------------------------------------------

cat("\n=== Table 2 ===\n")
print(table2_out, n = Inf)

write_excel_csv(table2_out, "outputs/tables/12_table2.csv", na = "")
cat("\n保存完了: outputs/tables/12_table2.csv\n")
