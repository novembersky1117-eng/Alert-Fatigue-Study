# =============================================================================
# 12c_lagged_interaction.R
# 探索的感度分析: burden_lag × priority 交互作用の確認
#
# 【目的】
#   12b で確認した遅延効果（burden_lag）が、
#   優先度（ADVISORY / WARNING / CRISIS）によって異なるかを検証する。
#   主解析（alarm_burden × priority）と同様のパターンが見られるか確認。
#
# 【モデル構造】
#   Model B（再掲）:
#     silenced ~ alarm_burden * priority_fct + burden_lag +
#                zone_fct + shift + weekend + ward + (1 | ベッド名)
#
#   Model C（交互作用追加）:
#     silenced ~ alarm_burden * priority_fct + burden_lag * priority_fct +
#                zone_fct + shift + weekend + ward + (1 | ベッド名)
#
# 【出力】
#   outputs/tables/12c_lagged_interaction_results.csv
#   outputs/tables/12c_model_comparison.csv
# =============================================================================

library(dplyr)
library(lubridate)
library(lme4)
library(broom.mixed)
library(readr)

# -----------------------------------------------------------------------------
# 0. データ読み込み・前処理
# -----------------------------------------------------------------------------

df <- readRDS("data/proceeded/04_analysis_lagged.rds")

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

cat("読み込み件数:", nrow(df_model), "\n")

# -----------------------------------------------------------------------------
# 1. Model B: burden_lag 主効果のみ（12b の再現）
# -----------------------------------------------------------------------------

cat("\n=== Model B: burden_lag 主効果のみ ===\n")

fit_B <- glmer(
  silenced ~ alarm_burden * priority_fct + burden_lag +
    zone_fct + shift + weekend + ward + (1 | ベッド名),
  data    = df_model,
  family  = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

cat("収束確認 - isSingular:", isSingular(fit_B), "\n")
cat("AIC:", AIC(fit_B), "/ BIC:", BIC(fit_B), "\n")

# -----------------------------------------------------------------------------
# 2. Model C: burden_lag × priority 交互作用追加
# -----------------------------------------------------------------------------

cat("\n=== Model C: burden_lag × priority 交互作用追加 ===\n")

fit_C <- tryCatch(
  glmer(
    silenced ~ alarm_burden * priority_fct + burden_lag * priority_fct +
      zone_fct + shift + weekend + ward + (1 | ベッド名),
    data    = df_model,
    family  = binomial,
    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
  ),
  warning = function(w) {
    cat("  [WARNING]", conditionMessage(w), "\n")
    suppressWarnings(
      glmer(
        silenced ~ alarm_burden * priority_fct + burden_lag * priority_fct +
          zone_fct + shift + weekend + ward + (1 | ベッド名),
        data    = df_model,
        family  = binomial,
        control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
      )
    )
  }
)

cat("収束確認 - isSingular:", isSingular(fit_C), "\n")
cat("AIC:", AIC(fit_C), "/ BIC:", BIC(fit_C), "\n")

# -----------------------------------------------------------------------------
# 3. AIC/BIC 比較
# -----------------------------------------------------------------------------

cat("\n=== AIC/BIC 比較 ===\n")

comparison <- data.frame(
  model       = c("B: burden_lag 主効果のみ",
                  "C: burden_lag × priority 交互作用追加"),
  n_params    = c(length(fixef(fit_B)), length(fixef(fit_C))),
  AIC         = c(AIC(fit_B), AIC(fit_C)),
  BIC         = c(BIC(fit_B), BIC(fit_C)),
  is_singular = c(isSingular(fit_B), isSingular(fit_C))
) |>
  mutate(dAIC = AIC - min(AIC),
         dBIC = BIC - min(BIC))

print(comparison)

write_excel_csv(comparison, "outputs/tables/12c_model_comparison.csv")
cat("\n保存: outputs/tables/12c_model_comparison.csv\n")

# -----------------------------------------------------------------------------
# 4. Model C の結果テーブル
# -----------------------------------------------------------------------------

cat("\n=== Model C 結果（OR, 95%CI）===\n")

results_C <- tidy(fit_C, effects = "fixed", conf.int = TRUE, exponentiate = TRUE) |>
  mutate(
    OR      = round(estimate, 3),
    CI_low  = round(conf.low, 3),
    CI_high = round(conf.high, 3),
    p_value = round(p.value, 4)
  ) |>
  select(term, OR, CI_low, CI_high, p_value)

print(results_C, n = Inf)

write_excel_csv(results_C, "outputs/tables/12c_lagged_interaction_results.csv")
cat("\n保存: outputs/tables/12c_lagged_interaction_results.csv\n")

# -----------------------------------------------------------------------------
# 5. 交互作用項に注目したサマリー
# -----------------------------------------------------------------------------

cat("\n=== burden_lag 関連項の効果（注目箇所）===\n")

lag_terms <- results_C |>
  filter(grepl("burden_lag", term))

print(lag_terms, n = Inf)

cat("\n解釈:\n")
for (i in seq_len(nrow(lag_terms))) {
  row <- lag_terms[i, ]
  sig <- if (row$p_value < 0.05) "有意" else "非有意"
  cat(sprintf("  %-45s OR=%s, p=%s → %s\n",
              row$term, row$OR, row$p_value, sig))
}

# -----------------------------------------------------------------------------
# 6. 尤度比検定（Model B vs Model C）
# -----------------------------------------------------------------------------

cat("\n=== 尤度比検定（交互作用項の必要性）===\n")
lrt <- anova(fit_B, fit_C)
print(lrt)
