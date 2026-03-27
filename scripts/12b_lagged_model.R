# =============================================================================
# 12b_lagged_model.R
# 探索的感度分析: lagged alarm burden を追加した混合ロジスティック回帰
#
# 【目的】
#   主解析モデルに burden_lag（2〜1時間前の臨床アラーム件数）を追加し、
#   即時効果（alarm_burden: 直前30分）と遅延効果（burden_lag）を同時推定する。
#
# 【モデル構造】
#   Model A（主解析再現）:
#     silenced ~ alarm_burden * priority_fct +
#                zone_fct + shift + weekend + ward + (1 | ベッド名)
#
#   Model B（lagged burden追加）:
#     silenced ~ alarm_burden * priority_fct + burden_lag +
#                zone_fct + shift + weekend + ward + (1 | ベッド名)
#
#   burden_lag は主効果のみ追加（交互作用は先にシンプルなモデルで確認）
#
# 【出力】
#   outputs/tables/12b_lagged_model_results.csv
#   outputs/tables/12b_model_comparison.csv
# =============================================================================

library(dplyr)
library(lubridate)
library(lme4)
library(broom.mixed)
library(readr)

# VIF計算用
# install.packages("car") が必要な場合あり
library(car)

# -----------------------------------------------------------------------------
# 0. データ読み込み・前処理
# -----------------------------------------------------------------------------

df <- readRDS("data/proceeded/04_analysis_lagged.rds")
cat("読み込み件数:", nrow(df), "\n")

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

# -----------------------------------------------------------------------------
# 1. Model A: 主解析モデル（再現）
# -----------------------------------------------------------------------------

cat("\n=== Model A: 主解析モデル（再現）===\n")

fit_A <- glmer(
  silenced ~ alarm_burden * priority_fct +
    zone_fct + shift + weekend + ward + (1 | ベッド名),
  data    = df_model,
  family  = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

cat("収束確認 - isSingular:", isSingular(fit_A), "\n")
cat("AIC:", AIC(fit_A), "/ BIC:", BIC(fit_A), "\n")

# -----------------------------------------------------------------------------
# 2. Model B: burden_lag 追加モデル
# -----------------------------------------------------------------------------

cat("\n=== Model B: burden_lag 追加モデル ===\n")

fit_B <- tryCatch(
  glmer(
    silenced ~ alarm_burden * priority_fct + burden_lag +
      zone_fct + shift + weekend + ward + (1 | ベッド名),
    data    = df_model,
    family  = binomial,
    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
  ),
  warning = function(w) {
    cat("  [WARNING]", conditionMessage(w), "\n")
    suppressWarnings(
      glmer(
        silenced ~ alarm_burden * priority_fct + burden_lag +
          zone_fct + shift + weekend + ward + (1 | ベッド名),
        data    = df_model,
        family  = binomial,
        control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
      )
    )
  }
)

cat("収束確認 - isSingular:", isSingular(fit_B), "\n")
cat("AIC:", AIC(fit_B), "/ BIC:", BIC(fit_B), "\n")

# -----------------------------------------------------------------------------
# 3. AIC/BIC 比較
# -----------------------------------------------------------------------------

cat("\n=== AIC/BIC 比較 ===\n")

comparison <- data.frame(
  model       = c("A: 主解析（burden 30min のみ）",
                  "B: burden_lag 追加（30min + 2〜1h前）"),
  n_params    = c(length(fixef(fit_A)), length(fixef(fit_B))),
  AIC         = c(AIC(fit_A), AIC(fit_B)),
  BIC         = c(BIC(fit_A), BIC(fit_B)),
  is_singular = c(isSingular(fit_A), isSingular(fit_B))
) |>
  mutate(dAIC = AIC - min(AIC),
         dBIC = BIC - min(BIC))

print(comparison)

write_excel_csv(comparison, "outputs/tables/12b_model_comparison.csv")
cat("\n保存: outputs/tables/12b_model_comparison.csv\n")

# -----------------------------------------------------------------------------
# 4. VIF確認（Model B）
# -----------------------------------------------------------------------------

cat("\n=== VIF（Model B 固定効果）===\n")
vif_B <- vif(fit_B)
print(vif_B)

max_vif <- max(vif_B[, "GVIF"])
if (max_vif > 10) {
  cat("\n[注意] VIF > 10 の変数あり → 多重共線性の問題が深刻\n")
} else if (max_vif > 5) {
  cat("\n[注意] VIF > 5 の変数あり → 多重共線性に注意\n")
} else {
  cat("\n[OK] すべてのVIF < 5 → 多重共線性の問題なし\n")
}

# -----------------------------------------------------------------------------
# 5. Model B の結果テーブル
# -----------------------------------------------------------------------------

cat("\n=== Model B 結果（OR, 95%CI）===\n")

results_B <- tidy(fit_B, effects = "fixed", conf.int = TRUE, exponentiate = TRUE) |>
  mutate(
    OR      = round(estimate, 3),
    CI_low  = round(conf.low, 3),
    CI_high = round(conf.high, 3),
    p_value = round(p.value, 4)
  ) |>
  select(term, OR, CI_low, CI_high, p_value)

print(results_B, n = Inf)

write_excel_csv(results_B, "outputs/tables/12b_lagged_model_results.csv")
cat("\n保存: outputs/tables/12b_lagged_model_results.csv\n")

# -----------------------------------------------------------------------------
# 6. burden_lag の係数に注目したサマリー
# -----------------------------------------------------------------------------

cat("\n=== burden_lag の効果（注目箇所）===\n")
lag_row <- results_B |> filter(term == "burden_lag")
cat("  burden_lag OR    :", lag_row$OR, "\n")
cat("  95%CI            :", lag_row$CI_low, "–", lag_row$CI_high, "\n")
cat("  p値              :", lag_row$p_value, "\n")

if (lag_row$p_value < 0.05) {
  cat("  → 遅延効果が有意（2〜1時間前の負荷が消音率に影響）\n")
} else {
  cat("  → 遅延効果は非有意（直近の負荷のみが消音率に影響）\n")
}
