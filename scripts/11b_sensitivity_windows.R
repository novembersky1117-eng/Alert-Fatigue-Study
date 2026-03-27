# =============================================================================
# 11b_sensitivity_windows.R
# 感度分析: 混合ロジスティック回帰（時間窓 15 / 30 / 45 分の比較）
#
# 【研究問】
#   alarm burden の時間窓（15 / 30 / 45 分）によって結果が変わるか？
#   主解析（11_main_analysis.R: 5 / 10 / 30 分窓）の頑健性を等間隔の窓で検証する。
#
# 【モデル構造】
#   silenced ~ burden_Xm * priority_fct +
#              zone_fct + shift + weekend + ward +
#              (1 | ベッド名)
#
#   X = 15, 30, 45 の 3 モデルを並列推定し、AIC/BIC で比較
#
# 【時間窓の選択根拠】
#   15 分: 看護師の巡回サイクルに近い、臨床的に意味のある単位
#   30 分: 主解析と同一窓（結果の直接比較が可能）
#   45 分: 短期シフト内の累積負荷を反映する上限窓
#   等間隔の 3 窓で一貫した効果が確認されれば、時間窓選択によるバイアスを否定できる。
#
# 【入力】
#   data/proceeded/03b_analysis_ready_sensitivity.rds（10b スクリプト出力）
#
# 【出力】
#   outputs/tables/11b_model_comparison.csv   AIC/BIC 比較
#   outputs/tables/11b_results_15m.csv        15 分窓モデル結果
#   outputs/tables/11b_results_30m.csv        30 分窓モデル結果
#   outputs/tables/11b_results_45m.csv        45 分窓モデル結果
# =============================================================================

library(dplyr)
library(lubridate)
library(lme4)
library(broom.mixed)
library(readr)

# -----------------------------------------------------------------------------
# 0. データ読み込み
# -----------------------------------------------------------------------------

df <- readRDS("data/proceeded/03b_analysis_ready_sensitivity.rds")

cat("読み込み件数:", nrow(df), "\n")

# -----------------------------------------------------------------------------
# 1. 解析用変数の追加
# -----------------------------------------------------------------------------

df_model <- df |>
  mutate(
    # シフト（実際の看護配置に基づく 3 区分）
    # day    : 08:30–17:29  日勤
    # evening: 17:30–21:29  準夜勤
    # night  : 21:30–08:29  深夜勤
    minute_of_day = hour(datetime) * 60L + minute(datetime),
    shift = case_when(
      minute_of_day >= 510L  & minute_of_day < 1050L ~ "day",
      minute_of_day >= 1050L & minute_of_day < 1290L ~ "evening",
      TRUE                                            ~ "night"
    ) |> factor(levels = c("day", "evening", "night")),

    # 週末フラグ（土=6, 日=7; week_start=1 で月曜起算）
    weekend = wday(datetime, week_start = 1) >= 6L
  )

# -----------------------------------------------------------------------------
# 2. 解析前確認
# -----------------------------------------------------------------------------

cat("\n=== 解析前確認 ===\n")

cat("\npriority × silenced クロス集計:\n")
print(table(df_model$priority_fct, df_model$silenced, dnn = c("priority", "silenced")))

cat("\nシフト分布:\n")
print(table(df_model$shift))

cat("\n週末フラグ分布:\n")
print(table(df_model$weekend, dnn = "weekend"))

cat("\nゾーン分布:\n")
print(table(df_model$zone_fct))

cat("\nward 分布:\n")
print(table(df_model$ward))

# burden 3 変数の要約
cat("\nburen 3 変数の要約:\n")
df_model |>
  summarise(
    across(c(burden_15m, alarm_burden, burden_45m),
           list(min    = min,
                median = median,
                mean   = \(x) round(mean(x), 1),
                max    = max),
           .names = "{.col}__{.fn}")
  ) |>
  tidyr::pivot_longer(everything(),
                      names_to  = c("variable", "stat"),
                      names_sep = "__") |>
  tidyr::pivot_wider(names_from = stat, values_from = value) |>
  print()

# -----------------------------------------------------------------------------
# 3. 3 モデルの並列推定
# -----------------------------------------------------------------------------

burden_vars <- c("burden_15m", "alarm_burden", "burden_45m")

cat("\n=== モデル推定開始 ===\n")

fits <- lapply(burden_vars, function(bv) {
  cat("\n推定中:", bv, "窓モデル ...\n")

  fml <- as.formula(paste0(
    "silenced ~ ", bv, " * priority_fct + ",
    "zone_fct + shift + weekend + ward + (1 | ベッド名)"
  ))

  fit <- tryCatch(
    glmer(fml, data = df_model, family = binomial,
          control = glmerControl(optimizer  = "bobyqa",
                                 optCtrl    = list(maxfun = 2e5))),
    warning = function(w) {
      cat("  [WARNING]", conditionMessage(w), "\n")
      suppressWarnings(
        glmer(fml, data = df_model, family = binomial,
              control = glmerControl(optimizer  = "bobyqa",
                                     optCtrl    = list(maxfun = 2e5)))
      )
    }
  )

  cat("  収束確認 - isSingular:", isSingular(fit), "\n")
  cat("  AIC:", AIC(fit), "/ BIC:", BIC(fit), "\n")

  fit
})

names(fits) <- burden_vars

# -----------------------------------------------------------------------------
# 4. AIC/BIC 比較
# -----------------------------------------------------------------------------

cat("\n=== AIC/BIC 比較 ===\n")

comparison <- data.frame(
  window      = c("15min", "30min", "45min"),
  variable    = burden_vars,
  AIC         = sapply(fits, AIC),
  BIC         = sapply(fits, BIC),
  is_singular = sapply(fits, isSingular),
  row.names   = NULL
) |>
  mutate(
    dAIC = AIC - min(AIC),
    dBIC = BIC - min(BIC)
  )

print(comparison)

write_excel_csv(comparison, "outputs/tables/11b_model_comparison.csv")
cat("\n保存: outputs/tables/11b_model_comparison.csv\n")

# -----------------------------------------------------------------------------
# 5. 結果テーブル（OR, 95%CI）
# -----------------------------------------------------------------------------

format_results <- function(fit, window_label) {
  tidy(fit, effects = "fixed", conf.int = TRUE, exponentiate = TRUE) |>
    mutate(
      window  = window_label,
      OR      = round(estimate, 3),
      CI_low  = round(conf.low, 3),
      CI_high = round(conf.high, 3),
      p_value = round(p.value, 4)
    ) |>
    select(window, term, OR, CI_low, CI_high, p_value)
}

results_list <- list(
  "15min" = format_results(fits[["burden_15m"]],   "15min"),
  "30min" = format_results(fits[["alarm_burden"]], "30min"),
  "45min" = format_results(fits[["burden_45m"]],   "45min")
)

# 各モデルの結果を表示・保存
for (nm in names(results_list)) {
  cat("\n--- モデル結果:", nm, "窓 ---\n")
  print(results_list[[nm]], n = Inf)

  fname <- paste0("outputs/tables/11b_results_", gsub("min", "m", nm), ".csv")
  write_excel_csv(results_list[[nm]], fname)
  cat("保存:", fname, "\n")
}

# -----------------------------------------------------------------------------
# 6. 最適モデルのサマリー
# -----------------------------------------------------------------------------

best_window <- comparison$variable[which.min(comparison$AIC)]
cat("\n=== AIC で選択された最適モデル:", best_window, "===\n")
print(summary(fits[[best_window]]))
