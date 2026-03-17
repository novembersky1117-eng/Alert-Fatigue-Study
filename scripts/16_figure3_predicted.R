# =============================================================================
# 16_figure3_predicted.R
# Figure 3: モデル予測値による alarm burden × priority 交互作用プロット
#
# 【目的】
#   30分窓混合ロジスティック回帰モデルの予測値を用いて、
#   alarm burden（連続）× priority の交互作用を視覚化する。
#   観察値ベースの四分位プロット（13_interaction_plot.R）に代わる、
#   論文メインフィギュアとして設計。
#
# 【設計の根拠】
#   - X軸を連続値にすることで、傾きの差（= 交互作用）が直接視覚化できる
#   - モデルの共変量（zone, shift, weekend, ward）を基準値に固定した
#     population-level 予測値を使用（ggeffects::ggpredict, type="fixed"）
#   - 予測線 + 95% CI リボンで不確実性を提示
#
# 【出力】
#   outputs/figures/journal/16_figure3_predicted.pdf      (3.5 × 3.5 inch)
#   outputs/figures/presentation/16_figure3_predicted.pdf (10 × 7 inch)
# =============================================================================

library(dplyr)
library(lubridate)
library(lme4)
library(ggplot2)

# ggeffects が未インストールの場合はインストール
if (!requireNamespace("ggeffects", quietly = TRUE)) {
  install.packages("ggeffects", repos = "https://cran.r-project.org")
}
library(ggeffects)

# -----------------------------------------------------------------------------
# 0. データ読み込み・モデル変数作成（11_main_analysis.R と同一）
# -----------------------------------------------------------------------------

df <- readRDS("data/proceeded/03_analysis_ready.rds")
cat("読み込み件数:", nrow(df), "\n")

df_model <- df |>
  mutate(
    minute_of_day = hour(datetime) * 60L + minute(datetime),
    shift = case_when(
      minute_of_day >= 510L  & minute_of_day < 1050L ~ "day",
      minute_of_day >= 1050L & minute_of_day < 1290L ~ "evening",
      TRUE                                            ~ "night"
    ) |> factor(levels = c("day", "evening", "night")),
    weekend  = as.integer(wday(datetime, week_start = 1) >= 6L),
    silenced = as.integer(silenced)
  )

# -----------------------------------------------------------------------------
# 1. 30分窓モデルの再推定
#    （モデルオブジェクトは保存されていないため再推定）
# -----------------------------------------------------------------------------

cat("\n=== 30分窓モデル推定中 ===\n")

fit_30m <- glmer(
  silenced ~ alarm_burden * priority_fct +
    zone_fct + shift + weekend + ward + (1 | ベッド名),
  data    = df_model,
  family  = binomial,
  control = glmerControl(optimizer = "bobyqa",
                         optCtrl   = list(maxfun = 2e5))
)

cat("収束確認 - isSingular:", isSingular(fit_30m), "\n")
cat("AIC:", AIC(fit_30m), "\n")

# -----------------------------------------------------------------------------
# 2. 予測値の取得
#    - alarm_burden: 0〜80 の連続値（97件まであるが外れ値が多いため80で打ち切り）
#    - priority_fct: 3水準
#    - type = "fixed": population-level 予測（ランダム効果 = 0）
#      共変量（zone, shift, weekend, ward）は参照水準に固定
# -----------------------------------------------------------------------------

pred <- ggpredict(
  fit_30m,
  terms           = c("alarm_burden [0:80 by=1]", "priority_fct"),
  type            = "fixed",
  bias_correction = TRUE
)

pred_df <- as.data.frame(pred) |>
  rename(
    alarm_burden = x,
    priority_fct = group,
    pred         = predicted,
    ci_low       = conf.low,
    ci_high      = conf.high
  ) |>
  mutate(
    pred_pct    = pred    * 100,
    ci_low_pct  = ci_low  * 100,
    ci_high_pct = ci_high * 100,
    priority_fct = factor(priority_fct,
                          levels = c("ADVISORY", "WARNING", "CRISIS"))
  )

cat("\n=== 予測値サマリー（burden=0, 40, 80） ===\n")
print(pred_df[pred_df$alarm_burden %in% c(0, 40, 80),
              c("alarm_burden", "priority_fct", "pred_pct", "ci_low_pct", "ci_high_pct")])

# -----------------------------------------------------------------------------
# 3. カラー・スタイル設定
# -----------------------------------------------------------------------------

priority_colors <- c(
  "ADVISORY" = "#999999",
  "WARNING"  = "#E69F00",
  "CRISIS"   = "#D55E00"
)

# -----------------------------------------------------------------------------
# 4. プロット関数
# -----------------------------------------------------------------------------

make_plot <- function(base_size, line_size, ribbon_alpha) {
  ggplot(pred_df,
         aes(x     = alarm_burden,
             y     = pred_pct,
             color = priority_fct,
             fill  = priority_fct,
             group = priority_fct)) +
    geom_ribbon(
      aes(ymin = ci_low_pct, ymax = ci_high_pct),
      alpha = ribbon_alpha,
      color = NA
    ) +
    geom_line(linewidth = line_size) +
    scale_color_manual(values = priority_colors, name = "Alarm priority") +
    scale_fill_manual( values = priority_colors, name = "Alarm priority") +
    scale_y_continuous(
      limits = c(0, 100),
      breaks = seq(0, 100, 20),
      labels = paste0(seq(0, 100, 20), "%")
    ) +
    scale_x_continuous(
      limits = c(0, 80),
      breaks = seq(0, 80, 20)
    ) +
    labs(
      x = "Alarm burden (clinical alarms in preceding 30 min)",
      y = "Predicted silencing probability (%)"
    ) +
    theme_bw(base_size = base_size) +
    theme(
      legend.position  = "top",
      legend.title     = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )
}

# -----------------------------------------------------------------------------
# 5. 保存（journal / presentation）
# -----------------------------------------------------------------------------

p_journal <- make_plot(base_size = 8, line_size = 0.6, ribbon_alpha = 0.15)

cairo_pdf("outputs/figures/journal/16_figure3_predicted.pdf",
          width = 3.5, height = 3.5)
print(p_journal)
dev.off()
cat("保存: outputs/figures/journal/16_figure3_predicted.pdf\n")

p_pres <- make_plot(base_size = 18, line_size = 1.4, ribbon_alpha = 0.15)

cairo_pdf("outputs/figures/presentation/16_figure3_predicted.pdf",
          width = 10, height = 7)
print(p_pres)
dev.off()
cat("保存: outputs/figures/presentation/16_figure3_predicted.pdf\n")
