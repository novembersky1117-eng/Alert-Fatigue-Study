# =============================================================================
# 13_interaction_plot.R
# Figure: alarm burden × priority → 消音率の交互作用プロット（メインフィギュア）
#
# 【メッセージ】
#   alarm burden が増大してもCRISISアラームへの応答は安定して維持される。
#   WARNINGはわずかに低下するが、CRISISの線は水平を保つ。
#   → Safety-II / Resilience Engineering の観点からの視覚的証拠。
#
# 【データ】
#   実測消音率 + 95% Wilson CI（burden四分位 × priority別集計）
#   モデルによる推論はTable 2（12_table2.R）を参照。
#
# 【出力】
#   outputs/figures/journal/13_interaction_plot.pdf      （3.5 × 3.5 inch）
#   outputs/figures/presentation/13_interaction_plot.pdf （10 × 7 inch）
#   outputs/figure_legends.txt に追記
# =============================================================================

library(dplyr)
library(lubridate)
library(ggplot2)
library(readr)

# -----------------------------------------------------------------------------
# 0. データ読み込み
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

# -----------------------------------------------------------------------------
# 1. burden四分位 × priority 別の実測消音率（95% Wilson CI）
# -----------------------------------------------------------------------------

# Wilson CI を計算するヘルパー（low / high を別々に返す）
wilson_low <- function(x, n, z = 1.96) {
  p      <- x / n
  denom  <- 1 + z^2 / n
  center <- (p + z^2 / (2 * n)) / denom
  margin <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / denom
  pmax(0, center - margin)
}

wilson_high <- function(x, n, z = 1.96) {
  p      <- x / n
  denom  <- 1 + z^2 / n
  center <- (p + z^2 / (2 * n)) / denom
  margin <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / denom
  pmin(1, center + margin)
}

plot_data <- df_model |>
  group_by(burden_quartile, burden_q_label, priority_fct) |>
  summarise(
    n          = n(),
    n_silenced = sum(silenced),
    prop       = mean(silenced),
    .groups    = "drop"
  ) |>
  mutate(
    ci_low      = wilson_low(n_silenced, n),
    ci_high     = wilson_high(n_silenced, n),
    prop_pct    = prop    * 100,
    ci_low_pct  = ci_low  * 100,
    ci_high_pct = ci_high * 100,
    # N数ラベルの配置: CRISISはリボン下、WARNINGはリボン上、ADVISORYはグラフ下部固定
    n_y = case_when(
      priority_fct == "CRISIS"   ~ ci_low_pct  - 4,
      priority_fct == "WARNING"   ~ ci_low_pct  - 4,
      priority_fct == "ADVISORY" ~ -1,



      
      TRUE                       ~ ci_high_pct + 3
    )
  )

cat("=== 集計結果 ===\n")
print(plot_data |> select(burden_quartile, priority_fct, n, n_silenced, prop_pct, ci_low_pct, ci_high_pct),
      n = Inf)

# -----------------------------------------------------------------------------
# 2. X軸ラベルの整形（2行表示）
# -----------------------------------------------------------------------------

# "Q1 (0–9 alarms/30min)" → "Q1\n(0–9)"
plot_data <- plot_data |>
  mutate(
    x_label = burden_q_label |>
      as.character() |>
      gsub(" alarms/30min\\)", ") ", x = _) |>
      gsub(" \\(", "\n(", x = _) |>
      factor(levels = gsub(" alarms/30min\\)", ") ",
                           gsub(" \\(", "\n(",
                                levels(burden_q_label))))
  )

# -----------------------------------------------------------------------------
# 3. カラー・スタイル設定
# -----------------------------------------------------------------------------

priority_colors <- c(
  "ADVISORY" = "#999999",   # gray
  "WARNING"  = "#E69F00",   # orange
  "CRISIS"   = "#D55E00"    # red
)

priority_shapes <- c(
  "ADVISORY" = 16,
  "WARNING"  = 17,
  "CRISIS"   = 15
)

# -----------------------------------------------------------------------------
# 4. プロット関数
# -----------------------------------------------------------------------------

make_plot <- function(base_size, point_size, line_size, text_size) {
  ggplot(plot_data,
         aes(x     = x_label,
             y     = prop_pct,
             color = priority_fct,
             fill  = priority_fct,
             shape = priority_fct,
             group = priority_fct)) +
    geom_ribbon(
      aes(ymin = ci_low_pct, ymax = ci_high_pct),
      alpha = 0.2,
      color = NA
    ) +
    geom_line(linewidth = line_size) +
    geom_point(size = point_size) +
    geom_text(
      aes(y = n_y, label = paste0("n=", n)),
      size        = text_size,
      show.legend = FALSE
    ) +
    scale_color_manual(
      values = priority_colors,
      name   = "Alarm priority"
    ) +
    scale_fill_manual(
      values = priority_colors,
      name   = "Alarm priority"
    ) +
    scale_shape_manual(
      values = priority_shapes,
      name   = "Alarm priority"
    ) +
    scale_y_continuous(
      breaks = seq(0, 100, 20),
      labels = paste0(seq(0, 100, 20), "%")
    ) +
    labs(
      x     = "Alarm burden (preceding 30 min, quartile)",
      y     = "Silencing rate (%)",
      title = NULL
    ) +
    theme_bw(base_size = base_size) +
    theme(
      legend.position  = "top",
      legend.title     = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      axis.text.x      = element_text(lineheight = 0.9)
    ) +
    coord_cartesian(ylim = c(0, 100), clip = "off")
}

# -----------------------------------------------------------------------------
# 5. 保存（journal / presentation）
# -----------------------------------------------------------------------------

# journal: 3.5 × 3.5 inch
p_journal <- make_plot(base_size = 8, point_size = 1.8,
                       line_size = 0.5, text_size = 1.8)

cairo_pdf("outputs/figures/journal/13_interaction_plot.pdf",
          width = 3.5, height = 3.5)
print(p_journal)
dev.off()
cat("保存: outputs/figures/journal/13_interaction_plot.pdf\n")

# presentation: 10 × 7 inch
p_pres <- make_plot(base_size = 18, point_size = 4,
                    line_size = 1.2, text_size = 4.5)

cairo_pdf("outputs/figures/presentation/13_interaction_plot.pdf",
          width = 10, height = 7)
print(p_pres)
dev.off()
cat("保存: outputs/figures/presentation/13_interaction_plot.pdf\n")

# -----------------------------------------------------------------------------
# 6. figure_legends.txt に追記
# -----------------------------------------------------------------------------

legend_text <- paste0(
  "\n---\n",
  "Figure [X]: Alarm silencing rate by alarm burden quartile and priority level\n",
  "\n",
  "Observed silencing rates (%) with 95% Wilson confidence intervals are shown\n",
  "for each combination of alarm burden quartile (Q1–Q4, preceding 30 minutes)\n",
  "and alarm priority (ADVISORY, WARNING, CRISIS).\n",
  "Alarm burden quartiles: Q1 = 0–9, Q2 = 9–18, Q3 = 18–31, Q4 = 31–97 alarms/30 min.\n",
  "N = ", format(nrow(df_model), big.mark = ","), " clinical alarms.\n",
  "Shaded ribbons represent 95% Wilson confidence intervals.\n",
  "Numbers adjacent to each point indicate the sample size (n) per cell.\n"
)

cat(legend_text, file = "outputs/figure_legends.txt", append = TRUE)
cat("figure_legends.txt に追記完了\n")
