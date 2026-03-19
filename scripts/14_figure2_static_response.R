# =============================================================================
# 14_figure2_static_response.R
# Figure 2: priority別消音率・応答時間（静的構造の視覚化）
#
# 【メッセージ】
#   優先度が高いほど消音率が高く、応答時間が短い。
#   特にCRISISは消音率71.2%・中央値8秒と突出しており、
#   臨床現場で優先度に基づく選別が系統的に機能していることを示す。
#
# 【データ】
#   03_analysis_ready.rds（臨床アラーム全件）
#   Panel A: 優先度別消音率 + 95% Wilson CI
#   Panel B: 消音ありアラームの応答時間（中央値 + IQR）
#
# 【出力】
#   outputs/figures/journal/14_figure2_static_response.pdf      （3.5 × 5 inch）
#   outputs/figures/presentation/14_figure2_static_response.pdf （10 × 12 inch）
#   outputs/figure_legends.txt に追記
# =============================================================================

library(dplyr)
library(ggplot2)
library(patchwork)

# -----------------------------------------------------------------------------
# 0. データ読み込み
# -----------------------------------------------------------------------------

df <- readRDS("data/proceeded/03_analysis_ready.rds")

# -----------------------------------------------------------------------------
# 1. Panel A: 優先度別消音率（95% Wilson CI）
# -----------------------------------------------------------------------------

# Wilson CI ヘルパー（13_interaction_plot.R と同一実装）
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

panel_a_data <- df |>
  group_by(priority_fct) |>
  summarise(
    n           = n(),
    n_silenced  = sum(silenced),
    prop        = mean(silenced),
    .groups     = "drop"
  ) |>
  mutate(
    ci_low      = wilson_low(n_silenced, n),
    ci_high     = wilson_high(n_silenced, n),
    prop_pct    = prop    * 100,
    ci_low_pct  = ci_low  * 100,
    ci_high_pct = ci_high * 100
  )

cat("=== Panel A: 消音率 ===\n")
print(panel_a_data)

# -----------------------------------------------------------------------------
# 2. Panel B: 応答時間（中央値 + IQR、消音ありのみ）
# -----------------------------------------------------------------------------

panel_b_data <- df |>
  filter(silenced == TRUE, !is.na(response_sec)) |>
  group_by(priority_fct) |>
  summarise(
    n       = n(),
    med     = median(response_sec),
    q25     = quantile(response_sec, 0.25),
    q75     = quantile(response_sec, 0.75),
    .groups = "drop"
  )

cat("\n=== Panel B: 応答時間（消音ありのみ） ===\n")
print(panel_b_data)

# -----------------------------------------------------------------------------
# 3. カラー・スタイル設定（13_interaction_plot.R と統一）
# -----------------------------------------------------------------------------

priority_colors <- c(
  "ADVISORY" = "#999999",
  "WARNING"  = "#E69F00",
  "CRISIS"   = "#D55E00"
)

priority_shapes <- c(
  "ADVISORY" = 16,
  "WARNING"  = 17,
  "CRISIS"   = 15
)

# -----------------------------------------------------------------------------
# 4. プロット関数（patchwork 2段、パネル密着）
# -----------------------------------------------------------------------------

make_plot <- function(base_size, point_size, line_size) {

  base_theme <- theme_bw(base_size  = base_size,
                         base_family = "Helvetica") +
    theme(
      panel.grid.minor = element_blank(),
      axis.title.y     = element_text(margin = margin(r = 3))
    )

  # 上段：消音率（X軸ラベル・ティック非表示、下マージン=0）
  pA <- ggplot(panel_a_data,
               aes(x = priority_fct, y = prop_pct,
                   color = priority_fct, shape = priority_fct)) +
    geom_errorbar(aes(ymin = ci_low_pct, ymax = ci_high_pct),
                  width = 0.15, linewidth = line_size) +
    geom_point(size = point_size) +
    scale_color_manual(values = priority_colors, name = "Alarm priority") +
    scale_shape_manual(values = priority_shapes, name = "Alarm priority") +
    scale_y_continuous(
      limits = c(0, 100),
      breaks = seq(0, 100, 20),
      labels = paste0(seq(0, 100, 20), "%")
    ) +
    labs(x = NULL, y = "Silencing rate (%)") +
    base_theme +
    theme(
      legend.position  = "top",
      legend.title     = element_text(face = "bold"),
      axis.text.x      = element_blank(),
      axis.ticks.x     = element_blank(),
      plot.margin      = margin(t = 14, r = 2, b = 0, l = 6)
    )

  # 下段：応答時間（上マージン=0）
  pB <- ggplot(panel_b_data,
               aes(x = priority_fct, y = med,
                   color = priority_fct, shape = priority_fct)) +
    geom_errorbar(aes(ymin = q25, ymax = q75),
                  width = 0.15, linewidth = line_size) +
    geom_point(size = point_size) +
    scale_color_manual(values = priority_colors, guide = "none") +
    scale_shape_manual(values = priority_shapes, guide = "none") +
    scale_y_continuous(limits = c(0, NA)) +
    labs(x = "Alarm priority",
         y = "Time-to-silence (sec)\nMedian [IQR]") +
    base_theme +
    theme(plot.margin = margin(t = 0, r = 2, b = 2, l = 6))

  pA / pB + plot_layout(heights = c(1, 1))
}

# -----------------------------------------------------------------------------
# 5. 保存（journal / presentation）
# -----------------------------------------------------------------------------

p_journal <- make_plot(base_size = 8, point_size = 1.8, line_size = 0.5)

cairo_pdf("outputs/figures/journal/14_figure2_static_response.pdf",
          width = 3.5, height = 4)
print(p_journal)
dev.off()
cat("保存: outputs/figures/journal/14_figure2_static_response.pdf\n")

p_pres <- make_plot(base_size = 18, point_size = 4, line_size = 1.2)

cairo_pdf("outputs/figures/presentation/14_figure2_static_response.pdf",
          width = 10, height = 12)
print(p_pres)
dev.off()
cat("保存: outputs/figures/presentation/14_figure2_static_response.pdf\n")

# -----------------------------------------------------------------------------
# 6. figure_legends.txt に追記
# -----------------------------------------------------------------------------

legend_text <- paste0(
  "\n---\n",
  "[Figure 2] 14_figure2_static_response\n",
  "Title: Alarm silencing rate and time-to-silence by alarm priority level\n",
  "\n",
  "(A) Observed silencing rates (%) with 95% Wilson confidence intervals by alarm priority\n",
  "level (ADVISORY, WARNING, CRISIS). (B) Median time-to-silence (seconds) with\n",
  "interquartile range (IQR) among silenced alarms, by priority level.\n",
  "N = ", format(nrow(df), big.mark = ","), " clinical alarms (Panel A);\n",
  "n = ", format(sum(df$silenced), big.mark = ","), " silenced alarms (Panel B).\n"
)

cat(legend_text, file = "outputs/figure_legends.txt", append = TRUE)
cat("figure_legends.txt に追記完了\n")

