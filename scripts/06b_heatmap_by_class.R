# =============================================================================
# Fig: 全アラームの時間帯×曜日ヒートマップ（技術的 vs 臨床的）
#
# 【目的】
#   全アラームを alarm_class（技術的・臨床的）で2分割し、
#   それぞれの時間帯×曜日パターンを視覚化する
#
# 【方針】
#   - X軸: 時刻（0〜23時）、Y軸: 曜日（月〜日）
#   - 技術的アラーム（上段）: #F0E442（黄）、臨床的アラーム（下段）: #0072B2（青）
#   - カラースケールは各パネル独立（件数レンジが大きく異なるため）
#   - journal（3.5inch幅）と presentation（10inch幅）の2種類を保存
#
# =============================================================================

library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(forcats)
library(patchwork)

# -----------------------------------------------------------------------------
# 0. データ読み込み・集計
# -----------------------------------------------------------------------------

df <- readRDS("data/proceeded/02_cleaned.rds")

counts <- df |>
  mutate(
    hour        = hour(datetime),
    dow         = wday(datetime, label = TRUE, abbr = FALSE, week_start = 1),
    alarm_class = if_else(alarm_class == "technical", "Technical", "Clinical")
  ) |>
  count(hour, dow, alarm_class)

# alarm_class ごとの総件数（パネルタイトル用）
n_tech     <- counts |> filter(alarm_class == "Technical") |> pull(n) |> sum()
n_clinical <- counts |> filter(alarm_class == "Clinical")  |> pull(n) |> sum()

# -----------------------------------------------------------------------------
# 1. ヒートマップ作成関数
# -----------------------------------------------------------------------------

make_heatmap <- function(data, cls, high_color, panel_label) {
  data |>
    filter(alarm_class == cls) |>
    tidyr::complete(
      hour = 0:23,
      dow,
      fill = list(n = 0)
    ) |>
    ggplot(aes(x = hour, y = fct_rev(dow), fill = n)) +
    geom_tile(color = "white", linewidth = 0.3) +
    scale_fill_gradient(
      low  = "white",
      high = high_color,
      name = "Count"
    ) +
    scale_x_continuous(
      breaks = seq(0, 23, by = 3),
      labels = paste0(seq(0, 23, by = 3), ":00"),
      expand = c(0, 0)
    ) +
    scale_y_discrete(expand = c(0, 0)) +
    labs(
      title = panel_label,
      x     = NULL,
      y     = NULL
    ) +
    theme_minimal(base_size = 9, base_family = "Helvetica") +
    theme(
      plot.title        = element_text(face = "bold", size = 9),
      panel.grid        = element_blank(),
      axis.text.x       = element_text(angle = 45, hjust = 1, size = 7),
      axis.text.y       = element_text(size = 7),
      legend.key.height = unit(0.4, "cm"),
      legend.key.width  = unit(0.3, "cm"),
      legend.title      = element_text(size = 7),
      legend.text       = element_text(size = 6)
    )
}

p_technical <- make_heatmap(
  counts, "Technical", "#F0E442",
  sprintf("Technical (n = %s)", format(n_tech, big.mark = ","))
)
p_clinical  <- make_heatmap(
  counts, "Clinical",  "#0072B2",
  sprintf("Clinical  (n = %s)", format(n_clinical, big.mark = ","))
)

# x軸ラベルは最下段（Clinical）のみ表示
p_technical <- p_technical + theme(axis.text.x = element_blank())

p_combined <- p_technical / p_clinical +
  plot_annotation(
    title    = "Alarms by Hour of Day and Day of Week",
    subtitle = "Observation period: 2025/7/15 – 9/9 (57 days)",
    theme    = theme(
      text          = element_text(family = "Helvetica"),
      plot.title    = element_text(face = "bold", size = 10),
      plot.subtitle = element_text(size = 8, color = "gray40")
    )
  )

# -----------------------------------------------------------------------------
# 2. 保存
# -----------------------------------------------------------------------------

# journal 版（3.5inch幅）
ggsave(
  "outputs/figures/journal/fig_heatmap_by_class.pdf",
  plot   = p_combined,
  width  = 3.5,
  height = 3.5,
  units  = "in",
  device = cairo_pdf
)

# presentation 版（10inch幅）
p_combined_pres <- p_technical / p_clinical +
  plot_annotation(
    title    = "Alarms by Hour of Day and Day of Week",
    subtitle = "Observation period: 2025/7/15 – 9/9 (57 days)",
    theme    = theme(
      plot.title    = element_text(face = "bold", size = 18),
      plot.subtitle = element_text(size = 13, color = "gray40")
    )
  )

ggsave(
  "outputs/figures/presentation/fig_heatmap_by_class.pdf",
  plot   = p_combined_pres,
  width  = 10,
  height = 7,
  units  = "in",
  device = cairo_pdf
)

cat("保存完了:\n")
cat("  outputs/figures/journal/fig_heatmap_by_class.pdf\n")
cat("  outputs/figures/presentation/fig_heatmap_by_class.pdf\n")

# -----------------------------------------------------------------------------
# 3. figure_legends.txt に追記
# -----------------------------------------------------------------------------

legend_text <- "
## fig_heatmap_by_class
Title: Alarms by Hour of Day and Day of Week
Legend:
  Heatmap showing the distribution of all alarms across hours of the day (x-axis)
  and days of the week (y-axis, Monday to Sunday). Each cell represents the total alarm
  count over the 57-day observation period (2025/7/15 – 9/9). Two panels correspond
  to alarm class: Technical (yellow, upper) and Clinical (blue, lower).
  Color scales are independent between panels to reflect the difference in total counts.
"

cat(legend_text, file = "outputs/figure_legends.txt", append = TRUE)
cat("figure_legends.txt に追記完了\n")
