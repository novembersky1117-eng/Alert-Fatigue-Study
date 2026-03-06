# =============================================================================
# Fig: 臨床的アラームの時間帯×曜日ヒートマップ（ゾーン別）
#
# 【目的】
#   高重症ゾーンと一般ゾーンでアラームの時間帯・曜日パターンを比較する
#
# 【方針】
#   - alarm_class == "clinical" のみ対象
#   - X軸: 時刻（0〜23時）、Y軸: 曜日（月〜日）
#   - 3行（ADVISORY / WARNING / CRISIS）× 2列（高重症 / 一般）の6パネル
#   - カラースケール: ADVISORY=緑、WARNING=黄、CRISIS=赤（白→濃色）
#   - 同一優先度内でスケールを統一して比較可能にする
#   - journal（7inch幅）と presentation（14inch幅）の2種類を保存
#
# =============================================================================

library(readr)
library(dplyr)
library(lubridate)
library(forcats)
library(tidyr)
library(ggplot2)
library(patchwork)

# -----------------------------------------------------------------------------
# 0. データ読み込み・集計
# -----------------------------------------------------------------------------

df <- readRDS("data/proceeded/02_cleaned.rds")
bed_zone <- read_csv("data/proceeded/bed_zone.csv", show_col_types = FALSE)

counts <- df |>
  filter(alarm_class == "clinical") |>
  left_join(bed_zone, by = "ベッド名") |>
  mutate(
    hour = hour(datetime),
    dow  = wday(datetime, label = TRUE, abbr = FALSE, week_start = 1)
  ) |>
  count(hour, dow, 優先度, ゾーン)

# 優先度別の最大値を取得（同一優先度内でカラースケールを統一するため）
scale_max <- counts |>
  group_by(優先度) |>
  summarise(max_n = max(n), .groups = "drop")

cat("優先度×ゾーン別件数:\n")
print(counts |> group_by(優先度, ゾーン) |> summarise(total = sum(n), .groups = "drop"))

# -----------------------------------------------------------------------------
# 1. ヒートマップ作成関数
# -----------------------------------------------------------------------------

make_heatmap <- function(data, priority, zone, high_color, max_n,
                         show_x = FALSE, show_title = FALSE, title_label = "") {
  d <- data |>
    filter(優先度 == priority, ゾーン == zone) |>
    complete(
      hour = 0:23,
      dow  = levels(data$dow),
      fill = list(n = 0)
    )

  p <- ggplot(d, aes(x = hour, y = fct_rev(dow), fill = n)) +
    geom_tile(color = "white", linewidth = 0.3) +
    scale_fill_gradient(
      low   = "white",
      high  = high_color,
      name  = "Count",
      limits = c(0, max_n)
    ) +
    scale_x_continuous(
      breaks = seq(0, 23, by = 3),
      labels = paste0(seq(0, 23, by = 3), ":00"),
      expand = c(0, 0)
    ) +
    scale_y_discrete(expand = c(0, 0)) +
    labs(x = NULL, y = NULL) +
    theme_minimal(base_size = 9) +
    theme(
      panel.grid        = element_blank(),
      axis.text.y       = element_text(size = 7),
      legend.key.height = unit(0.4, "cm"),
      legend.key.width  = unit(0.3, "cm"),
      legend.title      = element_text(size = 7),
      legend.text       = element_text(size = 6)
    )

  if (show_x) {
    p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7))
  } else {
    p <- p + theme(axis.text.x = element_blank())
  }

  if (show_title) {
    p <- p + labs(title = title_label) +
      theme(plot.title = element_text(face = "bold", size = 9, hjust = 0.5))
  }

  p
}

# 優先度別スケール上限を取得
max_advisory <- scale_max |> filter(優先度 == "ADVISORY") |> pull(max_n)
max_warning  <- scale_max |> filter(優先度 == "WARNING")  |> pull(max_n)
max_crisis   <- scale_max |> filter(優先度 == "CRISIS")   |> pull(max_n)

# 6パネル作成
p_adv_high <- make_heatmap(counts, "ADVISORY", "高重症", "#2ca02c", max_advisory,
                            show_title = TRUE, title_label = "High-acuity Zone")
p_adv_gen  <- make_heatmap(counts, "ADVISORY", "一般",   "#2ca02c", max_advisory,
                            show_title = TRUE, title_label = "General Zone")
p_war_high <- make_heatmap(counts, "WARNING",  "高重症", "#e6a817", max_warning)
p_war_gen  <- make_heatmap(counts, "WARNING",  "一般",   "#e6a817", max_warning)
p_cri_high <- make_heatmap(counts, "CRISIS",   "高重症", "#d62728", max_crisis,
                            show_x = TRUE)
p_cri_gen  <- make_heatmap(counts, "CRISIS",   "一般",   "#d62728", max_crisis,
                            show_x = TRUE)

# 行ラベル（優先度）を左側に追加
label_theme <- theme(
  plot.tag          = element_text(face = "bold", size = 8, angle = 90, vjust = 0.5),
  plot.tag.position = "left"
)

p_adv_high <- p_adv_high + labs(tag = "ADVISORY") + label_theme
p_war_high <- p_war_high + labs(tag = "WARNING")  + label_theme
p_cri_high <- p_cri_high + labs(tag = "CRISIS")   + label_theme

# -----------------------------------------------------------------------------
# 2. patchwork で結合
# -----------------------------------------------------------------------------

p_combined <- (p_adv_high | p_adv_gen) /
              (p_war_high | p_war_gen) /
              (p_cri_high | p_cri_gen) +
  plot_annotation(
    title    = "Clinical Alarms by Hour of Day and Day of Week",
    subtitle = "Stratified by ward zone. Observation period: 2025/7/15 – 9/9 (57 days), clinical alarms only.\nColor scale is unified within each priority level.",
    theme    = theme(
      plot.title    = element_text(face = "bold", size = 11),
      plot.subtitle = element_text(size = 8, color = "gray40")
    )
  )

# -----------------------------------------------------------------------------
# 3. 保存
# -----------------------------------------------------------------------------

# journal 版（7inch幅）
ggsave(
  "outputs/figures/journal/fig_heatmap_clinical_by_zone.pdf",
  plot   = p_combined,
  width  = 7,
  height = 6,
  units  = "in",
  device = cairo_pdf
)

# presentation 版（14inch幅）
p_combined_pres <- (p_adv_high | p_adv_gen) /
                   (p_war_high | p_war_gen) /
                   (p_cri_high | p_cri_gen) +
  plot_annotation(
    title    = "Clinical Alarms by Hour of Day and Day of Week",
    subtitle = "Stratified by ward zone. Observation period: 2025/7/15 – 9/9 (57 days), clinical alarms only.\nColor scale is unified within each priority level.",
    theme    = theme(
      plot.title    = element_text(face = "bold", size = 20),
      plot.subtitle = element_text(size = 13, color = "gray40")
    )
  )

ggsave(
  "outputs/figures/presentation/fig_heatmap_clinical_by_zone.pdf",
  plot   = p_combined_pres,
  width  = 14,
  height = 11,
  units  = "in",
  device = cairo_pdf
)

cat("保存完了:\n")
cat("  outputs/figures/journal/fig_heatmap_clinical_by_zone.pdf\n")
cat("  outputs/figures/presentation/fig_heatmap_clinical_by_zone.pdf\n")

# -----------------------------------------------------------------------------
# 4. figure_legends.txt に追記
# -----------------------------------------------------------------------------

legend_text <- "
## fig_heatmap_clinical_by_zone
Title: Clinical Alarms by Hour of Day and Day of Week (Stratified by Zone)
Legend:
  Six-panel heatmap comparing clinical alarm distribution between high-acuity zone
  (12 beds: 1101-A to E, Re-A/B, W10-01 to 05) and general zone (29 beds).
  Rows represent priority levels (ADVISORY, WARNING, CRISIS); columns represent zones.
  X-axis: hour of day (0-23); Y-axis: day of week (Monday to Sunday).
  Color intensity indicates alarm count per cell (hour × day combination) over the
  57-day observation period. Color scale is unified within each priority row to allow
  direct zone comparison.
"

cat(legend_text, file = "outputs/figure_legends.txt", append = TRUE)
cat("figure_legends.txt に追記完了\n")
