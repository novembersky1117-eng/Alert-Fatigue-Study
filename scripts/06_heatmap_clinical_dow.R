# =============================================================================
# Fig: 臨床的アラームの時間帯×曜日ヒートマップ
#
# 【目的】
#   臨床的アラームが1日のどの時間帯・曜日に集中しているかを視覚化する
#
# 【方針】
#   - alarm_class == "clinical" のみ対象
#   - X軸: 時刻（0〜23時）、Y軸: 曜日（月〜日）
#   - 優先度ごとに別パネル（patchwork で縦結合）
#   - カラースケール: ADVISORY=緑、WARNING=黄、CRISIS=赤（白→濃色）
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
  filter(alarm_class == "clinical") |>
  mutate(
    hour = hour(datetime),
    dow  = wday(datetime, label = TRUE, abbr = FALSE, week_start = 1)
  ) |>
  count(hour, dow, 優先度)

# 曜日の順序を月〜日に固定（lubridateのlabelがfactorなのでそのまま使える）

# -----------------------------------------------------------------------------
# 1. ヒートマップ作成関数
# -----------------------------------------------------------------------------

make_heatmap <- function(data, priority, high_color, panel_label) {
  data |>
    filter(優先度 == priority) |>
    # 件数0のセルも白で表示するため complete で補完
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
      plot.title      = element_text(face = "bold", size = 9),
      panel.grid      = element_blank(),
      axis.text.x     = element_text(angle = 45, hjust = 1, size = 7),
      axis.text.y     = element_text(size = 7),
      legend.key.height = unit(0.4, "cm"),
      legend.key.width  = unit(0.3, "cm"),
      legend.title    = element_text(size = 7),
      legend.text     = element_text(size = 6)
    )
}

p_advisory <- make_heatmap(counts, "ADVISORY", "#2ca02c", "ADVISORY (n = 1,649)")
p_warning  <- make_heatmap(counts, "WARNING",  "#e6a817", "WARNING  (n = 57,242)")
p_crisis   <- make_heatmap(counts, "CRISIS",   "#d62728", "CRISIS   (n = 222)")

# x軸ラベルは最下段のみ表示
p_advisory <- p_advisory + theme(axis.text.x = element_blank())
p_warning  <- p_warning  + theme(axis.text.x = element_blank())

p_combined <- p_advisory / p_warning / p_crisis +
  plot_annotation(
    title    = "Clinical Alarms by Hour of Day and Day of Week",
    subtitle = "Observation period: 2025/7/15 – 9/9 (57 days), clinical alarms only",
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
  "outputs/figures/journal/fig_heatmap_clinical_by_dow.pdf",
  plot   = p_combined,
  width  = 3.5,
  height = 4.5,
  units  = "in",
  device = cairo_pdf
)

# presentation 版（10inch幅）
p_combined_pres <- p_advisory / p_warning / p_crisis +
  plot_annotation(
    title    = "Clinical Alarms by Hour of Day and Day of Week",
    subtitle = "Observation period: 2025/7/15 – 9/9 (57 days), clinical alarms only",
    theme    = theme(
      plot.title    = element_text(face = "bold", size = 18),
      plot.subtitle = element_text(size = 13, color = "gray40")
    )
  )

ggsave(
  "outputs/figures/presentation/fig_heatmap_clinical_by_dow.pdf",
  plot   = p_combined_pres,
  width  = 10,
  height = 9,
  units  = "in",
  device = cairo_pdf
)

cat("保存完了:\n")
cat("  outputs/figures/journal/fig_heatmap_clinical_by_dow.pdf\n")
cat("  outputs/figures/presentation/fig_heatmap_clinical_by_dow.pdf\n")

# -----------------------------------------------------------------------------
# 3. figure_legends.txt に追記
# -----------------------------------------------------------------------------

legend_text <- "
## fig_heatmap_clinical_by_dow
Title: Clinical Alarms by Hour of Day and Day of Week
Legend:
  Heatmap showing the distribution of clinical alarms across hours of the day (x-axis)
  and days of the week (y-axis, Monday to Sunday). Each cell represents the total alarm
  count over the 57-day observation period (2025/7/15 – 9/9). Three panels correspond
  to alarm priority levels: ADVISORY (green), WARNING (yellow), and CRISIS (red).
  Only clinical alarms (alarm_class == 'clinical') are included.
"

cat(legend_text, file = "outputs/figure_legends.txt", append = TRUE)
cat("figure_legends.txt に追記完了\n")
