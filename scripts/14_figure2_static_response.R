# =============================================================================
# 14_figure2_static_response.R
# Figure 2: priority別消音率・応答時間・再発率（静的構造の視覚化）
#
# 【メッセージ】
#   優先度が高いほど消音率が高く、応答時間が短く、再発率が低い。
#   CRISISは「最も多く・最も速く・最も確実に対応される」ことを3パネルで示す。
#
# 【データ】
#   03_analysis_ready.rds（臨床アラーム全件）
#   Panel A: 優先度別消音率 + 95% Wilson CI
#   Panel B: 消音ありアラームの応答時間（中央値 + IQR）
#   Panel C: 消音後5分以内の再発率 + 95% Wilson CI（appropriateness proxy）
#
# 【出力】
#   outputs/figures/journal/14_figure2_static_response.pdf      （3.5 × 6 inch）
#   outputs/figures/presentation/14_figure2_static_response.pdf （10 × 18 inch）
# =============================================================================

library(dplyr)
library(lubridate)
library(ggplot2)
library(patchwork)

# -----------------------------------------------------------------------------
# 0. データ読み込み
# -----------------------------------------------------------------------------

df <- readRDS("data/proceeded/03_analysis_ready.rds") |>
  mutate(row_id = row_number())

# -----------------------------------------------------------------------------
# 1. Wilson CI ヘルパー
# -----------------------------------------------------------------------------

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

# -----------------------------------------------------------------------------
# 2. Panel A: 優先度別消音率（95% Wilson CI）
# -----------------------------------------------------------------------------

panel_a_data <- df |>
  group_by(priority_fct) |>
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
    ci_high_pct = ci_high * 100
  )

cat("=== Panel A: 消音率 ===\n")
print(panel_a_data)

# -----------------------------------------------------------------------------
# 3. Panel B: 応答時間（中央値 + IQR、消音ありのみ）
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
# 4. Panel C: 消音後5分以内の再発率（appropriateness proxy）
# -----------------------------------------------------------------------------

silenced_df <- df |>
  filter(silenced == TRUE) |>
  mutate(
    silence_time     = datetime + seconds(response_sec),
    recur_window_end = silence_time + minutes(5)
  )

all_ref <- df |>
  select(ref_id = row_id, ベッド名, 内容１, ref_time = datetime)

recur_result <- silenced_df |>
  select(row_id, ベッド名, 内容１, priority_fct, silence_time, recur_window_end) |>
  left_join(all_ref, by = c("ベッド名", "内容１"), relationship = "many-to-many") |>
  filter(
    ref_time > silence_time,
    ref_time <= recur_window_end,
    ref_id != row_id
  ) |>
  group_by(row_id) |>
  summarise(n_recur = n(), .groups = "drop")

panel_c_data <- silenced_df |>
  left_join(recur_result, by = "row_id") |>
  mutate(recurred = coalesce(n_recur, 0L) > 0) |>
  group_by(priority_fct) |>
  summarise(
    n          = n(),
    n_recurred = sum(recurred),
    prop       = mean(recurred),
    .groups    = "drop"
  ) |>
  mutate(
    ci_low      = wilson_low(n_recurred, n),
    ci_high     = wilson_high(n_recurred, n),
    prop_pct    = prop    * 100,
    ci_low_pct  = ci_low  * 100,
    ci_high_pct = ci_high * 100
  )

cat("\n=== Panel C: 再発率（消音後5分以内） ===\n")
print(panel_c_data)

# -----------------------------------------------------------------------------
# 5. カラー・スタイル設定
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
# 6. プロット関数（patchwork 3段）
# -----------------------------------------------------------------------------

make_plot <- function(base_size, point_size, line_size) {

  base_theme <- theme_bw(base_size = base_size) +
    theme(
      panel.grid.minor = element_blank(),
      axis.title.y     = element_text(margin = margin(r = 3))
    )

  # Panel A: 消音率（X軸非表示）
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
      legend.position = "top",
      legend.title    = element_text(face = "bold"),
      axis.text.x     = element_blank(),
      axis.ticks.x    = element_blank(),
      plot.margin     = margin(t = 14, r = 2, b = 0, l = 6)
    )

  # Panel B: 応答時間（X軸非表示）
  pB <- ggplot(panel_b_data,
               aes(x = priority_fct, y = med,
                   color = priority_fct, shape = priority_fct)) +
    geom_errorbar(aes(ymin = q25, ymax = q75),
                  width = 0.15, linewidth = line_size) +
    geom_point(size = point_size) +
    scale_color_manual(values = priority_colors, guide = "none") +
    scale_shape_manual(values = priority_shapes, guide = "none") +
    scale_y_continuous(limits = c(0, NA)) +
    labs(x = NULL, y = "Time-to-silence (sec)\nMedian [IQR]") +
    base_theme +
    theme(
      axis.text.x  = element_blank(),
      axis.ticks.x = element_blank(),
      plot.margin  = margin(t = 0, r = 2, b = 0, l = 6)
    )

  # Panel C: 再発率（X軸表示）
  pC <- ggplot(panel_c_data,
               aes(x = priority_fct, y = prop_pct,
                   color = priority_fct, shape = priority_fct)) +
    geom_errorbar(aes(ymin = ci_low_pct, ymax = ci_high_pct),
                  width = 0.15, linewidth = line_size) +
    geom_point(size = point_size) +
    scale_color_manual(values = priority_colors, guide = "none") +
    scale_shape_manual(values = priority_shapes, guide = "none") +
    scale_y_continuous(
      limits = c(0, 100),
      breaks = seq(0, 100, 20),
      labels = paste0(seq(0, 100, 20), "%")
    ) +
    labs(x = "Alarm priority",
         y = "Post-silence recurrence\nrate (%)") +
    base_theme +
    theme(plot.margin = margin(t = 0, r = 2, b = 2, l = 6))

  pA / pB / pC + plot_layout(heights = c(1, 1, 1))
}

# -----------------------------------------------------------------------------
# 7. 保存（journal / presentation）
# -----------------------------------------------------------------------------

p_journal <- make_plot(base_size = 8, point_size = 1.8, line_size = 0.5)

cairo_pdf("outputs/figures/journal/14_figure2_static_response.pdf",
          width = 3.5, height = 6)
print(p_journal)
dev.off()
cat("保存: outputs/figures/journal/14_figure2_static_response.pdf\n")

p_pres <- make_plot(base_size = 18, point_size = 4, line_size = 1.2)

cairo_pdf("outputs/figures/presentation/14_figure2_static_response.pdf",
          width = 10, height = 18)
print(p_pres)
dev.off()
cat("保存: outputs/figures/presentation/14_figure2_static_response.pdf\n")
