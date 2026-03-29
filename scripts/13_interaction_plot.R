# =============================================================================
# 13_interaction_plot.R
# Figure 3: alarm burden × priority の交互作用（2パネル）
#
# 【メッセージ】
#   Panel A: burden増大でもCRISIS消音率は維持、WARNINGは低下
#   Panel B: burden増大でもCRISIS再発率は安定、WARNINGは単調増加
#   → 「安全性の非対称性」：CRISISへの応答は量も質も resilient
#
# 【データ】
#   03_analysis_ready.rds（臨床アラーム全件）
#   Panel A: burden四分位 × priority 別消音率（95% Wilson CI）
#   Panel B: burden四分位 × priority 別再発率（消音後5分以内、WARNING・CRISISのみ）
#
# 【出力】
#   outputs/figures/journal/13_interaction_plot.pdf      （7 × 3.5 inch）
#   outputs/figures/presentation/13_interaction_plot.pdf （20 × 7 inch）
# =============================================================================

library(dplyr)
library(lubridate)
library(ggplot2)
library(patchwork)
library(readr)

# -----------------------------------------------------------------------------
# 0. データ読み込み
# -----------------------------------------------------------------------------

df <- readRDS("data/proceeded/03_analysis_ready.rds") |>
  mutate(row_id = row_number())

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
# 2. Panel A: burden四分位 × priority 別消音率
# -----------------------------------------------------------------------------

plot_data_a <- df_model |>
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
    n_y = case_when(
      priority_fct == "CRISIS"   ~ ci_low_pct - 4,
      priority_fct == "WARNING"  ~ ci_low_pct - 4,
      priority_fct == "ADVISORY" ~ -1,
      TRUE                       ~ ci_high_pct + 3
    )
  )

cat("=== Panel A: 消音率 ===\n")
print(plot_data_a |> select(burden_quartile, priority_fct, n, n_silenced, prop_pct),
      n = Inf)

# X軸ラベル整形
x_labels <- plot_data_a$burden_q_label |>
  as.character() |>
  gsub(" alarms/30min\\)", ") ", x = _) |>
  gsub(" \\(", "\n(", x = _)

x_levels <- levels(plot_data_a$burden_q_label) |>
  gsub(" alarms/30min\\)", ") ", x = _) |>
  gsub(" \\(", "\n(", x = _)

plot_data_a <- plot_data_a |>
  mutate(x_label = factor(x_labels, levels = x_levels))

# -----------------------------------------------------------------------------
# 3. Panel B: burden四分位 × priority 別再発率（WARNING・CRISISのみ）
# -----------------------------------------------------------------------------

silenced_df <- df |>
  filter(silenced == TRUE) |>
  mutate(
    silence_time     = datetime + seconds(response_sec),
    recur_window_end = silence_time + minutes(5)
  )

all_ref <- df |> select(ref_id = row_id, ベッド名, 内容１, ref_time = datetime)

recur_result <- silenced_df |>
  select(row_id, ベッド名, 内容１, priority_fct, burden_quartile,
         burden_q_label, silence_time, recur_window_end) |>
  left_join(all_ref, by = c("ベッド名", "内容１"), relationship = "many-to-many") |>
  filter(ref_time > silence_time, ref_time <= recur_window_end, ref_id != row_id) |>
  group_by(row_id) |>
  summarise(n_recur = n(), .groups = "drop")

silenced_flagged <- silenced_df |>
  left_join(recur_result, by = "row_id") |>
  mutate(recurred = coalesce(n_recur, 0L) > 0)

plot_data_b <- silenced_flagged |>
  filter(priority_fct %in% c("WARNING", "CRISIS")) |>
  group_by(burden_quartile, burden_q_label, priority_fct) |>
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
    ci_high_pct = ci_high * 100,
    n_y = case_when(
      priority_fct == "WARNING" ~ ci_low_pct - 4,
      priority_fct == "CRISIS"  ~ ci_low_pct - 4,
      TRUE                      ~ ci_high_pct + 3
    ),
    x_label = factor(
      gsub(" \\(", "\n(", gsub(" alarms/30min\\)", ") ", as.character(burden_q_label))),
      levels = x_levels
    )
  )

cat("\n=== Panel B: 再発率（消音後5分以内） ===\n")
print(plot_data_b |> select(burden_quartile, priority_fct, n, n_recurred, prop_pct),
      n = Inf)

# -----------------------------------------------------------------------------
# 4. カラー・スタイル設定
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
# 5. プロット関数
# -----------------------------------------------------------------------------

make_plots <- function(base_size, point_size, line_size, text_size) {

  base_theme <- theme_bw(base_size = base_size) +
    theme(
      legend.position  = "top",
      legend.title     = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      axis.text.x      = element_text(lineheight = 0.9)
    )

  # Panel A: 消音率（3 priority）
  pA <- ggplot(plot_data_a,
               aes(x = x_label, y = prop_pct,
                   color = priority_fct, fill = priority_fct,
                   shape = priority_fct, group = priority_fct)) +
    geom_ribbon(aes(ymin = ci_low_pct, ymax = ci_high_pct),
                alpha = 0.2, color = NA) +
    geom_line(linewidth = line_size) +
    geom_point(size = point_size) +
    geom_text(aes(y = n_y, label = paste0("n=", n)),
              size = text_size, show.legend = FALSE) +
    scale_color_manual(values = priority_colors, name = "Alarm priority") +
    scale_fill_manual(values = priority_colors, name = "Alarm priority") +
    scale_shape_manual(values = priority_shapes, name = "Alarm priority") +
    scale_y_continuous(breaks = seq(0, 100, 20),
                       labels = paste0(seq(0, 100, 20), "%")) +
    coord_cartesian(ylim = c(0, 100), clip = "off") +
    labs(x = "Alarm burden (preceding 30 min, quartile)",
         y = "Silencing rate (%)") +
    base_theme

  # Panel B: 再発率（WARNING・CRISISのみ）
  pB <- ggplot(plot_data_b,
               aes(x = x_label, y = prop_pct,
                   color = priority_fct, fill = priority_fct,
                   shape = priority_fct, group = priority_fct)) +
    geom_ribbon(aes(ymin = ci_low_pct, ymax = ci_high_pct),
                alpha = 0.2, color = NA) +
    geom_line(linewidth = line_size) +
    geom_point(size = point_size) +
    geom_text(aes(y = n_y, label = paste0("n=", n)),
              size = text_size, show.legend = FALSE) +
    scale_color_manual(values = priority_colors, name = "Alarm priority",
                       breaks = c("WARNING", "CRISIS")) +
    scale_fill_manual(values = priority_colors, name = "Alarm priority",
                      breaks = c("WARNING", "CRISIS")) +
    scale_shape_manual(values = priority_shapes, name = "Alarm priority",
                       breaks = c("WARNING", "CRISIS")) +
    scale_y_continuous(breaks = seq(0, 100, 20),
                       labels = paste0(seq(0, 100, 20), "%")) +
    coord_cartesian(ylim = c(0, 100), clip = "off") +
    labs(x = "Alarm burden (preceding 30 min, quartile)",
         y = "Post-silence recurrence rate (%)") +
    base_theme

  pA | pB
}

# -----------------------------------------------------------------------------
# 6. 保存（journal / presentation）
# -----------------------------------------------------------------------------

p_journal <- make_plots(base_size = 8, point_size = 1.8,
                        line_size = 0.5, text_size = 1.8)

cairo_pdf("outputs/figures/journal/13_interaction_plot.pdf",
          width = 7, height = 3.5)
print(p_journal)
dev.off()
cat("保存: outputs/figures/journal/13_interaction_plot.pdf\n")

p_pres <- make_plots(base_size = 18, point_size = 4,
                     line_size = 1.2, text_size = 4.5)

cairo_pdf("outputs/figures/presentation/13_interaction_plot.pdf",
          width = 20, height = 7)
print(p_pres)
dev.off()
cat("保存: outputs/figures/presentation/13_interaction_plot.pdf\n")
