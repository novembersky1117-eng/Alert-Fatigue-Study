# =============================================================================
# Phase 1: 可視化
# 1. 曜日別アラーム件数（箱ひげ図）
# 2. 時間帯別アラーム件数（技術的 vs 臨床的、積み上げ棒グラフ）
# 3. duration分布（消音あり vs なし）
# journal / presentation の2種類を出力
# =============================================================================

library(dplyr)
library(lubridate)
library(ggplot2)

df <- readRDS("data/proceeded/02_cleaned.rds")

# -----------------------------------------------------------------------------
# ヘルパー：journal / presentation の2種類を保存する関数
# -----------------------------------------------------------------------------

save_both <- function(plot, name) {
  ggsave(
    file.path("outputs/figures/journal", paste0(name, "_journal.pdf")),
    plot = plot(base_sz = 11, bold = FALSE),
    device = cairo_pdf, width = 3.5, height = 2.625
  )
  ggsave(
    file.path("outputs/figures/presentation", paste0(name, "_presentation.pdf")),
    plot = plot(base_sz = 22, bold = TRUE),
    device = cairo_pdf, width = 10, height = 6
  )
  cat("保存完了:", name, "\n")
}

# =============================================================================
# Figure 1: 曜日別アラーム件数（箱ひげ図）
# =============================================================================

daily <- df |>
  mutate(date = as.Date(datetime)) |>
  count(date, name = "n") |>
  mutate(dow = factor(
    wday(date, label = TRUE, abbr = TRUE, week_start = 1),
    levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
  ))

fig1 <- function(base_sz, bold) {
  face <- if (bold) "bold" else "plain"
  ggplot(daily, aes(x = dow, y = n)) +
    geom_boxplot(fill = "#0072B2", color = "black", alpha = 0.7,
                 outlier.shape = 16, outlier.size = if (bold) 3 else 1.5) +
    labs(x = "Day of Week", y = "Number of Alarms (per day)") +
    theme_classic(base_size = base_sz) +
    theme(
      axis.title = element_text(face = face),
      axis.text  = element_text(face = face),
      axis.line  = element_line(linewidth = if (bold) 1.2 else 0.5)
    )
}

save_both(fig1, "fig1_dow_boxplot")

# =============================================================================
# Figure 2: 時間帯別アラーム件数（技術的 vs 臨床的、積み上げ棒グラフ）
# =============================================================================

n_days <- as.numeric(as.Date("2025-09-09") - as.Date("2025-07-15")) + 1

hourly_class <- df |>
  mutate(
    hour        = hour(datetime),
    alarm_class = factor(alarm_class,
                         levels = c("technical", "clinical"),
                         labels = c("Technical", "Clinical"))
  ) |>
  count(hour, alarm_class, name = "total") |>
  mutate(per_day = total / n_days)

fig2 <- function(base_sz, bold) {
  face <- if (bold) "bold" else "plain"
  ggplot(hourly_class, aes(x = hour, y = per_day, fill = alarm_class)) +
    geom_col() +
    scale_fill_manual(values = c("Technical" = "#F0E442", "Clinical" = "#0072B2")) +
    scale_x_continuous(breaks = seq(0, 23, by = 3)) +
    labs(
      x    = "Hour of Day",
      y    = "Number of Alarms (per day)",
      fill = "Alarm Type"
    ) +
    theme_minimal(base_size = base_sz, base_family = "Helvetica") +
    theme(
      panel.grid      = element_blank(),
      axis.title      = element_text(face = face),
      axis.text       = element_text(size = if (bold) NULL else 7),
      legend.title    = element_text(face = face),
      legend.text     = element_text(face = face),
      axis.line         = element_line(linewidth = if (bold) 1.2 else 0.5),
      legend.position   = c(0.85, 0.95),
      legend.background = element_rect(fill = "white", color = NA),
      legend.key.size   = unit(if (bold) 0.5 else 0.3, "cm")
    )
}

dir.create("outputs/figures/journal/Figure1A",       showWarnings = FALSE, recursive = TRUE)
dir.create("outputs/figures/presentation/Figure1A",  showWarnings = FALSE, recursive = TRUE)

ggsave(
  "outputs/figures/journal/Figure1A/fig2_hourly_alarm_type_journal.pdf",
  plot   = fig2(base_sz = 9, bold = FALSE),
  device = cairo_pdf, width = 3.5, height = 2.625
)
ggsave(
  "outputs/figures/presentation/Figure1A/fig2_hourly_alarm_type_presentation.pdf",
  plot   = fig2(base_sz = 22, bold = TRUE),
  device = cairo_pdf, width = 10, height = 6
)
cat("保存完了: fig2_hourly_alarm_type\n")

# =============================================================================
# Figure 3: duration分布（消音あり vs なし）
# 対象：臨床的アラーム、duration 1〜300秒（外れ値除外して見やすく）
# =============================================================================

dur_data <- df |>
  filter(alarm_class == "clinical", duration_sec >= 1, duration_sec <= 300) |>
  mutate(
    silenced_label = factor(silenced,
                            levels = c(FALSE, TRUE),
                            labels = c("Not Silenced", "Silenced"))
  )

fig3 <- function(base_sz, bold) {
  face <- if (bold) "bold" else "plain"
  ggplot(dur_data, aes(x = duration_sec, fill = silenced_label)) +
    geom_histogram(binwidth = 10, position = "identity", alpha = 0.7) +
    scale_fill_manual(values = c("Not Silenced" = "#F0E442", "Silenced" = "#0072B2")) +
    labs(
      x    = "Alarm Duration (seconds)",
      y    = "Count",
      fill = NULL
    ) +
    theme_classic(base_size = base_sz) +
    theme(
      axis.title      = element_text(face = face),
      axis.text       = element_text(face = face),
      legend.text     = element_text(face = face),
      axis.line       = element_line(linewidth = if (bold) 1.2 else 0.5),
      legend.position = "top"
    )
}

save_both(fig3, "fig3_duration_dist")
