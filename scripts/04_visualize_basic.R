# =============================================================================
# Phase 1: 可視化 - 曜日別アラーム件数（箱ひげ図）
# Journal用・Presentation用の2種類を出力
# =============================================================================

library(dplyr)
library(lubridate)
library(ggplot2)

df <- readRDS("data/proceeded/02_cleaned.rds")

# 日別集計（曜日付き）
daily <- df |>
  mutate(date = as.Date(datetime)) |>
  count(date, name = "n") |>
  mutate(
    dow = wday(date, label = TRUE, abbr = TRUE, week_start = 1),
    dow = factor(dow, levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))
  )

# -----------------------------------------------------------------------------
# プロット関数（base_sizeだけ変えて2種類生成）
# -----------------------------------------------------------------------------

make_plot <- function(base_sz, bold = FALSE) {
  face <- if (bold) "bold" else "plain"
  ggplot(daily, aes(x = dow, y = n)) +
    geom_boxplot(fill = "#0072B2", color = "black", alpha = 0.7,
                 outlier.shape = 16, outlier.size = if (bold) 3 else 1.5) +
    labs(
      x = "Day of Week",
      y = "Number of Alarms (per day)"
    ) +
    theme_classic(base_size = base_sz) +
    theme(
      axis.title   = element_text(face = face),
      axis.text    = element_text(face = face),
      axis.line    = element_line(linewidth = if (bold) 1.2 else 0.5)
    )
}

# -----------------------------------------------------------------------------
# 保存
# -----------------------------------------------------------------------------

# Journal用
ggsave("outputs/figures/journal/fig_dow_boxplot_journal.pdf",
       plot   = make_plot(base_sz = 11),
       device = cairo_pdf, width = 3.5, height = 2.625)

# Presentation用
ggsave("outputs/figures/presentation/fig_dow_boxplot_presentation.pdf",
       plot   = make_plot(base_sz = 22, bold = TRUE),
       device = cairo_pdf, width = 10, height = 6)

cat("保存完了:\n")
cat("  outputs/figures/journal/fig_dow_boxplot_journal.pdf\n")
cat("  outputs/figures/presentation/fig_dow_boxplot_presentation.pdf\n")
