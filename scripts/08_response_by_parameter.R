# =============================================================================
# 08_response_by_parameter.R
# パラメータ（内容１）× 優先度 × 分類 別の消去率・消去時間
#
# 【目的】
#   各アラームパラメータに対して看護師がどの程度・どの速さで消去しているかを定量化する。
#   消去率と消去時間の組み合わせは「看護師の直感的トリアージ」を反映している可能性がある。
#
# 【解釈の枠組み】
#   消去率が高く消去時間が短い → 看護師が優先的に対応するパラメータ
#   消去率が低く消去時間が長い → 無視 or 鑑別困難なパラメータ
# =============================================================================

library(dplyr)
library(readr)
library(ggplot2)
library(ggrepel)  # install.packages("ggrepel") が必要

df       <- readRDS("data/proceeded/02_cleaned.rds")
bed_zone <- read_csv("data/proceeded/bed_zone.csv", show_col_types = FALSE)

MIN_N <- 10  # 件数足切り閾値

# -----------------------------------------------------------------------------
# 1. 集計
# -----------------------------------------------------------------------------

response_by_param <- df |>
  left_join(bed_zone, by = "ベッド名") |>
  group_by(内容１, 優先度, alarm_class) |>
  summarise(
    n                  = n(),
    消去あり           = sum(silenced),
    消去率_pct         = round(mean(silenced) * 100, 1),
    消去時間中央値_sec = median(response_sec, na.rm = TRUE),
    duration中央値_sec = median(duration_sec, na.rm = TRUE),
    高重症割合_pct     = round(mean(ゾーン == "高重症", na.rm = TRUE) * 100, 1),
    .groups = "drop"
  ) |>
  filter(n >= MIN_N) |>
  arrange(desc(消去率_pct))

print(response_by_param, n = Inf)

# -----------------------------------------------------------------------------
# 2. 保存
# -----------------------------------------------------------------------------

write_excel_csv(response_by_param,
                "outputs/stats/stats_response_by_parameter.csv")
cat("保存完了: outputs/stats/stats_response_by_parameter.csv\n")

# -----------------------------------------------------------------------------
# 3. 可視化：消去率 × 消去時間 散布図
#    消去時間がNA（消去ありが0件）のパラメータは除外してプロット
# -----------------------------------------------------------------------------

plot_data <- response_by_param |>
  filter(!is.na(消去時間中央値_sec)) |>
  mutate(
    param_label = recode(内容１,
      "電波切れ" = "Signal Loss",
      "電極確認" = "Electrode Check",
      "解析不能" = "Unanalyzable",
      .default   = 内容１
    )
  )

p <- ggplot(plot_data,
            aes(x     = 消去率_pct,
                y     = 消去時間中央値_sec,
                color = alarm_class,
                shape = 優先度,
                label = param_label)) +
  geom_point(aes(size = n), alpha = 0.7) +
  geom_text_repel(size = 2.8, max.overlaps = 25,
                  segment.color = "grey60", segment.size = 0.3) +
  scale_color_manual(
    values = c("clinical" = "#0072B2", "technical" = "#E69F00"),
    labels = c("Clinical", "Technical")
  ) +
  scale_size_area(max_size = 14, name = "Count",
                  labels = scales::comma) +
  scale_y_log10(
    breaks = c(1, 5, 10, 30, 60, 120, 300, 600, 1800),
    labels = c("1", "5", "10", "30", "60", "120", "300", "600", "1800")
  ) +
  labs(
    x        = "Dismissal Rate (%)",
    y        = "Median Time-to-Dismiss (sec, log scale)",
    color    = "Alarm Class",
    shape    = "Priority",
    title    = "Alarm Dismissal Rate vs. Time-to-Dismiss by Parameter",
    subtitle = paste0("Parameters with n ≥ ", MIN_N, " only. ",
                      "Point size proportional to alarm count.")
  ) +
  theme_classic(base_size = 12) +
  theme(legend.position = "right")

ggsave("outputs/figures/journal/fig_response_by_parameter.pdf",
       plot = p, device = cairo_pdf, width = 7, height = 5)
ggsave("outputs/figures/presentation/fig_response_by_parameter.pdf",
       plot = p, device = cairo_pdf, width = 12, height = 8)

cat("保存完了: fig_response_by_parameter.pdf (journal / presentation)\n")
