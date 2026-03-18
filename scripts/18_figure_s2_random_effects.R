# =============================================================================
# 18_figure_s2_random_effects.R
# Figure S2: Bed-level random intercepts from the mixed-effects logistic regression
#
# 【目的】
#   主解析モデル（30分窓）のベッドレベルランダム切片を可視化する。
#   特定のベッドロケーションが結果を歪めていないことを示す診断図。
#
# 【注意】
#   ベッド番号は患者個人ではなく「ロケーション」を表す。
#   観察期間中に患者は入れ替わるため、ランダム切片は
#   ベッド位置・担当看護師傾向・患者タイプの複合を反映する。
#
# 【前提】
#   data/proceeded/fit_30m.rds が存在すること。
#   なければ 11_main_analysis.R を先に実行すること。
#
# 【出力】
#   outputs/figures/journal/fig_s2_random_effects.pdf  （3.5 × 3 inch）
# =============================================================================

library(lme4)
library(ggplot2)
library(dplyr)

# -----------------------------------------------------------------------------
# 0. モデル読み込み
# -----------------------------------------------------------------------------

fit_30m <- readRDS("data/proceeded/fit_30m.rds")

# -----------------------------------------------------------------------------
# 1. ランダム効果（条件付き分散込み）の取り出し
# -----------------------------------------------------------------------------

re_cv <- ranef(fit_30m, condVar = TRUE)

re_df <- re_cv[["ベッド名"]] |>
  tibble::rownames_to_column("bed") |>
  rename(intercept = "(Intercept)") |>
  mutate(
    se      = sqrt(attr(re_cv[["ベッド名"]], "postVar")[1, 1, ]),
    ci_low  = intercept - 1.96 * se,
    ci_high = intercept + 1.96 * se
  ) |>
  arrange(intercept) |>
  mutate(bed_rank = row_number())

sigma_re <- sqrt(VarCorr(fit_30m)[["ベッド名"]][1, 1])

cat("ベッド数:", nrow(re_df), "\n")
cat("RE SD:", round(sigma_re, 3), "\n")
cat("RE variance:", round(sigma_re^2, 3), "\n")

# -----------------------------------------------------------------------------
# 2. Caterpillar plot
# -----------------------------------------------------------------------------

p <- ggplot(re_df, aes(x = bed_rank, y = intercept)) +
  geom_hline(yintercept = 0, linetype = "dashed",
             color = "gray50", linewidth = 0.5) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high),
                width = 0, linewidth = 0.4, color = "gray60") +
  geom_point(size = 1.8, color = "#2166ac") +
  annotate("text", x = 38, y = 1.85,
           label = paste0("SD = ", round(sigma_re, 3)),
           hjust = 1, size = 2.8, family = "Helvetica") +
  scale_x_continuous(breaks = NULL) +
  labs(
    x     = "Beds (ranked by random intercept)",
    y     = "Random intercept (log-odds scale)",
    title = NULL
  ) +
  theme_bw(base_size = 9, base_family = "Helvetica") +
  theme(panel.grid.minor = element_blank())

# -----------------------------------------------------------------------------
# 3. 保存
# -----------------------------------------------------------------------------

cairo_pdf("outputs/figures/journal/fig_s2_random_effects.pdf",
          width = 3.5, height = 3, family = "Helvetica")
print(p)
dev.off()
cat("保存: outputs/figures/journal/fig_s2_random_effects.pdf\n")
