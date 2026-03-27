# =============================================================================
# 12_lagged_burden.R
# 探索的感度分析: lagged alarm burden（遅延曝露）変数の計算と確認
#
# 【目的】
#   直近の負荷（alarm_burden: 直前30分）に加え、
#   時差のある負荷（burden_lag: 2時間前〜1時間前）を算出し、
#   両者の分布・相関を確認する。
#
# 【burden_lag の定義】
#   同一病棟の [datetime - 2h, datetime - 1h) 区間の臨床アラーム件数
#   主解析窓（直前30分）と重複しないよう 1 時間のギャップを設けている。
#
# 【出力】
#   data/proceeded/04_analysis_lagged.rds
# =============================================================================

library(dplyr)
library(lubridate)
library(slider)
library(ggplot2)

# -----------------------------------------------------------------------------
# 0. データ読み込み
# -----------------------------------------------------------------------------

df <- readRDS("data/proceeded/03_analysis_ready.rds")
cat("読み込み件数:", nrow(df), "\n")

# -----------------------------------------------------------------------------
# 1. burden_lag の計算
#    [datetime - 2h, datetime - 1h) のウィンドウ
#    = 直前2時間の合計 - 直前1時間の合計
# -----------------------------------------------------------------------------

df_lag <- df |>
  arrange(ward, datetime) |>
  group_by(ward) |>
  mutate(
    # 直前2時間の臨床アラーム数（自分含む）
    burden_2h_raw = slide_index_dbl(
      .x      = rep(1L, n()),
      .i      = datetime,
      .f      = sum,
      .before = dhours(2),
      .after  = 0
    ),
    # 直前1時間の臨床アラーム数（自分含む）
    burden_1h_raw = slide_index_dbl(
      .x      = rep(1L, n()),
      .i      = datetime,
      .f      = sum,
      .before = dhours(1),
      .after  = 0
    ),
    # ラグ窓 = 2時間合計 - 1時間合計（自分自身は1時間窓に含まれるので除外不要）
    burden_lag = burden_2h_raw - burden_1h_raw
  ) |>
  ungroup() |>
  select(-burden_2h_raw, -burden_1h_raw)

# -----------------------------------------------------------------------------
# 2. 分布確認
# -----------------------------------------------------------------------------

cat("\n=== burden_lag（2〜1時間前）の分布 ===\n")
cat("  最小値:", min(df_lag$burden_lag), "\n")
cat("  中央値:", median(df_lag$burden_lag), "\n")
cat("  平均値:", round(mean(df_lag$burden_lag), 1), "\n")
cat("  P75   :", quantile(df_lag$burden_lag, 0.75), "\n")
cat("  P95   :", quantile(df_lag$burden_lag, 0.95), "\n")
cat("  最大値:", max(df_lag$burden_lag), "\n")

cat("\n=== alarm_burden（直前30分）の分布（参考）===\n")
cat("  最小値:", min(df_lag$alarm_burden), "\n")
cat("  中央値:", median(df_lag$alarm_burden), "\n")
cat("  平均値:", round(mean(df_lag$alarm_burden), 1), "\n")
cat("  P75   :", quantile(df_lag$alarm_burden, 0.75), "\n")
cat("  P95   :", quantile(df_lag$alarm_burden, 0.95), "\n")
cat("  最大値:", max(df_lag$alarm_burden), "\n")

# -----------------------------------------------------------------------------
# 3. 相関確認
# -----------------------------------------------------------------------------

cat("\n=== alarm_burden × burden_lag の相関 ===\n")
r <- cor(df_lag$alarm_burden, df_lag$burden_lag, method = "pearson")
r_sp <- cor(df_lag$alarm_burden, df_lag$burden_lag, method = "spearman")
cat("  Pearson r  :", round(r, 3), "\n")
cat("  Spearman ρ :", round(r_sp, 3), "\n")

if (abs(r) > 0.7) {
  cat("  [注意] 相関が高い（r > 0.7）→ 多重共線性に注意\n")
} else {
  cat("  [OK] 相関は許容範囲内\n")
}

# -----------------------------------------------------------------------------
# 4. 保存
# -----------------------------------------------------------------------------

saveRDS(df_lag, "data/proceeded/04_analysis_lagged.rds")
cat("\n保存完了: data/proceeded/04_analysis_lagged.rds\n")
