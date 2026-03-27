# =============================================================================
# 10b_feature_engineering_sensitivity.R
# 感度分析用データセットの作成（alarm_burden 時間窓: 15 / 30 / 45 分）
#
# 【目的】
#   主解析（10_feature_engineering.R）では 5 / 10 / 30 分窓を使用したが、
#   査読者からの指摘を受け、より等間隔な 15 / 30 / 45 分窓での比較を行う。
#   本スクリプトは既存の 03_analysis_ready.rds を上書きせず、
#   感度分析専用の 03b_analysis_ready_sensitivity.rds を生成する。
#
# 【alarm_burden の定義】
#   各臨床アラームについて、
#   「同一病棟（西10 / 西11）の直前N分間の臨床アラーム件数（自分自身を除く）」
#
#   ウィンドウ: [datetime_i - N分, datetime_i)
#   病棟単位にした理由: 看護師はフロア全体のセントラルモニタを共有しており、
#   自分の担当床以外のアラームも視覚的・聴覚的に曝露されるため
#
# 【時間窓の選択根拠（15 / 30 / 45 分）】
#   等間隔の 3 窓により用量反応関係を評価しやすくする。
#   15 分: 看護師の巡回サイクルに近い臨床的に意味のある単位。
#   30 分: 主解析窓（後方互換のため alarm_burden の列名を維持）。
#   45 分: 短期シフト内の累積負荷を反映する上限窓。
#
# 【計算方法（slider パッケージ）】
#   slide_index_dbl() で各ローリング和を算出後、自分自身の 1 件を減算。
#
# 【出力】
#   data/proceeded/03b_analysis_ready_sensitivity.rds
#     臨床的アラームのみ（alarm_class == "clinical"）
#     duration フィルタは主解析スクリプト側で適用（ここでは全件保持）
#
# 【依存パッケージ】
#   slider: install.packages("slider")
# =============================================================================

library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(slider)   # 高速ローリング集計

# -----------------------------------------------------------------------------
# 0. データ読み込み・前処理
# -----------------------------------------------------------------------------

df       <- readRDS("data/proceeded/02_cleaned.rds")
bed_zone <- read_csv("data/proceeded/bed_zone.csv", show_col_types = FALSE)

# ward 変数の付与（ベッド名プレフィックスで判定）
df <- df |>
  mutate(
    ward = case_when(
      str_starts(ベッド名, "W10") ~ "West 10",
      TRUE                        ~ "West 11"   # W11-*, 1101-*, Re-*
    )
  ) |>
  left_join(bed_zone, by = "ベッド名")

cat("全アラーム件数:", nrow(df), "\n")
cat("ward 分布:\n"); print(table(df$ward))

# -----------------------------------------------------------------------------
# 1. 臨床的アラームに絞る
# -----------------------------------------------------------------------------

df_clinical <- df |>
  filter(alarm_class == "clinical") |>
  arrange(ward, datetime)    # slide_index は昇順ソートが必須

cat("\n臨床的アラーム件数:", nrow(df_clinical), "\n")
cat("優先度分布:\n"); print(table(df_clinical$優先度))

# -----------------------------------------------------------------------------
# 2. alarm_burden の計算（15 / 30 / 45 分ローリングウィンドウ・同一病棟）
# -----------------------------------------------------------------------------

# --- 2a. 臨床アラームのみの負荷（感度分析用・3 時間窓） ---
# 「臨床的判断の対象となるアラーム」に絞った定義。
# 15 / 30 / 45 分の 3 変数を一括計算する。
# alarm_burden は 30 分窓（主解析との比較を容易にするため列名を維持）。

df_clinical <- df_clinical |>
  group_by(ward) |>
  mutate(
    burden_15m = slide_index_dbl(
      .x      = rep(1L, n()),
      .i      = datetime,
      .f      = sum,
      .before = dminutes(15),
      .after  = 0
    ) - 1L,    # 自分自身を除外
    alarm_burden = slide_index_dbl(   # 30 分窓・主解析との後方互換のため列名維持
      .x      = rep(1L, n()),
      .i      = datetime,
      .f      = sum,
      .before = dminutes(30),
      .after  = 0
    ) - 1L,
    burden_45m = slide_index_dbl(
      .x      = rep(1L, n()),
      .i      = datetime,
      .f      = sum,
      .before = dminutes(45),
      .after  = 0
    ) - 1L
  ) |>
  ungroup()

# --- 2b. 全アラーム（技術的 + 臨床的）の負荷（感度分析用） ---
# 技術的アラームも看護師の聴覚・視覚に曝露されるため、
# 総負荷での感度分析に備えて計算しておく。メイン解析には使用しない。

# 全アラームの病棟別・時系列テーブル
all_ward_ts <- df |>
  arrange(ward, datetime) |>
  group_by(ward) |>
  mutate(
    burden_total_including_self = slide_index_dbl(
      .x      = rep(1L, n()),
      .i      = datetime,
      .f      = sum,
      .before = dminutes(30),
      .after  = 0
    )
  ) |>
  ungroup() |>
  select(ward, datetime, ベッド名, 内容１, burden_total_including_self)

# 臨床アラームの行に結合（同一 ward・datetime で join）
df_clinical <- df_clinical |>
  left_join(
    all_ward_ts |>
      filter(ward == ward) |>    # ward が一致する行のみ
      select(ward, datetime, ベッド名, 内容１, burden_total_including_self),
    by = c("ward", "datetime", "ベッド名", "内容１")
  ) |>
  mutate(
    # 自分自身の 1 件を除外
    alarm_burden_total = burden_total_including_self - 1L
  ) |>
  select(-burden_total_including_self)

# 確認: 各時間窓の分布
summarise_burden <- function(x, label) {
  cat("\n[感度分析用]", label, "の分布:\n")
  cat("  最小値:", min(x), "\n")
  cat("  中央値:", median(x), "\n")
  cat("  平均値:", round(mean(x), 1), "\n")
  cat("  P75   :", quantile(x, 0.75), "\n")
  cat("  P95   :", quantile(x, 0.95), "\n")
  cat("  最大値:", max(x), "\n")
}

summarise_burden(df_clinical$burden_15m,   "burden_15m（直前15分）")
summarise_burden(df_clinical$alarm_burden, "alarm_burden（直前30分）")
summarise_burden(df_clinical$burden_45m,   "burden_45m（直前45分）")

cat("\n[感度分析用] alarm_burden_total（全アラーム）の分布:\n")
cat("  最小値:", min(df_clinical$alarm_burden_total, na.rm = TRUE), "\n")
cat("  中央値:", median(df_clinical$alarm_burden_total, na.rm = TRUE), "\n")
cat("  平均値:", round(mean(df_clinical$alarm_burden_total, na.rm = TRUE), 1), "\n")
cat("  P75   :", quantile(df_clinical$alarm_burden_total, 0.75, na.rm = TRUE), "\n")
cat("  P95   :", quantile(df_clinical$alarm_burden_total, 0.95, na.rm = TRUE), "\n")
cat("  最大値:", max(df_clinical$alarm_burden_total, na.rm = TRUE), "\n")

# -----------------------------------------------------------------------------
# 3. burden_quartile の付与（可視化・層別解析用）
# -----------------------------------------------------------------------------

# 四分位ラベルに実際の件数範囲を表示する（主解析と同様に alarm_burden 基準）
quartile_ranges <- df_clinical |>
  mutate(burden_quartile = ntile(alarm_burden, 4)) |>
  group_by(burden_quartile) |>
  summarise(
    min_b = min(alarm_burden),
    max_b = max(alarm_burden),
    .groups = "drop"
  ) |>
  mutate(
    burden_q_label = paste0("Q", burden_quartile,
                            " (", min_b, "\u2013", max_b, " alarms/30min)")
  )

cat("\n負荷四分位ラベル:\n")
print(quartile_ranges)

df_clinical <- df_clinical |>
  mutate(burden_quartile = ntile(alarm_burden, 4)) |>
  left_join(quartile_ranges |> select(burden_quartile, burden_q_label),
            by = "burden_quartile") |>
  mutate(
    burden_quartile = factor(burden_quartile, levels = 1:4),
    burden_q_label  = factor(burden_q_label,
                             levels = quartile_ranges$burden_q_label)
  )

# -----------------------------------------------------------------------------
# 4. 主解析用変数の型・順序の整理
# -----------------------------------------------------------------------------

df_clinical <- df_clinical |>
  mutate(
    # 優先度を順序因子に（モデルの参照水準は ADVISORY）
    priority_fct = factor(優先度,
                          levels = c("ADVISORY", "WARNING", "CRISIS"),
                          ordered = FALSE),
    # ゾーンを因子に（参照水準は一般）
    zone_fct     = factor(ゾーン,
                          levels = c("一般", "高重症")),
    # 時間帯・曜日（共変量）
    hour         = hour(datetime),
    dow          = wday(datetime, label = FALSE, week_start = 1)  # 1=月〜7=日
  )

# -----------------------------------------------------------------------------
# 5. 最終確認と保存
# -----------------------------------------------------------------------------

cat("\n=== 感度分析データセット 概要 ===\n")
cat("行数:", nrow(df_clinical), "\n")
cat("列数:", ncol(df_clinical), "\n")

cat("\n優先度 × 消音 のクロス集計:\n")
print(table(df_clinical$priority_fct, df_clinical$silenced))

cat("\nゾーン分布:\n")
print(table(df_clinical$zone_fct, useNA = "ifany"))

cat("\nward 分布:\n")
print(table(df_clinical$ward))

cat("\nalarm_burden × priority_fct 中央値:\n")
df_clinical |>
  group_by(priority_fct) |>
  summarise(
    median_burden = median(alarm_burden),
    mean_burden   = round(mean(alarm_burden), 1),
    .groups = "drop"
  ) |>
  print()

# RDS 保存
saveRDS(df_clinical, "data/proceeded/03b_analysis_ready_sensitivity.rds")
cat("\n保存完了: data/proceeded/03b_analysis_ready_sensitivity.rds\n")
