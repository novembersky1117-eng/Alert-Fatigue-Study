# =============================================================================
# Phase 1: 探索的分析 - アラーム分類・看護師反応の実態把握
#
# 【目的】
# alarm_classおよびsilencedを用いて、技術的/臨床的アラームへの反応実態を把握する。
# この探索を通じて、Phase 2以降の解析方針（仮説と対象の絞り込み）を決定した。
#
# 【主な発見】
# 1. 技術的アラーム（69.5%）は看護師にほぼ無視されている（消音率0.6%）
# 2. 臨床的アラームの消音率も8.2%と低いが、CRISISは71.2%と高い
# 3. 「無反応」の多くは「短時間で自然終了」による可能性がある
#    → 消音あり中央値36秒 vs 消音なし中央値10秒
# 4. duration ≥30秒 に絞ると消音率が上昇し、優先度の差が明確になる
#    CRISIS:93%、WARNING:25%、ADVISORY:18%
# 5. 「無反応＝アラーム疲労」とは単純に言えない。優先度による合理的トリアージの
#    可能性がある。
#
# 【次のステップへの方針】
# 「同じ優先度（WARNING）のアラームでも、直前の曝露密度が高い時間帯ほど
#  消音率が低下するか」を検証する（Phase 2: 特徴量設計）。
# 対象: 臨床的WARNINGアラーム、duration ≥30秒
# =============================================================================

library(dplyr)
library(readr)

df <- readRDS("data/proceeded/02_cleaned.rds")

# -----------------------------------------------------------------------------
# 1. 技術的 vs 臨床的アラームの消音率
# -----------------------------------------------------------------------------

cat("=== 分類別 消音率・反応時間 ===\n")
df |>
  group_by(alarm_class) |>
  summarise(
    総件数     = n(),
    消音あり   = sum(silenced),
    消音率_pct = round(mean(silenced) * 100, 1),
    反応時間中央値_sec = median(response_sec, na.rm = TRUE),
    .groups = "drop"
  ) |> print()

cat("\n=== 優先度×分類 別の消音率・反応時間 ===\n")
df |>
  group_by(alarm_class, 優先度) |>
  summarise(
    総件数     = n(),
    消音あり   = sum(silenced),
    消音率_pct = round(mean(silenced) * 100, 1),
    反応時間中央値_sec = median(response_sec, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(alarm_class, 優先度) |> print()

# -----------------------------------------------------------------------------
# 2. duration分布の確認（閾値設定の根拠）
# -----------------------------------------------------------------------------

clinical <- df |> filter(alarm_class == "clinical")

cat("\n=== 臨床的アラームのduration_sec 分布 ===\n")
cat("総件数:", nrow(clinical), "\n")
cat("0秒    :", sum(clinical$duration_sec == 0), "件\n")
cat("1-9秒  :", sum(clinical$duration_sec >= 1  & clinical$duration_sec < 10), "件\n")
cat("10-29秒:", sum(clinical$duration_sec >= 10 & clinical$duration_sec < 30), "件\n")
cat("30-59秒:", sum(clinical$duration_sec >= 30 & clinical$duration_sec < 60), "件\n")
cat("60秒以上:", sum(clinical$duration_sec >= 60), "件\n")

cat("\n=== 消音あり vs なし 別のduration分布 ===\n")
clinical |>
  group_by(silenced) |>
  summarise(
    n           = n(),
    中央値      = median(duration_sec),
    IQR下限     = quantile(duration_sec, 0.25),
    IQR上限     = quantile(duration_sec, 0.75),
    under_10sec = sum(duration_sec < 10),
    under_30sec = sum(duration_sec < 30),
    .groups     = "drop"
  ) |> print()

# -----------------------------------------------------------------------------
# 3. 優先度別の消音率（duration ≥30秒に絞る）
# -----------------------------------------------------------------------------

cat("\n=== 優先度別 消音率（臨床的アラーム・duration ≥30秒）===\n")
clinical |>
  filter(duration_sec >= 30) |>
  group_by(優先度) |>
  summarise(
    n          = n(),
    消音あり   = sum(silenced),
    消音率_pct = round(mean(silenced) * 100, 1),
    .groups    = "drop"
  ) |> print()

# -----------------------------------------------------------------------------
# 4. 結果の保存
# -----------------------------------------------------------------------------

# 分類別・優先度別の消音率サマリー
response_summary <- df |>
  filter(duration_sec >= 30) |>
  group_by(alarm_class, 優先度) |>
  summarise(
    n          = n(),
    消音あり   = sum(silenced),
    消音率_pct = round(mean(silenced) * 100, 1),
    反応時間中央値_sec = median(response_sec, na.rm = TRUE),
    .groups    = "drop"
  )

write_excel_csv(response_summary, "outputs/stats/stats_response_by_class_priority.csv")
cat("\n保存完了: outputs/stats/stats_response_by_class_priority.csv\n")
