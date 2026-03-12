# =============================================================================
# 09_exposure_description.R
# Table 1: アラーム環境の定量的記述（論文用・発表用）
#
# 【目的】
#   レジリエンス論文の "ノイズ環境の定量化" セクション（Table 1）を構成する
#   統計量をすべてこのスクリプトで算出し、論文用英語CSV・発表用日本語CSVを出力する。
#
# 【出力ファイル】
#   outputs/stats/table1_main_EN.csv      論文用 Table 1（全体像・技術的 vs 臨床的）
#   outputs/stats/table1_clinical_EN.csv  論文用 Table 1b（臨床アラーム優先度別詳細）
#   outputs/stats/table1_main_JA.csv      発表用（同上・日本語・BOM付UTF-8）
#   outputs/stats/table1_clinical_JA.csv  発表用（同上・日本語・BOM付UTF-8）
#
# 【設計上の注意】
#   - 母集団: 02_cleaned.rds の全アラーム（duration フィルタなし）
#     → 主解析（10_feature_engineering.R 以降）で適用する duration ≥ 30秒 の
#       フィルタは Methods に記述し、Table 1 は研究環境の記述として全数を示す
#   - アラーム密度: n_beds = 41 床（高重症 12 + 一般 29）、n_days = 57 日
#   - TTD（消音時間）: silenced == TRUE の response_sec のみから算出
#   - ward 変数: ベッド名から西10 / 西11 を判定
#       W10-*        → 西10（West 10）
#       W11-*, 1101-*, Re-* → 西11（West 11）
# =============================================================================

library(dplyr)
library(readr)
library(stringr)

# -----------------------------------------------------------------------------
# 0. データ読み込み・前処理
# -----------------------------------------------------------------------------

df       <- readRDS("data/proceeded/02_cleaned.rds")
bed_zone <- read_csv("data/proceeded/bed_zone.csv", show_col_types = FALSE)

N_BEDS <- 41
N_DAYS <- 57

df <- df |>
  mutate(
    ward = case_when(
      str_starts(ベッド名, "W10") ~ "West 10",
      TRUE                        ~ "West 11"   # W11-*, 1101-*, Re-*
    )
  ) |>
  left_join(bed_zone, by = "ベッド名")

cat("読み込み完了:", nrow(df), "件\n")
cat("ward 確認:\n"); print(table(df$ward, useNA = "ifany"))
cat("ゾーン 確認:\n"); print(table(df$ゾーン, useNA = "ifany"))

# -----------------------------------------------------------------------------
# 1. ヘルパー関数
# -----------------------------------------------------------------------------

# "X,XXX (XX.X%)" 形式
fmt_n_pct <- function(n, total) {
  paste0(format(n, big.mark = ","), " (", round(n / total * 100, 1), "%)")
}

# "XXX [XXX–XXX]" 形式（median [IQR]）
fmt_median_iqr <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return("—")
  q <- quantile(x, c(0.25, 0.5, 0.75))
  paste0(round(q[2], 0), " [", round(q[1], 0), "\u2013", round(q[3], 0), "]")
}

# -----------------------------------------------------------------------------
# 2. 列ごとの統計量を計算する関数
# -----------------------------------------------------------------------------

make_col_stats <- function(d, total_n) {
  n <- nrow(d)
  list(
    n_pct     = fmt_n_pct(n, total_n),
    density   = as.character(round(n / N_BEDS / N_DAYS, 1)),
    advisory  = fmt_n_pct(sum(d$優先度 == "ADVISORY"), n),
    warning   = fmt_n_pct(sum(d$優先度 == "WARNING"),  n),
    crisis    = fmt_n_pct(sum(d$優先度 == "CRISIS"),   n),
    dismissed = fmt_n_pct(sum(d$silenced), n),
    ttd       = fmt_median_iqr(d$response_sec[d$silenced]),
    duration  = fmt_median_iqr(d$duration_sec)
  )
}

total_n    <- nrow(df)
all_s      <- make_col_stats(df,                                    total_n)
tech_s     <- make_col_stats(filter(df, alarm_class == "technical"), total_n)
clin_s     <- make_col_stats(filter(df, alarm_class == "clinical"),  total_n)

# -----------------------------------------------------------------------------
# 3. Table 1 Main の構築
# -----------------------------------------------------------------------------

build_table1_main <- function(all_s, tech_s, clin_s,
                               col_all, col_tech, col_clin) {
  tibble(
    Characteristic = c(
      "Events, n (%)",
      "Alarm density (/bed/day)",
      "Priority",
      "  ADVISORY, n (%)",
      "  WARNING, n (%)",
      "  CRISIS, n (%)",
      "Dismissed alarms",
      "  Dismissed, n (%)",
      "  Time-to-dismiss, median [IQR], sec",
      "Alarm duration",
      "  Duration, median [IQR], sec"
    ),
    !!col_all  := c(all_s$n_pct,  all_s$density,  "", all_s$advisory,  all_s$warning,  all_s$crisis,  "", all_s$dismissed,  all_s$ttd,  "", all_s$duration),
    !!col_tech := c(tech_s$n_pct, tech_s$density, "", tech_s$advisory, tech_s$warning, tech_s$crisis, "", tech_s$dismissed, tech_s$ttd, "", tech_s$duration),
    !!col_clin := c(clin_s$n_pct, clin_s$density, "", clin_s$advisory, clin_s$warning, clin_s$crisis, "", clin_s$dismissed, clin_s$ttd, "", clin_s$duration)
  )
}

table1_main_EN <- build_table1_main(
  all_s, tech_s, clin_s,
  "All Alarms", "Technical Alarms", "Clinical Alarms"
)

table1_main_JA <- build_table1_main(
  all_s, tech_s, clin_s,
  "全アラーム", "技術的アラーム", "臨床的アラーム"
) |>
  rename(
    "項目"         = Characteristic,
    "全アラーム"   = `全アラーム`,
    "技術的アラーム" = `技術的アラーム`,
    "臨床的アラーム" = `臨床的アラーム`
  ) |>
  mutate(項目 = recode(項目,
    "Events, n (%)"                          = "件数, n (%)",
    "Alarm density (/bed/day)"               = "アラーム密度（件/床/日）",
    "Priority"                               = "優先度",
    "  ADVISORY, n (%)"                      = "  ADVISORY, n (%)",
    "  WARNING, n (%)"                       = "  WARNING, n (%)",
    "  CRISIS, n (%)"                        = "  CRISIS, n (%)",
    "Dismissed alarms"                       = "消音行動",
    "  Dismissed, n (%)"                     = "  消音あり, n (%)",
    "  Time-to-dismiss, median [IQR], sec"   = "  消音時間 中央値 [IQR]（秒）",
    "Alarm duration"                         = "アラーム持続時間",
    "  Duration, median [IQR], sec"          = "  持続時間 中央値 [IQR]（秒）"
  ))

# -----------------------------------------------------------------------------
# 4. Table 1b Clinical の構築（優先度別・"針の目"指標付き）
# -----------------------------------------------------------------------------

clin_df     <- filter(df, alarm_class == "clinical")
total_clin  <- nrow(clin_df)

make_priority_stats <- function(d, all_n, clin_n) {
  n <- nrow(d)
  list(
    n_of_clin   = fmt_n_pct(n, clin_n),
    n_of_all    = fmt_n_pct(n, all_n),    # 針の目指標
    dismissed   = fmt_n_pct(sum(d$silenced), n),
    ttd         = fmt_median_iqr(d$response_sec[d$silenced]),
    duration    = fmt_median_iqr(d$duration_sec),
    high_acuity = fmt_n_pct(sum(d$ゾーン == "高重症", na.rm = TRUE), n),
    general     = fmt_n_pct(sum(d$ゾーン == "一般",   na.rm = TRUE), n)
  )
}

adv_s <- make_priority_stats(filter(clin_df, 優先度 == "ADVISORY"), total_n, total_clin)
war_s <- make_priority_stats(filter(clin_df, 優先度 == "WARNING"),  total_n, total_clin)
cri_s <- make_priority_stats(filter(clin_df, 優先度 == "CRISIS"),   total_n, total_clin)

build_table1_clinical <- function(adv_s, war_s, cri_s,
                                   row_clin, row_all,
                                   row_dismissed, row_ttd, row_duration,
                                   row_zone, row_high, row_general,
                                   col_adv, col_war, col_cri) {
  tibble(
    Characteristic = c(
      row_clin, row_all,
      row_dismissed, row_ttd, row_duration,
      row_zone, row_high, row_general
    ),
    !!col_adv := c(adv_s$n_of_clin, adv_s$n_of_all, adv_s$dismissed, adv_s$ttd, adv_s$duration, "", adv_s$high_acuity, adv_s$general),
    !!col_war := c(war_s$n_of_clin, war_s$n_of_all, war_s$dismissed, war_s$ttd, war_s$duration, "", war_s$high_acuity, war_s$general),
    !!col_cri := c(cri_s$n_of_clin, cri_s$n_of_all, cri_s$dismissed, cri_s$ttd, cri_s$duration, "", cri_s$high_acuity, cri_s$general)
  )
}

table1_clin_EN <- build_table1_clinical(
  adv_s, war_s, cri_s,
  row_clin      = "Events, n (% of clinical alarms)",
  row_all       = "Events, n (% of ALL alarms)",
  row_dismissed = "Dismissed, n (%)",
  row_ttd       = "Time-to-dismiss, median [IQR], sec",
  row_duration  = "Duration, median [IQR], sec",
  row_zone      = "Ward zone",
  row_high      = "  High-acuity zone, n (%)",
  row_general   = "  General zone, n (%)",
  col_adv = "ADVISORY", col_war = "WARNING", col_cri = "CRISIS"
)

table1_clin_JA <- build_table1_clinical(
  adv_s, war_s, cri_s,
  row_clin      = "件数（臨床的アラーム中の割合）",
  row_all       = "件数（全アラーム中の割合）",
  row_dismissed = "消音あり, n (%)",
  row_ttd       = "消音時間 中央値 [IQR]（秒）",
  row_duration  = "持続時間 中央値 [IQR]（秒）",
  row_zone      = "ベッドゾーン",
  row_high      = "  高重症ゾーン, n (%)",
  row_general   = "  一般ゾーン, n (%)",
  col_adv = "ADVISORY", col_war = "WARNING", col_cri = "CRISIS"
) |>
  rename("項目" = Characteristic)

# -----------------------------------------------------------------------------
# 5. 保存
# -----------------------------------------------------------------------------

# 英語版: write_csv（BOMなし・標準UTF-8）
write_csv(table1_main_EN, "outputs/stats/table1_main_EN.csv")
write_csv(table1_clin_EN, "outputs/stats/table1_clinical_EN.csv")

# 日本語版: write_excel_csv（UTF-8 BOM付き → Excelで文字化けしない）
write_excel_csv(table1_main_JA, "outputs/stats/table1_main_JA.csv")
write_excel_csv(table1_clin_JA, "outputs/stats/table1_clinical_JA.csv")

cat("\n保存完了:\n")
cat("  outputs/stats/table1_main_EN.csv\n")
cat("  outputs/stats/table1_clinical_EN.csv\n")
cat("  outputs/stats/table1_main_JA.csv\n")
cat("  outputs/stats/table1_clinical_JA.csv\n")

# -----------------------------------------------------------------------------
# 6. コンソール確認出力
# -----------------------------------------------------------------------------

cat("\n========== Table 1 Main (EN) ==========\n")
print(table1_main_EN, n = Inf)

cat("\n========== Table 1b Clinical (EN) ==========\n")
print(table1_clin_EN, n = Inf)
