# =============================================================================
# 17_table_s3_parameters.R
# Table S3: Alarm events by parameter and priority level
#           （技術的アラーム + 臨床的アラームの統合版）
#
# 【目的】
#   技術的アラームと臨床的アラームのパラメータ別内訳をPriority別に示す。
#   - 技術的アラーム: Signal/Device カテゴリ 1種類
#   - 臨床的アラーム: 生理的パラメータ別（旧Table S3と同内容）
#
# 【出力】
#   outputs/tables/table_s3_parameters_en.csv
# =============================================================================

library(dplyr)
library(tidyr)
library(readr)

# -----------------------------------------------------------------------------
# 0. データ読み込み
# -----------------------------------------------------------------------------

df <- readRDS("data/proceeded/02_cleaned.rds")

priority_levels <- c("ADVISORY", "WARNING", "CRISIS")

# =============================================================================
# SECTION 1: 技術的アラーム
# =============================================================================

tech_param_map <- tribble(
  ~内容１,      ~parameter_en,
  "SpO2",      "SpO2 probe disconnection",
  "電極確認",   "Electrode disconnection",
  "電波切れ",   "Wireless signal loss",
  "解析不能",   "Unanalyzable signal",
  "NIBP",      "NIBP measurement failure"
)

total_tech <- nrow(df |> filter(alarm_class == "technical"))

tech_counts <- df |>
  filter(alarm_class == "technical") |>
  left_join(tech_param_map, by = "内容１") |>
  mutate(
    parameter_en = coalesce(parameter_en, 内容１),
    category     = "Signal / Device",
    # Priority を因子化して全レベルを揃える
    優先度 = factor(優先度, levels = priority_levels)
  ) |>
  count(category, parameter_en, 優先度, .drop = FALSE) |>
  mutate(
    total_class = total_tech,
    pct         = n / total_class * 100,
    cell        = if_else(n == 0, "—", sprintf("%d (%.1f%%)", n, pct))
  )

# Total列（全Priority合計）
tech_total_col <- tech_counts |>
  group_by(category, parameter_en) |>
  summarise(n_total = sum(n), .groups = "drop") |>
  mutate(
    pct_total = n_total / total_tech * 100,
    Total     = sprintf("%d (%.1f%%)", n_total, pct_total)
  )

# Wide形式
tech_wide <- tech_counts |>
  select(category, parameter_en, 優先度, cell) |>
  pivot_wider(names_from = 優先度, values_from = cell, values_fill = "—") |>
  left_join(tech_total_col |> select(parameter_en, Total), by = "parameter_en") |>
  # パラメータをn降順に並べる
  left_join(tech_total_col |> select(parameter_en, n_total), by = "parameter_en") |>
  arrange(desc(n_total)) |>
  select(Category = category, Parameter = parameter_en,
         any_of(priority_levels), Total)

# Total行
tech_total_row <- tibble(
  Category  = NA_character_,
  Parameter = "Total",
  ADVISORY  = sprintf("%d (%.1f%%)",
                      sum(df$alarm_class == "technical" & df$優先度 == "ADVISORY"),
                      sum(df$alarm_class == "technical" & df$優先度 == "ADVISORY") / total_tech * 100),
  WARNING   = sprintf("%d (%.1f%%)",
                      sum(df$alarm_class == "technical" & df$優先度 == "WARNING"),
                      sum(df$alarm_class == "technical" & df$優先度 == "WARNING") / total_tech * 100),
  CRISIS    = sprintf("%d (%.1f%%)",
                      sum(df$alarm_class == "technical" & df$優先度 == "CRISIS"),
                      sum(df$alarm_class == "technical" & df$優先度 == "CRISIS") / total_tech * 100),
  Total     = sprintf("%d (100.0%%)", total_tech)
)

tech_section <- bind_rows(tech_wide, tech_total_row)

# =============================================================================
# SECTION 2: 臨床的アラーム
# =============================================================================

clinical_param_map <- tribble(
  ~内容１,        ~parameter_en,                   ~category,
  "V. FIB",       "Ventricular fibrillation",      "Cardiac rhythm",
  "V. TACHY",     "Ventricular tachycardia",       "Cardiac rhythm",
  "ASYSTOLE",     "Asystole",                      "Cardiac rhythm",
  "VPC RUN",      "VPC Run",                       "Cardiac rhythm",
  "TACHYCARDIA",  "Tachycardia",                   "Cardiac rhythm",
  "BRADYCARDIA",  "Bradycardia",                   "Cardiac rhythm",
  "PR (SpO2)",    "Pulse rate (via SpO2)",          "Cardiac rhythm",
  "SpO2",         "SpO2",                          "Oxygenation",
  "RR (APNEA)",   "Respiratory rate / Apnea",      "Respiratory",
  "NIBP",         "NIBP",                          "Hemodynamic",
  "NIBP (S)",     "NIBP (systolic)",               "Hemodynamic",
  # 件数が極めて少ないパラメータ（EtCO2, ART, T1, T2, Tb）はOtherに集約
  "EtCO2",        "Other",                         "Other",
  "ART",          "Other",                         "Other",
  "T1",           "Other",                         "Other",
  "T2",           "Other",                         "Other",
  "Tb",           "Other",                         "Other"
)

category_order <- c("Cardiac rhythm", "Oxygenation", "Respiratory",
                    "Hemodynamic", "Other")

param_order <- c(
  "Ventricular fibrillation", "Ventricular tachycardia", "Asystole",
  "VPC Run", "Tachycardia", "Bradycardia", "Pulse rate (via SpO2)",
  "SpO2",
  "Respiratory rate / Apnea",
  "NIBP", "NIBP (systolic)",
  "Other"
)

total_clinical <- nrow(df |> filter(alarm_class == "clinical"))

total_clinical_by_priority <- df |>
  filter(alarm_class == "clinical") |>
  count(優先度, name = "total_priority")

clinical_counts <- df |>
  filter(alarm_class == "clinical") |>
  left_join(clinical_param_map, by = "内容１") |>
  mutate(
    parameter_en = coalesce(parameter_en, 内容１),
    category     = coalesce(category, "Other")
  ) |>
  count(category, parameter_en, 優先度) |>
  left_join(total_clinical_by_priority, by = "優先度") |>
  mutate(
    pct  = n / total_priority * 100,
    cell = sprintf("%d (%.1f%%)", n, pct)
  )

clinical_total_col <- clinical_counts |>
  group_by(category, parameter_en) |>
  summarise(n_total = sum(n), .groups = "drop") |>
  mutate(
    pct_total = n_total / total_clinical * 100,
    Total     = sprintf("%d (%.1f%%)", n_total, pct_total)
  )

clinical_wide <- clinical_counts |>
  select(category, parameter_en, 優先度, cell) |>
  pivot_wider(
    names_from  = 優先度,
    values_from = cell,
    values_fill = "—"
  ) |>
  left_join(clinical_total_col |> select(parameter_en, Total),
            by = "parameter_en") |>
  mutate(
    category     = factor(category, levels = category_order),
    parameter_en = factor(parameter_en, levels = param_order)
  ) |>
  arrange(category, parameter_en) |>
  select(Category = category, Parameter = parameter_en,
         any_of(priority_levels), Total)

clinical_total_row <- total_clinical_by_priority |>
  mutate(cell = sprintf("%d (100.0%%)", total_priority)) |>
  select(優先度, cell) |>
  pivot_wider(names_from = 優先度, values_from = cell) |>
  mutate(
    Category  = NA_character_,
    Parameter = "Total",
    Total     = sprintf("%d (100.0%%)", total_clinical)
  ) |>
  select(Category, Parameter, any_of(priority_levels), Total)

clinical_section <- bind_rows(clinical_wide, clinical_total_row)

# =============================================================================
# SECTION 3: 結合・出力
# =============================================================================

# セクションヘッダー行を追加
header_tech     <- tibble(Category = "--- Technical Alarms (n = 134,739) ---",
                          Parameter = NA_character_, ADVISORY = NA, WARNING = NA,
                          CRISIS = NA, Total = NA)
header_clinical <- tibble(Category = "--- Clinical Alarms (n = 59,113) ---",
                          Parameter = NA_character_, ADVISORY = NA, WARNING = NA,
                          CRISIS = NA, Total = NA)
separator       <- tibble(Category = NA_character_, Parameter = NA_character_,
                          ADVISORY = NA, WARNING = NA, CRISIS = NA, Total = NA)

table_final <- bind_rows(
  header_tech,
  tech_section,
  separator,
  header_clinical,
  clinical_section
)

cat("=== Table S3 プレビュー ===\n")
print(table_final, n = Inf)

# UTF-8 BOM を明示的に付けて保存（Excelでの文字化け防止）
out_path <- "outputs/tables/table_s3_parameters_en.csv"
con <- file(out_path, open = "wb")
writeBin(as.raw(c(0xef, 0xbb, 0xbf)), con)  # UTF-8 BOM
# ヘッダー行を書いてからデータを追記
header_line <- paste(names(table_final), collapse = ",")
writeLines(header_line, con)
close(con)
write_csv(table_final, out_path, append = TRUE, col_names = FALSE, na = "")
cat("\n保存: outputs/tables/table_s3_parameters_en.csv\n")
