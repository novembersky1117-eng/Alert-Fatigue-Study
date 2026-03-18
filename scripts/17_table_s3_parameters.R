# =============================================================================
# 17_table_s3_parameters.R
# Table S3: Alarm events by parameter and priority level
#
# 【目的】
#   臨床アラームのパラメータ別内訳をPriority別に示す
#   CRISISが致死的不整脈のみで構成されていることを証明
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

# -----------------------------------------------------------------------------
# 1. パラメータ英語名マッピング + カテゴリ定義
# -----------------------------------------------------------------------------

param_map <- tribble(
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
  "EtCO2",        "EtCO2",                         "Respiratory",
  "NIBP",         "NIBP",                          "Hemodynamic",
  "NIBP (S)",     "NIBP (systolic)",               "Hemodynamic",
  "ART",          "Arterial pressure",             "Hemodynamic",
  "T1",           "Temperature (T1)",              "Temperature",
  "T2",           "Temperature (T2)",              "Temperature",
  "Tb",           "Body temperature (Tb)",         "Temperature"
)

category_order <- c("Cardiac rhythm", "Oxygenation", "Respiratory",
                    "Hemodynamic", "Temperature", "Other")

param_order <- c(
  "Ventricular fibrillation", "Ventricular tachycardia", "Asystole",
  "VPC Run", "Tachycardia", "Bradycardia", "Pulse rate (via SpO2)",
  "SpO2",
  "Respiratory rate / Apnea", "EtCO2",
  "NIBP", "NIBP (systolic)", "Arterial pressure",
  "Temperature (T1)", "Temperature (T2)", "Body temperature (Tb)"
)

# -----------------------------------------------------------------------------
# 2. Priority別・パラメータ別集計
# -----------------------------------------------------------------------------

total_by_priority <- df |>
  filter(alarm_class == "clinical") |>
  count(優先度, name = "total_priority")

counts <- df |>
  filter(alarm_class == "clinical") |>
  left_join(param_map, by = "内容１") |>
  mutate(
    parameter_en = coalesce(parameter_en, 内容１),
    category     = coalesce(category, "Other")
  ) |>
  count(category, parameter_en, 優先度) |>
  left_join(total_by_priority, by = "優先度") |>
  mutate(
    pct  = n / total_priority * 100,
    cell = sprintf("%d (%.1f%%)", n, pct)
  )

# -----------------------------------------------------------------------------
# 3. Total列（全Priority合計）
# -----------------------------------------------------------------------------

total_all <- nrow(df |> filter(alarm_class == "clinical"))

counts_total <- counts |>
  group_by(category, parameter_en) |>
  summarise(n_total = sum(n), .groups = "drop") |>
  mutate(
    pct_total  = n_total / total_all * 100,
    Total      = sprintf("%d (%.1f%%)", n_total, pct_total)
  )

# -----------------------------------------------------------------------------
# 4. Wide形式に整形
# -----------------------------------------------------------------------------

table_wide <- counts |>
  select(category, parameter_en, 優先度, cell) |>
  pivot_wider(
    names_from  = 優先度,
    values_from = cell,
    values_fill = "—"
  ) |>
  left_join(counts_total |> select(parameter_en, Total),
            by = "parameter_en") |>
  mutate(
    category     = factor(category, levels = category_order),
    parameter_en = factor(parameter_en, levels = param_order)
  ) |>
  arrange(category, parameter_en) |>
  select(Category = category, Parameter = parameter_en,
         ADVISORY, WARNING, CRISIS, Total)

# -----------------------------------------------------------------------------
# 5. Total行を末尾に追加
# -----------------------------------------------------------------------------

total_row <- total_by_priority |>
  mutate(cell = sprintf("%d (100.0%%)", total_priority)) |>
  select(優先度, cell) |>
  pivot_wider(names_from = 優先度, values_from = cell) |>
  mutate(
    Category  = NA_character_,
    Parameter = factor("Total"),
    Total     = sprintf("%d (100.0%%)", total_all)
  ) |>
  select(Category, Parameter, ADVISORY, WARNING, CRISIS, Total)

table_final <- bind_rows(table_wide, total_row)

cat("=== Table S3 プレビュー ===\n")
print(table_final, n = Inf)

# -----------------------------------------------------------------------------
# 6. CSV出力
# -----------------------------------------------------------------------------

write_excel_csv(table_final, "outputs/tables/table_s3_parameters_en.csv")
cat("\n保存: outputs/tables/table_s3_parameters_en.csv\n")
