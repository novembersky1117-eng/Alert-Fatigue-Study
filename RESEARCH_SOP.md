# 臨床研究・論文用Figure作成 標準手順書 (SOP)

## 1. グラフ作成・保存の共通原則
- 原則として `ggplot2` を使用する。
- 文字化け防止とベクター形式保持のため、`device = cairo_pdf` を必須とする。
- カラーパレット（2群）: 「青 (#0072B2)」と「黄 (#F0E442)」を基本とする。
- 不要な背景・3D表現・装飾は使用しない。
- 軸ラベルには単位を必ず明記する。
- 略語は図内または図注で定義する。

---

## 2. 【論文投稿用】Figure作成ルール (Journal)
- **サイズ**: `width = 3.5` (inch) のハーフカラムを標準とする。
- **比率**: `height` は `width` の 0.75倍 ($2.625$ inch) または 黄金比 ($2.16$ inch) を目安とする。
- **視認性**:
  - フォントサイズ: `base_size = 11` 相当。
  - 点・線の太さ: 精密さを重視し、標準的な太さ。
- 複雑な図（例：Forest plot）はフルカラム幅を許容する。

---

## 3. 【学会発表用】Figure作成ルール (Presentation)
- **前提**: Inkscapeでの編集およびベクター品質での出力を前提とする。
- **サイズ**: `width = 10` / `height = 6` (inch) 。
- **視認性 (遠距離重視)**:
  - フォントサイズ: `base_size = 20 〜 24`。
  - 原則として **太字 (Bold)** を使用。
- **プロット要素**:
  - 点 (`geom_point`): `size = 3 〜 4` (論文用の約3倍)。
  - 線 (`geom_line`, `geom_smooth`): `linewidth = 1.5 〜 2`。
  - 軸ライン: 太く強調する。
- **用途**: 学会等のスライド用。

---

## 4. 特殊グラフの指定
- **生存曲線**: `survminer::ggsurvplot` を優先。
- リスクテーブル、95%信頼区間、p値表示は図ごとに統一する。
- サイズは用途に応じて 3.5 inch または 10 inch を適用する。

## 5. 言語規則
- 論文・学会発表に使用するグラフ・表（Figure / Table）のラベル、タイトル、凡例、軸名はすべて**英語**で作成する。
- 解析過程の確認用（探索的集計・ログ等）は日本語のままで構わない。

## 5b. Figure出力規則
- Figureは必ず**論文用・発表用の2種類**を作成する。
- 保存先:
  - 論文用: `outputs/figures/journal/`（`width = 3.5`, `height = 2.625` inch, `base_size = 11`）
  - 発表用: `outputs/figures/presentation/`（`width = 10`, `height = 6` inch, `base_size = 22`, 太字）
- ファイル名は同一のベース名に `_journal` / `_presentation` サフィックスを付ける。
  - 例: `fig_dow_boxplot_journal.pdf` / `fig_dow_boxplot_presentation.pdf`

## 6. 保存規則
- ファイル名は内容.pdf とする。
- スクリーンショットは使用しない。

## 6. CSVファイルの保存規則
- 日本語を含むCSVを保存する際は、文字化け防止のため `write_excel_csv()` を使用する（UTF-8 BOM付き）。
- `write_csv()` は BOM なし UTF-8 のため、Excel で開くと文字化けするので使用しない。