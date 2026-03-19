---
marp: true
theme: gaia
paginate: true
backgroundColor: #fff
color: #333
style: |
  section {
    font-family: 'Helvetica Neue', 'Hiragino Sans', 'Noto Sans JP', sans-serif;
    font-size: 28px;
    padding: 40px 60px;
  }
  section.lead {
    text-align: left;
    display: flex;
    flex-direction: column;
    justify-content: center;
    align-items: flex-start;
  }
  section.lead h1 {
    font-size: 48px;
    color: #1a365d;
  }
  section.lead h2 {
    font-size: 34px;
    color: #4a5568;
    font-weight: normal;
  }
  section.section-divider {
    background: #fff;
    color: #333;
    text-align: left;
    display: flex;
    flex-direction: column;
    justify-content: center;
    padding-left: 100px;
  }
  section.section-divider h1 {
    font-size: 50px;
    color: #1a365d;
    border: none;
    margin-bottom: 10px;
  }
  section.section-divider h2 {
    font-size: 32px;
    color: #2b6cb0;
    font-weight: normal;
  }
  h1 {
    font-size: 40px;
    color: #1a365d;
    border-bottom: 3px solid #2b6cb0;
    padding-bottom: 8px;
    margin-bottom: 20px;
  }
  h2 {
    font-size: 32px;
    color: #2b6cb0;
    margin-bottom: 12px;
  }
  h3 {
    font-size: 28px;
    color: #4a5568;
  }
  strong {
    color: #da5a5aff;
  }
  table {
    width: 80%;
    font-size: 22px;
    margin: 20px auto;
    border-collapse: collapse;
    border: none;
    background-color: transparent;
  }
  th {
    background: transparent;
    color: #333;
    padding: 12px;
    border-top: 2.5px solid #333;
    border-bottom: 1.5px solid #333;
    border-left: none;
    border-right: none;
    font-weight: bold;
  }
  td {
    padding: 10px;
    border: none;
    color: #333;
  }
  tr:last-child td {
    border-bottom: 2.5px solid #333;
  }
  tr:nth-child(even) {
    background: transparent;
  }
  blockquote {
    background: #ebf8ff;
    border-left: 5px solid #2b6cb0;
    padding: 14px 20px;
    margin: 12px 0;
    font-size: 26px;
    border-radius: 0 8px 8px 0;
  }
  blockquote strong {
    color: #2b6cb0;
  }
  img {
    display: block;
    margin: 0 auto;
    border-radius: 4px;
  }
  ul { margin: 4px 0; }
  li { margin: 4px 0; line-height: 1.6; }
  footer {
    font-size: 16px;
    color: #a0aec0;
  }
---

<!-- _class: lead -->

# 外科病棟のアラーム疲労
## 高ノイズ環境下における臨床的レジリエンスの定量的評価


---

# 背景・目的

## なぜアラーム疲労が問題か

- 臨床現場では「アラームが多すぎて対応できない」状態が常態化
- 従来の対策は**件数削減（総量抑制）**に偏りがち
- しかし実態として看護師は全アラームを無視しているわけではない

## 研究の問い

> 膨大なノイズ下でも、看護師は**致命的なアラームへの応答を維持**できているか？

> そのメカニズムはどのように機能しているか？

## 理論的背景

- **Safety-II / Resilience Engineering**
- 「なぜ失敗するか」ではなく「なぜうまくいっているか」を問う
- High Reliability Organizationの視点

---

# データと方法

## データ

- **対象病棟**：外科病棟（西10・西11、計41床）
- **観察期間**：2025年7月15日〜9月9日（**57日間**）
- **総アラーム数**：193,852件
- **主解析対象**：臨床アラーム 59,113件

## アラーム分類

- **技術的アラーム**：SpO₂プローブ外れ等、臨床的意義乏しい
- **臨床的アラーム**：ADVISORY / WARNING / CRISIS の3優先度

## 主な解析

- アウトカム：消音応答の有無（silenced: YES/NO）
- 曝露変数：アラーム負荷（直前30分間の臨床アラーム件数）
- **混合ロジスティック回帰**（ベッドレベルランダム効果）

---

<!-- _class: section-divider -->

# 記述統計
## 結果 I ── どんな環境か？

---

# Table 1a｜全アラームの特性

| 特性 | 全アラーム | 技術的 | 臨床的 |
|---|---|---|---|
| 件数 | 193,852 (100%) | **134,739 (69.5%)** | 59,113 (30.5%) |
| アラーム密度（件/床/日） | 82.9 | 57.7 | 25.3 |
| ADVISORY | 135,783 (70%) | 134,134 (99.6%) | 1,649 (2.8%) |
| WARNING | 57,847 (29.8%) | 605 (0.4%) | 57,242 (96.8%) |
| CRISIS | 222 (0.1%) | 0 (0%) | **222 (0.4%)** |
| 消音あり | 5,759 (3%) | 914 (0.7%) | 4,845 (8.2%) |
| 消音までの時間（中央値 [IQR]） | 20 [10–49] 秒 | 206 [17–3025] 秒 | 18 [10–36] 秒 |

> 全アラームの**69.5%が技術的アラーム**という高ノイズ環境。
> 技術的アラームはほぼ消音されず（0.7%）放置されている。

---

# Figure 1A｜時間帯別アラーム種別

![w:820](../figures/journal/Figure1A/fig2_hourly_alarm_type_journal.png)

---

# Figure 1B｜臨床アラームの時間帯×曜日パターン

![w:820](../figures/journal/Figure1B/fig_heatmap_clinical_by_dow.png)

---

# Table 1b｜臨床アラーム：優先度別内訳

| 特性 | ADVISORY | WARNING | CRISIS |
|---|---|---|---|
| 件数（臨床アラーム中） | 1,649 (2.8%) | 57,242 (96.8%) | **222 (0.4%)** |
| 消音あり | 61 (3.7%) | 4,626 (8.1%) | **158 (71.2%)** |
| 消音までの時間（中央値 [IQR]） | 19 [12–69] 秒 | 19 [10–37] 秒 | **8 [6–12] 秒** |
| 持続時間（中央値 [IQR]） | 18 [10–19] 秒 | 11 [4–23] 秒 | 31 [9–37] 秒 |
| 高重症ゾーン | 770 (46.7%) | 8,150 (14.2%) | 105 (47.3%) |

> CRISISは件数こそ0.4%と僅少だが、**消音率71.2%・中央値8秒**と
> 突出した応答を受けている。「針の目」指標。

---

<!-- _class: section-divider -->

# 解析手法
## 方法の解説 ── 混合ロジスティック回帰

---

# 解析手法①｜なぜ「混合」ロジスティック回帰か

## アウトカムが 0/1 → ロジスティック回帰

- 各アラームの消音有無（YES=1 / NO=0）を予測

## 「繰り返し測定」の問題

- 1つのベッドで1日に何十件もアラームが発生する
- 同じベッドのアラームは互いに**独立でない**（患者の状態が共通）
- 通常のロジスティック回帰では標準誤差が過小評価される

## ランダム効果で補正

- **ベッドレベルのランダム切片**を加えることで相関を吸収
- 「ベッドごとに消音しやすさのベースラインが違う」ことを許容
- ※ベッドは観察期間中に患者が入れ替わるため、**ロケーション効果**を反映

---

# 解析手法②｜交互作用項の意味

## 検証したいこと

> アラーム負荷が増えたとき、その影響は優先度によって**違う**か？

## 交互作用なし（帰無仮説）

- 負荷が増えると、ADVISORY も WARNING も CRISIS も同じように消音率が下がる

## 交互作用あり（対立仮説）

- 負荷が増えると **WARNING は下がるが CRISIS は下がらない** →「選別」の証拠

## モデルの交互作用項

- `alarm_burden × priority` のOR が**1から離れていれば**優先度ごとに効果が異なる
- CRISIS の交互作用OR = 1.00（変化なし）が本研究の**主要発見**

---

<!-- _class: section-divider -->

# 主解析結果
## 結果 II ── 負荷下での優先度別応答

---

# Figure 2｜優先度別：消音率と応答時間（静的構造）

![w:500](../figures/journal/FIgure2/14_figure2_static_response.png)

---

# Table 2｜アラーム負荷四分位×優先度 観測消音率

| 優先度 | Q1（低負荷：0–9件） | Q2（9–18件） | Q3（18–31件） | Q4（高負荷：31–97件） |
|---|---|---|---|---|
| ADVISORY | 3.3%（n=777） | 2.8%（n=394） | 5.2%（n=271） | 4.8%（n=207） |
| WARNING | **10.6%**（n=13,921） | 8.4%（n=14,329） | 7.6%（n=14,455） | **5.8%**（n=14,537） |
| CRISIS | 70.4%（n=81） | 69.1%（n=55） | 76.9%（n=52） | **67.6%**（n=34） |

> WARNINGは低負荷時10.6% → 高負荷時5.8%へ**45%相対低下**。
> CRISISは全四分位を通じて**67–77%を維持**。

---

# Figure 3｜メイン発見：負荷×優先度 交互作用プロット

![w:820](../figures/journal/FIgure3/13_interaction_plot.png)

---

# Table 3｜混合ロジスティック回帰結果（30分窓モデル）

| 変数 | OR | 95% CI | p値 |
|---|---|---|---|
| 優先度（ref: ADVISORY） | | | |
|　WARNING | 4.940 | 3.357–7.271 | <0.001 |
|　CRISIS | **74.842** | 40.409–138.617 | <0.001 |
| 交互作用：負荷増加あたり（ADVISORY） | 1.011 | 0.993–1.029 | 0.236 |
| 　× WARNING | **0.978** | 0.960–0.996 | 0.015 |
| 　× CRISIS | **1.000** | 0.972–1.029 | 0.999 |
| 高重症ゾーン（ref: 一般） | 1.461 | 0.971–2.198 | 0.069 |
| 夕方シフト（ref: 日勤） | 0.780 | 0.706–0.861 | <0.001 |
| 夜勤（ref: 日勤） | 1.459 | 1.365–1.560 | <0.001 |
| 週末（ref: 平日） | 1.141 | 1.065–1.222 | <0.001 |

*N = 59,113；ベッドレベル分散 0.302；AIC = 31,653.3*

---

# 結論と臨床的意義

## 主要発見

- 高ノイズ環境（技術的アラーム69.5%）下でも、**CRISISへの応答は維持**されていた
- 負荷増大に伴い**WARNINGを選択的に抑制**することでCRISISを守っている
- これは「見落とし」ではなく、**臨床的レジリエンス（適応能力）**の証拠

## Safety-IIの観点

> 看護師はアラーム負荷に対し、CRISISを優先的に反応するために
> 低優先度を**戦略的に抑制**していた。
> アラーム管理の要諦は一律の件数削減ではなく、
> **優先度層ごとの最適化**である。

---

# 共著者への論点

## Limitationsとして議論したい点

1. **消音 ≠ 臨床対応**：ボタン押下が患者評価を意味しない
2. **CRISIS件数の少なさ**：222件（silenced=158件）で統計的検出力に限界
3. **ランダム効果はロケーション**：患者個人ではなくベッドを反映

## 今後の方向性（ご意見求む）

- ゾーン（高重症 vs 一般）の効果：高重症ゾーンの高重症ゾーンのOR 1.46（p=0.069）をどう解釈するか？
- Figure S3（欠損値フローチャート）の必要性
- 投稿先の候補（BMJ Q&S, JAMIA 等）

---

<!-- _class: section-divider -->

# 補足
## Appendix

---

# Figure S2｜ランダム効果分布（ベッドレベル切片）

![w:820](../figures/journal/FigureS2/fig_s2_random_effects.png)

*各ベッドの消音しやすさのばらつきを示す。分散 = 0.302。*
