# Macro3 Project V2 — Dark Matter Replication and Extension

Replication and extension of Hausmann & Sturzenegger (2006), **“Global Imbalances or Bad Accounting? The Missing Dark Matter in the Wealth of Nations.”**

This repository contains the code, data inputs, generated figures and tables, report material, presentation material, and research logbook for an **Advanced Macroeconomics** project in the **M1 APE program at Paris School of Economics**.

**Authors:** Eliot Gharib, Youssef Benzakour, Benjamin Frémy  
**Course:** Advanced Macroeconomics, M1 APE, Paris School of Economics  
**Date:** May 2026

---

## 1. Project summary

Hausmann and Sturzenegger (2006) start from a striking puzzle. Between 1982 and 2005, the United States accumulated more than five trillion dollars in cumulative current-account deficits, but its net investment income remained close to zero and often positive. Standard international accounting would predict that a country with such accumulated deficits should become a large net debtor paying substantial income to the rest of the world.

H&S propose to reverse the usual accounting logic. Instead of inferring net foreign assets from cumulated current-account flows, they infer them from the income they generate:

```math
NFA^{DM}_t = \frac{NII_t}{r}, \qquad r = 0.05
```

The difference between this income-implied net foreign asset position and the official net international investment position is called **dark matter**:

```math
DM_t = NFA^{DM}_t - NIIP^{off}_t
```

This project replicates the original paper and extends it to the modern global financial system.

---

## 2. Research objectives

The project has three main objectives.

### 2.1 Replication of H&S

We replicate the central empirical results from Hausmann and Sturzenegger (2006):

- the US accounting puzzle;
- the cross-country offset mechanism between official current accounts and dark matter exports;
- the determinant regressions on FDI, output volatility, Rule of Law, R&D, OPEC status, and HIPC status;
- additional H&S figures on US returns, global NFA positions, and dark matter stocks.

### 2.2 Extension to 2022

We extend the analysis beyond the original 1980–2003 window and test whether the H&S mechanism survives:

- the global financial crisis;
- the euro-area crisis;
- the COVID period;
- the rise of China as a major official creditor;
- the increasing role of safe assets and international financial centres.

### 2.3 Asset-class decomposition and mechanism tests

We go beyond the flat 5% capitalisation assumption by decomposing net investment income into:

- FDI income;
- portfolio equity income;
- portfolio debt income;
- other investment income.

We then test several candidate mechanisms behind dark matter:

- intangible capital and knowledge rents;
- insurance services and macroeconomic stability;
- global financial risk and safe-asset demand;
- geopolitical risk;
- financial development and safe-asset production;
- FDI mismeasurement, phantom FDI, and multinational accounting.

The main conclusion is that dark matter is better interpreted as a **layered residual** than as one single hidden stock of assets. It combines return differentials, safe-asset services, portfolio composition, FDI mismeasurement, intangible rents, and accounting frictions.

---

## 3. Repository structure

```text
Macro3_Project_V2/
│
├── README.md
├── Macro3_Project_V2.Rproj
├── journal_de_bord_macro3.Rmd
├── .gitignore
├── .gitattributes
│
├── Paper/
│   └── Dark_Matter_Hausmann.pdf
│
└── code/
    ├── Dark_Matter_Replication.R
    ├── Dark_Matter_Replication_V2.R
    │
    ├── data/
    │   ├── Current_account_primary_income_1975_2005.csv
    │   ├── Current_account_primary_income_1975_2022.csv
    │   ├── EWN-dataset-year-end-2024_4.9.26.xlsx
    │   ├── GDP_constant_1975_2005.csv
    │   ├── GDP_constant_1975_2022.csv
    │   ├── GDP_IMF.csv
    │   ├── gpr_web_latest.xlsx
    │   ├── RND_expenditure_1975_2005.csv
    │   ├── vix_daily.csv
    │   ├── wdi_ca_bop_1975_2005.csv
    │   ├── wdi_findev.csv
    │   ├── wdi_intangibles.csv
    │   ├── wdi_nii_bop_1975_2005.csv
    │   └── World_Governance_Indicator_1965-2005.csv
    │
    └── output/
        ├── figures/
        │   ├── part_I/
        │   ├── part_II/
        │   └── part_III/
        │
        ├── tables/
        │   ├── part_I/
        │   ├── part_II/
        │   └── part_III/
        │
        ├── Graphs/
        └── Tables/
```

The main script creates separate output folders for the replication, extension, and mechanism sections, so that figures and tables from different parts do not mix.

---

## 4. Main files

### `code/Dark_Matter_Replication.R`

This is the main script used for the final report. It:

- loads and cleans the IMF BOP, EWN, WDI, WGI, VIX, and GPR data;
- reconstructs the H&S dark matter variables;
- replicates the original 1980–2003 results;
- extends the analysis to 2022;
- computes asset-class decompositions under several scenarios;
- runs mechanism regressions;
- exports figures and tables to `code/output/`.

This is the script to run if you want to reproduce the final results.

### `code/Dark_Matter_Replication_V2.R`

This is an earlier version of the project script. It is kept for traceability, but it was **not** the main script used for the final report.

Compared with the final script, it has a simpler output structure and focuses more narrowly on the original replication workflow. It should be treated as a legacy or development version.

### `journal_de_bord_macro3.Rmd`

Research logbook documenting the evolution of the project. It covers:

- initial replication strategy;
- data choices and sample restrictions;
- WDI vs. IFS GDP comparison;
- extension to 2022;
- insurance-channel decomposition;
- asset-class decomposition;
- mechanism tests;
- literature positioning;
- final methodological lessons.

This file is not required to reproduce the numerical results, but it explains the research decisions behind the final report.

### `Paper/Dark_Matter_Hausmann.pdf`

Original Hausmann & Sturzenegger paper used as the reference for replication.

---

## 5. Data sources

| Data source | Files / use |
|---|---|
| IMF Balance of Payments | Current account, net investment income, income sub-components |
| External Wealth of Nations, Brookings 2024 update | Official NIIP, gross external assets and liabilities, FDI, equity and debt stocks |
| World Bank WDI | Real GDP, R&D, patents, ICT services, high-technology exports, financial development |
| World Governance Indicators | Rule of Law |
| VIX | Global financial risk |
| Caldara-Iacoviello GPR index | Geopolitical risk |
| H&S original paper | Reference paper for replication |

Some datasets are large. If you clone the repository and some data files appear as Git LFS pointers rather than full files, run:

```bash
git lfs install
git lfs pull
```

---

## 6. Reproducibility workflow

### 6.1 Clone the repository

```bash
git clone https://github.com/eliotgharib/Macro3_Project_V2.git
cd Macro3_Project_V2
```

### 6.2 Open the R project

Open the file:

```text
Macro3_Project_V2.Rproj
```

in RStudio.

Using the `.Rproj` file is recommended because the code relies on relative paths through the `here` package.

### 6.3 Install required R packages

The main script attempts to install missing packages automatically. The package list used in the final script is:

```r
pkgs <- c(
  "tidyverse",
  "readxl",
  "countrycode",
  "here",
  "mFilter",
  "plm",
  "ggrepel",
  "scales",
  "broom",
  "stargazer",
  "tinytex",
  "modelsummary",
  "kableExtra",
  "WDI",
  "fixest",
  "sandwich",
  "patchwork",
  "zoo"
)
```

If needed, install them manually with:

```r
install.packages(c(
  "tidyverse", "readxl", "countrycode", "here",
  "mFilter", "plm", "ggrepel", "scales", "broom",
  "stargazer", "tinytex", "modelsummary", "kableExtra",
  "WDI", "fixest", "sandwich", "patchwork", "zoo"
))
```

The script also uses `tinytex` to compile some LaTeX tables.

### 6.4 Run the final script

From the repository root:

```r
source("code/Dark_Matter_Replication.R")
```

The script creates output folders automatically if they do not already exist.

### 6.5 Locate generated outputs

Figures are written to:

```text
code/output/figures/part_I/
code/output/figures/part_II/
code/output/figures/part_III/
```

Tables are written to:

```text
code/output/tables/part_I/
code/output/tables/part_II/
code/output/tables/part_III/
```

---

## 7. Methodology

### 7.1 Baseline dark matter measure

The baseline H&S measure capitalises net investment income at 5%:

```math
NFA^{DM}_t = \frac{NII_t}{0.05}
```

The implied current account is:

```math
CA^{DM}_t = NFA^{DM}_t - NFA^{DM}_{t-1}
```

Dark matter exports are:

```math
XDM_t = CA^{DM}_t - CA^{off}_t
```

The dark matter stock is:

```math
DM_t = NFA^{DM}_t - NIIP^{off}_t
```

### 7.2 Cross-sectional replication

For the original H&S period, the main cross-section uses the 1980–2003 window. When exact endpoints are unavailable in the updated data, the script uses the first and last available observations within the window.

Variables used in cross-sectional regressions are winsorised at 1%.

### 7.3 Output volatility

Output volatility is computed using the standard deviation of the HP-filtered log real GDP cycle, with annual smoothing parameter:

```math
\lambda = 100
```

### 7.4 Asset-class decomposition

The project decomposes net investment income into asset classes:

```math
j \in \{FDI, equity, debt, other\}
```

and computes:

```math
NFA^{comp}_t = \sum_j \frac{NII^j_t}{r_j}
```

Three scenarios are implemented.

| Scenario | Logic |
|---|---|
| Scenario A | Universal asset-class rates: FDI, equity, debt, and other income receive different discount rates, common across countries. |
| Scenario B | Country-group-specific rates: privilege, advanced, and emerging economies receive different rates. |
| Scenario C | Gross-position approach: assets and liabilities receive separate return rates by asset class and country group. |

Scenario C is conceptually important because it directly captures the return-privilege mechanism: countries may earn high returns on gross external assets while paying low returns on gross external liabilities.

### 7.5 China imputation in the asset-class decomposition

China’s BOP income sub-components are incomplete or unreliable in several years of the IMF extract. The final script preserves total NII but reallocates it across asset classes using EWN gross-position weights when sub-component coverage is weak.

This choice keeps the aggregate income figure unchanged while allowing the decomposition to remain interpretable.

---

## 8. Main outputs

### 8.1 Part I — Replication

Important figures:

```text
fig1_us_ca_nii.pdf
fig2_us_interest_spread.pdf
fig3a_scatter.pdf
fig3b_scatter_zoom.pdf
fig4_us_net_asset_position.pdf
fig5b_dm_vs_ca.pdf
fig6a_nfa_official_figures.pdf
fig6c_global_nfa.pdf
fig8_us_dm_stock.pdf
```

Important tables:

```text
table1
table2
table3
table4
table4b
```

### 8.2 Part II — Extension and decomposition

Important figures:

```text
fig_II_01_us_dm_stock.pdf
fig_II_02_global_nfa_regions.pdf
fig_II_03_scatter_cumCA_vs_DM.pdf
fig_II_04_scatter_DM_exports_vs_CA.pdf
fig_II_05_nfa_decomp_scen_A.pdf
fig_II_06_nfa_decomp_scen_B.pdf
fig_II_07_nfa_decomp_scen_C.pdf
fig_II_07_dm_decomp_by_asset_class.pdf
fig_II_S2_us_china_dark_matter_stock.pdf
fig_II_S2_us_china_gross_portfolio_structure.pdf
```

Important tables include the extended offset regressions, asset-class scenario comparisons, and gross portfolio decomposition outputs.

### 8.3 Part III — Mechanisms

Important figures:

```text
p3_fig_E1_intangibles_scatter.pdf
p3_fig_E2c_vix_vs_gpr_timeseries.pdf
p3_fig_E3_financial_development_scatter.pdf
p3_fig_E4_nii_decomposition_showcase.pdf
```

Important tables:

```text
p3_table_E1_intangibles
p3_table_E2a_vix_5pct
p3_table_E2b_vix_component_method
p3_table_E2c_vix_gpr
p3_table_E2d_vix_gfc_split
p3_table_E3_findev
```

---

## 9. Research logbook

The file `journal_de_bord_macro3.Rmd` provides a narrative account of the project. It is useful for understanding why some choices were made, especially:

- why the sample was restricted to the H&S country lists;
- why WDI GDP was preferred to IFS GDP for the main output-volatility measure;
- how the extension to 2022 was interpreted;
- why Scenario C became central in the asset-class decomposition;
- why the mechanism tests are interpreted as suggestive rather than causal.

To render the logbook in RStudio, open `journal_de_bord_macro3.Rmd` and click **Knit**. Alternatively:

```r
rmarkdown::render("journal_de_bord_macro3.Rmd")
```

---

## 10. Key findings

1. The US accounting puzzle is replicated: cumulative US current-account deficits deteriorate sharply, while US net investment income remains close to zero or positive.

2. The central H&S offset mechanism is reproduced for 1980–2003. Countries with official deficits tend to export dark matter, while countries with official surpluses tend to import it.

3. The offset mechanism survives in the extended 1980–2022 sample.

4. Output volatility remains the most robust determinant of dark matter exports, supporting the insurance interpretation.

5. The FDI channel is less stable in updated data, likely reflecting revisions in external wealth data and the increasing role of special purpose entities, phantom FDI, and profit shifting.

6. The asset-class decomposition shows that the flat 5% rate hides important differences across FDI, equity, debt, and other investment income.

7. The United States appears stronger in income-implied terms because it earns relatively high returns on outward FDI and equity while issuing safe, liquid liabilities at low cost.

8. China illustrates the opposite configuration: a positive official NIIP can coexist with weak income-implied wealth when external assets are low-yielding and inward FDI liabilities are costly.

9. Mechanism tests suggest that dark matter is heterogeneous. It combines return differentials, safe-asset demand, insurance services, FDI mismeasurement, intangible rents, and multinational accounting.

---

## 11. Working with the LaTeX report or slides

If you compile a LaTeX report or presentation using the generated figures, make sure the figure paths point to the right output folders.

A robust LaTeX setup is:

```latex
\graphicspath{
  {./}
  {figures/}
  {code/output/figures/part_I/}
  {code/output/figures/part_II/}
  {code/output/figures/part_III/}
}
```

If a figure fails to appear and LaTeX prints the filename instead, check that:

1. the figure exists in one of the folders above;
2. the filename in `\includegraphics{...}` exactly matches the generated filename;
3. LaTeX is not compiling in `draft` mode;
4. Git LFS files have been pulled correctly.

---

## 12. Known caveats

- The project uses updated IMF BOP and EWN data, so some results differ from the original H&S estimates.
- Some original H&S countries have incomplete historical BOP coverage.
- Cross-sectional regressions use flexible endpoints when exact 1980 and 2003 values are unavailable.
- Asset-class NII sub-components are not equally well reported across countries.
- China’s asset-class decomposition partly relies on EWN gross-position weights when BOP sub-component coverage is incomplete.
- The decomposition is an interpretive exercise, not a structural asset-pricing model.
- Mechanism tests are suggestive rather than causal.
- Dark matter should be read as a diagnostic residual, not as proof that global imbalances are harmless.

---

## 13. Citation

If using this repository, cite the project as:

```text
Gharib, E., Benzakour, Y., and Frémy, B. (2026).
Global Imbalances or Bad Accounting? Replication and Extension of Hausmann & Sturzenegger (2006).
Paris School of Economics, M1 APE — Advanced Macroeconomics.
```

Please also cite the original paper:

```text
Hausmann, R. and Sturzenegger, F. (2006).
Global Imbalances or Bad Accounting? The Missing Dark Matter in the Wealth of Nations.
CID Working Paper No. 124, Harvard University.
```

---

## 14. License

No license file is currently specified in the repository.

If the repository remains without a `LICENSE` file, the default legal position is that reuse is not explicitly granted. If the repository is intended to be reusable, add a license at the root of the repository.

Suggested options:

- **MIT License** for code;
- **CC BY 4.0** for written material;
- a short note clarifying that raw data remain subject to the licenses and terms of the original providers.

---

## 15. Contributors

- Eliot Gharib
- Youssef Benzakour
- Benjamin Frémy

---

## 16. Acknowledgements

This project was prepared for the **Advanced Macroeconomics** course in the **M1 APE program at Paris School of Economics**.
