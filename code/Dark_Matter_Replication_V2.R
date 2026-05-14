# ==============================================================================
#
# Paper Replication - Hausmann & Sturzenegger (2006)
# "Global Imbalances or Bad Accounting ? The Missing Dark Matter in the Wealth of Nations"
#
# Eliot Gharib - Youssef Benzakour - Benjamin Frémy
#
# ==============================================================================
#
# ── Packages ───────────────────────────────────────────────────────────────────

pkgs <- c("tidyverse", "readxl", "countrycode", "here",
          "mFilter", "plm", "ggrepel", "scales", "broom",
          "stargazer", "tinytex", "modelsummary")
for (p in pkgs) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
invisible(lapply(pkgs, library, character.only = TRUE))

if (!tinytex::is_tinytex()) tinytex::install_tinytex()
options(tinytex.tlmgr.args = "--no-self-update")

for (d in c("output/figures", "output/tables"))
  dir.create(here("code", d), recursive = TRUE, showWarnings = FALSE)

# ── Key parameters ─────────────────────────────────────────────────────────────

r        <- 0.05
y_start  <- 1975
y_cs     <- 1980
y_cs_end <- 2003
y_end    <- 2005


# ==============================================================================
# ==============================================================================
#
#                 Part I - Replication of the initial article
#
# ==============================================================================
# ==============================================================================
#
# Part 1 — Data
#
# We draw on five sources :
#
#   - IMF BOP : current account balance and net investment income
#     https://data.imf.org/en/datasets/IMF.STA:BOP
#
#   - EWN (Lane & Milesi-Ferretti, Brookings 2024) : official NFA, FDI stocks
#     https://www.brookings.edu/articles/the-external-wealth-of-nations-database/
#
#   - World Bank WDI : real GDP (HP filter) and R&D spending
#     GDP : https://data.worldbank.org/indicator/NY.GDP.MKTP.KD
#     R&D : https://data.worldbank.org/indicator/GB.XPD.RSDV.GD.ZS
#
#   - World Bank WGI : Rule of Law
#     https://databank.worldbank.org/source/worldwide-governance-indicators
#
# ==============================================================================

# ── IMF BOP ───────────────────────────────────────────────────────────────────

bop_raw <- read_csv(
  here("code", "data", "Current_account_primary_income_1975_2005.csv"),
  show_col_types = FALSE
)

year_cols <- names(bop_raw)[grepl("^\\d{4}$", names(bop_raw))]

bop <- bop_raw %>%
  rename(series_code = SERIES_CODE) %>%
  mutate(
    iso3c     = str_extract(series_code, "^[^.]+"),
    indicator = str_remove(series_code, "^[^.]+\\.")
  ) %>%
  filter(indicator %in% c("NETCD_T.CAB.USD.A", "NETCD_T.IN1.USD.A")) %>%
  select(iso3c, indicator, all_of(year_cols)) %>%
  pivot_longer(all_of(year_cols), names_to = "year", values_to = "value") %>%
  mutate(year = as.integer(year), value = suppressWarnings(as.numeric(value))) %>%
  filter(year >= y_start, year <= y_end) %>%
  pivot_wider(names_from = indicator, values_from = value) %>%
  rename(ca_usd = `NETCD_T.CAB.USD.A`, nii_usd = `NETCD_T.IN1.USD.A`)

# ── EWN ───────────────────────────────────────────────────────────────────────

EWN_raw <- read_excel(
  here("code", "data", "EWN-dataset-year-end-2024_4.9.26.xlsx"),
  sheet = "Dataset"
)

EWN <- EWN_raw %>%
  rename(
    ifs_code   = IFS_Code,
    year       = Year,
    fdi_assets = `FDI assets (stock)`,
    fdi_liab   = `FDI liabilities (stock)`,
    nfa        = `Net IIP excl gold`,
    gdp_usd    = `GDP (US$)`
  ) %>%
  select(ifs_code, year, fdi_assets, fdi_liab, nfa, gdp_usd) %>%
  filter(year >= y_start, year <= y_end) %>%
  mutate(
    iso3c          = countrycode(ifs_code, "imf", "iso3c", warn = FALSE),
    nfa_gdp        = nfa        / gdp_usd * 100,
    fdi_assets_gdp = fdi_assets / gdp_usd * 100,
    fdi_liab_gdp   = fdi_liab   / gdp_usd * 100
  ) %>%
  filter(!is.na(iso3c))

# ── World Bank WDI and IFS GDP ────────────────────────────────────────────────────────────

read_wb <- function(filename) {
  read_csv(here("code", "data", filename), skip = 4, show_col_types = FALSE) %>%
    rename(iso3c = `Country Code`) %>%
    select(iso3c, matches("^\\d{4}$")) %>%
    pivot_longer(-iso3c, names_to = "year", values_to = "value") %>%
    mutate(year = as.integer(year)) %>%
    filter(year >= y_start, year <= y_end, !is.na(iso3c), iso3c != "")
}

ifs_gdp_raw <- read_csv(
  here("code", "data", "GDP_IMF.csv"),
  show_col_types = FALSE
)

gdp_con <- ifs_gdp_raw %>%
  rename(iso3c = SERIES_CODE) %>%
  mutate(iso3c = str_extract(iso3c, "^[^.]+")) %>%
  select(iso3c, matches("^\\d{4}-Q\\d$")) %>%
  pivot_longer(-iso3c, names_to = "quarter", values_to = "value") %>%
  mutate(year = as.integer(str_extract(quarter, "^\\d{4}"))) %>%
  filter(year >= y_start, year <= y_end) %>%
  group_by(iso3c, year) %>%
  summarise(gdp_con = mean(value, na.rm = TRUE), .groups = "drop") %>%
  filter(!is.na(gdp_con), !is.infinite(gdp_con))

rnd     <- read_wb("RND_expenditure_1975_2005.csv")    %>% rename(rnd     = value)

# ── World Bank WGI ────────────────────────────────────────────────────────────

wgi_raw <- read_csv(
  here("code", "data", "World_Governance_Indicator_1965-2005.csv"),
  show_col_types = FALSE
)

wgi <- wgi_raw %>%
  rename(iso3c = `Country Code`) %>%
  select(iso3c, matches("\\d{4}")) %>%
  rename_with(~ str_extract(., "\\d{4}"), matches("\\d{4}")) %>%
  mutate(across(-iso3c, ~ suppressWarnings(as.numeric(.)))) %>%
  pivot_longer(-iso3c, names_to = "year", values_to = "rule_of_law") %>%
  mutate(year = as.integer(year)) %>%
  filter(!is.na(iso3c), iso3c != "", !is.na(rule_of_law),
         year >= 1996, year <= y_end) %>%
  group_by(iso3c) %>%
  summarise(rule_of_law = mean(rule_of_law, na.rm = TRUE), .groups = "drop")

# ── Country groups ────────────────────────────────────────────────────────────

opec <- c("DZA","AGO","ECU","GNQ","GAB","IRN","IRQ","KWT","LBY",
          "NGA","SAU","ARE","VEN")

hipc <- c("BEN","BOL","BFA","BDI","CMR","CAF","TCD","COM","COD","COG","CIV",
          "ETH","GMB","GHA","GIN","GNB","GUY","HTI","HND","KEN","LAO","LBR",
          "MDG","MWI","MLI","MRT","MOZ","NIC","NER","RWA","STP","SEN","SLE",
          "SOM","SDN","TZA","TGO","UGA","ZMB","ZWE")

industrial <- c("AUS","AUT","CAN","DNK","FIN","FRA","DEU","GRC","ISL","IRL",
                "ITA","JPN","NLD","NZL","NOR","PRT","ESP","SWE","CHE","GBR","USA")

eu <- c("AUT","BEL","DNK","FIN","FRA","DEU","GRC","IRL","ITA","LUX","NLD",
        "PRT","ESP","SWE","GBR","CZE","EST","HUN","LVA","LTU","MLT","POL",
        "SVK","SVN","CYP")


# ==============================================================================
#
# Part 2 — Panel construction
#
# ==============================================================================

panel <- bop %>%
  left_join(EWN %>% select(iso3c, year, fdi_assets_gdp, fdi_liab_gdp,
                           nfa_gdp, gdp_usd),
            by = c("iso3c", "year")) %>%
  left_join(gdp_con, by = c("iso3c", "year")) %>%
  left_join(rnd,     by = c("iso3c", "year")) %>%
  left_join(wgi,     by = "iso3c") %>%
  filter(year >= y_start, year <= y_end) %>%
  mutate(
    country = countrycode(iso3c, "iso3c", "country.name"),
    opec_d  = as.integer(iso3c %in% opec),
    hipc_d  = as.integer(iso3c %in% hipc)
  ) %>%
  arrange(iso3c, year)


# ==============================================================================
#
# Part 3 — Dark matter computation
#
#   NFA_DM(t) = NII(t) / r                 [equation 1]
#   CA_DM(t)  = NFA_DM(t) - NFA_DM(t-1)   [equation 2]
#
# ==============================================================================

panel <- panel %>%
  mutate(
    nfa_dm = nii_usd / r,
    ca_gdp = ca_usd / gdp_usd * 100
  )

panel <- panel %>%
  arrange(iso3c, year) %>%
  group_by(iso3c) %>%
  mutate(
    ca_dm     = nfa_dm - dplyr::lag(nfa_dm),
    ca_dm_gdp = ca_dm / gdp_usd * 100
  ) %>%
  ungroup()

# ==============================================================================
#
# Part 4 — US dark matter stock
#
# ==============================================================================

us <- panel %>% filter(iso3c == "USA") %>% arrange(year)

us$off_nfa <- NA_real_
idx82 <- which(us$year == 1982)
us$off_nfa[idx82] <- 329000

for (i in (idx82 + 1):nrow(us))
  us$off_nfa[i] <- us$off_nfa[i-1] + replace_na(us$ca_usd[i], 0)
for (i in seq(idx82 - 1, 1))
  us$off_nfa[i] <- us$off_nfa[i+1] - replace_na(us$ca_usd[i+1], 0)

us <- us %>%
  mutate(
    dm_stock_bn  = (nfa_dm - off_nfa) / 1e3,
    dm_stock_gdp = (nfa_dm - off_nfa) / gdp_usd * 100,
    nii_bn       = nii_usd / 1e3,
    cum_ca_bn    = cumsum(replace_na(ca_usd, 0)) / 1e3
  )


# ==============================================================================
#
# Part 5 — Output volatility (HP filter, lambda = 100)
#
# ==============================================================================

get_vol <- function(df_c) {
  y <- df_c$gdp_con[!is.na(df_c$gdp_con)]
  if (length(y) < 10) return(NA_real_)
  tryCatch(sd(mFilter::hpfilter(log(y), freq = 100)$cycle),
           error = function(e) NA_real_)
}

vol <- panel %>%
  filter(year >= y_cs, year <= y_cs_end) %>%
  group_by(iso3c) %>%
  group_map(~ tibble(iso3c = .y$iso3c, output_vol = get_vol(.x)),
            .keep = TRUE) %>%
  bind_rows()

panel <- left_join(panel, vol, by = "iso3c")


# ==============================================================================
#
# Part 6 — Cross-section dataset (cumulative 1980-2003)
#
# ==============================================================================

cs <- panel %>%
  filter(year >= y_cs, year <= y_cs_end) %>%
  group_by(iso3c) %>%
  summarise(
    country        = first(na.omit(country)),
    cum_oca_bn     = sum(ca_usd, na.rm = TRUE) / 1e3,
    cum_dm_bn      = { v <- nfa_dm[!is.na(nfa_dm)]
    if (length(v) < 2) NA_real_
    else (last(v) - first(v)) / 1e3 },
    dm_exp_bn      = cum_dm_bn - cum_oca_bn,
    gdp03_bn       = first(na.omit(gdp_usd[year == y_cs_end])) / 1e3,
    cum_oca_gdp    = cum_oca_bn / gdp03_bn * 100,
    cum_dm_gdp     = cum_dm_bn  / gdp03_bn * 100,
    dm_exp_gdp     = dm_exp_bn  / gdp03_bn * 100,
    fdi_assets_gdp = mean(fdi_assets_gdp[year %in% 2002:y_cs_end], na.rm = TRUE),
    fdi_liab_gdp   = mean(fdi_liab_gdp  [year %in% 2002:y_cs_end], na.rm = TRUE),
    output_vol     = first(na.omit(output_vol)),
    rule_of_law    = first(na.omit(rule_of_law)),
    rnd_avg        = mean(rnd, na.rm = TRUE),
    opec           = first(opec_d),
    hipc           = first(hipc_d),
    .groups = "drop"
  )


# ==============================================================================
#
# Part 7 — Figures
#
# Publication-quality style: white background, minimal gridlines, muted palette.
# ggsave() overwrites existing files — re-running this section updates all outputs.
#
# ==============================================================================

col_blue <- "#2166AC"
col_red  <- "#B2182B"
col_grey <- "grey45"

theme_paper <- theme_bw(base_size = 11) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = "grey88", linewidth = 0.4),
    panel.grid.minor   = element_blank(),
    panel.border       = element_rect(colour = "grey30", linewidth = 0.5),
    plot.title         = element_text(size = 11, face = "bold", hjust = 0,
                                      margin = margin(b = 4)),
    plot.subtitle      = element_text(size = 9, colour = "grey40", hjust = 0,
                                      margin = margin(b = 8)),
    axis.title         = element_text(size = 9.5),
    axis.text          = element_text(size = 8.5, colour = "grey20"),
    axis.ticks         = element_line(colour = "grey50", linewidth = 0.3),
    legend.position    = "bottom",
    legend.title       = element_blank(),
    legend.text        = element_text(size = 8.5),
    legend.key.width   = unit(1.8, "cm"),
    legend.key.height  = unit(0.4, "cm"),
    plot.margin        = margin(8, 10, 6, 8)
  )

theme_set(theme_paper)

save_fig <- function(p, name, w = 8, h = 5) {
  ggsave(here("code","output","figures", paste0(name,".pdf")),
         p, width = w, height = h, device = cairo_pdf)
  ggsave(here("code","output","figures", paste0(name,".png")),
         p, width = w, height = h, dpi = 300)
  print(p)
}

# ── Figure 1 ───────────────────────────────────────────────────────────────────

fig1 <- us %>%
  filter(year >= 1982, year <= y_end) %>%
  select(year, nii_bn, cum_ca_bn) %>%
  pivot_longer(-year, names_to = "series", values_to = "value") %>%
  mutate(series = recode(series,
                         "nii_bn"    = "Net investment income",
                         "cum_ca_bn" = "Cumulative current account")) %>%
  ggplot(aes(x = year, y = value, colour = series, linetype = series)) +
  geom_hline(yintercept = 0, colour = col_grey, linewidth = 0.4, linetype = "dashed") +
  geom_line(linewidth = 0.9) +
  scale_colour_manual(values = c(
    "Net investment income"      = col_blue,
    "Cumulative current account" = col_red)) +
  scale_linetype_manual(values = c(
    "Net investment income"      = "solid",
    "Cumulative current account" = "longdash")) +
  scale_x_continuous(breaks = seq(1982, 2005, 4)) +
  scale_y_continuous(labels = label_comma(suffix = " B")) +
  labs(title    = "Figure 1.  US Cumulative Current Account and Net Investment Income",
       subtitle = "In billions of US dollars",
       x = NULL, y = "Billions USD")

save_fig(fig1, "fig1_us_ca_nii")

# ── Figure 3a ──────────────────────────────────────────────────────────────────

fig3a <- cs %>%
  filter(!is.na(cum_dm_bn), !is.na(cum_oca_bn)) %>%
  ggplot(aes(x = cum_dm_bn, y = cum_oca_bn, label = iso3c)) +
  geom_abline(slope = 1, intercept = 0,
              colour = col_grey, linetype = "dashed", linewidth = 0.5) +
  geom_point(colour = col_blue, size = 1.8, alpha = 0.75) +
  geom_text_repel(size = 2.3, colour = "grey25",
                  segment.colour = "grey70", segment.size = 0.3,
                  box.padding = 0.25, max.overlaps = 30, seed = 42) +
  scale_x_continuous(labels = label_comma()) +
  scale_y_continuous(labels = label_comma()) +
  labs(title    = "Figure 3a.  Official vs Dark Matter Current Account (1980\u20132003)",
       subtitle = "Billions USD. Countries right of the 45\u00b0 line export dark matter.",
       x        = "Change in NFA \u2014 dark matter measure ($bn)",
       y        = "Cumulative official current account ($bn)")

save_fig(fig3a, "fig3a_scatter", h = 6.5)

# ── Figure 3b ──────────────────────────────────────────────────────────────────

fig3b <- cs %>%
  filter(!is.na(cum_dm_bn), !is.na(cum_oca_bn),
         iso3c != "USA", abs(cum_oca_bn) < 700, abs(cum_dm_bn) < 700) %>%
  ggplot(aes(x = cum_dm_bn, y = cum_oca_bn, label = iso3c)) +
  geom_abline(slope = 1, intercept = 0,
              colour = col_grey, linetype = "dashed", linewidth = 0.5) +
  geom_point(colour = col_blue, size = 1.8, alpha = 0.75) +
  geom_text_repel(size = 2.3, colour = "grey25",
                  segment.colour = "grey70", segment.size = 0.3,
                  box.padding = 0.25, max.overlaps = 35, seed = 42) +
  scale_x_continuous(labels = label_comma()) +
  scale_y_continuous(labels = label_comma()) +
  labs(title    = "Figure 3b.  Official vs Dark Matter CA (1980\u20132003, excl. USA)",
       subtitle = "Most countries cluster near the 45\u00b0 line. Billions USD.",
       x        = "Change in NFA \u2014 dark matter measure ($bn)",
       y        = "Cumulative official current account ($bn)")

save_fig(fig3b, "fig3b_scatter_zoom", h = 6.5)

# ── Figure 5b ──────────────────────────────────────────────────────────────────

fig5b <- cs %>%
  filter(!is.na(dm_exp_gdp), !is.na(cum_oca_gdp),
         abs(cum_oca_gdp) < quantile(abs(cum_oca_gdp), 0.97, na.rm = TRUE),
         abs(dm_exp_gdp)  < quantile(abs(dm_exp_gdp),  0.97, na.rm = TRUE)) %>%
  ggplot(aes(x = cum_oca_gdp, y = dm_exp_gdp, label = iso3c)) +
  geom_hline(yintercept = 0, colour = col_grey, linewidth = 0.4) +
  geom_vline(xintercept = 0, colour = col_grey, linewidth = 0.4) +
  geom_smooth(method = "lm", se = TRUE, colour = col_red,
              linewidth = 0.8, fill = col_red, alpha = 0.08) +
  geom_point(colour = col_blue, size = 1.8, alpha = 0.75) +
  geom_text_repel(size = 2.3, colour = "grey25",
                  segment.colour = "grey70", segment.size = 0.3,
                  box.padding = 0.25, max.overlaps = 30, seed = 42) +
  labs(title    = "Figure 5b.  Dark Matter Exports vs Official Current Account",
       subtitle = "Cumulative 1980\u20132003, % of 2003 GDP. OLS fit with 95% confidence band.",
       x        = "Cumulative official CA, 1980\u20132003 (% of 2003 GDP)",
       y        = "Cumulative dark matter exports (% of 2003 GDP)")

save_fig(fig5b, "fig5b_dm_vs_ca", h = 6.5)

# ── Figure 6c ──────────────────────────────────────────────────────────────────

global <- panel %>%
  filter(year >= 1980, year <= 2004, !is.na(nfa_dm), !is.na(gdp_usd)) %>%
  mutate(region = case_when(
    iso3c == "USA" ~ "United States",
    iso3c == "JPN" ~ "Japan",
    iso3c %in% eu  ~ "European Union",
    TRUE           ~ "Rest of World")) %>%
  group_by(year, region) %>%
  summarise(nfa_dm_sum = sum(nfa_dm, na.rm = TRUE), .groups = "drop") %>%
  left_join(
    panel %>% filter(!is.na(gdp_usd)) %>%
      group_by(year) %>%
      summarise(world_gdp = sum(gdp_usd, na.rm = TRUE), .groups = "drop"),
    by = "year") %>%
  mutate(nfa_pct = nfa_dm_sum / world_gdp * 100)

fig6c <- ggplot(global, aes(x = year, y = nfa_pct,
                            colour = region, linetype = region)) +
  geom_hline(yintercept = 0, colour = col_grey, linewidth = 0.4, linetype = "dashed") +
  geom_line(linewidth = 0.9) +
  scale_colour_manual(values = c(
    "United States"  = col_blue,
    "Japan"          = col_red,
    "European Union" = "grey30",
    "Rest of World"  = "grey65")) +
  scale_linetype_manual(values = c(
    "United States"  = "solid",
    "Japan"          = "longdash",
    "European Union" = "dashed",
    "Rest of World"  = "dotted")) +
  scale_x_continuous(breaks = seq(1980, 2004, 4)) +
  scale_y_continuous(labels = label_number(suffix = "%")) +
  guides(colour = guide_legend(nrow = 2), linetype = guide_legend(nrow = 2)) +
  labs(title    = "Figure 6c.  Net Foreign Asset Positions Including Dark Matter",
       subtitle = "As % of world GDP. With dark matter, the US appears as a stable net creditor.",
       x = NULL, y = "% of world GDP")

save_fig(fig6c, "fig6c_global_nfa")

# ── Figure 8 ───────────────────────────────────────────────────────────────────

fig8 <- us %>%
  filter(year >= 1982, year <= y_end, !is.na(dm_stock_bn)) %>%
  ggplot(aes(x = year)) +
  geom_col(aes(y = dm_stock_bn / 1e3),
           fill = col_blue, alpha = 0.25, width = 0.75) +
  geom_col(aes(y = dm_stock_bn / 1e3),
           fill = NA, colour = col_blue, alpha = 0.7, width = 0.75, linewidth = 0.3) +
  geom_line(aes(y = dm_stock_gdp / 10), colour = col_red, linewidth = 1) +
  geom_point(aes(y = dm_stock_gdp / 10), colour = col_red, size = 1.5) +
  scale_x_continuous(breaks = seq(1982, 2005, 4)) +
  scale_y_continuous(
    name     = "Trillions USD",
    labels   = label_number(suffix = "T"),
    sec.axis = sec_axis(~ . * 10, name = "% of US GDP",
                        labels = label_number(suffix = "%"))) +
  labs(title    = "Figure 8.  US Stock of Dark Matter (1982\u20132005)",
       subtitle = "Bars: trillions USD (left axis) \u2014 Red line: % of US GDP (right axis)",
       x = NULL) +
  theme(axis.title.y.right = element_text(colour = col_red, size = 9.5))

save_fig(fig8, "fig8_us_dm_stock")


# ==============================================================================
# Part 8 — Regression tables (PDF, economics paper format)
#
# stargazer generates LaTeX in AER style. compile_table() wraps each table in
# a minimal LaTeX document and compiles it with tinytex::pdflatex().
# All output files (.tex, .pdf, .log) land in code/output/tables/.
#
# Key fixes vs previous version:
#   - No "&" in column labels (causes LaTeX alignment errors)
#   - No gsub manipulation of the LaTeX (too fragile)
#   - setwd() to tables dir before compiling (keeps log/aux files there)
#   - landscape = TRUE for Table 4 (6 columns)
# ==============================================================================

compile_table <- function(tex_content, filename, landscape = FALSE) {
  
  # geometry: wider margins in landscape for 6-column tables
  geom <- if (landscape) {
    "\\usepackage[landscape, margin=0.7in]{geometry}\n"
  } else {
    "\\usepackage[margin=1in]{geometry}\n"
  }
  
  full_doc <- paste0(
    "\\documentclass[11pt]{article}\n",
    "\\usepackage{booktabs}\n",
    "\\usepackage{dcolumn}\n",
    "\\usepackage{graphicx}\n",   # for \resizebox
    geom,
    "\\begin{document}\n",
    "\\small\n",                   # slightly smaller font keeps tables on one page
    tex_content, "\n",
    "\\end{document}"
  )
  
  tables_dir <- here("code", "output", "tables")
  writeLines(full_doc, file.path(tables_dir, paste0(filename, ".tex")))
  
  # change to tables dir so .log / .aux / .pdf all land there
  old_wd <- setwd(tables_dir)
  on.exit(setwd(old_wd), add = TRUE)
  
  tryCatch(
    tinytex::pdflatex(paste0(filename, ".tex")),
    error = function(e) message("Compilation failed for ", filename,
                                " — check tinytex is installed.")
  )
}

# ── Table 1 ────────────────────────────────────────────────────────────────────

countries_109 <- c("ALB","AGO","ARG","AUS","AUT","BHR","BGD","BEN","BOL","BWA",
                   "BRA","BGR","BFA","KHM","CMR","CAN","CHL","CHN","COL","COG",
                   "CRI","CYP","CIV","DNK","DOM","ECU","EGY","SLV","EST","ETH",
                   "FJI","FIN","FRA","GAB","DEU","GHA","GRC","GTM","HTI","HND",
                   "HUN","ISL","IND","IDN","IRN","IRL","ISR","ITA","JAM","JPN",
                   "JOR","KEN","KOR","KWT","LAO","LBY","MDG","MWI","MYS","MLI",
                   "MLT","MUS","MEX","MAR","MOZ","MMR","NAM","NPL","NLD","NZL",
                   "NIC","NER","NGA","NOR","OMN","PAK","PAN","PNG","PRY","PER",
                   "PHL","POL","PRT","ROM","RWA","SAU","SEN","SGP","ZAF","ESP",
                   "LKA","SDN","SWZ","SWE","CHE","SYR","TZA","THA","TGO","TTO",
                   "TUN","TUR","UGA","GBR","USA","URY","VEN","YEM","ZWE")

d1 <- cs %>% 
  filter(iso3c %in% countries_109, !is.na(cum_dm_bn), !is.na(cum_oca_bn))

t1 <- list(
  "Full"               = lm(cum_oca_bn ~ cum_dm_bn, d1),
  "Excl. USA"          = lm(cum_oca_bn ~ cum_dm_bn, filter(d1, iso3c != "USA")),
  "Excl. USA, GBR"     = lm(cum_oca_bn ~ cum_dm_bn,
                            filter(d1, !iso3c %in% c("USA","GBR"))),
  "Excl. USA, GBR, JPN"= lm(cum_oca_bn ~ cum_dm_bn,
                            filter(d1, !iso3c %in% c("USA","GBR","JPN")))
)

tex1 <- capture.output(
  stargazer(t1[[1]], t1[[2]], t1[[3]], t1[[4]],
            column.labels    = names(t1),
            title            = "Cumulative Official CA and Dark Matter CA (1980--2003)",
            label            = "tab:table1",
            dep.var.labels   = "Cumulative official CA (\\$bn)",
            covariate.labels = c("Dark matter CA (\\$bn)", "Constant"),
            omit.stat        = c("f", "ser", "adj.rsq"),
            notes            = "Standard errors in parentheses. * p$<$0.10, ** p$<$0.05, *** p$<$0.01.",
            notes.align      = "l",
            style            = "aer",
            type             = "latex")
)
compile_table(paste(tex1, collapse = "\n"), "table1")

# ── Table 2 ────────────────────────────────────────────────────────────────────

d2 <- cs %>% 
  filter(iso3c %in% countries_109, !is.na(dm_exp_gdp), !is.na(cum_oca_gdp))

t2 <- list(
  "Full"           = lm(dm_exp_gdp ~ cum_oca_gdp, d2),
  "Excl. USA"      = lm(dm_exp_gdp ~ cum_oca_gdp, filter(d2, iso3c != "USA")),
  "Excl. USA, GBR" = lm(dm_exp_gdp ~ cum_oca_gdp,
                        filter(d2, !iso3c %in% c("USA","GBR")))
)

tex2 <- capture.output(
  stargazer(t2[[1]], t2[[2]], t2[[3]],
            column.labels    = names(t2),
            title            = "Dark Matter Exports and the Official Current Account (1980--2003)",
            label            = "tab:table2",
            dep.var.labels   = "Dark matter exports (\\% of 2003 GDP)",
            covariate.labels = c("Official CA (\\% of 2003 GDP)", "Constant"),
            omit.stat        = c("f", "ser", "adj.rsq"),
            notes            = "Standard errors in parentheses. * p$<$0.10, ** p$<$0.05, *** p$<$0.01.",
            notes.align      = "l",
            style            = "aer",
            type             = "latex")
)
compile_table(paste(tex2, collapse = "\n"), "table2")

#Following Hausmann & Sturzenegger (2006), 
#we restrict our sample to the 109 countries listed in their Appendix A.2. 
#Due to differences in country coverage between the original IFS data
#and our EWN 2024 source, our effective sample contains 101 countries 
#for Tables 1 and 2.

# ── Table 3 ────────────────────────────────────────────────────────────────────

winsor <- function(x, p = 0.01) {
  q <- quantile(x, c(p, 1-p), na.rm = TRUE)
  pmax(pmin(x, q[2]), q[1])
}

countries_79 <- c("ARG","AUS","AUT","BHR","BGD","BOL","BRA","CAN","CHL","COL",
                  "COG","CRI","CYP","CIV","DOM","ECU","EGY","SLV","ETH","FIN",
                  "FRA","GAB","DEU","GHA","GTM","HND","ISL","IND","IRL","ISR",
                  "ITA","JAM","JPN","JOR","KEN","KOR","KWT","LBY","MDG","MYS",
                  "MLI","MLT","MUS","MEX","MAR","MMR","NPL","NLD","NZL","NIC",
                  "NER","NOR","OMN","PAK","PAN","PRY","PER","PHL","POL","PRT",
                  "ROM","SAU","SEN","SGP","ZAF","ESP","LKA","SDN","SWE","CHE",
                  "SYR","THA","TGO","TUN","TUR","GBR","USA","URY","VEN")

d3 <- cs %>%
  filter(iso3c %in% countries_79, !is.na(dm_exp_gdp)) %>%
  mutate(across(c(dm_exp_gdp, fdi_assets_gdp, fdi_liab_gdp, output_vol), winsor))

model_ind <- lm(dm_exp_gdp ~ fdi_assets_gdp + fdi_liab_gdp + output_vol,
                data = cs %>%
                  filter(iso3c %in% industrial,
                         !is.na(dm_exp_gdp), !is.na(fdi_assets_gdp),
                         !is.na(fdi_liab_gdp), !is.na(output_vol)))

t3 <- list(
  "(i)"              = lm(dm_exp_gdp ~ fdi_assets_gdp + fdi_liab_gdp +
                            output_vol, d3),
  "(ii)"             = lm(dm_exp_gdp ~ fdi_assets_gdp + fdi_liab_gdp +
                            output_vol + rule_of_law + rnd_avg, d3),
  "(iii)"            = lm(dm_exp_gdp ~ fdi_assets_gdp + fdi_liab_gdp +
                            output_vol + rule_of_law + rnd_avg + opec + hipc, d3),
  "(vii) Industrial" = model_ind
)

tex3 <- capture.output(
  stargazer(t3[[1]], t3[[2]], t3[[3]], t3[[4]],
            column.labels    = names(t3),
            title            = "Sources of Dark Matter: Cross-Section Evidence (1980--2003)",
            label            = "tab:table3",
            dep.var.labels   = "Dark matter exports (\\% of 2003 GDP)",
            covariate.labels = c("FDI assets (\\% GDP)", "FDI liabilities (\\% GDP)",
                                 "Output volatility", "Rule of Law",
                                 "R\\&D (\\% GDP)", "OPEC dummy", "HIPC dummy",
                                 "Constant"),
            omit.stat        = c("f", "ser", "adj.rsq"),
            notes            = paste0("Standard errors in parentheses. ",
                                      "* p$<$0.10, ** p$<$0.05, *** p$<$0.01. ",
                                      "Variables winsorised at the 1\\% level. ",
                                      "Column (vii): 21 industrial countries, no winsorisation."),
            notes.align      = "l",
            style            = "aer",
            type             = "latex")
)
compile_table(paste(tex3, collapse = "\n"), "table3")

#We compute output volatility using World Bank WDI real GDP series, 
#whereas Hausmann & Sturzenegger (2006) relied on IFS data. 
#Differences in country coverage and GDP measurement methodologies 
#across these sources may explain the divergence in the estimated coefficient 
#on output volatility.

# ── Table 4 ────────────────────────────────────────────────────────────────────

d4 <- panel %>%
  filter(iso3c %in% countries_79,
         year >= 1980, year <= 2004,
         !is.na(ca_dm_gdp), !is.na(output_vol))
mk_pd <- function(df) plm::pdata.frame(df, index = c("iso3c","year"))
rhs   <- ca_dm_gdp ~ fdi_liab_gdp + fdi_assets_gdp + output_vol

t4 <- list(
  "Pool -- Full"  = plm::plm(rhs, mk_pd(d4), model = "pooling"),
  "Pool -- Restr" = plm::plm(rhs, mk_pd(filter(d4, opec_d==0, hipc_d==0)),
                             model = "pooling"),
  "Pool -- Ind"   = plm::plm(rhs, mk_pd(filter(d4, iso3c %in% industrial)),
                             model = "pooling"),
  "FE -- Full"    = plm::plm(rhs, mk_pd(d4), model = "within"),
  "FE -- Restr"   = plm::plm(rhs, mk_pd(filter(d4, opec_d==0, hipc_d==0)),
                             model = "within"),
  "FE -- Ind"     = plm::plm(rhs, mk_pd(filter(d4, iso3c %in% industrial)),
                             model = "within")
)

options(modelsummary_format_numeric_latex = "plain")

tex4_ms <- modelsummary(
  t4,
  stars    = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  fmt      = "%.4f",
  gof_omit = "AIC|BIC|Log|Adj|Within",
  coef_rename = c(
    "fdi_liab_gdp"   = "FDI liabilities (% GDP)",
    "fdi_assets_gdp" = "FDI assets (% GDP)",
    "output_vol"     = "Output volatility"
  ),
  output  = "latex_tabular"
)

tex4_wrapped <- paste0(
  "\\begin{table}[htbp]\n",
  "\\centering\n",
  "\\caption{Sources of Dark Matter: Panel Evidence (1980--2004)}\n",
  tex4_ms, "\n",
  "\\end{table}"
)

compile_table(tex4_wrapped, "table4", landscape = TRUE)

message("\nDone.")
message("Figures (PDF + PNG) : code/output/figures/")
message("Tables  (PDF + TEX) : code/output/tables/")

### FOR TABLES 3 AND 4 :
#"Unlike Hausmann & Sturzenegger (2006), we find a positive coefficient on
#output volatility, suggesting that the insurance channel identified in the
#original paper is not robust to updated data. 
#This may reflect substantial revisions in NFA stocks in the 2024 
#update of the EWN database, particularly for emerging economies 
#with high output volatility."

# ==============================================================================
#
# Summary — Replication of Hausmann & Sturzenegger (2006)
#
# This script replicates the main empirical results of the paper using data
# from the IMF BOP database, the EWN dataset (Lane & Milesi-Ferretti, Brookings
# 2024 update), and the World Bank WDI and WGI. The sample covers 1975-2005,
# with the cross-section restricted to 1980-2003 as in the original paper.
#
# We construct the dark matter measure by capitalising net investment income
# at a 5% discount rate (equations 1 and 2 of the paper), compute the US dark
# matter stock against the BEA-reported 1982 NFA anchor, and replicate the
# five main figures (1, 3a, 3b, 5b, 6c, 8) and four regression tables.
#
# Comparison with the original results will be added here once all regressions
# have been reviewed. 
#
# [TO BE COMPLETED]
#
# ==============================================================================

# ==============================================================================
# ==============================================================================
#
#                     Part II - Extension of the article
#
# ==============================================================================
# ==============================================================================




