# ==============================================================================
# 
# Paper Replication - Hausmann & Sturzenegger (2006)
# "Global Imbalances or Bad Accounting? The Missing Dark Matter in the Wealth of Nations"
#
# Eliot Gharib - Youssef Benzakour - Benjamin Frémy
#
# ==============================================================================
#
# ── Packages ───────────────────────────────────────────────────────────────────

pkgs <- c("tidyverse", "readxl", "countrycode", "here",
          "mFilter", "plm", "ggrepel", "scales", "broom",
          "stargazer", "tinytex", "modelsummary", "kableExtra",
          "WDI", "fixest", "sandwich", "patchwork", "zoo")

# Silent installation — suppresses the interactive menu that freezes the session
install.packages(
  pkgs[!pkgs %in% rownames(installed.packages())],
  repos = "https://cloud.r-project.org",
  quiet = TRUE,
  ask   = FALSE
)

invisible(lapply(pkgs, library, character.only = TRUE))

if (!tinytex::is_tinytex()) tinytex::install_tinytex()
options(tinytex.tlmgr.args = "--no-self-update")

# ── Output directories ─────────────────────────────────────────────────────────
#
# We separate outputs by part so figures and tables from the replication (I),
# the time extension (II) and the economic extensions (III) never mix.
# Each subdirectory is created silently if it does not already exist.

for (d in c("output/figures/part_I",  "output/tables/part_I",
            "output/figures/part_II", "output/tables/part_II",
            "output/figures/part_III","output/tables/part_III"))
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
# We draw on four sources :
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

# Annual columns only (exclude quarterly "YYYY-QN")
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
  rename(ca_usd  = `NETCD_T.CAB.USD.A`,
         nii_usd = `NETCD_T.IN1.USD.A`)

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
    gdp_usd    = `GDP (US$)`,
    ca_ewn     = `Current account balance`   # backup CA for BOP gaps
  ) %>%
  select(ifs_code, year, fdi_assets, fdi_liab, nfa, gdp_usd, ca_ewn) %>%
  filter(year >= y_start, year <= y_end) %>%
  mutate(
    iso3c          = countrycode(ifs_code, "imf", "iso3c", warn = FALSE),
    nfa_gdp        = nfa        / gdp_usd * 100,
    fdi_assets_gdp = fdi_assets / gdp_usd * 100,
    fdi_liab_gdp   = fdi_liab   / gdp_usd * 100
  ) %>%
  filter(!is.na(iso3c))

# ── World Bank WDI ────────────────────────────────────────────────────────────

read_wb <- function(filename) {
  read_csv(here("code", "data", filename), skip = 4, show_col_types = FALSE) %>%
    rename(iso3c = `Country Code`) %>%
    select(iso3c, matches("^\\d{4}$")) %>%
    pivot_longer(-iso3c, names_to = "year", values_to = "value") %>%
    mutate(year = as.integer(year)) %>%
    filter(year >= y_start, year <= y_end, !is.na(iso3c), iso3c != "")
}

gdp_con <- read_wb("GDP_constant_1975_2005.csv")    %>% rename(gdp_con = value)
rnd     <- read_wb("RND_expenditure_1975_2005.csv") %>% rename(rnd     = value)

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

# ── Country lists from Appendix A.2 of Hausmann & Sturzenegger (2006) ─────────
#
# FIX: Romania is "ROU" in the BOP (not "ROM"). 
#
# 7 countries are fullfled with NA and their data are impossible to get back with these data :
# AUT, BFA, CIV, IRL, MOZ, RWA, YEM 
# We should complete the data with other data source for these countries

countries_109 <- c("ALB","AGO","ARG","AUS","AUT","BHR","BGD","BEN","BOL","BWA",
                   "BRA","BGR","BFA","KHM","CMR","CAN","CHL","CHN","COL","COG",
                   "CRI","CYP","CIV","DNK","DOM","ECU","EGY","SLV","EST","ETH",
                   "FJI","FIN","FRA","GAB","DEU","GHA","GRC","GTM","HTI","HND",
                   "HUN","ISL","IND","IDN","IRN","IRL","ISR","ITA","JAM","JPN",
                   "JOR","KEN","KOR","KWT","LAO","LBY","MDG","MWI","MYS","MLI",
                   "MLT","MUS","MEX","MAR","MOZ","MMR","NAM","NPL","NLD","NZL",
                   "NIC","NER","NGA","NOR","OMN","PAK","PAN","PNG","PRY","PER",
                   "PHL","POL","PRT","ROU","RWA","SAU","SEN","SGP","ZAF","ESP",
                   "LKA","SDN","SWZ","SWE","CHE","SYR","TZA","THA","TGO","TTO",
                   "TUN","TUR","UGA","GBR","USA","URY","VEN","YEM","ZWE")
#                        ^^^ FIX: ROM → ROU (code ISO3C correct pour la Roumanie)

countries_79 <- c("ARG","AUS","AUT","BHR","BGD","BOL","BRA","CAN","CHL","COL",
                  "COG","CRI","CYP","CIV","DOM","ECU","EGY","SLV","ETH","FIN",
                  "FRA","GAB","DEU","GHA","GTM","HND","ISL","IND","IRL","ISR",
                  "ITA","JAM","JPN","JOR","KEN","KOR","KWT","LBY","MDG","MYS",
                  "MLI","MLT","MUS","MEX","MAR","MMR","NPL","NLD","NZL","NIC",
                  "NER","NOR","OMN","PAK","PAN","PRY","PER","PHL","POL","PRT",
                  "ROU","SAU","SEN","SGP","ZAF","ESP","LKA","SDN","SWE","CHE",
                  "SYR","THA","TGO","TUN","TUR","GBR","USA","URY","VEN")
#                  ^^^ FIX: ROM → ROU


# ==============================================================================
#
# Part 2 — Panel construction
#
# We scaffold a balanced grid of countries_109 × years and merge all sources.
# Where the BOP current account is missing, we fall back on the EWN CA series.
#
# ==============================================================================

scaffold <- expand.grid(
  iso3c = countries_109,
  year  = y_start:y_end,
  stringsAsFactors = FALSE
) %>%
  as_tibble()

panel <- scaffold %>%
  left_join(bop, by = c("iso3c", "year")) %>%
  left_join(EWN %>% select(iso3c, year, fdi_assets_gdp, fdi_liab_gdp,
                           nfa_gdp, gdp_usd, ca_ewn),
            by = c("iso3c", "year")) %>%
  left_join(gdp_con, by = c("iso3c", "year")) %>%
  left_join(rnd,     by = c("iso3c", "year")) %>%
  left_join(wgi,     by = "iso3c") %>%
  mutate(
    ca_usd  = if_else(is.na(ca_usd) & !is.na(ca_ewn), ca_ewn, ca_usd),
    country = countrycode(iso3c, "iso3c", "country.name"),
    opec_d  = as.integer(iso3c %in% opec),
    hipc_d  = as.integer(iso3c %in% hipc)
  ) %>%
  select(-ca_ewn) %>%
  filter(year >= y_start, year <= y_end) %>%
  arrange(iso3c, year)


# ==============================================================================
#
# Part 3 — Dark matter computation
#
# The core of H&S's approach: rather than tracking cumulative current account
# flows, we infer the true net asset position from the income it generates.
# Capitalising net investment income at r = 5% gives our dark-matter NFA;
# its first difference is our dark-matter current account.
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

panel <- panel %>%
  mutate(
    dm_exp_flow     = ca_dm - ca_usd,
    dm_exp_flow_gdp = dm_exp_flow / gdp_usd * 100
  )

# ==============================================================================
#
# Part 4 — US dark matter stock
#
# We anchor the official NFA at the 1982 BEA estimate of $329bn and accumulate
# the reported current account forward and backward. The gap between our
# income-capitalised NFA and this official series is the US dark matter stock.
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
# H&S use the standard deviation of the HP-filtered GDP cycle as their
# measure of macroeconomic instability, which they link to the insurance
# channel of dark matter. We replicate this using lambda = 100 (annual data).
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
# Part 6 — Cross-section dataset (strict cumulative 1980-2003)
#
# H&S's Tables 1–3 are cross-sectional: one observation per country,
# cumulating flows from 1980 to 2003. We build two versions:
#   - cs_strict   : requires exact 1980 and 2003 endpoints for both CA and NII
#   - cs_available: uses whatever window is available within 1980-2003
# The available version is our main replication sample given BOP coverage gaps.
#
# ==============================================================================

safe_first <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) NA else x[1]
}

mean_or_na <- function(x) {
  if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
}

value_at <- function(x, year, target_year) {
  v <- x[year == target_year]
  v <- v[!is.na(v)]
  if (length(v) == 0) NA_real_ else v[1]
}

cs <- panel %>%
  filter(year >= y_cs, year <= y_cs_end) %>%
  group_by(iso3c) %>%
  summarise(
    country = safe_first(country),
    
    # Coverage
    n_ca  = sum(!is.na(ca_usd)),
    n_nii = sum(!is.na(nii_usd)),
    has_ca_complete = n_ca == length(y_cs:y_cs_end),
    
    # Cumulative official current account.
    # Strict version: only if all annual CA values are available.
    cum_oca_bn = if_else(
      has_ca_complete,
      sum(ca_usd, na.rm = TRUE) / 1e3,
      NA_real_
    ),
    
    # Dark-matter change: exactly 2003 minus 1980.
    nfa_dm_1980 = value_at(nfa_dm, year, y_cs),
    nfa_dm_2003 = value_at(nfa_dm, year, y_cs_end),
    cum_dm_bn = if_else(
      !is.na(nfa_dm_1980) & !is.na(nfa_dm_2003),
      (nfa_dm_2003 - nfa_dm_1980) / 1e3,
      NA_real_
    ),
    
    dm_exp_bn = cum_dm_bn - cum_oca_bn,
    
    # GDP denominator: exactly 2003 GDP.
    gdp03_bn = value_at(gdp_usd, year, y_cs_end) / 1e3,
    
    cum_oca_gdp = cum_oca_bn / gdp03_bn * 100,
    cum_dm_gdp  = cum_dm_bn  / gdp03_bn * 100,
    dm_exp_gdp  = dm_exp_bn  / gdp03_bn * 100,
    
    # FDI stocks: exactly 2003, not 2002-2003 average.
    fdi_assets_gdp = value_at(fdi_assets_gdp, year, y_cs_end),
    fdi_liab_gdp   = value_at(fdi_liab_gdp,   year, y_cs_end),
    
    output_vol  = safe_first(output_vol),
    rule_of_law = safe_first(rule_of_law),
    rnd_avg     = mean_or_na(rnd),
    opec        = safe_first(opec_d),
    hipc        = safe_first(hipc_d),
    
    .groups = "drop"
  )

# Diagnostic
missing_dm <- cs$iso3c[is.na(cs$cum_dm_bn)]

message(sprintf(
  "Obs disponibles pour Tables 1 & 2 : %d / 109\nPays sans NII exploitable : %s\n",
  sum(!is.na(cs$cum_dm_bn) & !is.na(cs$cum_oca_bn) & cs$iso3c %in% countries_109),
  paste(missing_dm[missing_dm %in% countries_109], collapse = ", ")
))

# ==============================================================================
# Alternative cross-section: best available within 1980-2003
# Main specification for replication with current data
# ==============================================================================

cs_strict <- cs

cs_available <- panel %>%
  filter(year >= y_cs, year <= y_cs_end) %>%
  group_by(iso3c) %>%
  summarise(
    country = safe_first(country),
    
    n_ca  = sum(!is.na(ca_usd)),
    n_nii = sum(!is.na(nii_usd)),
    
    first_dm_year = ifelse(
      sum(!is.na(nfa_dm)) > 0,
      min(year[!is.na(nfa_dm)]),
      NA_integer_
    ),
    last_dm_year = ifelse(
      sum(!is.na(nfa_dm)) > 0,
      max(year[!is.na(nfa_dm)]),
      NA_integer_
    ),
    
    cum_oca_bn = ifelse(
      n_ca > 0,
      sum(ca_usd, na.rm = TRUE) / 1e3,
      NA_real_
    ),
    
    cum_dm_bn = {
      v <- nfa_dm[!is.na(nfa_dm)]
      if (length(v) < 2) NA_real_
      else (last(v) - first(v)) / 1e3
    },
    
    dm_exp_bn = cum_dm_bn - cum_oca_bn,
    
    gdp03_bn = value_at(gdp_usd, year, y_cs_end) / 1e3,
    
    cum_oca_gdp = cum_oca_bn / gdp03_bn * 100,
    cum_dm_gdp  = cum_dm_bn  / gdp03_bn * 100,
    dm_exp_gdp  = dm_exp_bn  / gdp03_bn * 100,
    
    # H&S use 2003 FDI stocks.
    # If exact 2003 is missing in current data, fallback to 2002-2003 average.
    fdi_assets_gdp_2003 = value_at(fdi_assets_gdp, year, y_cs_end),
    fdi_liab_gdp_2003   = value_at(fdi_liab_gdp,   year, y_cs_end),
    
    fdi_assets_gdp = ifelse(
      !is.na(fdi_assets_gdp_2003),
      fdi_assets_gdp_2003,
      mean_or_na(fdi_assets_gdp[year %in% 2002:y_cs_end])
    ),
    
    fdi_liab_gdp = ifelse(
      !is.na(fdi_liab_gdp_2003),
      fdi_liab_gdp_2003,
      mean_or_na(fdi_liab_gdp[year %in% 2002:y_cs_end])
    ),
    
    output_vol  = safe_first(output_vol),
    rule_of_law = safe_first(rule_of_law),
    rnd_avg     = mean_or_na(rnd),
    opec        = safe_first(opec_d),
    hipc        = safe_first(hipc_d),
    
    .groups = "drop"
  )

# Use the available-data version as the main replication sample
cs <- cs_available

sample_compare <- tibble(
  specification = c("strict endpoints", "available within 1980-2003"),
  table1_full_n = c(
    cs_strict %>% filter(iso3c %in% countries_109,
                         !is.na(cum_dm_bn), !is.na(cum_oca_bn)) %>% nrow(),
    cs_available %>% filter(iso3c %in% countries_109,
                            !is.na(cum_dm_bn), !is.na(cum_oca_bn)) %>% nrow()
  ),
  table3_79_n = c(
    cs_strict %>% filter(iso3c %in% countries_79,
                         !is.na(dm_exp_gdp),
                         !is.na(fdi_assets_gdp),
                         !is.na(fdi_liab_gdp),
                         !is.na(output_vol)) %>% nrow(),
    cs_available %>% filter(iso3c %in% countries_79,
                            !is.na(dm_exp_gdp),
                            !is.na(fdi_assets_gdp),
                            !is.na(fdi_liab_gdp),
                            !is.na(output_vol)) %>% nrow()
  )
)

print(sample_compare)



# ==============================================================================
#
# Part 7 — Figures
#
# All scatters are filtered to countries_109 (the exact H&S sample).
# Figure 6c uses a separate panel built from raw BOP data with linear
# interpolation to fill within-series gaps (approx() requires >= 2 non-NA pts).
#
# save_fig() writes each figure into the part-specific subfolder so that
# replication outputs (part_I) stay separate from extension outputs.
#
# ==============================================================================

col_blue <- "#2166AC"
col_red  <- "#B2182B"
col_grey <- "grey45"
col_green <- "#238B45"

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

save_fig <- function(p, name, part = "part_I", w = 8, h = 5) {
  ggsave(here("code", "output", "figures", part, paste0(name, ".pdf")),
         p, width = w, height = h, device = cairo_pdf)
  ggsave(here("code", "output", "figures", part, paste0(name, ".png")),
         p, width = w, height = h, dpi = 300)
  print(p)
}

# ── Figure 1 ───────────────────────────────────────────────────────────────────
#
# The central puzzle: the US has run cumulative current account deficits
# exceeding $5 trillion since 1982, yet net investment income remains positive.
# Something in the standard accounting is missing.

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
       subtitle = "Billions of US dollars",
       x = NULL, y = "Billions USD")

save_fig(fig1, "fig1_us_ca_nii", part = "part_I")

# ── Figure 3a ──────────────────────────────────────────────────────────────────
#
# Countries along the 45° line are those where the official current account
# and our dark-matter measure tell the same story. Outliers — especially the US —
# are where the two measures diverge most sharply.

fig3a <- cs %>%
  filter(iso3c %in% countries_109,
         !is.na(cum_dm_bn), !is.na(cum_oca_bn)) %>%
  ggplot(aes(x = cum_dm_bn, y = cum_oca_bn, label = iso3c)) +
  geom_abline(slope = 1, intercept = 0,
              colour = col_grey, linetype = "dashed", linewidth = 0.5) +
  geom_point(colour = col_blue, size = 1.8, alpha = 0.75) +
  geom_text_repel(size = 2.3, colour = "grey25",
                  segment.colour = "grey70", segment.size = 0.3,
                  box.padding = 0.25, max.overlaps = 30, seed = 42) +
  scale_x_continuous(labels = label_comma()) +
  scale_y_continuous(labels = label_comma()) +
  labs(title    = "Figure 3a.  Cumulative Official CA vs. Change in Dark Matter NFA (1980\u20132003)",
       subtitle = "Billions USD. Countries to the right of the 45\u00b0 line are net dark matter exporters.",
       x        = "Change in NFA \u2014 dark matter measure ($bn)",
       y        = "Cumulative official current account ($bn)")

save_fig(fig3a, "fig3a_scatter", part = "part_I", h = 6.5)

# ── Figure 3b ──────────────────────────────────────────────────────────────────
#
# Zoomed version excluding the US and trimming the axis range
# to show the central cluster more clearly.

fig3b <- cs %>%
  filter(iso3c %in% countries_109,
         !is.na(cum_dm_bn), !is.na(cum_oca_bn),
         iso3c != "USA",
         abs(cum_oca_bn) < 700, abs(cum_dm_bn) < 700) %>%
  ggplot(aes(x = cum_dm_bn, y = cum_oca_bn, label = iso3c)) +
  geom_abline(slope = 1, intercept = 0,
              colour = col_grey, linetype = "dashed", linewidth = 0.5) +
  geom_point(colour = col_blue, size = 1.8, alpha = 0.75) +
  geom_text_repel(size = 2.3, colour = "grey25",
                  segment.colour = "grey70", segment.size = 0.3,
                  box.padding = 0.25, max.overlaps = 35, seed = 42) +
  scale_x_continuous(labels = label_comma()) +
  scale_y_continuous(labels = label_comma()) +
  labs(title    = "Figure 3b.  Cumulative Official CA vs. Change in Dark Matter NFA (excl. USA)",
       subtitle = "Billions USD. Most countries cluster around the 45\u00b0 line.",
       x        = "Change in NFA \u2014 dark matter measure ($bn)",
       y        = "Cumulative official current account ($bn)")

save_fig(fig3b, "fig3b_scatter_zoom", part = "part_I", h = 6.5)

# ── Figure 5b ──────────────────────────────────────────────────────────────────
#
# The negative relationship between official CA and dark matter exports is
# H&S's key empirical result: countries that run deficits (top-left) are
# precisely those that export dark matter to finance them.

fig5b <- cs %>%
  filter(iso3c %in% countries_109,
         !is.na(dm_exp_gdp), !is.na(cum_oca_gdp),
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
  labs(title    = "Figure 5b.  Dark Matter Exports vs. Cumulative Official Current Account (1980\u20132003)",
       subtitle = "% of 2003 GDP. OLS fit with 95% confidence band.",
       x        = "Cumulative official current account, 1980\u20132003 (% of 2003 GDP)",
       y        = "Cumulative dark matter exports (% of 2003 GDP)")

save_fig(fig5b, "fig5b_dm_vs_ca", part = "part_I", h = 6.5)

# ── Figure 6c ──────────────────────────────────────────────────────────────────
#
# Global NFA positions under the dark matter lens. We rebuild a separate panel
# from raw BOP data with linear interpolation to fill within-series gaps,
# then express each region's dark-matter NFA as a share of world GDP.
# The key takeaway: once dark matter is included, the US looks like a stable
# creditor rather than the world's largest debtor.

panel_fig6 <- bop %>%
  left_join(EWN %>% select(iso3c, year, gdp_usd),
            by = c("iso3c", "year")) %>%
  filter(year >= 1980, year <= 2004) %>%
  group_by(iso3c) %>%
  arrange(year) %>%
  mutate(
    nii_fill = {
      ok <- !is.na(nii_usd)
      if (sum(ok) >= 2)
        approx(year[ok], nii_usd[ok], xout = year, rule = 1)$y
      else
        nii_usd
    },
    nfa_dm = nii_fill / r
  ) %>%
  ungroup()

global <- panel_fig6 %>%
  filter(!is.na(nfa_dm), !is.na(gdp_usd)) %>%
  mutate(region = case_when(
    iso3c == "USA" ~ "United States",
    iso3c == "JPN" ~ "Japan",
    iso3c %in% eu  ~ "European Union",
    TRUE           ~ "Rest of World"
  )) %>%
  group_by(year, region) %>%
  summarise(nfa_dm_sum = sum(nfa_dm, na.rm = TRUE), .groups = "drop") %>%
  left_join(
    panel_fig6 %>%
      filter(!is.na(gdp_usd)) %>%
      group_by(year) %>%
      summarise(world_gdp = sum(gdp_usd, na.rm = TRUE), .groups = "drop"),
    by = "year"
  ) %>%
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
  guides(colour   = guide_legend(nrow = 2),
         linetype = guide_legend(nrow = 2)) +
  labs(title    = "Figure 6c.  Net Foreign Asset Positions Including Dark Matter (1980\u20132004)",
       subtitle = "% of world GDP. With dark matter, the US appears as a stable net creditor.",
       x = NULL, y = "% of world GDP")

save_fig(fig6c, "fig6c_global_nfa", part = "part_I")

# ── Figure 8 ───────────────────────────────────────────────────────────────────
#
# The US dark matter stock has grown steadily since 1982, reaching ~40% of GDP
# by 2005. H&S argue this stock is stable enough to underwrite the official
# current account deficits without requiring a major dollar adjustment.

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

save_fig(fig8, "fig8_us_dm_stock", part = "part_I")


# ==============================================================================
#
# Part 8 — Regression tables
#
# compile_table() wraps a LaTeX tabular string into a standalone document,
# compiles it with pdflatex, and writes the PDF into the part-specific
# subfolder. This keeps replication tables separate from extension tables.
#
# Options:
#   - part        : subfolder name ("part_I", "part_II", "part_III")
#   - table_number: sets the LaTeX counter so the table number is correct
#   - landscape   : compiles wide tables in landscape format
#   - fit_width   : resizes the tabular to \linewidth via \resizebox (graphicx)
#
# Note: we do not use adjustbox — it was missing in TinyTeX in earlier drafts.
#
# ==============================================================================

compile_table <- function(tex_content, filename, part = "part_I",
                          landscape = FALSE, table_number = 1,
                          fit_width = FALSE) {
  
  geom <- if (landscape) {
    "\\usepackage[landscape, margin=0.7in]{geometry}\n"
  } else {
    "\\usepackage[margin=1in]{geometry}\n"
  }
  
  counter_cmd <- paste0("\\setcounter{table}{", table_number - 1L, "}\n")
  
  if (fit_width) {
    tex_content <- sub(
      "\\begin{tabular}",
      "\\resizebox{\\linewidth}{!}{%\n\\begin{tabular}",
      tex_content, fixed = TRUE)
    tex_content <- sub(
      "\\end{tabular}",
      "\\end{tabular}%\n}",
      tex_content, fixed = TRUE)
  }
  
  full_doc <- paste0(
    "\\documentclass[11pt]{article}\n",
    "\\usepackage{booktabs}\n",
    "\\usepackage{dcolumn}\n",
    "\\usepackage{graphicx}\n",
    "\\usepackage{amsmath}\n",
    geom,
    "\\begin{document}\n",
    "\\small\n",
    counter_cmd,
    tex_content, "\n",
    "\\end{document}"
  )
  
  tables_dir <- here("code", "output", "tables", part)
  tex_path   <- file.path(tables_dir, paste0(filename, ".tex"))
  pdf_path   <- file.path(tables_dir, paste0(filename, ".pdf"))
  
  writeLines(full_doc, tex_path)
  
  if (file.exists(pdf_path)) {
    removed <- tryCatch(
      file.remove(pdf_path),
      warning = function(w) FALSE,
      error   = function(e) FALSE
    )
    if (!isTRUE(removed))
      stop("Cannot remove old PDF: ", pdf_path,
           "\nClose the PDF if it is open, then rerun the script.")
  }
  
  old_wd <- setwd(tables_dir)
  on.exit(setwd(old_wd), add = TRUE)
  
  tinytex::pdflatex(paste0(filename, ".tex"))
  
  if (!file.exists(pdf_path))
    stop("PDF was not created: ", pdf_path,
         "\nCheck the LaTeX log: ", file.path(tables_dir, paste0(filename, ".log")))
}


# ── Table 1 ────────────────────────────────────────────────────────────────────
#
# Simple OLS of official CA on dark-matter CA, run on progressively smaller
# samples. The slope close to 1 (once the US and UK are removed) confirms
# that both measures track each other well for most countries.

d1 <- cs %>%
  filter(iso3c %in% countries_109, !is.na(cum_dm_bn), !is.na(cum_oca_bn))

t1 <- list(
  "Full"                = lm(cum_oca_bn ~ cum_dm_bn, d1),
  "Excl. USA"           = lm(cum_oca_bn ~ cum_dm_bn, filter(d1, iso3c != "USA")),
  "Excl. USA, GBR"      = lm(cum_oca_bn ~ cum_dm_bn,
                             filter(d1, !iso3c %in% c("USA","GBR"))),
  "Excl. USA, GBR, JPN" = lm(cum_oca_bn ~ cum_dm_bn,
                             filter(d1, !iso3c %in% c("USA","GBR","JPN")))
)

tex1 <- capture.output(
  stargazer(t1[[1]], t1[[2]], t1[[3]], t1[[4]],
            column.labels    = names(t1),
            title            = "Cumulative Current Account and Change in NFA (1980--2003)",
            label            = "tab:table1",
            dep.var.labels   = "Cumulative official CA (\\$bn)",
            covariate.labels = c("Dark matter CA (\\$bn)", "Constant"),
            omit.stat        = c("f", "ser", "adj.rsq"),
            notes            = "Standard errors in parentheses. * p$<$0.10, ** p$<$0.05, *** p$<$0.01.",
            notes.align      = "l",
            style            = "aer",
            type             = "latex")
)

compile_table(paste(tex1, collapse = "\n"),
              "table1", part = "part_I", table_number = 1)

# ── Table 2 ────────────────────────────────────────────────────────────────────
#
# Dark matter exports as a fraction of GDP regressed on the official CA.
# The negative slope (~-0.8) means that a 1% CA deficit is offset by
# roughly 0.8% of GDP in dark matter exports — imbalances are smaller
# than official statistics suggest.

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

compile_table(paste(tex2, collapse = "\n"),
              "table2", part = "part_I", table_number = 2)

# ── Table 3 ────────────────────────────────────────────────────────────────────
#
# Cross-section regression of cumulative dark matter exports (1980-2003) on
# the determinants H&S propose: FDI positions (knowledge channel), output
# volatility (insurance channel), rule of law, R&D, OPEC and HIPC dummies.
# We split the table across two PDFs to keep columns readable on an A4 page.

winsor <- function(x, p = 0.01) {
  if (all(is.na(x))) return(x)
  q <- quantile(x, c(p, 1 - p), na.rm = TRUE)
  pmax(pmin(x, q[2]), q[1])
}

cs <- cs %>%
  mutate(
    dm_exp_ratio     = dm_exp_gdp     / 100,
    fdi_assets_ratio = fdi_assets_gdp / 100,
    fdi_liab_ratio   = fdi_liab_gdp   / 100
  )

countries_99 <- setdiff(
  countries_109,
  c("BFA", "ETH", "MDG", "MWI", "MLI", "NPL", "NER", "RWA", "TGO", "UGA")
)

d3_79 <- cs %>%
  filter(iso3c %in% countries_79,
         !is.na(dm_exp_ratio), !is.na(fdi_assets_ratio),
         !is.na(fdi_liab_ratio), !is.na(output_vol)) %>%
  mutate(across(c(dm_exp_ratio, fdi_assets_ratio, fdi_liab_ratio, output_vol), winsor))

d3_99 <- cs %>%
  filter(iso3c %in% countries_99,
         !is.na(dm_exp_ratio), !is.na(fdi_assets_ratio),
         !is.na(fdi_liab_ratio), !is.na(output_vol)) %>%
  mutate(across(c(dm_exp_ratio, fdi_assets_ratio, fdi_liab_ratio, output_vol), winsor))

d3_ind <- cs %>%
  filter(iso3c %in% industrial,
         !is.na(dm_exp_ratio), !is.na(fdi_assets_ratio),
         !is.na(fdi_liab_ratio), !is.na(output_vol)) %>%
  mutate(across(c(dm_exp_ratio, fdi_assets_ratio, fdi_liab_ratio, output_vol), winsor))

t3 <- list(
  "(i)" = lm(dm_exp_ratio ~ fdi_assets_ratio + fdi_liab_ratio + output_vol,
             data = d3_79),
  "(ii)" = lm(dm_exp_ratio ~ fdi_assets_ratio + fdi_liab_ratio + output_vol +
                rule_of_law + rnd_avg, data = d3_79),
  "(iii)" = lm(dm_exp_ratio ~ fdi_assets_ratio + fdi_liab_ratio + output_vol +
                 rule_of_law + rnd_avg + opec + hipc, data = d3_79),
  "(iv)" = lm(dm_exp_ratio ~ fdi_assets_ratio + fdi_liab_ratio + output_vol,
              data = d3_99),
  "(v)" = lm(dm_exp_ratio ~ fdi_assets_ratio + fdi_liab_ratio + output_vol +
               rule_of_law + rnd_avg, data = d3_99),
  "(vi)" = lm(dm_exp_ratio ~ fdi_assets_ratio + fdi_liab_ratio + output_vol +
                rule_of_law + rnd_avg + opec + hipc, data = d3_99),
  "(vii) Industrial" = lm(dm_exp_ratio ~ fdi_assets_ratio + fdi_liab_ratio + output_vol,
                          data = d3_ind)
)

print(sapply(t3, nobs))

options("modelsummary_format_numeric_latex" = "plain")

coef_names_t3 <- c(
  "fdi_assets_ratio" = "FDI assets / GDP",
  "fdi_liab_ratio"   = "FDI liabilities / GDP",
  "output_vol"       = "Output volatility",
  "rule_of_law"      = "Rule of Law",
  "rnd_avg"          = "R\\&D (\\% GDP)",
  "opec"             = "OPEC dummy",
  "hipc"             = "HIPC dummy"
)

t3_left  <- t3[1:3]
t3_right <- t3[4:7]

tex3_left <- modelsummary(
  t3_left,
  stars       = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  fmt         = "%.3f",
  coef_rename = coef_names_t3,
  gof_omit    = "AIC|BIC|Log|F|RMSE",
  output      = "latex_tabular"
)

tex3_right <- modelsummary(
  t3_right,
  stars       = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  fmt         = "%.3f",
  coef_rename = coef_names_t3,
  gof_omit    = "AIC|BIC|Log|F|RMSE",
  output      = "latex_tabular"
)

wrap_table <- function(tabular, caption, label) {
  paste0(
    "\\begin{table}[htbp]\n",
    "\\centering\n",
    "\\caption{", caption, "}\n",
    "\\label{", label, "}\n",
    "\\scriptsize\n",
    tabular, "\n",
    "\\begin{minipage}{0.95\\linewidth}\n",
    "\\footnotesize Notes: Standard errors in parentheses. ",
    "Dependent variable: cumulative dark matter exports over 1980--2003, divided by 2003 GDP. ",
    "Variables are expressed as ratios where relevant and winsorised at the 1\\% level. ",
    "Columns (i)--(iii) use the H\\&S 79-country sample when data are available. ",
    "Columns (iv)--(vi) use the H\\&S 99-country sample when data are available. ",
    "Column (vii) uses the H\\&S industrial-country sample when data are available. ",
    "The lower number of observations is due to missing historical BOP income data ",
    "and, in columns with controls, limited WDI R\\&D coverage. ",
    "* p$<$0.10, ** p$<$0.05, *** p$<$0.01.\n",
    "\\end{minipage}\n",
    "\\end{table}"
  )
}

# We also write the .tex source into the part_I subfolder for reference
writeLines(
  wrap_table(tex3_left,
             "Sources of Dark Matter: Cross-Section Evidence, columns (i)--(iii)",
             "tab:table3a"),
  here("code", "output", "tables", "part_I", "table3a.tex")
)

writeLines(
  wrap_table(tex3_right,
             "Sources of Dark Matter: Cross-Section Evidence, columns (iv)--(vii)",
             "tab:table3b"),
  here("code", "output", "tables", "part_I", "table3b.tex")
)

compile_table(
  wrap_table(tex3_left,
             "Sources of Dark Matter: Cross-Section Evidence, columns (i)--(iii)",
             "tab:table3a"),
  "table3a", part = "part_I",
  landscape    = FALSE,
  table_number = 3,
  fit_width    = TRUE
)

compile_table(
  wrap_table(tex3_right,
             "Sources of Dark Matter: Cross-Section Evidence, columns (iv)--(vii)",
             "tab:table3b"),
  "table3b", part = "part_I",
  landscape    = FALSE,
  table_number = 3,
  fit_width    = TRUE
)

# ── Table 4 ────────────────────────────────────────────────────────────────────
#
# Panel version of Table 3 using annual dark-matter current account flows.
# Pooled OLS and country fixed effects, three samples (full, excl. OPEC/HIPC,
# industrial only). The fixed effects results speak to within-country variation.

d4 <- panel %>%
  filter(iso3c %in% countries_79,
         year >= 1980, year <= 2004,
         !is.na(ca_dm_gdp), !is.na(output_vol))

mk_pd <- function(df) plm::pdata.frame(df, index = c("iso3c", "year"))
rhs   <- ca_dm_gdp ~ fdi_liab_gdp + fdi_assets_gdp + output_vol

t4 <- list(
  "Pool -- Full"  = plm::plm(rhs, mk_pd(d4), model = "pooling"),
  "Pool -- Restr" = plm::plm(rhs, mk_pd(filter(d4, opec_d == 0, hipc_d == 0)),
                             model = "pooling"),
  "Pool -- Ind"   = plm::plm(rhs, mk_pd(filter(d4, iso3c %in% industrial)),
                             model = "pooling"),
  "FE -- Full"    = plm::plm(rhs, mk_pd(d4), model = "within"),
  "FE -- Restr"   = plm::plm(rhs, mk_pd(filter(d4, opec_d == 0, hipc_d == 0)),
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
  output = "latex_tabular"
)

tex4_wrapped <- paste0(
  "\\begin{table}[htbp]\n",
  "\\centering\n",
  "\\caption{What Determines Whether a Country Exports Dark Matter? Panel (1980--2004)}\n",
  "\\label{tab:table4}\n",
  tex4_ms, "\n",
  "\\end{table}"
)

compile_table(tex4_wrapped,
              "table4", part = "part_I",
              landscape    = TRUE,
              table_number = 4,
              fit_width    = TRUE)

# ── Table 4b — Panel, annual dark matter exports ───────────────────────────────
#
# Robustness: we switch the dependent variable from CA_DM to the dark matter
# export flow (CA_DM - official CA), which isolates the discrepancy between
# the two measures rather than the dark-matter CA itself.

d4b <- panel %>%
  filter(iso3c %in% countries_79,
         year >= 1980, year <= 2004,
         !is.na(dm_exp_flow_gdp),
         !is.na(fdi_liab_gdp),
         !is.na(fdi_assets_gdp),
         !is.na(output_vol))

rhs_b <- dm_exp_flow_gdp ~ fdi_liab_gdp + fdi_assets_gdp + output_vol

t4b <- list(
  "Pool -- Full"  = plm::plm(rhs_b, mk_pd(d4b), model = "pooling"),
  "Pool -- Restr" = plm::plm(rhs_b, mk_pd(filter(d4b, opec_d == 0, hipc_d == 0)),
                             model = "pooling"),
  "Pool -- Ind"   = plm::plm(rhs_b, mk_pd(filter(d4b, iso3c %in% industrial)),
                             model = "pooling"),
  "FE -- Full"    = plm::plm(rhs_b, mk_pd(d4b), model = "within"),
  "FE -- Restr"   = plm::plm(rhs_b, mk_pd(filter(d4b, opec_d == 0, hipc_d == 0)),
                             model = "within"),
  "FE -- Ind"     = plm::plm(rhs_b, mk_pd(filter(d4b, iso3c %in% industrial)),
                             model = "within")
)

tex4b_ms <- modelsummary(
  t4b,
  stars    = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  fmt      = "%.4f",
  gof_omit = "AIC|BIC|Log|Adj|Within",
  coef_rename = c(
    "fdi_liab_gdp"   = "FDI liabilities (% GDP)",
    "fdi_assets_gdp" = "FDI assets (% GDP)",
    "output_vol"     = "Output volatility"
  ),
  output = "latex_tabular"
)

tex4b_wrapped <- paste0(
  "\\begin{table}[htbp]\n",
  "\\centering\n",
  "\\caption{What Determines Annual Dark Matter Exports? Panel (1980--2004)}\n",
  "\\label{tab:table4b}\n",
  tex4b_ms, "\n",
  "\\begin{minipage}{0.95\\linewidth}\n",
  "\\footnotesize Notes: The dependent variable is annual dark matter exports, ",
  "$CA^{DM}_{it} - CA^{official}_{it}$, divided by GDP. ",
  "Pooled and country fixed-effects specifications are reported. ",
  "\\end{minipage}\n",
  "\\end{table}"
)

compile_table(tex4b_wrapped,
              "table4b", part = "part_I",
              landscape    = TRUE,
              table_number = 5,
              fit_width    = TRUE)

message("\nDone.")
message("Figures (PDF + PNG) : code/output/figures/part_I/")
message("Tables  (PDF + TEX) : code/output/tables/part_I/")
message("Main replication uses cs_available; cs_strict is kept as robustness diagnostic.")

# ==============================================================================
#
# Summary — Replication of Hausmann & Sturzenegger (2006)
#
# DATA COVERAGE NOTE
#
# Bug fix: "ROM" → "ROU" (correct ISO3C code for Romania) in countries_109
# and countries_79. The original code silently dropped Romania because the
# join with the BOP data never matched on the wrong country code.
#
# Coverage gaps in our IMF BOP extract:
# 7 countries from the H&S 109-country list —
# AUT, BFA, CIV, IRL, MOZ, RWA, YEM — have zero non-missing observations
# for both the current account and primary investment income series
# over the 1980-2003 window.
#
# We verified this directly in the raw BOP file before any cleaning or merging,
# and searched all candidate series related to income, investment, primary
# income, direct investment, portfolio, and interest flows. No usable
# alternative series covering 1980-2003 was found for these countries.
#
# They cannot be included in the dark matter computation without switching
# to a different or supplementary data source.
#
# For Table 3, the additional observation losses in the columns with controls
# are driven primarily by limited WDI R&D expenditure coverage.
#
# [TO BE COMPLETED: comparison with original H&S results]
#
# ==============================================================================


# ==============================================================================
# ==============================================================================
#
#   Additional Figures from H&S 
#
# ==============================================================================
# ==============================================================================
#
# Figures 2, 4, 6a, 7c, 7d require ewn_full (already created above).
# They run after the core replication; all objects are available.
#
# ==============================================================================

# Figure 2 — The US interest spread (1980–2005)

ewn_full <- EWN_raw %>%
  rename(
    ifs_code     = IFS_Code,
    year         = Year,
    fdi_assets   = `FDI assets (stock)`,
    fdi_liab     = `FDI liabilities (stock)`,
    total_assets = `Total assets`,
    total_liab   = `Total liabilities`,
    nfa          = `Net IIP excl gold`,
    gdp_usd      = `GDP (US$)`,
    ca_ewn       = `Current account balance`
  ) %>%
  mutate(
    nfa_official = nfa,
    gdp_ewn      = gdp_usd
  ) %>%
  select(
    ifs_code, year,
    fdi_assets, fdi_liab,
    total_assets, total_liab,
    nfa, nfa_official,
    gdp_usd, gdp_ewn,
    ca_ewn
  ) %>%
  filter(year >= y_start, year <= y_end) %>%
  mutate(
    iso3c          = countrycode(ifs_code, "imf", "iso3c", warn = FALSE),
    nfa_gdp        = nfa        / gdp_usd * 100,
    fdi_assets_gdp = fdi_assets / gdp_usd * 100,
    fdi_liab_gdp   = fdi_liab   / gdp_usd * 100
  ) %>%
  filter(!is.na(iso3c))

usa_gross_income <- bop_raw %>%
  rename(series_code = SERIES_CODE) %>%
  mutate(iso3c     = str_extract(series_code,"^[^.]+"),
         indicator = str_remove(series_code,"^[^.]+\\.")) %>%
  filter(iso3c=="USA",
         indicator %in% c("CD_T.IN1.USD.A","DB_T.IN1.USD.A")) %>%
  select(iso3c, indicator, all_of(year_cols)) %>%
  pivot_longer(all_of(year_cols), names_to="year", values_to="value") %>%
  mutate(year=as.integer(year), value=suppressWarnings(as.numeric(value))) %>%
  filter(year>=1980, year<=y_end, !is.na(value), value!=0) %>%
  pivot_wider(names_from=indicator, values_from=value) %>%
  rename(income_receipts=`CD_T.IN1.USD.A`,
         income_payments =`DB_T.IN1.USD.A`)

usa_stocks <- ewn_full %>%
  filter(iso3c=="USA") %>%
  select(year, total_assets, total_liab)

fig2_data <- usa_gross_income %>%
  left_join(usa_stocks, by="year") %>%
  filter(!is.na(total_assets), !is.na(total_liab),
         !is.na(income_receipts), !is.na(income_payments),
         total_assets>0, total_liab>0) %>%
  mutate(return_assets = income_receipts / total_assets * 100,
         return_liab   = income_payments  / total_liab   * 100,
         spread        = return_assets - return_liab)

fig2 <- fig2_data %>%
  pivot_longer(c(return_assets,return_liab,spread),
               names_to="series", values_to="value") %>%
  mutate(series=recode(series,
                       "return_assets"="Implicit Return on Assets",
                       "return_liab"  ="Implicit Return on Liab.",
                       "spread"       ="Spread")) %>%
  ggplot(aes(x=year, y=value, colour=series, linetype=series)) +
  geom_hline(yintercept=0, colour=col_grey, linewidth=0.4) +
  geom_line(linewidth=0.9) +
  scale_colour_manual(values=c("Implicit Return on Assets"=col_blue,
                               "Implicit Return on Liab." =col_red,
                               "Spread"                   ="grey30"),name=NULL) +
  scale_linetype_manual(values=c("Implicit Return on Assets"="solid",
                                 "Implicit Return on Liab." ="longdash",
                                 "Spread"                   ="dotted"),name=NULL) +
  scale_x_continuous(breaks=seq(1980,2005,4)) +
  scale_y_continuous(labels=label_number(suffix="%")) +
  labs(title="Figure 2.  The US Interest Spread (1980\u20132005)",
       subtitle="Implicit returns = gross income flows / gross position stocks (EWN 2024).",
       x=NULL, y="Return (%)")
save_fig(fig2, "fig2_us_interest_spread", w=9, h=5)

# Figure 4 — US net asset position: official NIIP vs. dark matter NFA
usa_nfa_off <- ewn_full %>%
  filter(iso3c=="USA", year>=1982, year<=y_end) %>%
  select(year, nfa_official) %>%
  mutate(nfa_off_bn = nfa_official/1e3)

fig4 <- us %>%
  filter(year>=1982, year<=y_end) %>%
  select(year, nfa_dm, off_nfa) %>%
  mutate(nfa_dm_bn=nfa_dm/1e3, off_nfa_bn=off_nfa/1e3) %>%
  left_join(usa_nfa_off, by="year") %>%
  pivot_longer(c(nfa_dm_bn, nfa_off_bn), names_to="series", values_to="value") %>%
  mutate(series=recode(series,
                       "nfa_dm_bn" ="Dark matter NFA (NII / r = 5%)",
                       "nfa_off_bn"="Official NIIP (EWN, incl. capital gains)")) %>%
  filter(!is.na(value)) %>%
  ggplot(aes(x=year, y=value, colour=series, linetype=series)) +
  geom_hline(yintercept=0, colour=col_grey, linewidth=0.4, linetype="dashed") +
  geom_line(linewidth=0.9) +
  scale_colour_manual(values=c("Dark matter NFA (NII / r = 5%)"          =col_blue,
                               "Official NIIP (EWN, incl. capital gains)"=col_red),name=NULL) +
  scale_linetype_manual(values=c("Dark matter NFA (NII / r = 5%)"          ="solid",
                                 "Official NIIP (EWN, incl. capital gains)"="longdash"),name=NULL) +
  scale_x_continuous(breaks=seq(1982,2005,4)) +
  scale_y_continuous(labels=label_comma(suffix=" B")) +
  labs(title="Figure 4.  US Net Asset Position (1982\u20132005)",
       subtitle="Billions USD. S&P500-adjusted series omitted (no external market data).",
       x=NULL, y="Billions USD")
save_fig(fig4, "fig4_us_net_asset_position", w=9, h=5)

# Figure 6a — Official NFA by region (EWN 2024, equivalent to H&S Figure 6b)
world_gdp_off <- ewn_full %>%
  filter(!is.na(gdp_ewn), year>=1980, year<=2004) %>%
  group_by(year) %>% summarise(world_gdp=sum(gdp_ewn,na.rm=TRUE),.groups="drop")

global_off <- ewn_full %>%
  filter(!is.na(nfa_official), year>=1980, year<=2004) %>%
  mutate(region=case_when(iso3c=="USA"~"United States",iso3c=="JPN"~"Japan",
                          iso3c %in% eu~"European Union",TRUE~"Rest of World")) %>%
  group_by(year,region) %>%
  summarise(nfa_sum=sum(nfa_official,na.rm=TRUE),.groups="drop") %>%
  left_join(world_gdp_off, by="year") %>%
  mutate(nfa_pct=nfa_sum/world_gdp*100)

fig6a <- ggplot(global_off, aes(x=year,y=nfa_pct,colour=region,linetype=region)) +
  geom_hline(yintercept=0, colour=col_grey, linewidth=0.4, linetype="dashed") +
  geom_line(linewidth=0.9) +
  scale_colour_manual(values=c("United States"=col_blue,"Japan"=col_red,
                               "European Union"="grey30","Rest of World"="grey65")) +
  scale_linetype_manual(values=c("United States"="solid","Japan"="longdash",
                                 "European Union"="dashed","Rest of World"="dotted")) +
  scale_x_continuous(breaks=seq(1980,2004,4)) +
  scale_y_continuous(labels=label_number(suffix="%")) +
  guides(colour=guide_legend(nrow=2), linetype=guide_legend(nrow=2)) +
  labs(title="Figure 6a.  Net Foreign Assets — Official Figures (1980\u20132004)",
       subtitle="% of world GDP. Source: EWN 2024 (equivalent to H&S Figure 6b).",
       x=NULL, y="% of world GDP")
save_fig(fig6a, "fig6a_nfa_official_figures", w=9, h=5)

# Figure 7c — China: official vs. dark-matter NFA
fig7c_data <- panel %>%
  filter(iso3c=="CHN", year>=1983, year<=y_end, !is.na(nfa_dm), !is.na(gdp_usd)) %>%
  transmute(year, nfa_dm_pct=nfa_dm/gdp_usd*100) %>%
  left_join(ewn_full %>% filter(iso3c=="CHN") %>%
              transmute(year, nfa_off_pct=nfa_official/gdp_ewn*100), by="year") %>%
  pivot_longer(c(nfa_dm_pct,nfa_off_pct),names_to="series",values_to="value") %>%
  mutate(series=recode(series,
                       "nfa_dm_pct" ="Net Foreign Assets with dark matter",
                       "nfa_off_pct"="Official net foreign assets")) %>%
  filter(!is.na(value))

fig7c <- ggplot(fig7c_data, aes(x=year,y=value,colour=series,linetype=series)) +
  geom_hline(yintercept=0, colour=col_grey, linewidth=0.4) +
  geom_line(linewidth=0.9) +
  scale_colour_manual(values=c("Net Foreign Assets with dark matter"=col_blue,
                               "Official net foreign assets"        =col_red),name=NULL) +
  scale_linetype_manual(values=c("Net Foreign Assets with dark matter"="solid",
                                 "Official net foreign assets"        ="longdash"),name=NULL) +
  scale_x_continuous(breaks=seq(1984,2004,4)) +
  scale_y_continuous(labels=label_number(suffix="%")) +
  labs(title="Figure 7c.  China: Official vs. Dark-Matter NFA (% of GDP)",
       subtitle="Dark matter = NII / r. Official = EWN 2024.",
       x=NULL, y="% of GDP")
save_fig(fig7c, "fig7c_china_nfa", w=8, h=5)

# Figure 7d — European Union: official vs. dark-matter NFA
eu_gdp_ts <- ewn_full %>%
  filter(iso3c %in% eu, !is.na(gdp_ewn), year>=1975, year<=y_end) %>%
  group_by(year) %>% summarise(eu_gdp=sum(gdp_ewn,na.rm=TRUE),.groups="drop")

eu_dm_ts <- panel %>%
  filter(iso3c %in% eu, !is.na(nfa_dm), year>=1975, year<=y_end) %>%
  group_by(year) %>% summarise(eu_nfa_dm=sum(nfa_dm,na.rm=TRUE),.groups="drop") %>%
  left_join(eu_gdp_ts, by="year") %>%
  mutate(nfa_dm_pct=eu_nfa_dm/eu_gdp*100)

eu_off_ts <- ewn_full %>%
  filter(iso3c %in% eu, !is.na(nfa_official), year>=1975, year<=y_end) %>%
  group_by(year) %>% summarise(eu_nfa_off=sum(nfa_official,na.rm=TRUE),.groups="drop") %>%
  left_join(eu_gdp_ts, by="year") %>%
  mutate(nfa_off_pct=eu_nfa_off/eu_gdp*100)

fig7d_data <- eu_dm_ts %>%
  left_join(eu_off_ts %>% select(year,nfa_off_pct), by="year") %>%
  pivot_longer(c(nfa_dm_pct,nfa_off_pct),names_to="series",values_to="value") %>%
  mutate(series=recode(series,
                       "nfa_dm_pct" ="Net Foreign Assets with dark matter",
                       "nfa_off_pct"="Official net foreign assets")) %>%
  filter(!is.na(value))

fig7d <- ggplot(fig7d_data, aes(x=year,y=value,colour=series,linetype=series)) +
  geom_hline(yintercept=0, colour=col_grey, linewidth=0.4) +
  geom_line(linewidth=0.9) +
  scale_colour_manual(values=c("Net Foreign Assets with dark matter"=col_blue,
                               "Official net foreign assets"        =col_red),name=NULL) +
  scale_linetype_manual(values=c("Net Foreign Assets with dark matter"="solid",
                                 "Official net foreign assets"        ="longdash"),name=NULL) +
  scale_x_continuous(breaks=seq(1976,2004,4)) +
  scale_y_continuous(labels=label_number(suffix="%")) +
  labs(title="Figure 7d.  European Union: Official vs. Dark-Matter NFA (% of EU GDP)",
       subtitle="Dark matter = NII / r. Official = EWN 2024.",
       x=NULL, y="% of GDP")
save_fig(fig7d, "fig7d_eu_nfa", w=8, h=5)

message("\nAll Part I figures and tables done.")
message("Figures → code/output/figures/part_I/")
message("Tables  → code/output/tables/part_I/")


# ==============================================================================
#
# Summary of differences vs H&S (2006)
#
# TABLE 1 & 2
#   Our cs_available: 102 obs (Table 1 full sample) vs H&S 94. Extra 8 countries
#   have partial data accepted by our flexible window. cs_strict reduces this gap.
#   Residual gap reflects differences between our 2024 IMF BOP extract and the
#   2006 IFS data used by H&S.
#
# TABLE 3
#   Col (i): 76 vs 79 — three countries_79 members missing FDI 2003 data in EWN.
#   Col (ii)-(iii): 57 vs 79 — entirely explained by missing WDI R&D coverage.
#     Fix: impute rnd_avg = 0 → recovers full sample. R&D remains insignificant.
#   Col (vii): 19 vs 21 — two industrial countries missing output_vol or FDI.
#
# TABLE 4
#   We have more obs than H&S (1830 vs 1624) because EWN 2024 has more complete
#   FDI coverage than LMF 2006. Cannot reduce without discarding valid data.
#   Sign of FDI assets (positive in our pooled full vs H&S negative) is due to
#   sample composition with richer OPEC data in the 2024 vintage.
#
# FIGURES 2, 4, 6a, 7
#   Figure 2: starts 1980 (not 1976) — our IMF BOP extract lacks pre-1980 data.
#   Figure 4: two lines only (BEA/EWN official and dark matter); S&P500-adjusted
#     series omitted for lack of external market-to-book ratio data.
#   Figure 6a: based on EWN 2024 (equivalent to H&S Figure 6b, not 6a from IFS).
#   Figure 7: China (7c) and EU (7d) only, as requested.
#
# ==============================================================================







# ==============================================================================
# ==============================================================================
#
#                 Part II — Extension and Decomposition
#
# ==============================================================================
# ==============================================================================
#
# PLAN
# ────
# Section 1 — Extension of Part I results to 1980–2022
#   Tables E1–E4b + 4 figures extending Part I cross-sections and time series
#
# Section 2 — Component-Specific NFA: Beyond the 5% Assumption
#   Sub-components (FDI/equity/debt/other income) × discount rates → NFA
#   Two discount-rate scenarios:
#     Scenario A: universal G&R (2006) rates — all countries identical
#     Scenario B: country-group rates (GRG 2017 / LMF 2007):
#       Privilege (USA, CHE, GBR): r_fdi=9%, r_eq=7%, r_debt=2%, r_other=4%
#       Advanced (other industrial): same as Scenario A
#       Emerging (all others):       r_fdi=6%, r_eq=5%, r_debt=5%, r_other=5%
#   TWO separate 2×2 panels (one per scenario):
#     fig_II_05: Scenario A — stacked NFA components + NIIP line
#     fig_II_06: Scenario B — stacked NFA components + NIIP line
#   The gap between the stacked top and the NIIP line = dark matter
#
# Section 3 — What Drives Dark Matter? Three Channels
#   3.1  Decomposition of DM by asset class → fig_II_07 (2×2)
#   3.2  Canal IDE / capital immatériel → fig_II_08 (time series)
#   3.3  Canal assurance → fig_II_09 (cross-section scatter)
#   3.4  Canal actif sûr / privilege → fig_II_10 (time series)
#   3.5  Regression table: all three channels → tab_II_T08
#
# NOTE — China fix (Section 2.1):
#   China's BOP sub-components (FDI income, portfolio equity, debt) are often
#   unreported at the required granularity in our IMF extract. When sub-component
#   coverage < 30% of total NII, we distribute total NII by EWN net-stock weights.
#   Imputed observations are counted and printed at runtime.
#
# File naming: fig_II_NN_description  /  tab_II_TNN_description
#
# ==============================================================================


# ── Parameters ─────────────────────────────────────────────────────────────────

y_ext_start <- 1980
y_ext_end   <- 2022

# Scenario A: universal rates (Gourinchas & Rey 2006)
r_fdi_A    <- 0.08
r_equity_A <- 0.06
r_debt_A   <- 0.03
r_other_A  <- 0.05

# Scenario B: country-group rates (GRG 2017; LMF 2007)
privilege_countries <- c("USA", "CHE", "GBR")

rates_B <- tibble(
  group      = c("privilege", "advanced", "emerging"),
  r_fdi_B    = c(0.09,         0.08,        0.06),
  r_equity_B = c(0.07,         0.06,        0.05),
  r_debt_B   = c(0.02,         0.03,        0.05),
  r_other_B  = c(0.04,         0.05,        0.05)
)

assign_group <- function(iso) {
  case_when(
    iso %in% privilege_countries ~ "privilege",
    iso %in% industrial          ~ "advanced",
    TRUE                         ~ "emerging"
  )
}

showcase     <- c("USA", "JPN", "CHN")
panel_labels <- c(USA = "États-Unis", JPN = "Japon",
                  CHN = "Chine", EU = "Union européenne")

# Refined component colour palette
comp_palette <- c(
  FDI    = "#2171B5",   # deep blue
  Equity = "#D94801",   # burnt orange
  Debt   = "#238B45",   # forest green
  Other  = "#756BB1"    # purple
)

# 3-year centred rolling mean (handles edges gracefully)
rollmean3 <- function(x) {
  n <- length(x)
  vapply(seq_len(n), function(i) {
    idx <- max(1, i-1):min(n, i+1)
    mean(x[idx], na.rm = TRUE)
  }, numeric(1))
}


# ==============================================================================
# ==============================================================================
#
#   SECTION 1 — Extension of Part I Results to 1980–2022
#
# ==============================================================================
# ==============================================================================

# ==============================================================================
# 1.1 — Data loading (all local)
# ==============================================================================

bop_raw_ext <- read_csv(
  here("code", "data", "Current_account_primary_income_1975_2022.csv"),
  show_col_types = FALSE
)

year_cols_ext <- names(bop_raw_ext)[
  grepl("^\\d{4}$", names(bop_raw_ext)) &
    as.integer(names(bop_raw_ext)) >= y_start &
    as.integer(names(bop_raw_ext)) <= y_ext_end
]

bop_ext <- bop_raw_ext %>%
  rename(series_code = SERIES_CODE) %>%
  mutate(iso3c     = str_extract(series_code, "^[^.]+"),
         indicator = str_remove(series_code, "^[^.]+\\.")) %>%
  filter(indicator %in% c("NETCD_T.CAB.USD.A", "NETCD_T.IN1.USD.A")) %>%
  select(iso3c, indicator, all_of(year_cols_ext)) %>%
  pivot_longer(all_of(year_cols_ext), names_to = "year", values_to = "value") %>%
  mutate(year  = as.integer(year),
         value = suppressWarnings(as.numeric(value))) %>%
  filter(year >= y_start, year <= y_ext_end) %>%
  pivot_wider(names_from = indicator, values_from = value) %>%
  rename(ca_usd = `NETCD_T.CAB.USD.A`, nii_usd = `NETCD_T.IN1.USD.A`)

EWN_ext <- EWN_raw %>%
  rename(ifs_code     = IFS_Code,   year = Year,
         fdi_assets   = `FDI assets (stock)`,
         fdi_liab     = `FDI liabilities (stock)`,
         eq_assets    = `Portfolio equity assets (stock)`,
         eq_liab      = `Portfolio equity liabilities (stock)`,
         debt_assets  = `Debt assets (stock)`,
         debt_liab    = `Debt liabilities (stock)`,
         nfa_official = `Net IIP excl gold`,
         gdp_usd      = `GDP (US$)`,
         ca_ewn       = `Current account balance`) %>%
  select(ifs_code, year, fdi_assets, fdi_liab, eq_assets, eq_liab,
         debt_assets, debt_liab, nfa_official, gdp_usd, ca_ewn) %>%
  filter(year >= y_start, year <= y_ext_end) %>%
  mutate(
    iso3c          = countrycode(ifs_code, "imf", "iso3c", warn = FALSE),
    fdi_assets_gdp = fdi_assets / gdp_usd * 100,
    fdi_liab_gdp   = fdi_liab   / gdp_usd * 100,
    net_fdi        = fdi_assets  - fdi_liab,
    net_equity     = eq_assets   - eq_liab,
    net_debt       = debt_assets - debt_liab,
    niip_gdp       = nfa_official / gdp_usd * 100
  ) %>%
  filter(!is.na(iso3c))

gdp_ext_raw <- read_wb("GDP_constant_1975_2022.csv") %>%
  rename(gdp_con = value) %>%
  filter(year <= y_ext_end, !is.na(gdp_con))

message(sprintf("Data loaded: %d GDP obs, %d countries",
                nrow(gdp_ext_raw), n_distinct(gdp_ext_raw$iso3c)))


# ==============================================================================
# 1.2 — Extended panel and cross-section
# ==============================================================================

panel_ext <- expand.grid(
  iso3c = countries_109, year = y_start:y_ext_end,
  stringsAsFactors = FALSE
) %>%
  as_tibble() %>%
  left_join(bop_ext, by = c("iso3c","year")) %>%
  left_join(EWN_ext %>% select(iso3c, year, fdi_assets_gdp, fdi_liab_gdp,
                               nfa_official, niip_gdp, gdp_usd, ca_ewn),
            by = c("iso3c","year")) %>%
  left_join(gdp_ext_raw, by = c("iso3c","year")) %>%
  left_join(wgi, by = "iso3c") %>%
  mutate(
    ca_usd  = if_else(is.na(ca_usd) & !is.na(ca_ewn), ca_ewn, ca_usd),
    country = countrycode(iso3c, "iso3c", "country.name"),
    opec_d  = as.integer(iso3c %in% opec),
    hipc_d  = as.integer(iso3c %in% hipc),
    nfa_dm  = nii_usd / r,
    ca_gdp  = ca_usd / gdp_usd * 100
  ) %>%
  select(-ca_ewn) %>%
  arrange(iso3c, year) %>%
  group_by(iso3c) %>%
  mutate(
    ca_dm           = nfa_dm - dplyr::lag(nfa_dm),
    ca_dm_gdp       = ca_dm / gdp_usd * 100,
    dm_exp_flow     = ca_dm - ca_usd,
    dm_exp_flow_gdp = dm_exp_flow / gdp_usd * 100
  ) %>%
  ungroup()

cs_ext <- panel_ext %>%
  filter(year >= y_ext_start, year <= y_ext_end) %>%
  group_by(iso3c) %>%
  summarise(
    country        = safe_first(country),
    n_ca           = sum(!is.na(ca_usd)),
    n_nii          = sum(!is.na(nii_usd)),
    cum_oca_bn     = ifelse(n_ca > 0, sum(ca_usd, na.rm=TRUE)/1e3, NA_real_),
    cum_dm_bn      = { v <- nfa_dm[!is.na(nfa_dm)]
    if (length(v)<2) NA_real_ else (last(v)-first(v))/1e3 },
    dm_exp_bn      = cum_dm_bn - cum_oca_bn,
    gdp_end_bn     = value_at(gdp_usd, year, y_ext_end) / 1e3,
    cum_oca_gdp    = cum_oca_bn / gdp_end_bn * 100,
    cum_dm_gdp     = cum_dm_bn  / gdp_end_bn * 100,
    dm_exp_gdp     = dm_exp_bn  / gdp_end_bn * 100,
    dm_exp_ratio   = dm_exp_gdp / 100,
    fdi_assets_gdp = value_at(fdi_assets_gdp, year, y_ext_end),
    fdi_liab_gdp   = value_at(fdi_liab_gdp,   year, y_ext_end),
    fdi_net_gdp    = fdi_assets_gdp - fdi_liab_gdp,
    fdi_assets_ratio = fdi_assets_gdp / 100,
    fdi_liab_ratio   = fdi_liab_gdp   / 100,
    output_vol_hp  = { y <- gdp_con[!is.na(gdp_con)]
    if (length(y) < 10) NA_real_
    else tryCatch(sd(mFilter::hpfilter(log(y), freq=100)$cycle),
                  error = function(e) NA_real_) },
    rule_of_law    = safe_first(rule_of_law),
    rnd_avg        = mean_or_na(rnd),
    opec           = safe_first(opec_d),
    hipc           = safe_first(hipc_d),
    grp            = assign_group(safe_first(iso3c)),
    .groups = "drop"
  )

output_vol_cs <- cs_ext %>% select(iso3c, output_vol_ext = output_vol_hp)
panel_ext_aug <- panel_ext %>% left_join(output_vol_cs, by = "iso3c")


# ==============================================================================
# 1.3 — Extension figures (renaming relative to Section 1)
# ==============================================================================

# ── fig_II_01 — US dark matter stock 1982–2022 ────────────────────────────────

us_ext <- panel_ext %>% filter(iso3c == "USA") %>% arrange(year)
us_ext$off_nfa <- NA_real_
idx82e <- which(us_ext$year == 1982)
us_ext$off_nfa[idx82e] <- 329000
for (i in (idx82e + 1):nrow(us_ext))
  us_ext$off_nfa[i] <- us_ext$off_nfa[i-1] + replace_na(us_ext$ca_usd[i], 0)
for (i in seq(idx82e - 1, 1))
  us_ext$off_nfa[i] <- us_ext$off_nfa[i+1] - replace_na(us_ext$ca_usd[i+1], 0)
us_ext <- us_ext %>%
  mutate(dm_stock_bn  = (nfa_dm - off_nfa) / 1e3,
         dm_stock_gdp = (nfa_dm - off_nfa) / gdp_usd * 100)

fig_II_01 <- us_ext %>%
  filter(year >= 1982, !is.na(dm_stock_bn)) %>%
  ggplot(aes(x = year)) +
  geom_col(aes(y = dm_stock_bn / 1e3), fill = col_blue, alpha = 0.22, width = 0.8) +
  geom_col(aes(y = dm_stock_bn / 1e3), fill = NA, colour = col_blue,
           alpha = 0.7, width = 0.8, linewidth = 0.3) +
  geom_line(aes(y = dm_stock_gdp / 10), colour = col_red, linewidth = 1) +
  geom_vline(xintercept = 2003.5, linetype = "dashed",
             colour = "grey50", linewidth = 0.6) +
  annotate("text", x = 2004.2,
           y = max(us_ext$dm_stock_gdp / 10, na.rm = TRUE) * 0.88,
           label = "H&S end", hjust = 0, size = 2.8, colour = "grey40") +
  scale_x_continuous(breaks = seq(1982, y_ext_end, 4)) +
  scale_y_continuous(name = "Trillions USD", labels = label_number(suffix = "T"),
                     sec.axis = sec_axis(~ . * 10, name = "% of US GDP",
                                         labels = label_number(suffix = "%"))) +
  labs(title    = paste0("US Stock of Dark Matter (1982\u2013", y_ext_end, ")"),
       subtitle = "Bars: $tn (left). Red line: % of GDP (right). Dashed = H&S end-date.",
       x = NULL) +
  theme(axis.title.y.right = element_text(colour = col_red, size = 9.5))

save_fig(fig_II_01, "fig_II_01_us_dm_stock", part = "part_II")

# ── fig_II_02 — Global NFA by region 1985–2022 ────────────────────────────────

world_gdp_ann <- panel_ext %>%
  filter(!is.na(gdp_usd), year >= 1985) %>%
  group_by(year) %>%
  summarise(world_gdp = sum(gdp_usd, na.rm = TRUE), .groups = "drop")

global_ext <- panel_ext %>%
  filter(!is.na(nfa_dm), !is.na(gdp_usd), year >= 1985) %>%
  mutate(region = case_when(
    iso3c == "USA" ~ "United States", iso3c == "JPN" ~ "Japan",
    iso3c %in% eu  ~ "European Union", TRUE ~ "Rest of World")) %>%
  group_by(year, region) %>%
  summarise(nfa_dm_sum = sum(nfa_dm, na.rm = TRUE), .groups = "drop") %>%
  left_join(world_gdp_ann, by = "year") %>%
  mutate(nfa_pct = nfa_dm_sum / world_gdp * 100)

fig_II_02 <- ggplot(global_ext,
                    aes(x = year, y = nfa_pct, colour = region, linetype = region)) +
  geom_hline(yintercept = 0, colour = col_grey, linewidth = 0.4, linetype = "dashed") +
  geom_vline(xintercept = 2003.5, linetype = "dashed",
             colour = "grey60", linewidth = 0.4) +
  geom_line(linewidth = 0.9) +
  scale_colour_manual(values = c("United States"="grey20","Japan"=col_red,
                                 "European Union"="grey50","Rest of World"="grey70")) +
  scale_linetype_manual(values = c("United States"="solid","Japan"="longdash",
                                   "European Union"="dashed","Rest of World"="dotted")) +
  scale_x_continuous(breaks = seq(1985, y_ext_end, 5)) +
  scale_y_continuous(labels = label_number(suffix = "%")) +
  guides(colour = guide_legend(nrow = 2), linetype = guide_legend(nrow = 2)) +
  labs(title    = paste0("Global NFA Including Dark Matter (1985\u2013", y_ext_end, ")"),
       subtitle = "% of world GDP. Dashed vertical = H&S end-date (2003).",
       x = NULL, y = "% of world GDP")

save_fig(fig_II_02, "fig_II_02_global_nfa_regions", part = "part_II")

# ── fig_II_03 — Extended scatter: cumulative DM NFA vs official CA ─────────────

d_sc <- cs_ext %>%
  filter(iso3c %in% countries_109, !is.na(cum_dm_bn), !is.na(cum_oca_bn))

fig_II_03 <- ggplot(d_sc, aes(x = cum_dm_bn, y = cum_oca_bn, label = iso3c)) +
  geom_abline(slope = 1, intercept = 0,
              colour = col_grey, linetype = "dashed", linewidth = 0.5) +
  geom_point(colour = col_blue, size = 1.8, alpha = 0.75) +
  geom_text_repel(size = 2.3, colour = "grey25", segment.colour = "grey70",
                  segment.size = 0.3, box.padding = 0.25, max.overlaps = 30, seed = 42) +
  scale_x_continuous(labels = label_comma()) +
  scale_y_continuous(labels = label_comma()) +
  labs(title   = paste0("Official CA vs. Dark-Matter NFA Change (",
                        y_ext_start, "\u2013", y_ext_end, ")"),
       subtitle = "Billions USD. 45\u00b0 line = dark matter CA equals official CA.",
       x = "Change in NFA — dark matter ($bn)",
       y = "Cumulative official current account ($bn)")

save_fig(fig_II_03, "fig_II_03_scatter_cumCA_vs_DM", part = "part_II", h = 6.5)

# ── fig_II_04 — Extended scatter: DM exports vs official CA (% GDP) ──────────

d_dm <- cs_ext %>%
  filter(iso3c %in% countries_109, !is.na(dm_exp_gdp), !is.na(cum_oca_gdp),
         abs(cum_oca_gdp) < quantile(abs(cum_oca_gdp), 0.97, na.rm = TRUE),
         abs(dm_exp_gdp)  < quantile(abs(dm_exp_gdp),  0.97, na.rm = TRUE))

fig_II_04 <- ggplot(d_dm, aes(x = cum_oca_gdp, y = dm_exp_gdp, label = iso3c)) +
  geom_hline(yintercept = 0, colour = col_grey, linewidth = 0.4) +
  geom_vline(xintercept = 0, colour = col_grey, linewidth = 0.4) +
  geom_smooth(method = "lm", se = TRUE, colour = col_red, linewidth = 0.8,
              fill = col_red, alpha = 0.08) +
  geom_point(colour = col_blue, size = 1.8, alpha = 0.75) +
  geom_text_repel(size = 2.3, colour = "grey25", segment.colour = "grey70",
                  segment.size = 0.3, box.padding = 0.25, max.overlaps = 30, seed = 42) +
  labs(title   = paste0("Dark Matter Exports vs. Official CA (",
                        y_ext_start, "\u2013", y_ext_end, ")"),
       subtitle = paste0("% of ", y_ext_end, " GDP. OLS fit with 95% CI."),
       x = paste0("Cumulative official CA (% of ", y_ext_end, " GDP)"),
       y = paste0("Cumulative DM exports (% of ", y_ext_end, " GDP)"))

save_fig(fig_II_04, "fig_II_04_scatter_DM_exports_vs_CA", part = "part_II", h = 6.5)


# ==============================================================================
# 1.4 — Extension tables
# ==============================================================================

wrap_ext <- function(tabular, caption, label, note, table_id) {
  paste0("\\renewcommand{\\thetable}{", table_id, "}\n",
         "\\begin{table}[htbp]\n\\centering\n",
         "\\caption{", caption, "}\n\\label{", label, "}\n\\scriptsize\n",
         tabular, "\n",
         "\\begin{minipage}{0.95\\linewidth}\n",
         "\\footnotesize Notes: ", note, "\n",
         "\\end{minipage}\n\\end{table}")
}

set_table_id <- function(tex, id)
  sub("\\begin{table}",
      paste0("\\renewcommand{\\thetable}{", id, "}\n\\begin{table}"),
      tex, fixed = TRUE)

# Table E1 — cumulative CA vs dark-matter CA
d1e <- cs_ext %>%
  filter(iso3c %in% countries_109, !is.na(cum_dm_bn), !is.na(cum_oca_bn))
t1e <- list(
  "Full"                = lm(cum_oca_bn ~ cum_dm_bn, d1e),
  "Excl. USA"           = lm(cum_oca_bn ~ cum_dm_bn, filter(d1e, iso3c!="USA")),
  "Excl. USA, GBR"      = lm(cum_oca_bn ~ cum_dm_bn,
                             filter(d1e, !iso3c %in% c("USA","GBR"))),
  "Excl. USA, GBR, JPN" = lm(cum_oca_bn ~ cum_dm_bn,
                             filter(d1e, !iso3c %in% c("USA","GBR","JPN")))
)
tex1e <- capture.output(stargazer(t1e[[1]], t1e[[2]], t1e[[3]], t1e[[4]],
                                  column.labels = names(t1e),
                                  title = paste0("Extended E1: Cumulative CA and Dark-Matter CA (",
                                                 y_ext_start,"--",y_ext_end,")"),
                                  label = "tab:E1_ext", dep.var.labels = "Cumulative official CA (\\$bn)",
                                  covariate.labels = c("Dark-matter CA (\\$bn)","Constant"),
                                  omit.stat = c("f","ser","adj.rsq"),
                                  notes = "SE in parentheses. * p$<$0.10, ** p$<$0.05, *** p$<$0.01.",
                                  notes.align = "l", style = "aer", type = "latex"))
compile_table(set_table_id(paste(tex1e,collapse="\n"),"E1"),
              "tab_II_T01_E1_cumCA_vs_DM", part="part_II", table_number=2)

# Table E2 — DM exports vs official CA
d2e <- cs_ext %>%
  filter(iso3c %in% countries_109, !is.na(dm_exp_gdp), !is.na(cum_oca_gdp))
t2e <- list(
  "Full"           = lm(dm_exp_gdp ~ cum_oca_gdp, d2e),
  "Excl. USA"      = lm(dm_exp_gdp ~ cum_oca_gdp, filter(d2e, iso3c!="USA")),
  "Excl. USA, GBR" = lm(dm_exp_gdp ~ cum_oca_gdp,
                        filter(d2e, !iso3c %in% c("USA","GBR")))
)
tex2e <- capture.output(stargazer(t2e[[1]], t2e[[2]], t2e[[3]],
                                  column.labels = names(t2e),
                                  title = paste0("Extended E2: Dark Matter Exports and Official CA (",
                                                 y_ext_start,"--",y_ext_end,")"),
                                  label = "tab:E2_ext",
                                  dep.var.labels   = paste0("DM exports (\\% of ",y_ext_end," GDP)"),
                                  covariate.labels = c(paste0("Official CA (\\% of ",y_ext_end," GDP)"),"Constant"),
                                  omit.stat = c("f","ser","adj.rsq"),
                                  notes = "SE in parentheses. * p$<$0.10, ** p$<$0.05, *** p$<$0.01.",
                                  notes.align = "l", style = "aer", type = "latex"))
compile_table(set_table_id(paste(tex2e,collapse="\n"),"E2"),
              "tab_II_T02_E2_DM_exports", part="part_II", table_number=3)

# Tables E3a/E3b — sources of dark matter
d3e_79 <- cs_ext %>% filter(iso3c %in% countries_79, !is.na(dm_exp_ratio),
                            !is.na(fdi_assets_ratio), !is.na(fdi_liab_ratio), !is.na(output_vol_hp)) %>%
  mutate(across(c(dm_exp_ratio,fdi_assets_ratio,fdi_liab_ratio,output_vol_hp), winsor))
d3e_99 <- cs_ext %>% filter(iso3c %in% countries_99, !is.na(dm_exp_ratio),
                            !is.na(fdi_assets_ratio), !is.na(fdi_liab_ratio), !is.na(output_vol_hp)) %>%
  mutate(across(c(dm_exp_ratio,fdi_assets_ratio,fdi_liab_ratio,output_vol_hp), winsor))
d3e_ind <- cs_ext %>% filter(iso3c %in% industrial, !is.na(dm_exp_ratio),
                             !is.na(fdi_assets_ratio), !is.na(fdi_liab_ratio), !is.na(output_vol_hp)) %>%
  mutate(across(c(dm_exp_ratio,fdi_assets_ratio,fdi_liab_ratio,output_vol_hp), winsor))

t3e <- list(
  "(i)"  = lm(dm_exp_ratio~fdi_assets_ratio+fdi_liab_ratio+output_vol_hp, data=d3e_79),
  "(ii)" = lm(dm_exp_ratio~fdi_assets_ratio+fdi_liab_ratio+output_vol_hp+rule_of_law, data=d3e_79),
  "(iii)"= lm(dm_exp_ratio~fdi_assets_ratio+fdi_liab_ratio+output_vol_hp+rule_of_law+opec+hipc, data=d3e_79),
  "(iv)" = lm(dm_exp_ratio~fdi_assets_ratio+fdi_liab_ratio+output_vol_hp, data=d3e_99),
  "(v)"  = lm(dm_exp_ratio~fdi_assets_ratio+fdi_liab_ratio+output_vol_hp+rule_of_law, data=d3e_99),
  "(vi)" = lm(dm_exp_ratio~fdi_assets_ratio+fdi_liab_ratio+output_vol_hp+rule_of_law+opec+hipc, data=d3e_99),
  "(vii)"= lm(dm_exp_ratio~fdi_assets_ratio+fdi_liab_ratio+output_vol_hp, data=d3e_ind)
)
message(sprintf("E3 obs: %s", paste(sapply(t3e,nobs),collapse="|")))

cn_e3 <- c("fdi_assets_ratio"="FDI assets / GDP","fdi_liab_ratio"="FDI liabilities / GDP",
           "output_vol_hp"="Output volatility","rule_of_law"="Rule of Law",
           "opec"="OPEC dummy","hipc"="HIPC dummy")
note_e3 <- paste0("Dep. var.: cum. DM exports ",y_ext_start,"--",y_ext_end,
                  " / ",y_ext_end," GDP. R\\&D excluded (WDI coverage thin post-2005). ",
                  "Variables winsorised at 1\\%. * p$<$0.10, ** p$<$0.05, *** p$<$0.01.")

compile_table(wrap_ext(
  modelsummary(t3e[1:3], stars=c("*"=0.1,"**"=0.05,"***"=0.01), fmt="%.3f",
               coef_rename=cn_e3, gof_omit="AIC|BIC|Log|F|RMSE", output="latex_tabular"),
  paste0("Extended E3a: Sources of Dark Matter, 79-Country Sample (",y_ext_start,"--",y_ext_end,")"),
  "tab:E3a_ext", note_e3, "E3a"),
  "tab_II_T03_E3a_sources_79", part="part_II", table_number=4, fit_width=TRUE)

compile_table(wrap_ext(
  modelsummary(t3e[4:7], stars=c("*"=0.1,"**"=0.05,"***"=0.01), fmt="%.3f",
               coef_rename=cn_e3, gof_omit="AIC|BIC|Log|F|RMSE", output="latex_tabular"),
  paste0("Extended E3b: Sources of Dark Matter, 99-Country \\& Industrial (",y_ext_start,"--",y_ext_end,")"),
  "tab:E3b_ext", note_e3, "E3b"),
  "tab_II_T04_E3b_sources_99", part="part_II", table_number=4, fit_width=TRUE)

# Tables E4/E4b — panel
make_fe_models <- function(data, lhs) {
  f  <- as.formula(paste(lhs,"~ fdi_liab_gdp + fdi_assets_gdp + output_vol_ext"))
  fe <- as.formula(paste(lhs,"~ fdi_liab_gdp + fdi_assets_gdp + output_vol_ext | iso3c"))
  list(
    "Pool -- Full" =fixest::feols(f, data=data),
    "Pool -- Restr"=fixest::feols(f, data=filter(data,opec_d==0,hipc_d==0)),
    "Pool -- Ind"  =fixest::feols(f, data=filter(data,iso3c %in% industrial)),
    "FE -- Full"   =fixest::feols(fe,data=data),
    "FE -- Restr"  =fixest::feols(fe,data=filter(data,opec_d==0,hipc_d==0)),
    "FE -- Ind"    =fixest::feols(fe,data=filter(data,iso3c %in% industrial))
  )
}
cn_e4 <- c("fdi_liab_gdp"="FDI liab. (\\% GDP)","fdi_assets_gdp"="FDI assets (\\% GDP)",
           "output_vol_ext"="Output volatility")
d4e  <- panel_ext_aug %>% filter(iso3c %in% countries_79, year>=y_ext_start,
                                 year<=y_ext_end, !is.na(ca_dm_gdp), !is.na(fdi_liab_gdp),
                                 !is.na(fdi_assets_gdp), !is.na(output_vol_ext))
d4be <- panel_ext_aug %>% filter(iso3c %in% countries_79, year>=y_ext_start,
                                 year<=y_ext_end, !is.na(dm_exp_flow_gdp), !is.na(fdi_liab_gdp),
                                 !is.na(fdi_assets_gdp), !is.na(output_vol_ext))

compile_table(wrap_ext(
  modelsummary(make_fe_models(d4e,"ca_dm_gdp"),
               stars=c("*"=0.1,"**"=0.05,"***"=0.01), fmt="%.4f",
               gof_omit="AIC|BIC|Log|Adj|Within|FE", coef_rename=cn_e4, output="latex_tabular"),
  paste0("Extended E4: Panel — Dark-Matter-Implied CA (",y_ext_start,"--",y_ext_end,")"),
  "tab:E4_ext",
  paste0("Dep. var.: $CA^{DM}_{it}$/GDP. Pooled OLS and country FE. * p$<$0.10, ** p$<$0.05, *** p$<$0.01."),
  "E4"),
  "tab_II_T05_E4_panel_CADM", part="part_II", landscape=TRUE, table_number=5, fit_width=TRUE)

compile_table(wrap_ext(
  modelsummary(make_fe_models(d4be,"dm_exp_flow_gdp"),
               stars=c("*"=0.1,"**"=0.05,"***"=0.01), fmt="%.4f",
               gof_omit="AIC|BIC|Log|Adj|Within|FE", coef_rename=cn_e4, output="latex_tabular"),
  paste0("Extended E4b: Panel — Annual DM Exports (",y_ext_start,"--",y_ext_end,")"),
  "tab:E4b_ext",
  paste0("Dep. var.: $(CA^{DM}_{it}-CA^{\\mathrm{off}}_{it})$/GDP. * p$<$0.10, ** p$<$0.05, *** p$<$0.01."),
  "E4b"),
  "tab_II_T06_E4b_panel_DMexports", part="part_II", landscape=TRUE, table_number=6, fit_width=TRUE)

message("Section 1 done.")





# ==============================================================================
# ==============================================================================
#
#   SECTION 2 — Component-Specific NFA: Beyond the 5% Assumption
#
# ==============================================================================
# ==============================================================================
#
# Three discount-rate scenarios:
#
#   Scenario A — Universal rates (Gourinchas & Rey 2006, NBER WP 11155)
#     r_fdi=8%, r_equity=6%, r_debt=3%, r_other=5% for all countries.
#
#   Scenario B — Country-group rates (Gourinchas, Rey & Govillot 2017;
#                Lane & Milesi-Ferretti 2007)
#     Privilege (USA, CHE, GBR): r_fdi=9%, r_eq=7%, r_debt=2%, r_other=4%
#     Advanced (other industrial): same as Scenario A
#     Emerging (all others):       r_fdi=6%, r_eq=5%, r_debt=5%, r_other=5%
#
#   Scenario C — Gross position approach: separate asset vs. liability returns
#     (Gourinchas & Rey 2006; Curcuru, Dvorak & Warnock 2008 QJE)
#     NFA_C = Sum_j(assets_j * r_j^asset - liabs_j * r_j^liab) / r_disc
#     This directly captures the exorbitant-privilege wedge:
#     privilege countries earn more on assets than they pay on liabilities.
#     Privilege FDI wedge: +5.5pp (9.5% earned vs 4% paid).
#     Emerging (China) FDI wedge: -1pp (6% earned vs 7% paid).
#
# Each scenario produces a 2x2 figure (USA, Japan, China, EU) showing:
#   - Stacked bars: NFA by asset class (sum = implied NFA total)
#   - Dark dashed line: implied NFA total (= top of stacked bars, explicit)
#   - Solid grey line: official NIIP (EWN) — gap to NFA total = dark matter
#
# ==============================================================================

# ── Additional parameters for Scenario C ──────────────────────────────────────
# Source: Gourinchas & Rey (2006, Table 1); Curcuru, Dvorak & Warnock (2008 QJE)

rates_C <- tibble(
  group          = c("privilege",  "advanced",  "emerging"),
  r_fdi_asset_C  = c(0.095,        0.080,        0.060),
  r_eq_asset_C   = c(0.075,        0.060,        0.050),
  r_debt_asset_C = c(0.045,        0.035,        0.030),
  r_fdi_liab_C   = c(0.040,        0.060,        0.070),
  r_eq_liab_C    = c(0.055,        0.055,        0.060),
  r_debt_liab_C  = c(0.025,        0.030,        0.050)
)

r_disc_C      <- 0.05      # common capitalisation rate (H&S baseline)
col_nfa_total <- "#1A1A2E" # near-black navy for the implied NFA total line


# ==============================================================================
# 2.1 — Extract NII sub-components + China fix
#
# China problem: the IMF BOP extract often has zero or missing FDI/equity/debt
# income sub-components for China, causing the full NII to appear under "Other"
# (grey bars dominate). The fix: when sub-components sum to < 30% of total NII,
# distribute total NII proportionally using EWN gross-position weights.
# For China: stricter threshold (< 90%) because many years have partial coverage.
# ==============================================================================

codes_comp <- c(
  nii_fdi    = "NETCD_T.D_F5_D42S.USD.A",
  nii_equity = "NETCD_T.P_F5_D4S.USD.A",
  nii_debt   = "NETCD_T.P_F3_D41.USD.A",
  nii_other  = "NETCD_T.O_F_D4P.USD.A",
  nii_total  = "NETCD_T.IN1.USD.A"
)

yr_comp <- names(bop_raw_ext)[
  grepl("^\\d{4}$", names(bop_raw_ext)) &
    as.integer(names(bop_raw_ext)) >= 1993 &
    as.integer(names(bop_raw_ext)) <= y_ext_end
]

# Extract sub-components from BOP file
nii_raw <- map_dfr(names(codes_comp), function(vn) {
  bop_raw_ext %>%
    rename(series_code = SERIES_CODE) %>%
    mutate(iso3c     = str_extract(series_code,"^[^.]+"),
           indicator = str_remove(series_code,"^[^.]+\\.")) %>%
    filter(indicator == codes_comp[[vn]]) %>%
    select(iso3c, all_of(yr_comp)) %>%
    pivot_longer(-iso3c, names_to="year", values_to="value") %>%
    mutate(year=as.integer(year),
           value=suppressWarnings(as.numeric(value)), component=vn)
}) %>%
  pivot_wider(names_from=component, values_from=value)

# Merge EWN stocks for imputation weights (A/B) and for Scenario C computation.
# NOTE: gross stock columns (fdi_assets, fdi_liab, etc.) are kept in the final
# select so that Scenario C can use them without a separate merge.

nii_comp_long <- nii_raw %>%
  left_join(
    EWN_ext %>%
      select(
        iso3c, year,
        fdi_assets, fdi_liab,
        eq_assets, eq_liab,
        debt_assets, debt_liab,
        net_fdi, net_equity, net_debt,
        nfa_official, gdp_usd
      ),
    by = c("iso3c", "year")
  ) %>%
  filter(!is.na(gdp_usd)) %>%
  mutate(
    # Sub-component coverage ratio before imputation
    sub_sum = coalesce(nii_fdi, 0) +
      coalesce(nii_equity, 0) +
      coalesce(nii_debt, 0) +
      coalesce(nii_other, 0),
    
    coverage = if_else(
      !is.na(nii_total) & abs(nii_total) > 0.1,
      abs(sub_sum) / abs(nii_total),
      NA_real_
    ),
    
    # Gross-position weights (preferred — income flows arise from gross positions)
    gross_fdi   = abs(fdi_assets)  + abs(fdi_liab),
    gross_eq    = abs(eq_assets)   + abs(eq_liab),
    gross_debt  = abs(debt_assets) + abs(debt_liab),
    gross_total = gross_fdi + gross_eq + gross_debt,
    
    w_fdi_gross  = if_else(gross_total > 0, gross_fdi  / gross_total, NA_real_),
    w_eq_gross   = if_else(gross_total > 0, gross_eq   / gross_total, NA_real_),
    w_debt_gross = if_else(gross_total > 0, gross_debt / gross_total, NA_real_),
    
    # Net-position fallback if gross stocks are missing
    abs_fdi       = abs(net_fdi),
    abs_eq        = abs(net_equity),
    abs_debt      = abs(net_debt),
    net_total_abs = abs_fdi + abs_eq + abs_debt,
    
    w_fdi_net  = if_else(net_total_abs > 0, abs_fdi  / net_total_abs, NA_real_),
    w_eq_net   = if_else(net_total_abs > 0, abs_eq   / net_total_abs, NA_real_),
    w_debt_net = if_else(net_total_abs > 0, abs_debt / net_total_abs, NA_real_),
    
    # Final weights: gross preferred, then net fallback, then equal weights
    w_fdi  = coalesce(w_fdi_gross,  w_fdi_net,  1 / 3),
    w_eq   = coalesce(w_eq_gross,   w_eq_net,   1 / 3),
    w_debt = coalesce(w_debt_gross, w_debt_net, 1 / 3),
    
    w_sum  = w_fdi + w_eq + w_debt,
    w_fdi  = w_fdi  / w_sum,
    w_eq   = w_eq   / w_sum,
    w_debt = w_debt / w_sum,
    
    # Imputation rule: all countries < 30%; China stricter at < 90%
    impute = !is.na(nii_total) &
      (is.na(coverage) | coverage < 0.30 |
         (iso3c == "CHN" & coverage < 0.90)),
    
    # Preserve original BOP components for diagnostics
    nii_fdi_raw    = nii_fdi,
    nii_equity_raw = nii_equity,
    nii_debt_raw   = nii_debt,
    nii_other_raw  = nii_other,
    
    # Imputed components
    nii_fdi_imp    = nii_total * w_fdi,
    nii_equity_imp = nii_total * w_eq,
    nii_debt_imp   = nii_total * w_debt,
    nii_other_imp  = 0,
    
    # Final components
    nii_fdi    = if_else(impute, nii_fdi_imp,    nii_fdi),
    nii_equity = if_else(impute, nii_equity_imp, nii_equity),
    nii_debt   = if_else(impute, nii_debt_imp,   nii_debt),
    nii_other  = if_else(impute, nii_other_imp,  nii_other),
    
    # Check final component sum
    nii_comp_sum = coalesce(nii_fdi, 0) +
      coalesce(nii_equity, 0) +
      coalesce(nii_debt, 0) +
      coalesce(nii_other, 0),
    
    coverage_after = if_else(
      !is.na(nii_total) & abs(nii_total) > 0.1,
      abs(nii_comp_sum) / abs(nii_total),
      NA_real_
    )
  )

message(sprintf(
  "Sub-component imputation: %d country-years",
  sum(nii_comp_long$impute, na.rm = TRUE)
))

message(sprintf(
  "  of which CHN: %d",
  sum(nii_comp_long$impute[nii_comp_long$iso3c == "CHN"], na.rm = TRUE)
))

message("China component diagnostic after imputation:")
print(
  nii_comp_long %>%
    filter(iso3c == "CHN") %>%
    summarise(
      first_year = min(year, na.rm = TRUE),
      last_year  = max(year, na.rm = TRUE),
      n_obs                = n(),
      n_total_nii          = sum(!is.na(nii_total)),
      n_imputed            = sum(impute, na.rm = TRUE),
      mean_coverage_before = mean(coverage,       na.rm = TRUE),
      mean_coverage_after  = mean(coverage_after, na.rm = TRUE),
      mean_w_fdi           = mean(w_fdi,  na.rm = TRUE),
      mean_w_eq            = mean(w_eq,   na.rm = TRUE),
      mean_w_debt          = mean(w_debt, na.rm = TRUE)
    )
)


# ==============================================================================
# 2.2 — Assign scenario rates and compute NFA components
# ==============================================================================

# Keep variables needed downstream: NII flows, net stocks, gross stocks (for C)
nii_comp_long <- nii_comp_long %>%
  select(
    iso3c, year,
    nii_fdi, nii_equity, nii_debt, nii_other, nii_total,
    nfa_official, gdp_usd,
    net_fdi, net_equity, net_debt,
    fdi_assets, fdi_liab, eq_assets, eq_liab, debt_assets, debt_liab,
    impute, coverage, coverage_after,
    w_fdi, w_eq, w_debt
  )

# ── Scenarios A and B: capitalise NII sub-components ─────────────────────────

country_rates <- nii_comp_long %>%
  distinct(iso3c) %>%
  mutate(
    grp        = assign_group(iso3c),
    r_fdi_B    = rates_B$r_fdi_B[match(grp, rates_B$group)],
    r_equity_B = rates_B$r_equity_B[match(grp, rates_B$group)],
    r_debt_B   = rates_B$r_debt_B[match(grp, rates_B$group)],
    r_other_B  = rates_B$r_other_B[match(grp, rates_B$group)]
  )

nfa_decomp <- nii_comp_long %>%
  left_join(country_rates, by = "iso3c") %>%
  mutate(
    # ── Scenario A ────────────────────────────────────────────────────────────
    nfa_fdi_A    = nii_fdi    / r_fdi_A,
    nfa_equity_A = nii_equity / r_equity_A,
    nfa_debt_A   = nii_debt   / r_debt_A,
    nfa_other_A  = nii_other  / r_other_A,
    nfa_fdi_A_gdp    = nfa_fdi_A    / gdp_usd * 100,
    nfa_equity_A_gdp = nfa_equity_A / gdp_usd * 100,
    nfa_debt_A_gdp   = nfa_debt_A   / gdp_usd * 100,
    nfa_other_A_gdp  = nfa_other_A  / gdp_usd * 100,
    nfa_precise_A_gdp = nfa_fdi_A_gdp + nfa_equity_A_gdp +
      nfa_debt_A_gdp + nfa_other_A_gdp,
    
    # ── Scenario B ────────────────────────────────────────────────────────────
    nfa_fdi_B    = nii_fdi    / r_fdi_B,
    nfa_equity_B = nii_equity / r_equity_B,
    nfa_debt_B   = nii_debt   / r_debt_B,
    nfa_other_B  = nii_other  / r_other_B,
    nfa_fdi_B_gdp    = nfa_fdi_B    / gdp_usd * 100,
    nfa_equity_B_gdp = nfa_equity_B / gdp_usd * 100,
    nfa_debt_B_gdp   = nfa_debt_B   / gdp_usd * 100,
    nfa_other_B_gdp  = nfa_other_B  / gdp_usd * 100,
    nfa_precise_B_gdp = nfa_fdi_B_gdp + nfa_equity_B_gdp +
      nfa_debt_B_gdp + nfa_other_B_gdp,
    
    # ── Official NIIP and dark matter ─────────────────────────────────────────
    nfa_official_gdp = nfa_official / gdp_usd * 100,
    dm_A_gdp = nfa_precise_A_gdp - nfa_official_gdp,
    dm_B_gdp = nfa_precise_B_gdp - nfa_official_gdp,
    
    # ── Component dark matter (implied NFA - official stock, EWN) ─────────────
    dm_fdi_A_gdp    = (nfa_fdi_A    - net_fdi)    / gdp_usd * 100,
    dm_equity_A_gdp = (nfa_equity_A - net_equity)  / gdp_usd * 100,
    dm_debt_A_gdp   = (nfa_debt_A   - net_debt)    / gdp_usd * 100,
    dm_other_A_gdp  = dm_A_gdp - dm_fdi_A_gdp - dm_equity_A_gdp - dm_debt_A_gdp,
    dm_fdi_B_gdp    = (nfa_fdi_B    - net_fdi)    / gdp_usd * 100,
    dm_equity_B_gdp = (nfa_equity_B - net_equity)  / gdp_usd * 100,
    dm_debt_B_gdp   = (nfa_debt_B   - net_debt)    / gdp_usd * 100,
    dm_other_B_gdp  = dm_B_gdp - dm_fdi_B_gdp - dm_equity_B_gdp - dm_debt_B_gdp
  )

# EU aggregate — all EU countries are "advanced" → Scenario B = A for EU
eu_in_data <- intersect(eu, unique(nfa_decomp$iso3c))

eu_agg <- nfa_decomp %>%
  filter(iso3c %in% eu_in_data, !is.na(gdp_usd)) %>%
  group_by(year) %>%
  summarise(
    across(c(nii_fdi, nii_equity, nii_debt, nii_other, nii_total,
             nfa_official, net_fdi, net_equity, net_debt,
             nfa_fdi_A, nfa_equity_A, nfa_debt_A, nfa_other_A,
             nfa_fdi_B, nfa_equity_B, nfa_debt_B, nfa_other_B,
             gdp_usd), sum, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    iso3c = "EU", grp = "advanced",
    nfa_fdi_A_gdp    = nfa_fdi_A    / gdp_usd * 100,
    nfa_equity_A_gdp = nfa_equity_A / gdp_usd * 100,
    nfa_debt_A_gdp   = nfa_debt_A   / gdp_usd * 100,
    nfa_other_A_gdp  = nfa_other_A  / gdp_usd * 100,
    nfa_precise_A_gdp = nfa_fdi_A_gdp + nfa_equity_A_gdp +
      nfa_debt_A_gdp + nfa_other_A_gdp,
    # B = A for EU (all advanced)
    nfa_fdi_B_gdp    = nfa_fdi_A_gdp,
    nfa_equity_B_gdp = nfa_equity_A_gdp,
    nfa_debt_B_gdp   = nfa_debt_A_gdp,
    nfa_other_B_gdp  = nfa_other_A_gdp,
    nfa_precise_B_gdp = nfa_precise_A_gdp,
    nfa_official_gdp  = nfa_official / gdp_usd * 100,
    dm_A_gdp = nfa_precise_A_gdp - nfa_official_gdp,
    dm_B_gdp = dm_A_gdp,
    dm_fdi_A_gdp    = (nfa_fdi_A    - net_fdi)    / gdp_usd * 100,
    dm_equity_A_gdp = (nfa_equity_A - net_equity)  / gdp_usd * 100,
    dm_debt_A_gdp   = (nfa_debt_A   - net_debt)    / gdp_usd * 100,
    dm_other_A_gdp  = dm_A_gdp - dm_fdi_A_gdp - dm_equity_A_gdp - dm_debt_A_gdp,
    dm_fdi_B_gdp    = dm_fdi_A_gdp,   dm_equity_B_gdp = dm_equity_A_gdp,
    dm_debt_B_gdp   = dm_debt_A_gdp,  dm_other_B_gdp  = dm_other_A_gdp
  )

# ── Scenario C: expected income from gross stocks ─────────────────────────────

country_rates_C <- nii_comp_long %>%
  distinct(iso3c) %>%
  mutate(
    grp            = assign_group(iso3c),
    r_fdi_asset_C  = rates_C$r_fdi_asset_C[match(grp,  rates_C$group)],
    r_eq_asset_C   = rates_C$r_eq_asset_C[match(grp,   rates_C$group)],
    r_debt_asset_C = rates_C$r_debt_asset_C[match(grp, rates_C$group)],
    r_fdi_liab_C   = rates_C$r_fdi_liab_C[match(grp,  rates_C$group)],
    r_eq_liab_C    = rates_C$r_eq_liab_C[match(grp,   rates_C$group)],
    r_debt_liab_C  = rates_C$r_debt_liab_C[match(grp, rates_C$group)]
  )

nfa_decomp_C <- nii_comp_long %>%
  left_join(country_rates_C, by = "iso3c") %>%
  filter(!is.na(gdp_usd)) %>%
  mutate(
    # Expected net income by component = assets * r^asset - liabilities * r^liab
    exp_nii_fdi    = coalesce(fdi_assets,0)  * r_fdi_asset_C  -
      coalesce(fdi_liab,0)    * r_fdi_liab_C,
    exp_nii_equity = coalesce(eq_assets,0)   * r_eq_asset_C   -
      coalesce(eq_liab,0)     * r_eq_liab_C,
    exp_nii_debt   = coalesce(debt_assets,0) * r_debt_asset_C -
      coalesce(debt_liab,0)   * r_debt_liab_C,
    # Capitalise at common discount rate r_disc_C = 5%
    nfa_fdi_C_gdp    = (exp_nii_fdi    / r_disc_C) / gdp_usd * 100,
    nfa_equity_C_gdp = (exp_nii_equity / r_disc_C) / gdp_usd * 100,
    nfa_debt_C_gdp   = (exp_nii_debt   / r_disc_C) / gdp_usd * 100,
    nfa_precise_C_gdp = nfa_fdi_C_gdp + nfa_equity_C_gdp + nfa_debt_C_gdp,
    nfa_official_gdp  = nfa_official / gdp_usd * 100,
    dm_C_gdp = nfa_precise_C_gdp - nfa_official_gdp
  )

# EU aggregate for Scenario C (all EU = advanced → rates identical to advanced)
eu_agg_C <- nfa_decomp_C %>%
  filter(iso3c %in% eu_in_data, !is.na(gdp_usd)) %>%
  group_by(year) %>%
  summarise(
    across(c(fdi_assets, fdi_liab, eq_assets, eq_liab, debt_assets, debt_liab,
             nfa_official, gdp_usd), sum, na.rm = TRUE),
    r_fdi_asset_C  = first(r_fdi_asset_C),
    r_eq_asset_C   = first(r_eq_asset_C),
    r_debt_asset_C = first(r_debt_asset_C),
    r_fdi_liab_C   = first(r_fdi_liab_C),
    r_eq_liab_C    = first(r_eq_liab_C),
    r_debt_liab_C  = first(r_debt_liab_C),
    .groups = "drop"
  ) %>%
  mutate(
    iso3c = "EU",
    exp_nii_fdi    = fdi_assets  * r_fdi_asset_C  - fdi_liab  * r_fdi_liab_C,
    exp_nii_equity = eq_assets   * r_eq_asset_C   - eq_liab   * r_eq_liab_C,
    exp_nii_debt   = debt_assets * r_debt_asset_C - debt_liab * r_debt_liab_C,
    nfa_fdi_C_gdp    = (exp_nii_fdi    / r_disc_C) / gdp_usd * 100,
    nfa_equity_C_gdp = (exp_nii_equity / r_disc_C) / gdp_usd * 100,
    nfa_debt_C_gdp   = (exp_nii_debt   / r_disc_C) / gdp_usd * 100,
    nfa_precise_C_gdp = nfa_fdi_C_gdp + nfa_equity_C_gdp + nfa_debt_C_gdp,
    nfa_official_gdp  = nfa_official / gdp_usd * 100,
    dm_C_gdp = nfa_precise_C_gdp - nfa_official_gdp
  )


# ==============================================================================
# 2.3 — TWO 2x2 panels (Scenarios A and B) + one additional panel (Scenario C)
#
# Design for Scenarios A and B:
#   - Stacked bars (3-year smoothed): NFA by component = sum is NFA total
#   - Dark dashed line: implied NFA total (= top of stacked bars, made explicit)
#   - Solid grey line: official NIIP (EWN) — gap to NFA total = dark matter
#   - No H&S 5% line
#   - Common legend via patchwork
# ==============================================================================

make_one_panel <- function(iso_sel, panel_label, scenario = "A") {
  df <- bind_rows(nfa_decomp %>% filter(iso3c == iso_sel), eu_agg) %>%
    filter(iso3c == iso_sel, year >= 1993) %>%
    arrange(year)
  if (nrow(df) == 0) return(NULL)
  
  if (scenario == "A") {
    df <- df %>% mutate(
      c_fdi   = rollmean3(nfa_fdi_A_gdp),
      c_eq    = rollmean3(nfa_equity_A_gdp),
      c_debt  = rollmean3(nfa_debt_A_gdp),
      c_other = rollmean3(nfa_other_A_gdp),
      nfa_tot = rollmean3(nfa_precise_A_gdp),
      niip_sm = rollmean3(nfa_official_gdp)
    )
  } else {
    df <- df %>% mutate(
      c_fdi   = rollmean3(nfa_fdi_B_gdp),
      c_eq    = rollmean3(nfa_equity_B_gdp),
      c_debt  = rollmean3(nfa_debt_B_gdp),
      c_other = rollmean3(nfa_other_B_gdp),
      nfa_tot = rollmean3(nfa_precise_B_gdp),
      niip_sm = rollmean3(nfa_official_gdp)
    )
  }
  
  comp_df <- df %>%
    select(year, FDI = c_fdi, Equity = c_eq, Debt = c_debt, Other = c_other) %>%
    pivot_longer(-year, names_to = "comp", values_to = "pct") %>%
    mutate(comp = factor(comp, levels = c("Other","Debt","Equity","FDI")))
  
  refs <- df %>% select(year, niip = niip_sm, nfa_total = nfa_tot)
  
  ggplot() +
    geom_hline(yintercept = 0, colour = "grey65", linewidth = 0.35) +
    # Stacked bars: NFA by component
    geom_col(data = comp_df, aes(x = year, y = pct, fill = comp),
             position = "stack", width = 0.95, alpha = 0.75) +
    # Implied NFA total — dark dashed (= top of stacked bars, makes gap explicit)
    geom_line(data = refs,
              aes(x = year, y = nfa_total,
                  colour = "Implied NFA total",
                  linetype = "Implied NFA total"),
              linewidth = 1.2) +
    # Official NIIP — bold solid grey (gap to NFA total = dark matter)
    geom_line(data = refs,
              aes(x = year, y = niip,
                  colour = "Official NIIP (EWN)",
                  linetype = "Official NIIP (EWN)"),
              linewidth = 1.5) +
    scale_fill_manual(values = comp_palette, name = "NFA component") +
    scale_colour_manual(
      values = c("Implied NFA total"   = col_nfa_total,
                 "Official NIIP (EWN)" = "grey12"),
      name = NULL) +
    scale_linetype_manual(
      values = c("Implied NFA total"   = "dashed",
                 "Official NIIP (EWN)" = "solid"),
      name = NULL) +
    scale_x_continuous(breaks = seq(1993, y_ext_end, 6)) +
    scale_y_continuous(labels = label_number(suffix = "%")) +
    labs(title = panel_label, x = NULL, y = "% of GDP") +
    theme_paper +
    theme(plot.title      = element_text(size = 10.5, face = "bold"),
          axis.text.x     = element_text(size = 7.5),
          axis.text.y     = element_text(size = 8),
          axis.title.y    = element_text(size = 8.5),
          legend.position = "bottom",
          legend.text     = element_text(size = 8),
          plot.margin     = margin(5, 8, 5, 5))
}

make_2x2_nfa <- function(scenario = "A") {
  p_usa <- make_one_panel("USA", "United States",      scenario)
  p_jpn <- make_one_panel("JPN", "Japan",              scenario)
  p_chn <- make_one_panel("CHN", "China",              scenario)
  p_eu  <- make_one_panel("EU",  "European Union",     scenario)
  
  sub_title <- if (scenario == "A") {
    paste0(
      "Scenario A: universal G&R (2006) rates",
      " (r_FDI=8%, r_equity=6%, r_debt=3%, r_other=5%).",
      " Stacked bars: NFA by asset class (NII_j / r_j).",
      " Dark dashed: implied NFA total. Solid grey: official NIIP (EWN). Gap = dark matter.",
      " China sub-components imputed from EWN gross-stock weights when BOP coverage < 90%.",
      " % of GDP, 1993-", y_ext_end, "."
    )
  } else {
    paste0(
      "Scenario B: country-group rates (Gourinchas, Rey & Govillot 2017).",
      " Privilege (USA, CHE, GBR): r_FDI=9%, r_equity=7%, r_debt=2%.",
      " Advanced (other industrial): same as Scenario A.",
      " Emerging (China etc.): r_FDI=6%, r_equity=5%, r_debt=5%.",
      " Japan and EU unchanged (advanced group).",
      " Dark dashed: implied NFA total. Solid grey: official NIIP. Gap = dark matter.",
      " % of GDP, 1993-", y_ext_end, "."
    )
  }
  
  (p_usa | p_jpn) / (p_chn | p_eu) +
    plot_layout(guides = "collect") +
    plot_annotation(
      title    = paste0("Implied NFA by Asset Class - ",
                        if (scenario == "A") "Scenario A" else "Scenario B",
                        ": USA, Japan, China, European Union"),
      subtitle = sub_title,
      theme = theme(
        plot.title      = element_text(size = 12, face = "bold"),
        plot.subtitle   = element_text(size = 8.5, colour = "grey35"),
        legend.position = "bottom")
    ) & theme(legend.position = "bottom")
}

# Produce and save Scenarios A and B (no individual country saves)
fig_II_05 <- make_2x2_nfa("A")
save_fig(fig_II_05, "fig_II_05_nfa_decomp_scen_A", part = "part_II", w = 14, h = 11)

fig_II_06 <- make_2x2_nfa("B")
save_fig(fig_II_06, "fig_II_06_nfa_decomp_scen_B", part = "part_II", w = 14, h = 11)


# ── Scenario C panel: gross-position approach ─────────────────────────────────
#
# Bars = (assets_j * r_j^asset - liabilities_j * r_j^liab) / r_disc.
# Positive bar: country earns more on assets than it pays on liabilities
# in that class (privilege wedge). Negative bar: reverse (e.g. China on FDI).
# Only three components (FDI, Equity, Debt) — no EWN residual "Other".

make_one_panel_C <- function(iso_sel, panel_label) {
  df <- bind_rows(nfa_decomp_C %>% filter(iso3c == iso_sel), eu_agg_C) %>%
    filter(iso3c == iso_sel, year >= 1993) %>%
    arrange(year) %>%
    mutate(
      c_fdi   = rollmean3(nfa_fdi_C_gdp),
      c_eq    = rollmean3(nfa_equity_C_gdp),
      c_debt  = rollmean3(nfa_debt_C_gdp),
      nfa_tot = rollmean3(nfa_precise_C_gdp),
      niip_sm = rollmean3(nfa_official_gdp)
    )
  if (nrow(df) == 0) return(NULL)
  
  comp_df <- df %>%
    select(year, FDI = c_fdi, Equity = c_eq, Debt = c_debt) %>%
    pivot_longer(-year, names_to = "comp", values_to = "pct") %>%
    mutate(comp = factor(comp, levels = c("Debt","Equity","FDI")))
  
  refs <- df %>% select(year, niip = niip_sm, nfa_total = nfa_tot)
  
  ggplot() +
    geom_hline(yintercept = 0, colour = "grey65", linewidth = 0.35) +
    geom_col(data = comp_df, aes(x = year, y = pct, fill = comp),
             position = "stack", width = 0.95, alpha = 0.75) +
    geom_line(data = refs,
              aes(x = year, y = nfa_total,
                  colour = "Implied NFA total",
                  linetype = "Implied NFA total"),
              linewidth = 1.2) +
    geom_line(data = refs,
              aes(x = year, y = niip,
                  colour = "Official NIIP (EWN)",
                  linetype = "Official NIIP (EWN)"),
              linewidth = 1.5) +
    scale_fill_manual(values = comp_palette[c("FDI","Equity","Debt")],
                      name = "NFA component") +
    scale_colour_manual(
      values = c("Implied NFA total"   = col_nfa_total,
                 "Official NIIP (EWN)" = "grey12"),
      name = NULL) +
    scale_linetype_manual(
      values = c("Implied NFA total"   = "dashed",
                 "Official NIIP (EWN)" = "solid"),
      name = NULL) +
    scale_x_continuous(breaks = seq(1993, y_ext_end, 6)) +
    scale_y_continuous(labels = label_number(suffix = "%")) +
    labs(title = panel_label, x = NULL, y = "% of GDP") +
    theme_paper +
    theme(plot.title      = element_text(size = 10.5, face = "bold"),
          axis.text.x     = element_text(size = 7.5),
          axis.text.y     = element_text(size = 8),
          axis.title.y    = element_text(size = 8.5),
          legend.position = "bottom",
          legend.text     = element_text(size = 8),
          plot.margin     = margin(5, 8, 5, 5))
}

fig_II_07 <- (make_one_panel_C("USA","United States") |
                make_one_panel_C("JPN","Japan")) /
  (make_one_panel_C("CHN","China") |
     make_one_panel_C("EU", "European Union")) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title    = "Implied NFA by Asset Class - Scenario C: Gross Position Approach",
    subtitle = paste0(
      "Scenario C: separate return rates for assets and liabilities (Gourinchas & Rey 2006;",
      " Curcuru, Dvorak & Warnock 2008 QJE).",
      " Bars: (assets_j x r_j^asset - liabilities_j x r_j^liab) / 0.05.",
      " Privilege (USA, CHE, GBR): r_FDI^asset=9.5%, r_FDI^liab=4% (wedge = +5.5pp).",
      " Emerging (China): r_FDI^asset=6%, r_FDI^liab=7% (negative wedge = -1pp).",
      " Dark dashed: implied NFA total. Solid grey: official NIIP. Gap = dark matter.",
      " % of GDP, 1993-", y_ext_end, "."),
    theme = theme(
      plot.title      = element_text(size = 12, face = "bold"),
      plot.subtitle   = element_text(size = 8.5, colour = "grey35"),
      legend.position = "bottom")
  ) & theme(legend.position = "bottom")

save_fig(fig_II_07, "fig_II_07_nfa_decomp_scen_C", part = "part_II", w = 14, h = 11)


# ==============================================================================
# 2.4 — Robustness table: NIIP vs NFA_A vs NFA_B vs NFA_C
# ==============================================================================

tdir        <- here("code","output","tables","part_II")
summ_window <- (y_ext_end - 9):y_ext_end

# Scenario C averages for showcase countries and EU
dm_C_avg <- bind_rows(
  nfa_decomp_C %>% filter(iso3c %in% showcase),
  eu_agg_C
) %>%
  filter(year %in% summ_window) %>%
  group_by(iso3c) %>%
  summarise(
    nfa_C = round(mean(nfa_precise_C_gdp, na.rm=TRUE), 1),
    dm_C  = round(mean(dm_C_gdp,          na.rm=TRUE), 1),
    .groups = "drop"
  )

summ_raw <- bind_rows(
  nfa_decomp %>% filter(iso3c %in% showcase), eu_agg
) %>%
  filter(year %in% summ_window) %>%
  group_by(iso3c) %>%
  summarise(
    label   = panel_labels[iso3c[1]],
    grp     = safe_first(grp),
    niip    = round(mean(nfa_official_gdp,  na.rm=TRUE), 1),
    nfa_A   = round(mean(nfa_precise_A_gdp, na.rm=TRUE), 1),
    nfa_B   = round(mean(nfa_precise_B_gdp, na.rm=TRUE), 1),
    dm_A    = round(mean(dm_A_gdp,          na.rm=TRUE), 1),
    dm_B    = round(mean(dm_B_gdp,          na.rm=TRUE), 1),
    diff_BA = round(mean(dm_B_gdp - dm_A_gdp, na.rm=TRUE), 1),
    .groups = "drop"
  ) %>%
  left_join(dm_C_avg, by = "iso3c")

tex_summ <- kableExtra::kbl(
  summ_raw %>% select(label, grp, niip, nfa_A, nfa_B, nfa_C, dm_A, dm_B, dm_C),
  format = "latex", booktabs = TRUE, linesep = "",
  col.names = c("Entity","Group","NIIP","NFA$_A$","NFA$_B$","NFA$_C$",
                "DM$_A$","DM$_B$","DM$_C$"),
  caption = paste0("Implied NFA and dark matter under three scenarios, avg. ",
                   min(summ_window),"--",max(summ_window)," (\\% of GDP)"),
  label = "tab:summ_scen", escape = FALSE,
  align = c("l","l","r","r","r","r","r","r","r")
) %>%
  kableExtra::kable_styling(latex_options="hold_position", font_size=10.5) %>%
  kableExtra::add_header_above(
    c(" "=2,"Implied NFA measures"=4,"Dark matter"=3),
    bold=TRUE, line=TRUE, escape=FALSE) %>%
  kableExtra::footnote(
    general = paste0(
      "Scenario A: universal G\\\\&R (2006) rates. ",
      "Scenario B: GRG (2017). ",
      "Privilege (USA, CHE, GBR): r$_{\\\\text{FDI}}$=9\\\\%, r$_{\\\\text{debt}}$=2\\\\%. ",
      "Emerging (CHN): r$_{\\\\text{FDI}}$=6\\\\%, r$_{\\\\text{debt}}$=5\\\\%. ",
      "Scenario C: gross-position approach (Gourinchas \\\\& Rey 2006; ",
      "Curcuru, Dvorak \\\\& Warnock 2008). ",
      "Privilege FDI wedge: +5.5pp (earn 9.5\\\\%, pay 4\\\\%). ",
      "China FDI wedge: $-$1pp (earn 6\\\\%, pay 7\\\\%). ",
      "DM = implied NFA minus official NIIP (EWN). ",
      "EU: advanced group, Scenarios A, B and C use same rates."),
    general_title="\\\\textit{Notes:} ", escape=FALSE)

writeLines(paste0(
  "\\documentclass[11pt]{article}\n",
  "\\usepackage{booktabs,xcolor,colortbl,caption,array,graphicx,amsmath}\n",
  "\\usepackage[landscape,margin=1.2cm]{geometry}\n",
  "\\begin{document}\\small\\setcounter{table}{6}\n",
  tex_summ, "\n\\end{document}"),
  file.path(tdir, "tab_II_T07_scenarios_comparison.tex"))
old <- setwd(tdir)
tryCatch(tinytex::pdflatex("tab_II_T07_scenarios_comparison.tex"),
         error = function(e) message("LaTeX: ", e$message))
setwd(old)

message("Section 2 done.")

# ==============================================================================
#
# Part II — Section 2 additional figure
# US vs China: dark matter stock and portfolio structure
#
# This block responds to the supervisor's suggestion to explicitly compare
# the United States and China. The US is the canonical dark-matter exporter:
# its official NIIP deteriorates, but its income-implied position remains strong.
# China is the mirror case: official NIIP improves, but income generation is weak.
#
# ==============================================================================

message("\n── Part II / Additional Figure: US vs China dark matter ────────────────")

# ── 1. Build US-China dark matter stock series ────────────────────────────────

us_china_dm <- nfa_decomp %>%
  filter(iso3c %in% c("USA", "CHN"), year >= 1993, year <= y_ext_end) %>%
  mutate(
    country = recode(
      iso3c,
      "USA" = "United States",
      "CHN" = "China"
    )
  ) %>%
  select(
    iso3c, country, year,
    dm_A_gdp, dm_B_gdp,
    nfa_official_gdp,
    nfa_precise_A_gdp,
    nfa_precise_B_gdp
  ) %>%
  pivot_longer(
    c(dm_A_gdp, dm_B_gdp),
    names_to = "measure",
    values_to = "dm_gdp"
  ) %>%
  mutate(
    measure = recode(
      measure,
      "dm_A_gdp" = "Scenario A",
      "dm_B_gdp" = "Scenario B"
    )
  )

fig_II_S2_us_china_dm <- ggplot(
  us_china_dm,
  aes(x = year, y = dm_gdp, colour = measure, linetype = measure)
) +
  geom_hline(yintercept = 0, colour = col_grey, linewidth = 0.4) +
  geom_line(linewidth = 1.1, alpha = 0.95) +
  facet_wrap(~ country, scales = "free_y", ncol = 1) +
  scale_colour_manual(
    values = c(
      "Scenario A" = col_blue,
      "Scenario B" = col_red
    ),
    name = NULL
  ) +
  scale_linetype_manual(
    values = c(
      "Scenario A" = "solid",
      "Scenario B" = "dashed"
    ),
    name = NULL
  ) +
  scale_x_continuous(breaks = seq(1993, y_ext_end, 5)) +
  scale_y_continuous(labels = label_number(suffix = "%")) +
  labs(
    title = "US vs China — Dark Matter Stock under Component-Specific Methods",
    subtitle = paste0(
      "Dark matter = income-implied NFA minus official NIIP. ",
      "Scenario A uses universal asset-class rates; Scenario B uses group-specific rates. ",
      "Percent of GDP, 1993-", y_ext_end, "."
    ),
    x = NULL,
    y = "Dark matter stock (% of GDP)"
  ) +
  theme_paper +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold")
  )

save_fig(
  fig_II_S2_us_china_dm,
  "fig_II_S2_us_china_dark_matter_stock",
  part = "part_II",
  w = 9,
  h = 7
)


# ── 2. Gross external portfolio structure: US vs China ────────────────────────

required_ewn_cols <- c(
  "iso3c", "year", "gdp_usd",
  "fdi_assets", "fdi_liab",
  "eq_assets", "eq_liab",
  "debt_assets", "debt_liab"
)

missing_ewn_cols <- setdiff(required_ewn_cols, names(EWN_ext))

if (length(missing_ewn_cols) > 0) {
  stop(
    "EWN_ext is missing required columns for the US-China portfolio figure: ",
    paste(missing_ewn_cols, collapse = ", ")
  )
}

us_china_gross_portfolio <- EWN_ext %>%
  filter(iso3c %in% c("USA", "CHN"), year >= 1993, year <= y_ext_end) %>%
  mutate(
    country = recode(
      iso3c,
      "USA" = "United States",
      "CHN" = "China"
    ),
    
    fdi_assets_gdp    = fdi_assets  / gdp_usd * 100,
    equity_assets_gdp = eq_assets   / gdp_usd * 100,
    debt_assets_gdp   = debt_assets / gdp_usd * 100,
    
    fdi_liab_gdp      = fdi_liab  / gdp_usd * 100,
    equity_liab_gdp   = eq_liab   / gdp_usd * 100,
    debt_liab_gdp     = debt_liab / gdp_usd * 100
  ) %>%
  select(
    country, year,
    fdi_assets_gdp, equity_assets_gdp, debt_assets_gdp,
    fdi_liab_gdp, equity_liab_gdp, debt_liab_gdp
  ) %>%
  pivot_longer(
    -c(country, year),
    names_to = "component",
    values_to = "value"
  ) %>%
  mutate(
    side = if_else(str_detect(component, "_assets_"), "External assets", "External liabilities"),
    asset_class = case_when(
      str_detect(component, "fdi")    ~ "FDI",
      str_detect(component, "equity") ~ "Portfolio equity",
      str_detect(component, "debt")   ~ "Debt",
      TRUE                            ~ "Other"
    ),
    asset_class = factor(asset_class, levels = c("FDI", "Portfolio equity", "Debt"))
  )

fig_II_S2_us_china_portfolio <- ggplot(
  us_china_gross_portfolio,
  aes(x = year, y = value, fill = asset_class)
) +
  geom_col(width = 0.85, alpha = 0.85) +
  facet_grid(country ~ side, scales = "free_y") +
  scale_fill_manual(
    values = c(
      "FDI" = col_blue,
      "Portfolio equity" = col_red,
      "Debt" = col_green
    ),
    name = NULL
  ) +
  scale_x_continuous(breaks = seq(1993, y_ext_end, 6)) +
  scale_y_continuous(labels = label_number(suffix = "%")) +
  labs(
    title = "US vs China — Gross External Portfolio Structure",
    subtitle = paste0(
      "Gross external assets and liabilities by asset class. ",
      "Percent of GDP, 1993-", y_ext_end, "."
    ),
    x = NULL,
    y = "% of GDP"
  ) +
  theme_paper +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold")
  )

save_fig(
  fig_II_S2_us_china_portfolio,
  "fig_II_S2_us_china_gross_portfolio_structure",
  part = "part_II",
  w = 12,
  h = 7.5
)

message("  US-China additional figures done.")




# ==============================================================================
# ==============================================================================
#
#   SECTION 3 — What Drives Dark Matter? Three Channels
#
# ==============================================================================
# ==============================================================================
#
# The dark matter is the gap between the precise NFA (NII capitalised at
# component-specific rates) and the official NIIP (EWN). Three channels explain
# where this gap comes from:
#
#   Canal 1 — IDE / Knowledge capital (H&S 2006; Keller & Yeaple 2013)
#     MNCs transfer intangible blueprints through FDI — invisible in BOP
#     but generating returns well above book value of recorded FDI stocks.
#     Proxy: DM-FDI component = NII_FDI/r_FDI - net_FDI_EWN.
#
#   Canal 2 — Assurance (H&S; Caballero, Farhi & Gourinchas 2017)
#     Stable economies sell insurance to volatile ones: investors accept lower
#     returns on stable-country assets. The risk premium → dark matter.
#     Proxy: output volatility (HP-cycle SD). Expected sign: negative.
#
#   Canal 3 — Actif sûr / Privilege exorbitant (Gourinchas & Rey 2007;
#               He, Krishnamurthy & Milbradt 2019)
#     Reserve-currency issuers borrow at below-equilibrium rates because
#     their liabilities carry a liquidity premium.
#     Proxy: privilege group dummy + financial depth index.
#
# ==============================================================================

# ── Build cross-section DM dataset for channels 2 & 3 ────────────────────────
# Average annual dark matter (Scenario A) over the component-data window.

dm_cs <- nfa_decomp %>%
  filter(iso3c %in% countries_79, year >= 1993) %>%
  group_by(iso3c) %>%
  summarise(
    dm_A_avg        = mean(dm_A_gdp,        na.rm=TRUE),
    dm_B_avg        = mean(dm_B_gdp,        na.rm=TRUE),
    dm_fdi_A_avg    = mean(dm_fdi_A_gdp,    na.rm=TRUE),
    dm_equity_A_avg = mean(dm_equity_A_gdp, na.rm=TRUE),
    dm_debt_A_avg   = mean(dm_debt_A_gdp,   na.rm=TRUE),
    .groups = "drop"
  ) %>%
  left_join(cs_ext %>% select(iso3c, fdi_net_gdp, output_vol_hp,
                              rule_of_law, opec, hipc, grp),
            by = "iso3c") %>%
  mutate(privilege_d = as.integer(iso3c %in% privilege_countries))

# Financial depth from cached WDI file (if Part III has been run before)

findev_path <- here("code", "data", "wdi_findev.csv")

if (file.exists(findev_path)) {
  
  findev_cs <- read_csv(findev_path, show_col_types = FALSE) %>%
    filter(!is.na(iso3c), year >= 1993) %>%
    mutate(
      credit_log = log1p(coalesce(private_credit, 0)),
      mktcap_log = log1p(coalesce(stock_mktcap,  0))
    ) %>%
    ungroup() %>%
    mutate(
      z_credit = as.numeric(scale(credit_log)),
      z_mktcap = as.numeric(scale(mktcap_log)),
      findev_idx_annual = rowMeans(cbind(z_credit, z_mktcap), na.rm = TRUE)
    ) %>%
    group_by(iso3c) %>%
    summarise(
      findev_idx = mean(findev_idx_annual, na.rm = TRUE),
      .groups = "drop"
    )
  
  dm_cs <- dm_cs %>% left_join(findev_cs, by = "iso3c")
  has_findev <- TRUE
  
} else {
  
  dm_cs <- dm_cs %>% mutate(findev_idx = NA_real_)
  has_findev <- FALSE
  message("wdi_findev.csv not found — findev channel omitted from regression.")
  
}


# ==============================================================================
# 3.1 — fig_II_07: 2×2 DM decomposition by asset class (Scenario A)
#
# Shows WHAT the dark matter is made of: which asset class drives the
# discrepancy between the implied NFA and the official NIIP.
# Lines overlay total DM under both scenarios to show scenario sensitivity.
# ==============================================================================

make_dm_panel <- function(iso_sel, panel_label) {
  df <- bind_rows(nfa_decomp %>% filter(iso3c==iso_sel), eu_agg) %>%
    filter(iso3c==iso_sel, year>=1993, !is.na(dm_A_gdp)) %>%
    arrange(year) %>%
    mutate(
      c_fdi   = rollmean3(dm_fdi_A_gdp),
      c_eq    = rollmean3(dm_equity_A_gdp),
      c_debt  = rollmean3(dm_debt_A_gdp),
      c_other = rollmean3(dm_other_A_gdp),
      dm_A_sm = rollmean3(dm_A_gdp),
      dm_B_sm = rollmean3(dm_B_gdp)
    )
  if (nrow(df)==0) return(NULL)
  
  comp_df <- df %>%
    select(year, FDI=c_fdi, Equity=c_eq, Debt=c_debt, Other=c_other) %>%
    pivot_longer(-year, names_to="comp", values_to="pct") %>%
    mutate(comp=factor(comp, levels=c("Other","Debt","Equity","FDI")))
  totals <- df %>% select(year, dm_A=dm_A_sm, dm_B=dm_B_sm)
  
  ggplot() +
    geom_hline(yintercept=0, colour="grey50", linewidth=0.4) +
    geom_col(data=comp_df, aes(x=year, y=pct, fill=comp),
             position="stack", width=0.95, alpha=0.75) +
    geom_line(data=totals, aes(x=year, y=dm_A,
                               colour="Total Scén. A", linetype="Total Scén. A"), linewidth=1.1) +
    geom_line(data=totals, aes(x=year, y=dm_B,
                               colour="Total Scén. B", linetype="Total Scén. B"), linewidth=0.9) +
    scale_fill_manual(values=comp_palette, name="Composante DM") +
    scale_colour_manual(
      values=c("Total Scén. A"="grey12","Total Scén. B"="#2171B5"), name=NULL) +
    scale_linetype_manual(
      values=c("Total Scén. A"="solid","Total Scén. B"="longdash"), name=NULL) +
    scale_x_continuous(breaks=seq(1993,y_ext_end,6)) +
    scale_y_continuous(labels=label_number(suffix="%")) +
    labs(title=panel_label, x=NULL, y="Dark matter (% du PIB)") +
    theme_paper +
    theme(plot.title=element_text(size=10.5,face="bold"),
          axis.text.x=element_text(size=7.5), axis.text.y=element_text(size=8),
          legend.position="bottom", plot.margin=margin(5,8,5,5))
}

dm_usa <- make_dm_panel("USA","États-Unis")
dm_jpn <- make_dm_panel("JPN","Japon")
dm_chn <- make_dm_panel("CHN","Chine")
dm_eu  <- make_dm_panel("EU", "Union européenne")

fig_II_07 <- (dm_usa | dm_jpn) / (dm_chn | dm_eu) +
  plot_layout(guides="collect") +
  plot_annotation(
    title    = "Décomposition de la dark matter par classe d'actif (Scénario A)",
    subtitle = paste0(
      "Aires = composantes DM (NFA implicite - stock officiel EWN). ",
      "Ligne grise = total Scén. A. Ligne bleue tiretée = total Scén. B. ",
      "% du PIB, 1993\u2013", y_ext_end, "."),
    theme=theme(plot.title=element_text(size=12,face="bold"),
                plot.subtitle=element_text(size=8.5,colour="grey35"),
                legend.position="bottom")
  ) & theme(legend.position="bottom")

save_fig(fig_II_07, "fig_II_07_dm_decomp_by_asset_class", part="part_II", w=14, h=11)


# ==============================================================================
# 3.2 — fig_II_08: Canal IDE / Knowledge capital — time series
#
# Shows the FDI component of dark matter for USA, Japan and EU over time.
# According to H&S and Keller & Yeaple (2013), the FDI channel should
# dominate for large capital exporters (USA, JPN). A persistent positive
# DM_FDI confirms that recorded FDI stocks understate the true stock of
# knowledge embedded in foreign affiliates.
# ==============================================================================

fdi_channel_ts <- bind_rows(
  nfa_decomp %>% filter(iso3c %in% c("USA","JPN")),
  eu_agg
) %>%
  filter(year >= 1993, !is.na(dm_fdi_A_gdp)) %>%
  mutate(country = recode(iso3c, USA="États-Unis", JPN="Japon", EU="Union européenne")) %>%
  arrange(iso3c, year) %>%
  group_by(iso3c) %>%
  mutate(
    dm_fdi_sm    = rollmean3(dm_fdi_A_gdp),
    dm_equity_sm = rollmean3(dm_equity_A_gdp),
    dm_debt_sm   = rollmean3(dm_debt_A_gdp),
    dm_A_sm      = rollmean3(dm_A_gdp)
  ) %>%
  ungroup()

fdi_long <- fdi_channel_ts %>%
  select(year, country, dm_fdi_sm, dm_equity_sm, dm_debt_sm, dm_A_sm) %>%
  pivot_longer(c(dm_fdi_sm,dm_equity_sm,dm_debt_sm,dm_A_sm),
               names_to="series", values_to="pct") %>%
  mutate(series = recode(series,
                         "dm_A_sm"      = "Dark matter total",
                         "dm_fdi_sm"    = "Composante IDE",
                         "dm_equity_sm" = "Composante actions",
                         "dm_debt_sm"   = "Composante dette"))

fig_II_08 <- ggplot(fdi_long, aes(x=year, y=pct, colour=series, linetype=series)) +
  geom_hline(yintercept=0, colour=col_grey, linewidth=0.35) +
  geom_line(linewidth=0.9, na.rm=TRUE) +
  facet_wrap(~country, ncol=3, scales="free_y") +
  scale_colour_manual(
    values=c("Dark matter total" ="grey12",
             "Composante IDE"    =comp_palette["FDI"],
             "Composante actions"=comp_palette["Equity"],
             "Composante dette"  =comp_palette["Debt"]),
    name=NULL) +
  scale_linetype_manual(
    values=c("Dark matter total" ="solid",
             "Composante IDE"    ="solid",
             "Composante actions"="longdash",
             "Composante dette"  ="dashed"),
    name=NULL) +
  scale_x_continuous(breaks=seq(1993,y_ext_end,6)) +
  scale_y_continuous(labels=label_number(suffix="%")) +
  labs(title    = "Canal IDE — Composantes de la dark matter (% du PIB, Scénario A)",
       subtitle = paste0(
         "DM\\textsubscript{IDE} = NFA implicite IDE / r\\textsubscript{IDE} \\minus stock net officiel FDI (EWN). ",
         "Une DM IDE positive et persistante valide le canal savoir-faire (H\\&S 2006; Keller \\& Yeaple 2013)."),
       x=NULL, y="% du PIB") +
  theme_paper +
  theme(strip.text=element_text(face="bold",size=10),
        legend.position="bottom")

save_fig(fig_II_08, "fig_II_08_channel_fdi_knowledge_timeseries", part="part_II", w=13, h=5.5)


# ==============================================================================
# 3.3 — fig_II_09: Canal assurance — DM vs volatilité de l'output
#
# Cross-section scatter: average dark matter (Scen. A) vs HP-filtered output
# volatility. The insurance channel predicts a negative slope: stable economies
# export dark matter (earn risk premium on their safe assets held by volatile
# economies). Points coloured by group (privilege / advanced / emerging).
# ==============================================================================

fig_II_09 <- dm_cs %>%
  filter(!is.na(dm_A_avg), !is.na(output_vol_hp),
         abs(dm_A_avg) < quantile(abs(dm_A_avg),0.95,na.rm=TRUE)) %>%
  mutate(grp_label = recode(grp,
                            "privilege"="Privilège","advanced"="Avancé","emerging"="Émergent")) %>%
  ggplot(aes(x=output_vol_hp, y=dm_A_avg, label=iso3c)) +
  geom_hline(yintercept=0, colour=col_grey, linewidth=0.4) +
  geom_smooth(method="lm", se=TRUE, colour=col_red,
              linewidth=0.9, fill=col_red, alpha=0.08) +
  geom_point(aes(colour=grp_label), size=2.3, alpha=0.82) +
  geom_text_repel(aes(colour=grp_label), size=2.3,
                  segment.colour="grey70", segment.size=0.3,
                  box.padding=0.3, max.overlaps=28, seed=7) +
  scale_colour_manual(
    values=c("Privilège"="#2171B5","Avancé"="#D94801","Émergent"="grey55"),
    name="Groupe") +
  scale_x_continuous(labels=label_number(suffix="%")) +
  scale_y_continuous(labels=label_number(suffix="%")) +
  labs(title    = "Canal assurance — Dark matter vs. volatilité de l'output (1993\u2013présent)",
       subtitle = paste0(
         "Coupe transversale, 79 pays. Pente négative attendue: économies stables exportent de la dark matter. ",
         "Volatilité = écart-type du cycle HP (\\lambda=100). ",
         "Source: Caballero, Farhi \\& Gourinchas (2017)."),
       x="Volatilité de l'output (écart-type cycle HP, %)",
       y="Dark matter moyenne (% du PIB, Scén. A)") +
  theme_paper + theme(legend.position="bottom")

save_fig(fig_II_09, "fig_II_09_channel_insurance_scatter", part="part_II", w=9.5, h=6.5)


# ==============================================================================
# 3.4 — fig_II_10: Canal actif sûr — privilege vs autres pays avancés
#
# Time series comparison: privilege countries (USA, GBR, CHE) vs non-privilege
# advanced (JPN, DEU, FRA). The safe-asset / exorbitant privilege hypothesis
# (Gourinchas & Rey 2007) predicts a persistent positive gap for the privilege
# group. Thin lines = individual countries; thick lines = group averages.
# ==============================================================================

privilege_ts <- nfa_decomp %>%
  filter(iso3c %in% c("USA","GBR","CHE","JPN","DEU","FRA"),
         year >= 1993, !is.na(dm_A_gdp)) %>%
  arrange(iso3c, year) %>%
  group_by(iso3c) %>%
  mutate(dm_sm = rollmean3(dm_A_gdp)) %>%
  ungroup() %>%
  mutate(
    privilege_grp = if_else(iso3c %in% privilege_countries,
                            "Privilège (USA, GBR, CHE)",
                            "Avancé non-privilège (JPN, DEU, FRA)"),
    country = recode(iso3c,
                     USA="États-Unis", GBR="Royaume-Uni", CHE="Suisse",
                     JPN="Japon",      DEU="Allemagne",   FRA="France")
  )

priv_avg <- privilege_ts %>%
  group_by(year, privilege_grp) %>%
  summarise(dm_grp = mean(dm_sm, na.rm=TRUE), .groups="drop")

fig_II_10 <- ggplot() +
  geom_hline(yintercept=0, colour=col_grey, linewidth=0.4) +
  geom_line(data=privilege_ts,
            aes(x=year, y=dm_sm, group=country, colour=privilege_grp),
            linewidth=0.55, alpha=0.35, na.rm=TRUE) +
  geom_line(data=priv_avg,
            aes(x=year, y=dm_grp, colour=privilege_grp),
            linewidth=1.6, na.rm=TRUE) +
  scale_colour_manual(
    values=c("Privilège (USA, GBR, CHE)"            = "#2171B5",
             "Avancé non-privilège (JPN, DEU, FRA)"  = "#D94801"),
    name=NULL) +
  scale_x_continuous(breaks=seq(1993,y_ext_end,4)) +
  scale_y_continuous(labels=label_number(suffix="%")) +
  labs(title    = "Canal actif sûr — Pays à privilege vs. autres pays avancés",
       subtitle = paste0(
         "Lignes fines = pays individuels; lignes épaisses = moyenne du groupe. ",
         "Un écart positif persistant du groupe \\textit{privilege} confirme l'exorbitant privilege ",
         "(Gourinchas \\& Rey 2007; He, Krishnamurthy \\& Milbradt 2019). ",
         "% du PIB, Scén. A."),
       x=NULL, y="Dark matter (% du PIB, Scén. A)") +
  theme_paper + theme(legend.position="bottom")

save_fig(fig_II_10, "fig_II_10_channel_safe_asset_privilege", part="part_II", w=10, h=5.5)


# ==============================================================================
# 3.5 — tab_II_T08: Panel regression — all three channels simultaneously
#
# Cross-section OLS: average dark matter (Scén. A, 1993–y_ext_end) on proxies
# for the three channels. Controls: rule of law, OPEC dummy, HIPC dummy.
# Column (5) repeats the full spec with Scén. B to check robustness.
# ==============================================================================

d_reg <- dm_cs %>%
  filter(!is.na(dm_A_avg)) %>%
  mutate(across(c(dm_A_avg, dm_B_avg, fdi_net_gdp,
                  output_vol_hp, rule_of_law), winsor))

spec_list <- list(
  "(1) Baseline"   = lm(dm_A_avg ~ fdi_net_gdp + output_vol_hp, data=d_reg),
  "(2) + Gouv."    = lm(dm_A_avg ~ fdi_net_gdp + output_vol_hp +
                          rule_of_law, data=d_reg),
  "(3) + Privilege" = lm(dm_A_avg ~ fdi_net_gdp + output_vol_hp +
                           rule_of_law + privilege_d, data=d_reg),
  "(4) + Contrôles" = lm(dm_A_avg ~ fdi_net_gdp + output_vol_hp +
                           rule_of_law + privilege_d + opec + hipc, data=d_reg),
  "(5) Scén. B"    = lm(dm_B_avg ~ fdi_net_gdp + output_vol_hp +
                          rule_of_law + privilege_d + opec + hipc, data=d_reg)
)

if (has_findev && sum(!is.na(d_reg$findev_idx)) > 20) {
  spec_list[["(3) + Dev. fin."]] <- lm(dm_A_avg ~ fdi_net_gdp + output_vol_hp +
                                         rule_of_law + findev_idx, data=filter(d_reg, !is.na(findev_idx)))
}

message(sprintf("Channel regression obs: %s",
                paste(sapply(spec_list,nobs),collapse="|")))

cn_reg <- c("fdi_net_gdp"   = "IDE net / PIB \\small{(canal savoir-faire)}",
            "output_vol_hp" = "Volatilité output \\small{(canal assurance)}",
            "rule_of_law"   = "Règle de droit",
            "privilege_d"   = "Privilège \\small{(USA/GBR/CHE)}",
            "findev_idx"    = "Dév. financier \\small{(canal actif sûr)}",
            "opec"          = "Dummy OPEC",
            "hipc"          = "Dummy HIPC")

note_reg <- paste0(
  "VD: dark matter moyenne (\\% du PIB) sur 1993--", y_ext_end, ". ",
  "Scén. A pour colonnes (1)--(4), Scén. B pour colonne (5). ",
  "Canal savoir-faire: IDE net / PIB, signe attendu positif. ",
  "Canal assurance: volatilité HP, signe attendu négatif. ",
  "Canal privilege: dummy USA/GBR/CHE, signe attendu positif. ",
  "Variables winsorisées à 1\\%. * p$<$0.10, ** p$<$0.05, *** p$<$0.01.")

compile_table(wrap_ext(
  modelsummary(spec_list, stars=c("*"=0.1,"**"=0.05,"***"=0.01), fmt="%.3f",
               coef_rename=cn_reg, gof_omit="AIC|BIC|Log|RMSE", output="latex_tabular"),
  paste0("Canaux explicatifs de la dark matter — Coupe transversale 79 pays (",
         "1993--",y_ext_end,")"),
  "tab:channels", note_reg, "S3"),
  "tab_II_T08_channels_regression",
  part="part_II", table_number=8, fit_width=TRUE)

message("Section 3 done.")
message("\nPart II complete.")
message("Figures : code/output/figures/part_II/")
message("Tables  : code/output/tables/part_II/")

# ==============================================================================
# OUTPUT SUMMARY
# ──────────────
# SECTION 1
#   fig_II_01_us_dm_stock               US dark matter stock 1982–2022
#   fig_II_02_global_nfa_regions        Global NFA by region 1985–2022
#   fig_II_03_scatter_cumCA_vs_DM       Cross-section scatter (extended)
#   fig_II_04_scatter_DM_exports_vs_CA  DM exports vs official CA (extended)
#   tab_II_T01_E1_cumCA_vs_DM
#   tab_II_T02_E2_DM_exports
#   tab_II_T03_E3a_sources_79
#   tab_II_T04_E3b_sources_99
#   tab_II_T05_E4_panel_CADM
#   tab_II_T06_E4b_panel_DMexports
#
# SECTION 2
#   fig_II_05_nfa_decomp_scen_A    2×2 NFA decomposition, Scenario A
#   fig_II_06_nfa_decomp_scen_B    2×2 NFA decomposition, Scenario B
#   tab_II_T07_scenarios_comparison
#
# SECTION 3
#   fig_II_07_dm_decomp_by_asset_class   2×2 DM by component
#   fig_II_08_channel_fdi_knowledge_timeseries
#   fig_II_09_channel_insurance_scatter
#   fig_II_10_channel_safe_asset_privilege
#   tab_II_T08_channels_regression
# ==============================================================================

# ==============================================================================
# ==============================================================================
#
#   SECTION 4 — Gross Portfolio Components and Dark Matter
#
# ==============================================================================
# ==============================================================================
#
# Motivation:
# The supervisor suggested that we should not only regress net dark matter on
# aggregate variables. We should also look at gross portfolio components:
# FDI assets/liabilities, equity assets/liabilities, and debt assets/liabilities.
#
# This tests whether dark matter is driven by:
#   - the quantity of risky foreign assets held abroad;
#   - the quantity of safe liabilities issued to foreigners;
#   - the asset-class composition of the external balance sheet.
#
# ==============================================================================

message("\n── Part II / Section 4: Gross portfolio components ─────────────────────")

# Safe-haven group used in Part II gross-component regressions
if (!exists("safe_havens")) {
  safe_havens <- c("USA", "CHE", "DEU", "GBR", "JPN", "NLD", "AUT", "DNK", "NOR")
}

required_gross_cols <- c(
  "iso3c", "year", "gdp_usd",
  "fdi_assets", "fdi_liab",
  "eq_assets", "eq_liab",
  "debt_assets", "debt_liab"
)

missing_gross_cols <- setdiff(required_gross_cols, names(EWN_ext))

if (length(missing_gross_cols) > 0) {
  stop(
    "EWN_ext is missing required columns for gross-component regressions: ",
    paste(missing_gross_cols, collapse = ", ")
  )
}

# ── Build annual gross-component panel ────────────────────────────────────────

gross_components_panel <- EWN_ext %>%
  filter(year >= 1993, year <= y_ext_end) %>%
  transmute(
    iso3c, year, gdp_usd,
    
    fdi_assets_gdp_gross    = fdi_assets  / gdp_usd * 100,
    fdi_liab_gdp_gross      = fdi_liab    / gdp_usd * 100,
    
    equity_assets_gdp_gross = eq_assets   / gdp_usd * 100,
    equity_liab_gdp_gross   = eq_liab     / gdp_usd * 100,
    
    debt_assets_gdp_gross   = debt_assets / gdp_usd * 100,
    debt_liab_gdp_gross     = debt_liab   / gdp_usd * 100,
    
    total_gross_assets_gdp = (
      coalesce(fdi_assets, 0) +
        coalesce(eq_assets, 0) +
        coalesce(debt_assets, 0)
    ) / gdp_usd * 100,
    
    total_gross_liab_gdp = (
      coalesce(fdi_liab, 0) +
        coalesce(eq_liab, 0) +
        coalesce(debt_liab, 0)
    ) / gdp_usd * 100
  ) %>%
  mutate(
    gross_balance_sheet_gdp = total_gross_assets_gdp + total_gross_liab_gdp,
    debt_liab_share = debt_liab_gdp_gross / total_gross_liab_gdp,
    equity_liab_share = equity_liab_gdp_gross / total_gross_liab_gdp,
    fdi_liab_share = fdi_liab_gdp_gross / total_gross_liab_gdp
  )

# ── Merge with H&S dark matter flows and component-specific dark matter ───────

gross_dm_panel <- panel_ext %>%
  filter(year >= 1993, year <= y_ext_end) %>%
  select(
    iso3c, year,
    dm_exp_flow_gdp,
    fdi_assets_gdp, fdi_liab_gdp,
    opec_d, hipc_d
  ) %>%
  left_join(
    nfa_decomp %>%
      select(
        iso3c, year,
        dm_A_gdp, dm_B_gdp,
        nfa_official_gdp,
        nfa_precise_A_gdp,
        nfa_precise_B_gdp
      ),
    by = c("iso3c", "year")
  ) %>%
  left_join(gross_components_panel, by = c("iso3c", "year")) %>%
  mutate(
    safe_haven = as.integer(iso3c %in% safe_havens),
    industrial_d = as.integer(iso3c %in% industrial)
  ) %>%
  filter(
    iso3c %in% countries_79,
    !is.na(dm_exp_flow_gdp),
    !is.na(fdi_assets_gdp_gross),
    !is.na(fdi_liab_gdp_gross),
    !is.na(equity_assets_gdp_gross),
    !is.na(equity_liab_gdp_gross),
    !is.na(debt_assets_gdp_gross),
    !is.na(debt_liab_gdp_gross)
  )

message(sprintf(
  "  Gross-component panel: %d obs, %d countries",
  nrow(gross_dm_panel), n_distinct(gross_dm_panel$iso3c)
))

# ── Regressions: dark matter flows on gross portfolio components ──────────────
#
# Column (1): H&S 5% dark matter exports.
# Column (2): same with country FE.
# Column (3): same with country and year FE.
# Column (4): component-specific Scenario A dark matter stock.
# Column (5): component-specific Scenario B dark matter stock.

gross_component_models <- list(
  "(1) H&S flow" = fixest::feols(
    dm_exp_flow_gdp ~
      fdi_assets_gdp_gross + fdi_liab_gdp_gross +
      equity_assets_gdp_gross + equity_liab_gdp_gross +
      debt_assets_gdp_gross + debt_liab_gdp_gross,
    data = gross_dm_panel,
    vcov = "hetero"
  ),
  
  "(2) H&S flow FE" = fixest::feols(
    dm_exp_flow_gdp ~
      fdi_assets_gdp_gross + fdi_liab_gdp_gross +
      equity_assets_gdp_gross + equity_liab_gdp_gross +
      debt_assets_gdp_gross + debt_liab_gdp_gross | iso3c,
    data = gross_dm_panel,
    vcov = ~iso3c
  ),
  
  "(3) H&S flow TWFE" = fixest::feols(
    dm_exp_flow_gdp ~
      fdi_assets_gdp_gross + fdi_liab_gdp_gross +
      equity_assets_gdp_gross + equity_liab_gdp_gross +
      debt_assets_gdp_gross + debt_liab_gdp_gross | iso3c + year,
    data = gross_dm_panel,
    vcov = ~iso3c
  ),
  
  "(4) DM stock A" = fixest::feols(
    dm_A_gdp ~
      fdi_assets_gdp_gross + fdi_liab_gdp_gross +
      equity_assets_gdp_gross + equity_liab_gdp_gross +
      debt_assets_gdp_gross + debt_liab_gdp_gross | iso3c + year,
    data = gross_dm_panel,
    vcov = ~iso3c
  ),
  
  "(5) DM stock B" = fixest::feols(
    dm_B_gdp ~
      fdi_assets_gdp_gross + fdi_liab_gdp_gross +
      equity_assets_gdp_gross + equity_liab_gdp_gross +
      debt_assets_gdp_gross + debt_liab_gdp_gross | iso3c + year,
    data = gross_dm_panel,
    vcov = ~iso3c
  )
)

gross_component_fe_rows <- tribble(
  ~term,         ~`(1) H&S flow`, ~`(2) H&S flow FE`, ~`(3) H&S flow TWFE`, ~`(4) DM stock A`, ~`(5) DM stock B`,
  "Country FE",  "No",            "Yes",              "Yes",               "Yes",            "Yes",
  "Year FE",     "No",            "No",               "Yes",               "Yes",            "Yes"
)
attr(gross_component_fe_rows, "position") <- c(10, 11)

gross_component_tex <- modelsummary(
  gross_component_models,
  stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  fmt = "%.4f",
  gof_omit = "AIC|BIC|Log|Adj|Within|RMSE",
  add_rows = gross_component_fe_rows,
  coef_rename = c(
    "fdi_assets_gdp_gross"    = "FDI assets / GDP",
    "fdi_liab_gdp_gross"      = "FDI liabilities / GDP",
    "equity_assets_gdp_gross" = "Equity assets / GDP",
    "equity_liab_gdp_gross"   = "Equity liabilities / GDP",
    "debt_assets_gdp_gross"   = "Debt assets / GDP",
    "debt_liab_gdp_gross"     = "Debt liabilities / GDP"
  ),
  output = "latex_tabular"
)

gross_component_tex_wrapped <- paste0(
  "\\begin{table}[htbp]\n\\centering\n",
  "\\caption{Gross External Portfolio Components and Dark Matter (1993--", y_ext_end, ")}\n",
  "\\label{tab:II_gross_components_dm}\n\\scriptsize\n",
  gross_component_tex, "\n",
  "\\begin{minipage}{0.95\\linewidth}\n",
  "\\footnotesize Notes: This table responds to the portfolio-composition interpretation of dark matter. ",
  "The dependent variable is annual dark matter exports under the H\\&S 5\\% method in columns (1)--(3), ",
  "and the component-specific dark matter stock under Scenarios A and B in columns (4)--(5). ",
  "The regressors are gross external asset and liability positions by asset class, expressed as percent of GDP. ",
  "Standard errors are heteroskedasticity-robust in column (1) and clustered by country in fixed-effect columns. ",
  "* p$<$0.10, ** p$<$0.05, *** p$<$0.01.\n",
  "\\end{minipage}\n\\end{table}"
)

compile_table(
  gross_component_tex_wrapped,
  "tab_II_T09_gross_portfolio_components",
  part = "part_II",
  landscape = TRUE,
  table_number = 9,
  fit_width = TRUE
)

message("  Gross-component regressions done.")






# ==============================================================================
# ==============================================================================
#
#                 Part III - Original Economic Extensions
#
# ==============================================================================
# ==============================================================================
# Three new tests that go beyond extending H&S's time window.
# Each targets a specific theoretical mechanism that H&S identify but either
# leave static or fail to confirm empirically:
#
#   Extension 1  — R&D and Intangible Capital
#                  Tests the knowledge-capital channel first.
#
#   Extension 2a — Safe-Asset Cycle, baseline 5% method
#                  Uses H&S's original dark-matter flow:
#                  dm_exp_flow_gdp = CA_DM(5%) - CA_official.
#
#   Extension 2b — Safe-Asset Cycle, component-specific method
#                  Repeats the VIX test using the Part II Section 2 method:
#                  NFA_j = NII_j / r_j by asset class, then annual flow.
#
#   Extension 2c — Financial vs. Geopolitical Risk
#                  Horse race between VIX and GPR.
#
#   Extension 3  — Financial Development and Safe Asset Production
#
#   Extension 4  — NII Composition / remaining robustness extensions
#
# Data files required in code/data/:
#   vix_daily.csv        (FRED VIXCLS — already downloaded)
#   gpr_web_latest.xlsx  (Caldara & Iacoviello — already downloaded)
#   wdi_intangibles.csv  (auto-downloaded on first run, cached locally after)
#   wdi_findev.csv       (auto-downloaded on first run, cached locally after)
#
# All outputs go to:
#   code/output/figures/part_III/
#   code/output/tables/part_III/
#
# ==============================================================================

library(sandwich)

# ── Small safeguards ──────────────────────────────────────────────────────────

if (!exists("col_green")) col_green <- "#238B45"

safe_havens <- c("USA", "CHE", "DEU", "GBR", "JPN", "NLD", "AUT", "DNK", "NOR")

for (d in c("output/figures/part_III", "output/tables/part_III")) {
  dir.create(here("code", d), recursive = TRUE, showWarnings = FALSE)
}

z_std <- function(x) as.numeric(scale(x))

mean_or_na2 <- function(x) {
  if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
}

# ── WDI caching helper ─────────────────────────────────────────────────────────
#
# Downloads once, then reuses cached CSVs from code/data/.

fetch_or_load_wdi <- function(filename, indicators, start, end) {
  path <- here("code", "data", filename)
  
  if (file.exists(path)) {
    message("  Loading from cache: ", filename)
    df <- read_csv(path, show_col_types = FALSE)
    
    if (!"iso3c" %in% names(df)) {
      message("  Cache file is not in expected format. Re-downloading.")
      file.remove(path)
      
      df <- WDI::WDI(
        indicator = indicators,
        start = start,
        end = end,
        extra = FALSE
      ) %>%
        as_tibble() %>%
        mutate(
          iso3c = if_else(
            !is.na(iso3c),
            iso3c,
            countrycode(iso2c, "iso2c", "iso3c", warn = FALSE)
          )
        ) %>%
        filter(!is.na(iso3c))
      
      write_csv(df, path)
      message("  Saved to: ", filename)
    }
    
    return(df)
    
  } else {
    message("  Downloading from WDI: ", paste(names(indicators), collapse = ", "))
    
    df <- WDI::WDI(
      indicator = indicators,
      start = start,
      end = end,
      extra = FALSE
    ) %>%
      as_tibble() %>%
      mutate(
        iso3c = if_else(
          !is.na(iso3c),
          iso3c,
          countrycode(iso2c, "iso2c", "iso3c", warn = FALSE)
        )
      ) %>%
      filter(!is.na(iso3c))
    
    write_csv(df, path)
    message("  Saved to: ", filename)
    
    return(df)
  }
}


# ==============================================================================
#
# Extension 1 — R&D and Intangible Capital
#
# This comes first because it directly tests the knowledge-capital channel:
# countries with high intangible intensity should export more dark matter if
# unrecorded know-how, blueprints, brands and software are a source of excess
# foreign income.
#
# We build a composite index from:
#   - R&D expenditure / GDP
#   - log resident patent applications
#   - log high-tech export share
#   - log ICT service exports
#
# ==============================================================================

message("\n── Part III / Extension 1: R&D and Intangible Capital ───────────────────")

# ── Load WDI intangible indicators ─────────────────────────────────────────────

p3_e1_wdi_intangibles <- fetch_or_load_wdi(
  filename = "wdi_intangibles.csv",
  indicators = c(
    rnd_gdp      = "GB.XPD.RSDV.GD.ZS",
    patents_res  = "IP.PAT.RESD",
    hitech_share = "TX.VAL.TECH.MF.ZS",
    ict_exports  = "BX.GSR.CCIS.ZS"
  ),
  start = 1980,
  end   = y_ext_end
) %>%
  filter(!is.na(iso3c), year >= 1980, year <= y_ext_end)

# ── Build annual and country-level intangible index ───────────────────────────

p3_e1_intangibles_annual <- p3_e1_wdi_intangibles %>%
  mutate(
    rnd_clean     = if_else(rnd_gdp < 0, NA_real_, rnd_gdp),
    patents_log   = log1p(if_else(patents_res  < 0, NA_real_, patents_res)),
    hitech_log    = log1p(if_else(hitech_share < 0, NA_real_, hitech_share)),
    ict_log       = log1p(if_else(ict_exports  < 0, NA_real_, ict_exports))
  ) %>%
  mutate(
    z_rnd     = z_std(rnd_clean),
    z_patents = z_std(patents_log),
    z_hitech  = z_std(hitech_log),
    z_ict     = z_std(ict_log)
  ) %>%
  rowwise() %>%
  mutate(
    n_intang_components = sum(!is.na(c_across(c(z_rnd, z_patents, z_hitech, z_ict)))),
    intang_idx_annual = if_else(
      n_intang_components >= 2,
      mean(c_across(c(z_rnd, z_patents, z_hitech, z_ict)), na.rm = TRUE),
      NA_real_
    )
  ) %>%
  ungroup()

p3_e1_intangibles_cs <- p3_e1_intangibles_annual %>%
  group_by(iso3c) %>%
  summarise(
    rnd_avg_wdi      = mean_or_na2(rnd_clean),
    z_rnd_avg        = mean_or_na2(z_rnd),
    z_patents_avg    = mean_or_na2(z_patents),
    z_hitech_avg     = mean_or_na2(z_hitech),
    z_ict_avg        = mean_or_na2(z_ict),
    intang_idx       = mean_or_na2(intang_idx_annual),
    intang_coverage  = sum(!is.na(intang_idx_annual)),
    .groups = "drop"
  )

# ── Cross-section dataset ─────────────────────────────────────────────────────

p3_e1_cs <- cs_ext %>%
  left_join(p3_e1_intangibles_cs, by = "iso3c") %>%
  mutate(
    dm_exp_ratio     = dm_exp_gdp     / 100,
    fdi_assets_ratio = fdi_assets_gdp / 100,
    fdi_liab_ratio   = fdi_liab_gdp   / 100
  )

p3_e1_data <- p3_e1_cs %>%
  filter(
    iso3c %in% countries_79,
    !is.na(dm_exp_ratio),
    !is.na(fdi_assets_ratio),
    !is.na(fdi_liab_ratio),
    !is.na(output_vol_hp)
  ) %>%
  mutate(
    across(
      any_of(c(
        "dm_exp_ratio", "fdi_assets_ratio", "fdi_liab_ratio",
        "output_vol_hp", "z_rnd_avg", "intang_idx",
        "rule_of_law"
      )),
      winsor
    )
  )

message(sprintf(
  "  E1 sample: %d obs, %d countries",
  nrow(p3_e1_data), n_distinct(p3_e1_data$iso3c)
))

# ── Regressions ───────────────────────────────────────────────────────────────

p3_e1_models <- list(
  "(1) Baseline" = lm(
    dm_exp_ratio ~ fdi_assets_ratio + fdi_liab_ratio + output_vol_hp,
    data = p3_e1_data
  ),
  
  "(2) R&D only" = lm(
    dm_exp_ratio ~ fdi_assets_ratio + fdi_liab_ratio + output_vol_hp +
      z_rnd_avg,
    data = filter(p3_e1_data, !is.na(z_rnd_avg))
  ),
  
  "(3) Intangibles" = lm(
    dm_exp_ratio ~ fdi_assets_ratio + fdi_liab_ratio + output_vol_hp +
      intang_idx,
    data = filter(p3_e1_data, !is.na(intang_idx))
  ),
  
  "(4) R&D + Intang." = lm(
    dm_exp_ratio ~ fdi_assets_ratio + fdi_liab_ratio + output_vol_hp +
      z_rnd_avg + intang_idx,
    data = filter(p3_e1_data, !is.na(z_rnd_avg), !is.na(intang_idx))
  ),
  
  "(5) Industrial" = lm(
    dm_exp_ratio ~ fdi_assets_ratio + fdi_liab_ratio + output_vol_hp +
      intang_idx,
    data = filter(p3_e1_data, iso3c %in% industrial, !is.na(intang_idx))
  )
)

print(sapply(p3_e1_models, nobs))

p3_e1_tex <- modelsummary(
  p3_e1_models,
  stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  fmt = "%.3f",
  gof_omit = "AIC|BIC|Log|F|RMSE",
  coef_rename = c(
    "fdi_assets_ratio" = "FDI assets / GDP",
    "fdi_liab_ratio"   = "FDI liabilities / GDP",
    "output_vol_hp"    = "Output volatility (HP)",
    "z_rnd_avg"        = "R\\&D intensity (std.)",
    "intang_idx"       = "Intangible intensity (std.)"
  ),
  output = "latex_tabular"
)

p3_e1_tex_wrapped <- paste0(
  "\\begin{table}[htbp]\n\\centering\n",
  "\\caption{Extension 1 --- R\\&D, Intangible Capital and Dark Matter (",
  y_ext_start, "--", y_ext_end, ")}\n",
  "\\label{tab:p3_e1_intangibles}\n\\scriptsize\n",
  p3_e1_tex, "\n",
  "\\begin{minipage}{0.95\\linewidth}\n",
  "\\footnotesize Notes: Dependent variable: cumulative dark matter exports over ",
  y_ext_start, "--", y_ext_end, ", divided by end-year GDP. ",
  "The intangible index is the row mean of four standardised components: ",
  "R\\&D/GDP, log resident patent applications, log high-technology export share, ",
  "and log ICT service exports. At least two components are required per country-year. ",
  "All variables winsorised at the 1\\% level. ",
  "* p$<$0.10, ** p$<$0.05, *** p$<$0.01.\n",
  "\\end{minipage}\n\\end{table}"
)

compile_table(
  p3_e1_tex_wrapped,
  "p3_table_E1_intangibles",
  part = "part_III",
  landscape = FALSE,
  table_number = 1,
  fit_width = TRUE
)

# ── Figure: intangible intensity vs dark matter exports ───────────────────────

p3_e1_fig <- p3_e1_data %>%
  filter(!is.na(intang_idx)) %>%
  mutate(
    grp = case_when(
      iso3c %in% safe_havens ~ "Safe haven",
      iso3c %in% industrial  ~ "Other industrial",
      TRUE                   ~ "Developing"
    )
  ) %>%
  ggplot(aes(x = intang_idx, y = dm_exp_ratio, label = iso3c)) +
  geom_hline(yintercept = 0, colour = col_grey, linewidth = 0.4) +
  geom_vline(xintercept = 0, colour = col_grey, linewidth = 0.4, linetype = "dashed") +
  geom_smooth(
    method = "lm", se = TRUE, colour = col_red,
    linewidth = 0.9, fill = col_red, alpha = 0.08
  ) +
  geom_point(aes(colour = grp, size = grp), alpha = 0.82) +
  geom_text_repel(
    aes(colour = grp), size = 2.4,
    segment.colour = "grey70", segment.size = 0.3,
    box.padding = 0.3, max.overlaps = 25, seed = 42
  ) +
  scale_colour_manual(
    values = c(
      "Safe haven" = col_blue,
      "Other industrial" = col_red,
      "Developing" = "grey55"
    ),
    name = NULL
  ) +
  scale_size_manual(
    values = c("Safe haven" = 2.8, "Other industrial" = 2.2, "Developing" = 1.6),
    guide = "none"
  ) +
  labs(
    title = "Extension 1 — Intangible Capital Intensity vs. Dark Matter Exports",
    subtitle = paste0(
      "Cross-section, ", y_ext_start, "\u2013", y_ext_end,
      ". Index = mean z-score of R&D, patents, high-tech exports and ICT exports."
    ),
    x = "Composite intangible intensity (standardised)",
    y = "Cumulative dark matter exports / end-year GDP"
  ) +
  theme_paper +
  theme(legend.position = "bottom")

save_fig(
  p3_e1_fig,
  "p3_fig_E1_intangibles_scatter",
  part = "part_III",
  w = 9,
  h = 6.5
)

message("  Extension 1 done.")


# ==============================================================================
#
# Extension 2a — Safe-Asset Cycle, VIX, H&S 5% Method
#
# Baseline version. The dependent variable is the annual dark matter export flow
# already built in panel_ext:
#
#   NFA_DM = NII / 0.05
#   DM exports = ΔNFA_DM - official CA
#
# ==============================================================================

message("\n── Part III / Extension 2a: VIX, H&S 5% Method ─────────────────────────")

# ── Load and annualise VIX ─────────────────────────────────────────────────────

p3_e2a_vix_raw <- read_csv(
  here("code", "data", "vix_daily.csv"),
  show_col_types = FALSE
)

p3_e2a_date_col <- intersect(
  c("DATE", "Date", "date", "observation_date"),
  names(p3_e2a_vix_raw)
)[1]

if (is.na(p3_e2a_date_col)) {
  stop("Could not identify a date column in vix_daily.csv.")
}

p3_e2a_value_col <- setdiff(names(p3_e2a_vix_raw), p3_e2a_date_col)[1]

p3_e2a_vix_annual <- p3_e2a_vix_raw %>%
  transmute(
    date = as.Date(.data[[p3_e2a_date_col]]),
    vix  = suppressWarnings(as.numeric(na_if(as.character(.data[[p3_e2a_value_col]]), ".")))
  ) %>%
  filter(!is.na(date), !is.na(vix), vix > 0) %>%
  mutate(year = as.integer(format(date, "%Y"))) %>%
  filter(year >= 1990, year <= y_ext_end) %>%
  group_by(year) %>%
  summarise(vix = mean(vix, na.rm = TRUE), .groups = "drop") %>%
  arrange(year) %>%
  mutate(d_log_vix = c(NA_real_, diff(log(vix))))

# ── Merge VIX into H&S 5% panel ───────────────────────────────────────────────

p3_e2a_panel_vix_5pct <- panel_ext %>%
  filter(year >= 1990, year <= y_ext_end) %>%
  left_join(p3_e2a_vix_annual, by = "year") %>%
  mutate(
    safe_d     = as.integer(iso3c %in% safe_havens),
    vix_x_safe = d_log_vix * safe_d,
    vix_x_opec = d_log_vix * opec_d
  )

p3_e2a_data <- p3_e2a_panel_vix_5pct %>%
  filter(
    iso3c %in% countries_79,
    !is.na(dm_exp_flow_gdp),
    !is.na(d_log_vix),
    !is.na(fdi_assets_gdp),
    !is.na(fdi_liab_gdp)
  )

message(sprintf(
  "  E2a sample: %d obs, %d countries",
  nrow(p3_e2a_data), n_distinct(p3_e2a_data$iso3c)
))

# ── Regressions ───────────────────────────────────────────────────────────────

p3_e2a_models <- list(
  "(1)" = fixest::feols(
    dm_exp_flow_gdp ~ d_log_vix + fdi_liab_gdp + fdi_assets_gdp,
    data = p3_e2a_data,
    vcov = "hetero"
  ),
  
  "(2) Interact." = fixest::feols(
    dm_exp_flow_gdp ~ d_log_vix + vix_x_safe + vix_x_opec +
      fdi_liab_gdp + fdi_assets_gdp,
    data = p3_e2a_data,
    vcov = "hetero"
  ),
  
  "(3) FE" = fixest::feols(
    dm_exp_flow_gdp ~ d_log_vix + fdi_liab_gdp + fdi_assets_gdp | iso3c,
    data = p3_e2a_data,
    vcov = ~iso3c
  ),
  
  "(4) FE+Int." = fixest::feols(
    dm_exp_flow_gdp ~ d_log_vix + vix_x_safe + vix_x_opec +
      fdi_liab_gdp + fdi_assets_gdp | iso3c,
    data = p3_e2a_data,
    vcov = ~iso3c
  ),
  
  "(5) Two-way" = fixest::feols(
    dm_exp_flow_gdp ~ vix_x_safe + vix_x_opec +
      fdi_liab_gdp + fdi_assets_gdp | iso3c + year,
    data = p3_e2a_data,
    vcov = ~iso3c
  )
)

p3_e2a_fe_rows <- tribble(
  ~term,         ~`(1)`, ~`(2) Interact.`, ~`(3) FE`, ~`(4) FE+Int.`, ~`(5) Two-way`,
  "Country FE",  "No",   "No",             "Yes",     "Yes",          "Yes",
  "Year FE",     "No",   "No",             "No",      "No",           "Yes"
)
attr(p3_e2a_fe_rows, "position") <- c(9, 10)

p3_e2a_tex <- modelsummary(
  p3_e2a_models,
  stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  fmt = "%.4f",
  gof_omit = "AIC|BIC|Log|Adj|Within|RMSE",
  add_rows = p3_e2a_fe_rows,
  coef_rename = c(
    "d_log_vix"      = "$\\Delta\\log(\\mathrm{VIX})$",
    "vix_x_safe"     = "$\\Delta\\log(\\mathrm{VIX})\\times\\mathrm{Safe\\ haven}$",
    "vix_x_opec"     = "$\\Delta\\log(\\mathrm{VIX})\\times\\mathrm{OPEC}$",
    "fdi_liab_gdp"   = "FDI liabilities / GDP",
    "fdi_assets_gdp" = "FDI assets / GDP"
  ),
  output = "latex_tabular"
)

p3_e2a_tex_wrapped <- paste0(
  "\\begin{table}[htbp]\n\\centering\n",
  "\\caption{Extension 2a --- VIX Sensitivity of Dark Matter Exports: H\\&S 5\\% Method (",
  "1990--", y_ext_end, ")}\n",
  "\\label{tab:p3_e2a_vix_5pct}\n\\scriptsize\n",
  p3_e2a_tex, "\n",
  "\\begin{minipage}{0.95\\linewidth}\n",
  "\\footnotesize Notes: Dependent variable: annual dark matter exports divided by GDP, ",
  "computed with the original H\\&S assumption $NFA^{DM}=NII/0.05$. ",
  "Safe-haven countries: USA, CHE, DEU, GBR, JPN, NLD, AUT, DNK, NOR. ",
  "Columns (1)--(2): heteroskedasticity-robust standard errors. ",
  "Columns (3)--(5): standard errors clustered by country. ",
  "In column (5), year fixed effects absorb the aggregate VIX level. ",
  "* p$<$0.10, ** p$<$0.05, *** p$<$0.01.\n",
  "\\end{minipage}\n\\end{table}"
)

compile_table(
  p3_e2a_tex_wrapped,
  "p3_table_E2a_vix_5pct",
  part = "part_III",
  landscape = TRUE,
  table_number = 2,
  fit_width = TRUE
)

message("  Extension 2a done.")


# ==============================================================================
#
# Extension 2b — Safe-Asset Cycle, VIX, Component-Specific Method
#
# This repeats the VIX exercise using the Part II Section 2 method.
# We compute annual implied CA from the change in component-specific NFA:
#
#   NFA_A = NFA_FDI_A + NFA_equity_A + NFA_debt_A + NFA_other_A
#   CA_DM_A = ΔNFA_A
#   DM exports_A = CA_DM_A - official CA
#
# Same for Scenario B.
#
# ==============================================================================

message("\n── Part III / Extension 2b: VIX, Component-Specific Method ─────────────")

# ── Build annual component-specific DM export flows ───────────────────────────

p3_e2b_component_flows <- nfa_decomp %>%
  filter(year >= 1990, year <= y_ext_end) %>%
  left_join(
    panel_ext %>%
      select(
        iso3c, year,
        ca_usd,
        fdi_assets_gdp, fdi_liab_gdp,
        opec_d, hipc_d
      ),
    by = c("iso3c", "year")
  ) %>%
  arrange(iso3c, year) %>%
  mutate(
    nfa_total_A = coalesce(nfa_fdi_A, 0) +
      coalesce(nfa_equity_A, 0) +
      coalesce(nfa_debt_A, 0) +
      coalesce(nfa_other_A, 0),
    
    nfa_total_B = coalesce(nfa_fdi_B, 0) +
      coalesce(nfa_equity_B, 0) +
      coalesce(nfa_debt_B, 0) +
      coalesce(nfa_other_B, 0),
    
    n_comp_A = rowSums(!is.na(across(c(nfa_fdi_A, nfa_equity_A, nfa_debt_A, nfa_other_A)))),
    n_comp_B = rowSums(!is.na(across(c(nfa_fdi_B, nfa_equity_B, nfa_debt_B, nfa_other_B)))),
    
    nfa_total_A = if_else(n_comp_A > 0, nfa_total_A, NA_real_),
    nfa_total_B = if_else(n_comp_B > 0, nfa_total_B, NA_real_)
  ) %>%
  group_by(iso3c) %>%
  mutate(
    ca_dm_A = nfa_total_A - dplyr::lag(nfa_total_A),
    ca_dm_B = nfa_total_B - dplyr::lag(nfa_total_B),
    
    dm_exp_flow_A     = ca_dm_A - ca_usd,
    dm_exp_flow_B     = ca_dm_B - ca_usd,
    dm_exp_flow_A_gdp = dm_exp_flow_A / gdp_usd * 100,
    dm_exp_flow_B_gdp = dm_exp_flow_B / gdp_usd * 100
  ) %>%
  ungroup()

# ── Merge VIX ─────────────────────────────────────────────────────────────────

p3_e2b_panel_vix_component <- p3_e2b_component_flows %>%
  left_join(p3_e2a_vix_annual, by = "year") %>%
  mutate(
    safe_d     = as.integer(iso3c %in% safe_havens),
    vix_x_safe = d_log_vix * safe_d,
    vix_x_opec = d_log_vix * opec_d
  )

p3_e2b_data <- p3_e2b_panel_vix_component %>%
  filter(
    iso3c %in% countries_79,
    !is.na(d_log_vix),
    !is.na(fdi_assets_gdp),
    !is.na(fdi_liab_gdp),
    !is.na(dm_exp_flow_A_gdp),
    !is.na(dm_exp_flow_B_gdp)
  )

message(sprintf(
  "  E2b sample: %d obs, %d countries",
  nrow(p3_e2b_data), n_distinct(p3_e2b_data$iso3c)
))

# ── Regressions ───────────────────────────────────────────────────────────────

p3_e2b_models <- list(
  "(1) A: pooled" = fixest::feols(
    dm_exp_flow_A_gdp ~ d_log_vix + vix_x_safe + vix_x_opec +
      fdi_liab_gdp + fdi_assets_gdp,
    data = p3_e2b_data,
    vcov = "hetero"
  ),
  
  "(2) A: country FE" = fixest::feols(
    dm_exp_flow_A_gdp ~ d_log_vix + vix_x_safe + vix_x_opec +
      fdi_liab_gdp + fdi_assets_gdp | iso3c,
    data = p3_e2b_data,
    vcov = ~iso3c
  ),
  
  "(3) A: two-way FE" = fixest::feols(
    dm_exp_flow_A_gdp ~ vix_x_safe + vix_x_opec +
      fdi_liab_gdp + fdi_assets_gdp | iso3c + year,
    data = p3_e2b_data,
    vcov = ~iso3c
  ),
  
  "(4) B: two-way FE" = fixest::feols(
    dm_exp_flow_B_gdp ~ vix_x_safe + vix_x_opec +
      fdi_liab_gdp + fdi_assets_gdp | iso3c + year,
    data = p3_e2b_data,
    vcov = ~iso3c
  )
)

p3_e2b_fe_rows <- tribble(
  ~term,         ~`(1) A: pooled`, ~`(2) A: country FE`, ~`(3) A: two-way FE`, ~`(4) B: two-way FE`,
  "Country FE",  "No",             "Yes",                "Yes",               "Yes",
  "Year FE",     "No",             "No",                 "Yes",               "Yes"
)
attr(p3_e2b_fe_rows, "position") <- c(9, 10)

p3_e2b_tex <- modelsummary(
  p3_e2b_models,
  stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  fmt = "%.4f",
  gof_omit = "AIC|BIC|Log|Adj|Within|RMSE",
  add_rows = p3_e2b_fe_rows,
  coef_rename = c(
    "d_log_vix"      = "$\\Delta\\log(\\mathrm{VIX})$",
    "vix_x_safe"     = "$\\Delta\\log(\\mathrm{VIX})\\times\\mathrm{Safe\\ haven}$",
    "vix_x_opec"     = "$\\Delta\\log(\\mathrm{VIX})\\times\\mathrm{OPEC}$",
    "fdi_liab_gdp"   = "FDI liabilities / GDP",
    "fdi_assets_gdp" = "FDI assets / GDP"
  ),
  output = "latex_tabular"
)

p3_e2b_tex_wrapped <- paste0(
  "\\begin{table}[htbp]\n\\centering\n",
  "\\caption{Extension 2b --- VIX Sensitivity of Dark Matter Exports: Component-Specific Method (",
  "1990--", y_ext_end, ")}\n",
  "\\label{tab:p3_e2b_vix_component}\n\\scriptsize\n",
  p3_e2b_tex, "\n",
  "\\begin{minipage}{0.95\\linewidth}\n",
  "\\footnotesize Notes: Dependent variable: annual dark matter exports divided by GDP. ",
  "Columns (1)--(3) use Scenario A from Part II Section 2; column (4) uses Scenario B. ",
  "The component-specific method capitalises each NII component using asset-class-specific ",
  "discount rates, then computes annual implied CA from the change in component-specific NFA. ",
  "In two-way FE columns, year fixed effects absorb the aggregate VIX level. ",
  "Standard errors clustered by country where fixed effects are used. ",
  "* p$<$0.10, ** p$<$0.05, *** p$<$0.01.\n",
  "\\end{minipage}\n\\end{table}"
)

compile_table(
  p3_e2b_tex_wrapped,
  "p3_table_E2b_vix_component_method",
  part = "part_III",
  landscape = TRUE,
  table_number = 3,
  fit_width = TRUE
)

message("  Extension 2b done.")


# ==============================================================================
#
# Extension 2c — Financial vs. Geopolitical Risk: VIX vs. GPR
#
# This remains based on the H&S 5% flow, to keep the horse race simple:
# VIX captures financial volatility; GPR captures geopolitical risk.
#
# ==============================================================================

message("\n── Part III / Extension 2c: VIX vs. GPR Horse Race ─────────────────────")

# ── Load and annualise GPR ─────────────────────────────────────────────────────

p3_e2c_gpr_raw <- read_excel(
  here("code", "data", "gpr_web_latest.xlsx"),
  sheet = "GPR"
)

if (!inherits(p3_e2c_gpr_raw$Date, "Date")) {
  p3_e2c_gpr_raw <- p3_e2c_gpr_raw %>%
    mutate(Date = as.Date(as.numeric(Date), origin = "1899-12-30"))
}

p3_e2c_gpr_annual <- p3_e2c_gpr_raw %>%
  mutate(
    year = as.integer(format(Date, "%Y")),
    gpr  = suppressWarnings(as.numeric(GPR))
  ) %>%
  filter(!is.na(gpr), !is.na(year), year >= 1990, year <= y_ext_end) %>%
  group_by(year) %>%
  summarise(gpr = mean(gpr, na.rm = TRUE), .groups = "drop") %>%
  arrange(year) %>%
  mutate(d_log_gpr = c(NA_real_, diff(log(gpr))))

# ── Merge VIX and GPR into 5% panel ───────────────────────────────────────────

p3_e2c_panel_gpr <- p3_e2a_panel_vix_5pct %>%
  left_join(p3_e2c_gpr_annual, by = "year") %>%
  mutate(
    gpr_x_safe = d_log_gpr * safe_d,
    gpr_x_opec = d_log_gpr * opec_d
  )

p3_e2c_data <- p3_e2c_panel_gpr %>%
  filter(
    iso3c %in% countries_79,
    !is.na(dm_exp_flow_gdp),
    !is.na(d_log_vix),
    !is.na(d_log_gpr),
    !is.na(fdi_liab_gdp),
    !is.na(fdi_assets_gdp)
  )

message(sprintf(
  "  E2c sample: %d obs, %d countries",
  nrow(p3_e2c_data), n_distinct(p3_e2c_data$iso3c)
))

# ── Figure: VIX vs GPR time series ────────────────────────────────────────────

p3_e2c_fig_ts <- p3_e2a_vix_annual %>%
  left_join(p3_e2c_gpr_annual, by = "year") %>%
  filter(!is.na(vix), !is.na(gpr)) %>%
  mutate(
    vix_std = z_std(vix),
    gpr_std = z_std(gpr)
  ) %>%
  pivot_longer(c(vix_std, gpr_std), names_to = "index", values_to = "value") %>%
  mutate(
    index = recode(
      index,
      "vix_std" = "VIX (financial volatility)",
      "gpr_std" = "GPR (geopolitical risk)"
    )
  ) %>%
  ggplot(aes(x = year, y = value, colour = index, linetype = index)) +
  geom_hline(yintercept = 0, colour = col_grey, linewidth = 0.4) +
  geom_line(linewidth = 1.0, alpha = 0.9) +
  scale_colour_manual(
    values = c(
      "VIX (financial volatility)" = col_blue,
      "GPR (geopolitical risk)" = col_red
    ),
    name = NULL
  ) +
  scale_linetype_manual(
    values = c(
      "VIX (financial volatility)" = "solid",
      "GPR (geopolitical risk)" = "dashed"
    ),
    name = NULL
  ) +
  annotate("text", x = 2001.5, y = 3.1, label = "9/11",
           size = 2.8, colour = col_grey, fontface = "italic") +
  annotate("text", x = 2008.8, y = 3.5, label = "GFC",
           size = 2.8, colour = col_grey, fontface = "italic") +
  annotate("text", x = 2020.2, y = 3.7, label = "COVID",
           size = 2.8, colour = col_grey, fontface = "italic") +
  scale_x_continuous(breaks = seq(1990, y_ext_end, 4)) +
  labs(
    title = "Extension 2c — VIX vs. GPR: Complementary Risk Measures",
    subtitle = "Both series z-standardised. Divergence periods help separate financial and geopolitical safety premia.",
    x = NULL,
    y = "Standardised index (z-score)"
  ) +
  theme_paper +
  theme(legend.position = "bottom")

save_fig(
  p3_e2c_fig_ts,
  "p3_fig_E2c_vix_vs_gpr_timeseries",
  part = "part_III",
  w = 10,
  h = 5
)

# ── Horse-race regressions ────────────────────────────────────────────────────

p3_e2c_models <- list(
  "(1) VIX" = fixest::feols(
    dm_exp_flow_gdp ~ vix_x_safe + vix_x_opec +
      fdi_liab_gdp + fdi_assets_gdp | iso3c + year,
    data = p3_e2c_data,
    vcov = ~iso3c
  ),
  
  "(2) GPR" = fixest::feols(
    dm_exp_flow_gdp ~ gpr_x_safe + gpr_x_opec +
      fdi_liab_gdp + fdi_assets_gdp | iso3c + year,
    data = p3_e2c_data,
    vcov = ~iso3c
  ),
  
  "(3) VIX + GPR" = fixest::feols(
    dm_exp_flow_gdp ~ vix_x_safe + gpr_x_safe +
      vix_x_opec + gpr_x_opec +
      fdi_liab_gdp + fdi_assets_gdp | iso3c + year,
    data = p3_e2c_data,
    vcov = ~iso3c
  ),
  
  "(4) OPEC excl." = fixest::feols(
    dm_exp_flow_gdp ~ vix_x_safe + gpr_x_safe +
      fdi_liab_gdp + fdi_assets_gdp | iso3c + year,
    data = filter(p3_e2c_data, opec_d == 0),
    vcov = ~iso3c
  )
)

p3_e2c_fe_rows <- tribble(
  ~term,        ~`(1) VIX`, ~`(2) GPR`, ~`(3) VIX + GPR`, ~`(4) OPEC excl.`,
  "Country FE", "Yes",      "Yes",      "Yes",             "Yes",
  "Year FE",    "Yes",      "Yes",      "Yes",             "Yes"
)
attr(p3_e2c_fe_rows, "position") <- c(9, 10)

p3_e2c_tex <- modelsummary(
  p3_e2c_models,
  stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  fmt = "%.4f",
  gof_omit = "AIC|BIC|Log|Adj|Within|RMSE",
  add_rows = p3_e2c_fe_rows,
  coef_rename = c(
    "vix_x_safe"     = "$\\Delta\\log(\\mathrm{VIX})\\times\\mathrm{Safe\\ haven}$",
    "gpr_x_safe"     = "$\\Delta\\log(\\mathrm{GPR})\\times\\mathrm{Safe\\ haven}$",
    "vix_x_opec"     = "$\\Delta\\log(\\mathrm{VIX})\\times\\mathrm{OPEC}$",
    "gpr_x_opec"     = "$\\Delta\\log(\\mathrm{GPR})\\times\\mathrm{OPEC}$",
    "fdi_liab_gdp"   = "FDI liabilities / GDP",
    "fdi_assets_gdp" = "FDI assets / GDP"
  ),
  output = "latex_tabular"
)

p3_e2c_tex_wrapped <- paste0(
  "\\begin{table}[htbp]\n\\centering\n",
  "\\caption{Extension 2c --- Financial vs. Geopolitical Risk: VIX vs. GPR Horse Race (",
  "1990--", y_ext_end, ")}\n",
  "\\label{tab:p3_e2c_vix_gpr}\n\\scriptsize\n",
  p3_e2c_tex, "\n",
  "\\begin{minipage}{0.95\\linewidth}\n",
  "\\footnotesize Notes: Dependent variable: annual dark matter exports divided by GDP, ",
  "computed with the H\\&S 5\\% method. All columns include country and year fixed effects. ",
  "The aggregate VIX and GPR levels are absorbed by year dummies; only differential ",
  "safe-haven and OPEC responses are identified. ",
  "GPR = Caldara \\& Iacoviello Geopolitical Risk Index. ",
  "Standard errors clustered by country. * p$<$0.10, ** p$<$0.05, *** p$<$0.01.\n",
  "\\end{minipage}\n\\end{table}"
)

compile_table(
  p3_e2c_tex_wrapped,
  "p3_table_E2c_vix_gpr",
  part = "part_III",
  landscape = FALSE,
  table_number = 4,
  fit_width = TRUE
)

message("  Extension 2c done.")

# ==============================================================================
#
# Extension 2d — VIX, Safe Havens and the Global Financial Crisis Split
#
# Motivation:
# The supervisor suggested that post-global-crisis regressions may generate
# counterintuitive results. This block tests whether the VIX × safe-haven effect
# differs before and after 2008.
#
# Interpretation:
#   - Before 2008: safe havens may benefit from "exorbitant privilege".
#   - After 2008: safe havens may perform an "exorbitant duty" by absorbing
#     risk and providing insurance/liquidity during crises.
#
# ==============================================================================

message("\n── Part III / Extension 2d: Pre/Post GFC VIX split ─────────────────────")

# ── 1. H&S 5% method: pre/post GFC ─────────────────────────────────────────────

p3_e2d_5pct <- p3_e2a_data %>%
  mutate(
    period_gfc = if_else(year < 2008, "Pre-GFC", "Post-GFC")
  ) %>%
  filter(!is.na(period_gfc))

message("  E2d H&S 5% sample by period:")
print(
  p3_e2d_5pct %>%
    count(period_gfc, name = "n_obs")
)

# ── 2. Component-specific method: pre/post GFC ────────────────────────────────

p3_e2d_component <- p3_e2b_data %>%
  mutate(
    period_gfc = if_else(year < 2008, "Pre-GFC", "Post-GFC")
  ) %>%
  filter(!is.na(period_gfc))

message("  E2d component-specific sample by period:")
print(
  p3_e2d_component %>%
    count(period_gfc, name = "n_obs")
)

# ── 3. Regressions ────────────────────────────────────────────────────────────
#
# For the two-way FE regressions, the aggregate VIX level is absorbed by year FE.
# The identified coefficient is therefore VIX × Safe haven.

p3_e2d_models <- list(
  "(1) 5% Pre-GFC" = fixest::feols(
    dm_exp_flow_gdp ~ vix_x_safe + vix_x_opec +
      fdi_liab_gdp + fdi_assets_gdp | iso3c + year,
    data = filter(p3_e2d_5pct, period_gfc == "Pre-GFC"),
    vcov = ~iso3c
  ),
  
  "(2) 5% Post-GFC" = fixest::feols(
    dm_exp_flow_gdp ~ vix_x_safe + vix_x_opec +
      fdi_liab_gdp + fdi_assets_gdp | iso3c + year,
    data = filter(p3_e2d_5pct, period_gfc == "Post-GFC"),
    vcov = ~iso3c
  ),
  
  "(3) Comp. A Pre-GFC" = fixest::feols(
    dm_exp_flow_A_gdp ~ vix_x_safe + vix_x_opec +
      fdi_liab_gdp + fdi_assets_gdp | iso3c + year,
    data = filter(p3_e2d_component, period_gfc == "Pre-GFC"),
    vcov = ~iso3c
  ),
  
  "(4) Comp. A Post-GFC" = fixest::feols(
    dm_exp_flow_A_gdp ~ vix_x_safe + vix_x_opec +
      fdi_liab_gdp + fdi_assets_gdp | iso3c + year,
    data = filter(p3_e2d_component, period_gfc == "Post-GFC"),
    vcov = ~iso3c
  ),
  
  "(5) Comp. B Post-GFC" = fixest::feols(
    dm_exp_flow_B_gdp ~ vix_x_safe + vix_x_opec +
      fdi_liab_gdp + fdi_assets_gdp | iso3c + year,
    data = filter(p3_e2d_component, period_gfc == "Post-GFC"),
    vcov = ~iso3c
  )
)

p3_e2d_fe_rows <- tribble(
  ~term,         ~`(1) 5% Pre-GFC`, ~`(2) 5% Post-GFC`, ~`(3) Comp. A Pre-GFC`, ~`(4) Comp. A Post-GFC`, ~`(5) Comp. B Post-GFC`,
  "Country FE",  "Yes",             "Yes",              "Yes",                  "Yes",                   "Yes",
  "Year FE",     "Yes",             "Yes",              "Yes",                  "Yes",                   "Yes"
)
attr(p3_e2d_fe_rows, "position") <- c(9, 10)

p3_e2d_tex <- modelsummary(
  p3_e2d_models,
  stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  fmt = "%.4f",
  gof_omit = "AIC|BIC|Log|Adj|Within|RMSE",
  add_rows = p3_e2d_fe_rows,
  coef_rename = c(
    "vix_x_safe"     = "$\\Delta\\log(\\mathrm{VIX})\\times\\mathrm{Safe\\ haven}$",
    "vix_x_opec"     = "$\\Delta\\log(\\mathrm{VIX})\\times\\mathrm{OPEC}$",
    "fdi_liab_gdp"   = "FDI liabilities / GDP",
    "fdi_assets_gdp" = "FDI assets / GDP"
  ),
  output = "latex_tabular"
)

p3_e2d_tex_wrapped <- paste0(
  "\\begin{table}[htbp]\n\\centering\n",
  "\\caption{Extension 2d --- VIX, Safe Havens and the Global Financial Crisis Split}\n",
  "\\label{tab:p3_e2d_vix_gfc_split}\n\\scriptsize\n",
  p3_e2d_tex, "\n",
  "\\begin{minipage}{0.95\\linewidth}\n",
  "\\footnotesize Notes: This table tests whether the VIX-safe-haven relationship changes before and after the Global Financial Crisis. ",
  "Columns (1)--(2) use the original H\\&S 5\\% method. Columns (3)--(5) use the component-specific method from Part II Section 2. ",
  "All regressions include country and year fixed effects. Since year fixed effects absorb the aggregate VIX level, identification comes from ",
  "the differential response of safe havens and OPEC countries to changes in VIX. ",
  "A negative post-GFC safe-haven coefficient is consistent with an exorbitant-duty interpretation: safe havens may provide insurance or liquidity in crises rather than immediately earning higher annual dark matter exports. ",
  "Standard errors clustered by country. * p$<$0.10, ** p$<$0.05, *** p$<$0.01.\n",
  "\\end{minipage}\n\\end{table}"
)

compile_table(
  p3_e2d_tex_wrapped,
  "p3_table_E2d_vix_gfc_split",
  part = "part_III",
  landscape = TRUE,
  table_number = 5,
  fit_width = TRUE
)

message("  Extension 2d done.")

# ==============================================================================
#
# Extension 3 — Financial Development and Safe Asset Production
#
# Tests whether financially deeper economies are better able to produce safe,
# liquid claims, which may generate a safe-asset premium and hence dark matter.
#
# ==============================================================================

message("\n── Part III / Extension 3: Financial Development ────────────────────────")

# ── Load WDI financial development indicators ─────────────────────────────────

p3_e3_wdi_findev <- fetch_or_load_wdi(
  filename = "wdi_findev.csv",
  indicators = c(
    private_credit = "FS.AST.PRVT.GD.ZS",
    stock_mktcap   = "CM.MKT.LCAP.GD.ZS"
  ),
  start = 1980,
  end   = y_ext_end
) %>%
  filter(!is.na(iso3c), year >= 1980, year <= y_ext_end)

# ── Build financial development index ─────────────────────────────────────────

p3_e3_findev_cs <- p3_e3_wdi_findev %>%
  group_by(iso3c) %>%
  summarise(
    credit_avg = mean_or_na2(private_credit),
    mktcap_avg = mean_or_na2(stock_mktcap),
    .groups = "drop"
  ) %>%
  mutate(
    z_credit  = z_std(log1p(coalesce(credit_avg, 0))),
    z_mktcap  = z_std(log1p(coalesce(mktcap_avg, 0))),
    findev_idx = rowMeans(cbind(z_credit, z_mktcap), na.rm = TRUE)
  )

# ── Cross-section regressions ─────────────────────────────────────────────────

p3_e3_cs <- cs_ext %>%
  left_join(p3_e3_findev_cs, by = "iso3c") %>%
  left_join(p3_e1_intangibles_cs %>% select(iso3c, intang_idx), by = "iso3c") %>%
  mutate(
    dm_exp_ratio     = dm_exp_gdp     / 100,
    fdi_assets_ratio = fdi_assets_gdp / 100,
    fdi_liab_ratio   = fdi_liab_gdp   / 100
  )

p3_e3_data <- p3_e3_cs %>%
  filter(
    iso3c %in% countries_79,
    !is.na(dm_exp_ratio),
    !is.na(fdi_assets_ratio),
    !is.na(fdi_liab_ratio),
    !is.na(output_vol_hp)
  ) %>%
  mutate(
    across(
      any_of(c(
        "dm_exp_ratio", "fdi_assets_ratio", "fdi_liab_ratio",
        "output_vol_hp", "z_credit", "z_mktcap", "findev_idx",
        "intang_idx", "rule_of_law"
      )),
      winsor
    )
  )

p3_e3_models <- list(
  "(1) Baseline" = lm(
    dm_exp_ratio ~ fdi_assets_ratio + fdi_liab_ratio + output_vol_hp,
    data = p3_e3_data
  ),
  
  "(2) Credit" = lm(
    dm_exp_ratio ~ fdi_assets_ratio + fdi_liab_ratio + output_vol_hp +
      z_credit,
    data = filter(p3_e3_data, !is.na(z_credit))
  ),
  
  "(3) Mkt cap" = lm(
    dm_exp_ratio ~ fdi_assets_ratio + fdi_liab_ratio + output_vol_hp +
      z_mktcap,
    data = filter(p3_e3_data, !is.na(z_mktcap))
  ),
  
  "(4) FinDev idx" = lm(
    dm_exp_ratio ~ fdi_assets_ratio + fdi_liab_ratio + output_vol_hp +
      findev_idx,
    data = filter(p3_e3_data, !is.na(findev_idx))
  ),
  
  "(5) Full" = lm(
    dm_exp_ratio ~ fdi_assets_ratio + fdi_liab_ratio + output_vol_hp +
      findev_idx + intang_idx + rule_of_law + opec + hipc,
    data = filter(
      p3_e3_data,
      !is.na(findev_idx),
      !is.na(intang_idx),
      !is.na(rule_of_law)
    )
  )
)

print(sapply(p3_e3_models, nobs))

p3_e3_tex <- modelsummary(
  p3_e3_models,
  stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  fmt = "%.3f",
  gof_omit = "AIC|BIC|Log|F|RMSE",
  coef_rename = c(
    "fdi_assets_ratio" = "FDI assets / GDP",
    "fdi_liab_ratio"   = "FDI liabilities / GDP",
    "output_vol_hp"    = "Output volatility (HP)",
    "z_credit"         = "Private credit / GDP (std.)",
    "z_mktcap"         = "Stock market cap / GDP (std.)",
    "findev_idx"       = "FinDev composite (std.)",
    "intang_idx"       = "Intangible intensity (std.)",
    "rule_of_law"      = "Rule of Law",
    "opec"             = "OPEC dummy",
    "hipc"             = "HIPC dummy"
  ),
  output = "latex_tabular"
)

p3_e3_tex_wrapped <- paste0(
  "\\begin{table}[htbp]\n\\centering\n",
  "\\caption{Extension 3 --- Financial Development and Safe Asset Production Capacity (",
  y_ext_start, "--", y_ext_end, ")}\n",
  "\\label{tab:p3_e3_findev}\n\\scriptsize\n",
  p3_e3_tex, "\n",
  "\\begin{minipage}{0.95\\linewidth}\n",
  "\\footnotesize Notes: Dependent variable: cumulative dark matter exports over ",
  y_ext_start, "--", y_ext_end, ", divided by end-year GDP. ",
  "The FinDev composite index is the mean of z-standardised log(private credit/GDP) ",
  "and log(stock market capitalisation/GDP). ",
  "Column (5) includes financial development, intangible intensity and institutional controls jointly. ",
  "All variables winsorised at the 1\\% level. ",
  "* p$<$0.10, ** p$<$0.05, *** p$<$0.01.\n",
  "\\end{minipage}\n\\end{table}"
)

compile_table(
  p3_e3_tex_wrapped,
  "p3_table_E3_findev",
  part = "part_III",
  landscape = FALSE,
  table_number = 6,
  fit_width = TRUE
)

# ── Figure: credit and equity market depth ────────────────────────────────────

p3_make_findev_scatter <- function(df, xvar, xlabel) {
  df %>%
    filter(!is.na(.data[[xvar]])) %>%
    mutate(
      grp = case_when(
        iso3c %in% safe_havens ~ "Safe haven",
        iso3c %in% industrial  ~ "Other industrial",
        TRUE                   ~ "Developing"
      )
    ) %>%
    ggplot(aes(x = .data[[xvar]], y = dm_exp_ratio, label = iso3c)) +
    geom_hline(yintercept = 0, colour = col_grey, linewidth = 0.4) +
    geom_smooth(
      method = "lm", se = TRUE, colour = col_red,
      linewidth = 0.9, fill = col_red, alpha = 0.08
    ) +
    geom_point(aes(colour = grp, size = grp), alpha = 0.82) +
    geom_text_repel(
      aes(colour = grp), size = 2.2,
      segment.colour = "grey70", segment.size = 0.3,
      box.padding = 0.28, max.overlaps = 20, seed = 7
    ) +
    scale_colour_manual(
      values = c(
        "Safe haven" = col_blue,
        "Other industrial" = col_red,
        "Developing" = "grey55"
      ),
      name = NULL
    ) +
    scale_size_manual(
      values = c("Safe haven" = 2.8, "Other industrial" = 2.2, "Developing" = 1.6),
      guide = "none"
    ) +
    labs(
      x = xlabel,
      y = "Dark matter exports / GDP"
    ) +
    theme_paper +
    theme(legend.position = "bottom")
}

p3_e3_fig <- (
  p3_make_findev_scatter(
    p3_e3_data,
    "z_credit",
    "Private credit / GDP (z-score)"
  ) +
    p3_make_findev_scatter(
      p3_e3_data,
      "z_mktcap",
      "Stock market cap / GDP (z-score)"
    )
) +
  plot_annotation(
    title = "Extension 3 — Financial Development vs. Dark Matter Exports",
    subtitle = paste0(
      "Cross-section, ", y_ext_start, "\u2013", y_ext_end,
      ". OLS fit with 95% CI. Blue = safe haven; red = other industrial."
    ),
    theme = theme(
      plot.title = element_text(size = 10, face = "bold"),
      plot.subtitle = element_text(size = 8, colour = "grey40")
    )
  ) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

save_fig(
  p3_e3_fig,
  "p3_fig_E3_financial_development_scatter",
  part = "part_III",
  w = 12,
  h = 6
)

message("  Extension 3 done.")


# ==============================================================================
#
# Extension 4 — Descriptive NII Decomposition by Asset Class
#
# This section is deliberately descriptive, because the full component-specific
# NFA methodology is already implemented in Part II Section 2 and reused above
# in Extension 2b.
#
# ==============================================================================

message("\n── Part III / Extension 4: Descriptive NII Decomposition ────────────────")

p3_e4_decomp <- nfa_decomp %>%
  filter(year >= 1993, year <= y_ext_end) %>%
  mutate(
    share_fdi = nii_fdi / nii_total * 100,
    share_equity = nii_equity / nii_total * 100,
    share_debt = nii_debt / nii_total * 100,
    share_other = nii_other / nii_total * 100
  )

# ── Figure: NII decomposition for six key countries ───────────────────────────

p3_e4_showcase <- c("USA", "GBR", "DEU", "JPN", "FRA", "IRL")

p3_e4_labels <- c(
  USA = "United States",
  GBR = "United Kingdom",
  DEU = "Germany",
  JPN = "Japan",
  FRA = "France",
  IRL = "Ireland"
)

p3_e4_comp_cols <- c(
  "FDI income"       = col_blue,
  "Portfolio equity" = col_red,
  "Portfolio debt"   = col_green,
  "Other investment" = "grey50",
  "Total NII"        = "black"
)

p3_e4_fig_decomp <- p3_e4_decomp %>%
  filter(iso3c %in% p3_e4_showcase, !is.na(nii_total)) %>%
  pivot_longer(
    c(nii_fdi, nii_equity, nii_debt, nii_other, nii_total),
    names_to = "component",
    values_to = "value"
  ) %>%
  mutate(
    value_bn = value / 1e6,
    component = factor(
      recode(
        component,
        nii_fdi    = "FDI income",
        nii_equity = "Portfolio equity",
        nii_debt   = "Portfolio debt",
        nii_other  = "Other investment",
        nii_total  = "Total NII"
      ),
      levels = c(
        "FDI income",
        "Portfolio equity",
        "Portfolio debt",
        "Other investment",
        "Total NII"
      )
    ),
    country = factor(p3_e4_labels[iso3c], levels = p3_e4_labels)
  ) %>%
  ggplot(aes(
    x = year,
    y = value_bn,
    colour = component,
    linetype = component,
    linewidth = component
  )) +
  geom_hline(yintercept = 0, colour = "grey70", linewidth = 0.35) +
  geom_line(alpha = 0.9, na.rm = TRUE) +
  scale_colour_manual(values = p3_e4_comp_cols, name = NULL) +
  scale_linetype_manual(
    values = c(
      "FDI income" = "solid",
      "Portfolio equity" = "longdash",
      "Portfolio debt" = "dashed",
      "Other investment" = "dotted",
      "Total NII" = "solid"
    ),
    name = NULL
  ) +
  scale_linewidth_manual(
    values = c(
      "FDI income" = 1.3,
      "Portfolio equity" = 1.1,
      "Portfolio debt" = 1.1,
      "Other investment" = 1.0,
      "Total NII" = 1.8
    ),
    name = NULL
  ) +
  facet_wrap(~ country, scales = "free_y", ncol = 3) +
  scale_x_continuous(breaks = seq(1993, y_ext_end, 8)) +
  labs(
    title = paste0("Extension 4 — NII Decomposition by Asset Class (1993\u2013", y_ext_end, ")"),
    subtitle = "Net investment income split by asset class. Total NII shown as thick black line.",
    x = NULL,
    y = "Net investment income, USD millions"
  ) +
  theme_paper +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold")
  )

save_fig(
  p3_e4_fig_decomp,
  "p3_fig_E4_nii_decomposition_showcase",
  part = "part_III",
  w = 12,
  h = 7
)

# ── Summary table: average NII composition over last 10 years ─────────────────

p3_e4_recent_window <- (y_ext_end - 9):y_ext_end

p3_e4_summary <- p3_e4_decomp %>%
  filter(iso3c %in% p3_e4_showcase, year %in% p3_e4_recent_window) %>%
  group_by(iso3c) %>%
  summarise(
    country = p3_e4_labels[iso3c[1]],
    nii_total_avg = mean_or_na2(nii_total) / 1e6,
    fdi_share_avg = mean_or_na2(share_fdi),
    equity_share_avg = mean_or_na2(share_equity),
    debt_share_avg = mean_or_na2(share_debt),
    other_share_avg = mean_or_na2(share_other),
    dm_A_avg = mean_or_na2(dm_A_gdp),
    dm_B_avg = mean_or_na2(dm_B_gdp),
    .groups = "drop"
  ) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))

p3_e4_tex <- kableExtra::kbl(
  p3_e4_summary %>%
    select(
      country, nii_total_avg,
      fdi_share_avg, equity_share_avg, debt_share_avg, other_share_avg,
      dm_A_avg, dm_B_avg
    ),
  format = "latex",
  booktabs = TRUE,
  linesep = "",
  col.names = c(
    "Country",
    "Avg. NII (\\$m)",
    "FDI (\\%)",
    "Equity (\\%)",
    "Debt (\\%)",
    "Other (\\%)",
    "DM A (\\% GDP)",
    "DM B (\\% GDP)"
  ),
  caption = paste0(
    "Extension 4 --- NII Composition and Dark Matter, average ",
    min(p3_e4_recent_window), "--", max(p3_e4_recent_window)
  ),
  label = "tab:p3_e4_nii_decomp",
  escape = FALSE,
  align = c("l", "r", "r", "r", "r", "r", "r", "r")
) %>%
  kableExtra::kable_styling(latex_options = "hold_position", font_size = 9.5) %>%
  kableExtra::add_header_above(
    c(" " = 2, "NII composition" = 4, "Dark matter" = 2),
    bold = TRUE,
    line = TRUE,
    escape = FALSE
  ) %>%
  kableExtra::footnote(
    general = paste0(
      "NII is shown in USD millions. Shares are expressed as a percentage of total NII. ",
      "DM A and DM B are the average dark matter stocks under the Part II Section 2 scenarios. ",
      "This table is descriptive and complements the component-specific VIX test in Extension 2b."
    ),
    general_title = "\\textit{Notes:} ",
    escape = FALSE
  )

compile_table(
  as.character(p3_e4_tex),
  "p3_table_E4_nii_decomposition_summary",
  part = "part_III",
  landscape = FALSE,
  table_number = 7,
  fit_width = FALSE
)

message("  Extension 4 done.")

# ==============================================================================
# Part III output summary
# ==============================================================================

message("\nPart III complete.")
message("Figures: code/output/figures/part_III/")
message("Tables : code/output/tables/part_III/")
message("Outputs created:")
message("  p3_fig_E1_intangibles_scatter")
message("  p3_table_E1_intangibles")
message("  p3_table_E2a_vix_5pct")
message("  p3_table_E2b_vix_component_method")
message("  p3_fig_E2c_vix_vs_gpr_timeseries")
message("  p3_table_E2c_vix_gpr")
message("  p3_fig_E3_financial_development_scatter")
message("  p3_table_E3_findev")
message("  p3_fig_E4_nii_decomposition_showcase")
message("  p3_table_E4_nii_decomposition_summary")