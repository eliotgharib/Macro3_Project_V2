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
#   1.1  Data loading: BOP 2022, EWN extended, GDP local file (no API)
#   1.2  Extended panel and cross-section construction
#   1.3  Figures (6 figures extending the Part I set):
#          fig_us_dm_extended         US dark matter stock 1982–2022
#          fig_global_nfa_extended    Global NFA by region 1985–2022
#          fig_scatter_ext            Extended scatter (cum DM vs official CA)
#          fig_dm_vs_ca_ext           Extended scatter (DM exports vs official CA)
#   1.4  Tables E1–E4b replicating H&S on the extended window
#
# Section 2 — Beyond the 5% Assumption: Component-Specific NFA
#   2.1  NII sub-components from BOP 2022
#   2.2  Two discount-rate scenarios:
#        Scenario A — Universal G&R (2006) rates (all countries identical)
#        Scenario B — Country-group rates (Gourinchas, Rey & Govillot 2017)
#   2.3  One combined 2×2 figure (USA | JPN / CHN | EU):
#          - Official NIIP: thin grey dotted line
#          - H&S 5%: thin grey dashed line
#          - Stacked areas: Scenario A components summing to NFA total
#          - Scenario B total: blue dashed line (differs from A only for USA, CHN)
#   2.4  Robustness table: three NFA measures and two DM estimates
#
# Section 3 — Decomposing Dark Matter by Source
#   3.1  Dark matter = NFA_precise − NIIP_official, by asset class
#   3.2  One combined 2×2 figure: stacked DM components, A vs B total lines
#   3.3  Cross-country summary table (both scenarios)
#
# Benchmark rates:
#   Scenario A (G&R 2006): r_fdi=8%, r_equity=6%, r_debt=3%, r_other=5%
#   Scenario B (GRG 2017 / LMF 2007):
#     "Privilege" (USA, CHE, GBR): r_fdi=9%, r_equity=7%, r_debt=2%, r_other=4%
#     "Advanced" (other industrial): same as Scenario A
#     "Emerging" (all others):       r_fdi=6%, r_equity=5%, r_debt=5%, r_other=5%
#
# Data files (all local):
#   Current_account_primary_income_1975_2022.csv
#   EWN-dataset-year-end-2024_4.9.26.xlsx
#   GDP_constant_1975_2022.csv
#
# ==============================================================================


# ── Key parameters ─────────────────────────────────────────────────────────────

y_ext_start <- 1980
y_ext_end   <- 2022

# Scenario A: universal G&R (2006) rates
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

# Showcase countries for Sections 2 & 3
showcase     <- c("USA", "JPN", "CHN")
panel_labels <- c(USA = "United States", JPN = "Japan",
                  CHN = "China", EU = "European Union")

# Component colour palette
comp_palette <- c(FDI = col_blue, Equity = col_red,
                  Debt = col_green, Other = "grey58")


# ==============================================================================
# ==============================================================================
#
#   SECTION 1 — Extension of Part I Results to 1980–2022
#
# ==============================================================================
# ==============================================================================

# ==============================================================================
# 1.1 — Data loading (all local files)
# ==============================================================================

# ── Extended BOP ──────────────────────────────────────────────────────────────
# bop_raw is truncated at 2005; bop_raw_ext covers 1975-2022.

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

# ── Extended EWN ──────────────────────────────────────────────────────────────

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

# ── Extended real GDP — local file, no API ─────────────────────────────────────

gdp_ext_raw <- read_wb("GDP_constant_1975_2022.csv") %>%
  rename(gdp_con = value) %>%
  filter(year <= y_ext_end, !is.na(gdp_con))

message(sprintf("Data loaded: %d country-years GDP, %d countries",
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
    if (length(v) < 2) NA_real_ else (last(v)-first(v))/1e3 },
    dm_exp_bn      = cum_dm_bn - cum_oca_bn,
    gdp_end_bn     = value_at(gdp_usd, year, y_ext_end) / 1e3,
    cum_oca_gdp    = cum_oca_bn / gdp_end_bn * 100,
    cum_dm_gdp     = cum_dm_bn  / gdp_end_bn * 100,
    dm_exp_gdp     = dm_exp_bn  / gdp_end_bn * 100,
    dm_exp_ratio   = dm_exp_gdp / 100,
    fdi_assets_gdp = value_at(fdi_assets_gdp, year, y_ext_end),
    fdi_liab_gdp   = value_at(fdi_liab_gdp,   year, y_ext_end),
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
    .groups = "drop"
  )

output_vol_cs <- cs_ext %>% select(iso3c, output_vol_ext = output_vol_hp)
panel_ext_aug <- panel_ext %>% left_join(output_vol_cs, by = "iso3c")


# ==============================================================================
# 1.3 — Extension figures
# ==============================================================================

# ── Figure II.1 — US dark matter stock extended to 2022 ───────────────────────

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

fig_us_ext <- us_ext %>%
  filter(year >= 1982, !is.na(dm_stock_bn)) %>%
  ggplot(aes(x = year)) +
  geom_col(aes(y = dm_stock_bn / 1e3), fill = col_blue, alpha = 0.22, width = 0.8) +
  geom_col(aes(y = dm_stock_bn / 1e3), fill = NA, colour = col_blue,
           alpha = 0.7, width = 0.8, linewidth = 0.3) +
  geom_line(aes(y = dm_stock_gdp / 10), colour = col_red, linewidth = 1) +
  geom_vline(xintercept = 2003.5, linetype = "dashed",
             colour = "grey50", linewidth = 0.6) +
  annotate("text", x = 2004.2,
           y = max(us_ext$dm_stock_gdp / 10, na.rm=TRUE) * 0.88,
           label = "H&S end", hjust = 0, size = 2.8, colour = "grey40") +
  scale_x_continuous(breaks = seq(1982, y_ext_end, 4)) +
  scale_y_continuous(name = "Trillions USD", labels = label_number(suffix = "T"),
                     sec.axis = sec_axis(~ . * 10, name = "% of US GDP",
                                         labels = label_number(suffix = "%"))) +
  labs(title    = paste0("US Stock of Dark Matter (1982\u2013", y_ext_end, ")"),
       subtitle = "Bars: $tn (left). Red line: % of GDP (right). Dashed = H&S end-date.",
       x = NULL) +
  theme(axis.title.y.right = element_text(colour = col_red, size = 9.5))

save_fig(fig_us_ext, "fig_us_dm_extended", part = "part_II")

# ── Figure II.2 — Global NFA positions updated through 2022 ───────────────────

world_gdp_ann <- panel_ext %>%
  filter(!is.na(gdp_usd), year >= 1985) %>%
  group_by(year) %>%
  summarise(world_gdp = sum(gdp_usd, na.rm=TRUE), .groups="drop")

global_ext <- panel_ext %>%
  filter(!is.na(nfa_dm), !is.na(gdp_usd), year >= 1985) %>%
  mutate(region = case_when(
    iso3c == "USA" ~ "United States", iso3c == "JPN" ~ "Japan",
    iso3c %in% eu  ~ "European Union", TRUE ~ "Rest of World")) %>%
  group_by(year, region) %>%
  summarise(nfa_dm_sum = sum(nfa_dm, na.rm=TRUE), .groups="drop") %>%
  left_join(world_gdp_ann, by = "year") %>%
  mutate(nfa_pct = nfa_dm_sum / world_gdp * 100)

fig_global_ext <- ggplot(global_ext,
                         aes(x = year, y = nfa_pct, colour = region, linetype = region)) +
  geom_hline(yintercept = 0, colour = col_grey, linewidth = 0.4, linetype="dashed") +
  geom_vline(xintercept = 2003.5, linetype="dashed", colour="grey60", linewidth=0.4) +
  geom_line(linewidth = 0.9) +
  scale_colour_manual(values = c("United States"="grey20","Japan"=col_red,
                                 "European Union"="grey50","Rest of World"="grey70")) +
  scale_linetype_manual(values = c("United States"="solid","Japan"="longdash",
                                   "European Union"="dashed","Rest of World"="dotted")) +
  scale_x_continuous(breaks = seq(1985, y_ext_end, 5)) +
  scale_y_continuous(labels = label_number(suffix="%")) +
  guides(colour=guide_legend(nrow=2), linetype=guide_legend(nrow=2)) +
  labs(title    = paste0("Global NFA Including Dark Matter (1985\u2013", y_ext_end, ")"),
       subtitle = "% of world GDP. Dashed = H&S end-date (2003).",
       x = NULL, y = "% of world GDP")

save_fig(fig_global_ext, "fig_global_nfa_extended", part = "part_II")

# ── Figure II.3 — Extended scatter: cumulative DM NFA vs official CA ──────────
# Extends Figure 3a of Part I to the 1980–2022 cross-section.

d_sc <- cs_ext %>%
  filter(iso3c %in% countries_109, !is.na(cum_dm_bn), !is.na(cum_oca_bn))

fig_scatter_ext <- d_sc %>%
  ggplot(aes(x = cum_dm_bn, y = cum_oca_bn, label = iso3c)) +
  geom_abline(slope=1, intercept=0, colour=col_grey, linetype="dashed", linewidth=0.5) +
  geom_point(colour = col_blue, size = 1.8, alpha = 0.75) +
  geom_text_repel(size=2.3, colour="grey25", segment.colour="grey70",
                  segment.size=0.3, box.padding=0.25, max.overlaps=30, seed=42) +
  scale_x_continuous(labels=label_comma()) +
  scale_y_continuous(labels=label_comma()) +
  labs(title    = paste0("Official CA vs. Dark-Matter NFA Change (", y_ext_start, "\u2013", y_ext_end, ")"),
       subtitle  = "Billions USD. Countries right of the 45\u00b0 line are net dark matter exporters.",
       x = "Change in NFA \u2014 dark matter ($bn)",
       y = "Cumulative official current account ($bn)")

save_fig(fig_scatter_ext, "fig_scatter_extended", part="part_II", h=6.5)

# ── Figure II.4 — Extended scatter: DM exports vs official CA (% of GDP) ──────
# Extends Figure 5b of Part I to the extended cross-section.

d_dm <- cs_ext %>%
  filter(iso3c %in% countries_109, !is.na(dm_exp_gdp), !is.na(cum_oca_gdp),
         abs(cum_oca_gdp) < quantile(abs(cum_oca_gdp), 0.97, na.rm=TRUE),
         abs(dm_exp_gdp)  < quantile(abs(dm_exp_gdp),  0.97, na.rm=TRUE))

fig_dm_vs_ca_ext <- d_dm %>%
  ggplot(aes(x = cum_oca_gdp, y = dm_exp_gdp, label = iso3c)) +
  geom_hline(yintercept=0, colour=col_grey, linewidth=0.4) +
  geom_vline(xintercept=0, colour=col_grey, linewidth=0.4) +
  geom_smooth(method="lm", se=TRUE, colour=col_red, linewidth=0.8,
              fill=col_red, alpha=0.08) +
  geom_point(colour=col_blue, size=1.8, alpha=0.75) +
  geom_text_repel(size=2.3, colour="grey25", segment.colour="grey70",
                  segment.size=0.3, box.padding=0.25, max.overlaps=30, seed=42) +
  labs(title    = paste0("Dark Matter Exports vs. Official CA (", y_ext_start, "\u2013", y_ext_end, ")"),
       subtitle = paste0("% of ", y_ext_end, " GDP. OLS fit with 95% confidence band."),
       x = paste0("Cumulative official CA (% of ", y_ext_end, " GDP)"),
       y = paste0("Cumulative DM exports (% of ", y_ext_end, " GDP)"))

save_fig(fig_dm_vs_ca_ext, "fig_dm_vs_ca_extended", part="part_II", h=6.5)


# ==============================================================================
# 1.4 — Extended tables (E1–E4b)
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

# ── Table E1 ──────────────────────────────────────────────────────────────────

d1e <- cs_ext %>% filter(iso3c %in% countries_109,
                         !is.na(cum_dm_bn), !is.na(cum_oca_bn))
t1e <- list(
  "Full"                = lm(cum_oca_bn ~ cum_dm_bn, d1e),
  "Excl. USA"           = lm(cum_oca_bn ~ cum_dm_bn, filter(d1e, iso3c!="USA")),
  "Excl. USA, GBR"      = lm(cum_oca_bn ~ cum_dm_bn,
                             filter(d1e, !iso3c %in% c("USA","GBR"))),
  "Excl. USA, GBR, JPN" = lm(cum_oca_bn ~ cum_dm_bn,
                             filter(d1e, !iso3c %in% c("USA","GBR","JPN")))
)
tex1e <- capture.output(stargazer(t1e[[1]], t1e[[2]], t1e[[3]], t1e[[4]],
                                  column.labels=names(t1e),
                                  title=paste0("Extended E1: Cumulative CA and Dark-Matter-Implied CA (",
                                               y_ext_start,"--",y_ext_end,")"),
                                  label="tab:E1_ext", dep.var.labels="Cumulative official CA (\\$bn)",
                                  covariate.labels=c("Dark-matter CA (\\$bn)","Constant"),
                                  omit.stat=c("f","ser","adj.rsq"),
                                  notes="Standard errors in parentheses. * p$<$0.10, ** p$<$0.05, *** p$<$0.01.",
                                  notes.align="l", style="aer", type="latex"))
compile_table(set_table_id(paste(tex1e,collapse="\n"),"E1"),
              "tableE1_extended_CA_vs_DM", part="part_II", table_number=2)

# ── Table E2 ──────────────────────────────────────────────────────────────────

d2e <- cs_ext %>% filter(iso3c %in% countries_109,
                         !is.na(dm_exp_gdp), !is.na(cum_oca_gdp))
t2e <- list(
  "Full"           = lm(dm_exp_gdp ~ cum_oca_gdp, d2e),
  "Excl. USA"      = lm(dm_exp_gdp ~ cum_oca_gdp, filter(d2e, iso3c!="USA")),
  "Excl. USA, GBR" = lm(dm_exp_gdp ~ cum_oca_gdp,
                        filter(d2e, !iso3c %in% c("USA","GBR")))
)
tex2e <- capture.output(stargazer(t2e[[1]], t2e[[2]], t2e[[3]],
                                  column.labels=names(t2e),
                                  title=paste0("Extended E2: Dark Matter Exports and the Official CA (",
                                               y_ext_start,"--",y_ext_end,")"),
                                  label="tab:E2_ext",
                                  dep.var.labels=paste0("DM exports (\\% of ",y_ext_end," GDP)"),
                                  covariate.labels=c(paste0("Official CA (\\% of ",y_ext_end," GDP)"),"Constant"),
                                  omit.stat=c("f","ser","adj.rsq"),
                                  notes="Standard errors in parentheses. * p$<$0.10, ** p$<$0.05, *** p$<$0.01.",
                                  notes.align="l", style="aer", type="latex"))
compile_table(set_table_id(paste(tex2e,collapse="\n"),"E2"),
              "tableE2_extended_DM_vs_CA", part="part_II", table_number=3)

# ── Tables E3a / E3b ──────────────────────────────────────────────────────────

d3e_79 <- cs_ext %>% filter(iso3c %in% countries_79, !is.na(dm_exp_ratio),
                            !is.na(fdi_assets_ratio), !is.na(fdi_liab_ratio), !is.na(output_vol_hp)) %>%
  mutate(across(c(dm_exp_ratio,fdi_assets_ratio,fdi_liab_ratio,output_vol_hp),winsor))
d3e_99 <- cs_ext %>% filter(iso3c %in% countries_99, !is.na(dm_exp_ratio),
                            !is.na(fdi_assets_ratio), !is.na(fdi_liab_ratio), !is.na(output_vol_hp)) %>%
  mutate(across(c(dm_exp_ratio,fdi_assets_ratio,fdi_liab_ratio,output_vol_hp),winsor))
d3e_ind <- cs_ext %>% filter(iso3c %in% industrial, !is.na(dm_exp_ratio),
                             !is.na(fdi_assets_ratio), !is.na(fdi_liab_ratio), !is.na(output_vol_hp)) %>%
  mutate(across(c(dm_exp_ratio,fdi_assets_ratio,fdi_liab_ratio,output_vol_hp),winsor))

t3e <- list(
  "(i)"  =lm(dm_exp_ratio~fdi_assets_ratio+fdi_liab_ratio+output_vol_hp,data=d3e_79),
  "(ii)" =lm(dm_exp_ratio~fdi_assets_ratio+fdi_liab_ratio+output_vol_hp+rule_of_law,data=d3e_79),
  "(iii)"=lm(dm_exp_ratio~fdi_assets_ratio+fdi_liab_ratio+output_vol_hp+rule_of_law+opec+hipc,data=d3e_79),
  "(iv)" =lm(dm_exp_ratio~fdi_assets_ratio+fdi_liab_ratio+output_vol_hp,data=d3e_99),
  "(v)"  =lm(dm_exp_ratio~fdi_assets_ratio+fdi_liab_ratio+output_vol_hp+rule_of_law,data=d3e_99),
  "(vi)" =lm(dm_exp_ratio~fdi_assets_ratio+fdi_liab_ratio+output_vol_hp+rule_of_law+opec+hipc,data=d3e_99),
  "(vii)"=lm(dm_exp_ratio~fdi_assets_ratio+fdi_liab_ratio+output_vol_hp,data=d3e_ind)
)
message(sprintf("E3 obs: %s", paste(sapply(t3e,nobs),collapse="|")))

cn_e3 <- c("fdi_assets_ratio"="FDI assets / GDP","fdi_liab_ratio"="FDI liabilities / GDP",
           "output_vol_hp"="Output volatility","rule_of_law"="Rule of Law",
           "opec"="OPEC dummy","hipc"="HIPC dummy")
note_e3 <- paste0("Dep. var.: cum. DM exports ",y_ext_start,"--",y_ext_end,
                  " / ",y_ext_end," GDP. R\\&D excluded (WDI coverage too thin post-2005). ",
                  "Variables winsorised at 1\\%. * p$<$0.10, ** p$<$0.05, *** p$<$0.01.")

compile_table(wrap_ext(
  modelsummary(t3e[1:3],stars=c("*"=0.1,"**"=0.05,"***"=0.01),fmt="%.3f",
               coef_rename=cn_e3,gof_omit="AIC|BIC|Log|F|RMSE",output="latex_tabular"),
  paste0("Extended E3a: Sources of Dark Matter, 79-Country Sample (",y_ext_start,"--",y_ext_end,")"),
  "tab:E3a_ext", note_e3, "E3a"),
  "tableE3a_extended_sources_79", part="part_II", table_number=4, fit_width=TRUE)

compile_table(wrap_ext(
  modelsummary(t3e[4:7],stars=c("*"=0.1,"**"=0.05,"***"=0.01),fmt="%.3f",
               coef_rename=cn_e3,gof_omit="AIC|BIC|Log|F|RMSE",output="latex_tabular"),
  paste0("Extended E3b: Sources of Dark Matter, 99-Country \\& Industrial Samples (",y_ext_start,"--",y_ext_end,")"),
  "tab:E3b_ext", note_e3, "E3b"),
  "tableE3b_extended_sources_99", part="part_II", table_number=4, fit_width=TRUE)

# ── Tables E4 / E4b ───────────────────────────────────────────────────────────

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

d4e  <- panel_ext_aug %>% filter(iso3c %in% countries_79, year >= y_ext_start,
                                 year <= y_ext_end, !is.na(ca_dm_gdp), !is.na(fdi_liab_gdp),
                                 !is.na(fdi_assets_gdp), !is.na(output_vol_ext))
d4be <- panel_ext_aug %>% filter(iso3c %in% countries_79, year >= y_ext_start,
                                 year <= y_ext_end, !is.na(dm_exp_flow_gdp), !is.na(fdi_liab_gdp),
                                 !is.na(fdi_assets_gdp), !is.na(output_vol_ext))

compile_table(wrap_ext(
  modelsummary(make_fe_models(d4e,"ca_dm_gdp"),stars=c("*"=0.1,"**"=0.05,"***"=0.01),
               fmt="%.4f",gof_omit="AIC|BIC|Log|Adj|Within|FE",coef_rename=cn_e4,output="latex_tabular"),
  paste0("Extended E4: Panel — Dark-Matter-Implied CA (",y_ext_start,"--",y_ext_end,")"),
  "tab:E4_ext",
  paste0("Dep. var.: $CA^{DM}_{it}$/GDP. Pooled OLS and country FE. ",
         "* p$<$0.10, ** p$<$0.05, *** p$<$0.01."),
  "E4"),
  "tableE4_panel_CA_DM", part="part_II", landscape=TRUE, table_number=5, fit_width=TRUE)

compile_table(wrap_ext(
  modelsummary(make_fe_models(d4be,"dm_exp_flow_gdp"),stars=c("*"=0.1,"**"=0.05,"***"=0.01),
               fmt="%.4f",gof_omit="AIC|BIC|Log|Adj|Within|FE",coef_rename=cn_e4,output="latex_tabular"),
  paste0("Extended E4b: Panel — Annual Dark Matter Exports (",y_ext_start,"--",y_ext_end,")"),
  "tab:E4b_ext",
  paste0("Dep. var.: $(CA^{DM}_{it}-CA^{\\mathrm{off}}_{it})$/GDP. ",
         "* p$<$0.10, ** p$<$0.05, *** p$<$0.01."),
  "E4b"),
  "tableE4b_panel_DM_exports", part="part_II", landscape=TRUE, table_number=6, fit_width=TRUE)

message("Section 1 done.")


# ==============================================================================
# ==============================================================================
#
#   SECTION 2 — Beyond the 5% Assumption: Component-Specific NFA
#
# ==============================================================================
# ==============================================================================

# ==============================================================================
# 2.1 — NII sub-components from the extended BOP file
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

nii_comp_long <- map_dfr(names(codes_comp), function(vn) {
  bop_raw_ext %>%
    rename(series_code = SERIES_CODE) %>%
    mutate(iso3c = str_extract(series_code,"^[^.]+"),
           indicator = str_remove(series_code,"^[^.]+\\.")) %>%
    filter(indicator == codes_comp[[vn]]) %>%
    select(iso3c, all_of(yr_comp)) %>%
    pivot_longer(-iso3c, names_to="year", values_to="value") %>%
    mutate(year=as.integer(year),
           value=suppressWarnings(as.numeric(value)), component=vn)
}) %>% pivot_wider(names_from=component, values_from=value)


# ==============================================================================
# 2.2 — Assign scenario rates and compute NFA components
# ==============================================================================

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
  left_join(EWN_ext %>% select(iso3c, year, nfa_official, gdp_usd,
                               net_fdi, net_equity, net_debt),
            by = c("iso3c","year")) %>%
  filter(!is.na(gdp_usd)) %>%
  mutate(
    # Scenario A components (% of GDP)
    nfa_fdi_A_gdp    = (nii_fdi    / r_fdi_A)    / gdp_usd * 100,
    nfa_equity_A_gdp = (nii_equity / r_equity_A)  / gdp_usd * 100,
    nfa_debt_A_gdp   = (nii_debt   / r_debt_A)    / gdp_usd * 100,
    nfa_other_A_gdp  = (nii_other  / r_other_A)   / gdp_usd * 100,
    nfa_precise_A_gdp = nfa_fdi_A_gdp + nfa_equity_A_gdp +
      nfa_debt_A_gdp + nfa_other_A_gdp,
    
    # Scenario B total (% of GDP) — components in absolute for DM decomp
    nfa_fdi_B    = nii_fdi    / r_fdi_B,
    nfa_equity_B = nii_equity / r_equity_B,
    nfa_debt_B   = nii_debt   / r_debt_B,
    nfa_other_B  = nii_other  / r_other_B,
    nfa_precise_B_gdp = (nfa_fdi_B + nfa_equity_B +
                           nfa_debt_B + nfa_other_B) / gdp_usd * 100,
    
    # H&S flat 5% and official NIIP (% of GDP)
    nfa_hs_gdp       = (nii_total / r) / gdp_usd * 100,
    nfa_official_gdp = nfa_official     / gdp_usd * 100,
    
    # Dark matter (% of GDP) under both scenarios
    dm_A_gdp = nfa_precise_A_gdp - nfa_official_gdp,
    dm_B_gdp = nfa_precise_B_gdp - nfa_official_gdp,
    
    # Component dark matter Scenario A (% of GDP)
    nfa_fdi_A    = nii_fdi    / r_fdi_A,
    nfa_equity_A = nii_equity / r_equity_A,
    nfa_debt_A   = nii_debt   / r_debt_A,
    nfa_other_A  = nii_other  / r_other_A,
    dm_fdi_A_gdp    = (nfa_fdi_A    - net_fdi)    / gdp_usd * 100,
    dm_equity_A_gdp = (nfa_equity_A - net_equity)  / gdp_usd * 100,
    dm_debt_A_gdp   = (nfa_debt_A   - net_debt)    / gdp_usd * 100,
    dm_other_A_gdp  = dm_A_gdp - dm_fdi_A_gdp - dm_equity_A_gdp - dm_debt_A_gdp,
    
    # Component dark matter Scenario B (% of GDP)
    dm_fdi_B_gdp    = (nfa_fdi_B    - net_fdi)    / gdp_usd * 100,
    dm_equity_B_gdp = (nfa_equity_B - net_equity)  / gdp_usd * 100,
    dm_debt_B_gdp   = (nfa_debt_B   - net_debt)    / gdp_usd * 100,
    dm_other_B_gdp  = dm_B_gdp - dm_fdi_B_gdp - dm_equity_B_gdp - dm_debt_B_gdp
  )

# EU aggregate (all advanced → Scenario B = Scenario A)
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
    nfa_precise_B_gdp = nfa_precise_A_gdp,   # EU: A = B
    nfa_hs_gdp        = (nii_total / r) / gdp_usd * 100,
    nfa_official_gdp  = nfa_official   / gdp_usd * 100,
    dm_A_gdp = nfa_precise_A_gdp - nfa_official_gdp,
    dm_B_gdp = dm_A_gdp,
    dm_fdi_A_gdp    = (nfa_fdi_A    - net_fdi)    / gdp_usd * 100,
    dm_equity_A_gdp = (nfa_equity_A - net_equity)  / gdp_usd * 100,
    dm_debt_A_gdp   = (nfa_debt_A   - net_debt)    / gdp_usd * 100,
    dm_other_A_gdp  = dm_A_gdp - dm_fdi_A_gdp - dm_equity_A_gdp - dm_debt_A_gdp,
    dm_fdi_B_gdp    = dm_fdi_A_gdp, dm_equity_B_gdp = dm_equity_A_gdp,
    dm_debt_B_gdp   = dm_debt_A_gdp, dm_other_B_gdp = dm_other_A_gdp
  )

nfa_plot <- bind_rows(
  nfa_decomp %>% filter(iso3c %in% showcase),
  eu_agg
) %>%
  filter(!is.na(nfa_precise_A_gdp))


# ==============================================================================
# 2.3 — Combined 2×2 figure: stacked areas + two scenario lines
#
# Each panel shows (% of GDP):
#   - Stacked bars: Scenario A components (FDI, equity, debt, other)
#     The height of all stacked bars at year t = NFA_precise_A at year t
#   - Thin grey dotted:  official NIIP (EWN)     — discrete, background
#   - Thin grey dashed:  H&S flat r=5% NFA       — discrete, background
#   - Black solid line:  NFA Scenario A total     — main (same as stacked top)
#   - Blue longdash:     NFA Scenario B total     — differs only USA/CHN
# ==============================================================================

make_panel_plot <- function(iso_sel, panel_label) {
  
  df <- bind_rows(nfa_decomp %>% filter(iso3c == iso_sel), eu_agg) %>%
    filter(iso3c == iso_sel, year >= 1993, !is.na(nfa_precise_A_gdp))
  if (nrow(df) == 0) return(NULL)
  
  # Stacked bars: Scenario A components
  comp_df <- df %>%
    select(year, FDI = nfa_fdi_A_gdp, Equity = nfa_equity_A_gdp,
           Debt = nfa_debt_A_gdp, Other = nfa_other_A_gdp) %>%
    pivot_longer(-year, names_to="comp", values_to="pct") %>%
    mutate(comp = factor(comp, levels=c("Other","Debt","Equity","FDI")))
  
  refs <- df %>%
    select(year, nfa_A = nfa_precise_A_gdp, nfa_B = nfa_precise_B_gdp,
           niip = nfa_official_gdp, hs = nfa_hs_gdp)
  
  ggplot() +
    geom_hline(yintercept=0, colour="grey50", linewidth=0.35) +
    # Stacked annual bars: each component's contribution to NFA_A
    geom_col(data=comp_df, aes(x=year, y=pct, fill=comp),
             position="stack", width=0.92, alpha=0.70) +
    # Official NIIP — discrete: thin dotted grey
    geom_line(data=refs, aes(x=year, y=niip,
                             colour="NIIP officielle", linetype="NIIP officielle"),
              linewidth=0.60, alpha=0.75) +
    # H&S 5% — discrete: thin dashed light grey
    geom_line(data=refs, aes(x=year, y=hs,
                             colour="H&S (r=5%)", linetype="H&S (r=5%)"),
              linewidth=0.50, alpha=0.65) +
    # NFA Scenario A total — solid black (= top of stacked bars)
    geom_line(data=refs, aes(x=year, y=nfa_A,
                             colour="NFA Scén. A", linetype="NFA Scén. A"),
              linewidth=1.00) +
    # NFA Scenario B total — blue longdash
    geom_line(data=refs, aes(x=year, y=nfa_B,
                             colour="NFA Scén. B", linetype="NFA Scén. B"),
              linewidth=0.90) +
    scale_fill_manual(values=comp_palette, name="Composantes") +
    scale_colour_manual(
      values=c("NIIP officielle"="grey40","H&S (r=5%)"="grey68",
               "NFA Scén. A"="black","NFA Scén. B"=col_blue),
      name=NULL) +
    scale_linetype_manual(
      values=c("NIIP officielle"="dotted","H&S (r=5%)"="dashed",
               "NFA Scén. A"="solid","NFA Scén. B"="longdash"),
      name=NULL) +
    scale_x_continuous(breaks=seq(1993, y_ext_end, 6)) +
    scale_y_continuous(labels=label_number(suffix="%")) +
    labs(title=panel_label, x=NULL, y="% du PIB") +
    theme_paper +
    theme(plot.title      = element_text(size=9.5, face="bold"),
          legend.position = "none",
          axis.text.x     = element_text(size=7.5),
          axis.text.y     = element_text(size=7.5),
          axis.title.y    = element_text(size=8),
          plot.margin     = margin(4,6,4,4))
}

p_usa <- make_panel_plot("USA", "États-Unis")
p_jpn <- make_panel_plot("JPN", "Japon")
p_chn <- make_panel_plot("CHN", "Chine")
p_eu  <- make_panel_plot("EU",  "Union européenne")

# Build shared legends via a dummy panel with full legend visible
dummy <- make_panel_plot("USA", "") +
  theme(legend.position="bottom",
        legend.box="horizontal",
        legend.spacing.x=unit(0.4,"cm"))

leg_fill <- cowplot::get_legend(
  dummy + guides(colour="none", linetype="none"))
leg_lines <- cowplot::get_legend(
  dummy + guides(fill="none"))

# Assemble 2×2 with collected legends below
fig_nfa_2x2 <- (p_usa | p_jpn) / (p_chn | p_eu) +
  plot_annotation(
    title    = "NFA par composante et scénario de taux — USA, Japon, Chine, UE",
    subtitle = paste0(
      "Aires empilées = composantes NFA, Scénario A (taux G&R 2006 universels). ",
      "Ligne noire = total Scén. A. Ligne bleue pointillée = total Scén. B ",
      "(taux différenciés: privilège/avancé/émergent). ",
      "Pointillé gris = NIIP officielle (EWN). Tirets gris = NFA H&S (r=5%). ",
      "En % du PIB, 1993\u2013", y_ext_end, "."),
    theme = theme(
      plot.title    = element_text(size=11, face="bold"),
      plot.subtitle = element_text(size=8.5, colour="grey35"))
  )

# Save the 2×2 combined figure
save_fig(fig_nfa_2x2, "fig_nfa_decomp_4panels",
         part="part_II", w=14, h=10)

# Also save individual panels for potential paper use
save_fig(p_usa + theme(legend.position="none"),
         "fig_comp_nfa_usa", part="part_II", w=7, h=5)
save_fig(p_jpn + theme(legend.position="none"),
         "fig_comp_nfa_jpn", part="part_II", w=7, h=5)
save_fig(p_chn + theme(legend.position="none"),
         "fig_comp_nfa_chn", part="part_II", w=7, h=5)
save_fig(p_eu  + theme(legend.position="none"),
         "fig_comp_nfa_eu",  part="part_II", w=7, h=5)


# ==============================================================================
# 2.4 — Robustness table: three NFA measures and two dark matter estimates
# ==============================================================================

summ_window <- (y_ext_end - 9):y_ext_end
tdir <- here("code","output","tables","part_II")

summ_raw <- bind_rows(
  nfa_decomp %>% filter(iso3c %in% showcase), eu_agg
) %>%
  filter(year %in% summ_window) %>%
  group_by(iso3c) %>%
  summarise(
    label      = panel_labels[iso3c[1]],
    grp        = safe_first(grp),
    niip       = round(mean(nfa_official_gdp,  na.rm=TRUE), 1),
    nfa_hs     = round(mean(nfa_hs_gdp,         na.rm=TRUE), 1),
    nfa_A      = round(mean(nfa_precise_A_gdp,  na.rm=TRUE), 1),
    nfa_B      = round(mean(nfa_precise_B_gdp,  na.rm=TRUE), 1),
    dm_A       = round(mean(dm_A_gdp,            na.rm=TRUE), 1),
    dm_B       = round(mean(dm_B_gdp,            na.rm=TRUE), 1),
    diff_BA    = round(dm_B - dm_A,               1),
    .groups="drop"
  )

tex_summ <- kableExtra::kbl(
  summ_raw %>% select(label, grp, niip, nfa_hs, nfa_A, nfa_B, dm_A, dm_B, diff_BA),
  format="latex", booktabs=TRUE, linesep="",
  col.names=c("Entité","Groupe","NIIP","H\\&S (5\\%)","NFA$_A$","NFA$_B$",
              "DM$_A$","DM$_B$","B$-$A"),
  caption=paste0("NFA et dark matter sous deux scénarios de taux, moy. ",
                 min(summ_window),"--",max(summ_window)," (\\% du PIB)"),
  label="tab:summ_scen", escape=FALSE,
  align=c("l","l","r","r","r","r","r","r","r")
) %>%
  kableExtra::kable_styling(latex_options="hold_position", font_size=10) %>%
  kableExtra::add_header_above(
    c(" "=2,"Mesures NFA"=4,"Dark matter"=3),
    bold=TRUE, line=TRUE, escape=FALSE) %>%
  kableExtra::footnote(
    general=paste0(
      "Scén. A: taux G\\\\&R (2006) universels. ",
      "Scén. B: Gourinchas, Rey \\\\& Govillot (2017). ",
      "Privilège (USA, CHE, GBR): r\\\\textsubscript{FDI}=9\\\\%, r\\\\textsubscript{dette}=2\\\\%. ",
      "Émergents (CHN): r\\\\textsubscript{FDI}=6\\\\%, r\\\\textsubscript{dette}=5\\\\%. ",
      "Un r plus \\\\'{e}lev\\\\'{e} implique un stock capitalis\\\\'{e} plus faible pour le m\\\\^{e}me flux de revenu."),
    general_title="\\\\textit{Notes:} ", escape=FALSE)

writeLines(paste0(
  "\\documentclass[11pt]{article}\n\\usepackage{booktabs,xcolor,colortbl,caption,array,graphicx,amsmath}\n",
  "\\usepackage[landscape,margin=1.2cm]{geometry}\n",
  "\\begin{document}\\small\\setcounter{table}{6}\n", tex_summ, "\n\\end{document}"),
  file.path(tdir, "tableSumm_scenarios.tex"))
old <- setwd(tdir)
tryCatch(tinytex::pdflatex("tableSumm_scenarios.tex"),
         error=function(e) message("LaTeX: ",e$message))
setwd(old)

message("Section 2 done.")


# ==============================================================================
# ==============================================================================
#
#   SECTION 3 — Decomposing Dark Matter by Source
#
# ==============================================================================
# ==============================================================================

# ==============================================================================
# 3.1 — Combined 2×2 figure: DM decomposed by asset class, both scenario totals
# ==============================================================================
#
# Stacked bars show the Scenario A decomposition of dark matter by component.
# Lines show total dark matter under A (black) and B (blue).

make_dm_panel <- function(iso_sel, panel_label) {
  
  df <- bind_rows(nfa_decomp %>% filter(iso3c==iso_sel), eu_agg) %>%
    filter(iso3c==iso_sel, year>=1993, !is.na(dm_A_gdp))
  if (nrow(df) == 0) return(NULL)
  
  comp_df <- df %>%
    select(year, FDI=dm_fdi_A_gdp, Equity=dm_equity_A_gdp,
           Debt=dm_debt_A_gdp, Other=dm_other_A_gdp) %>%
    pivot_longer(-year, names_to="comp", values_to="pct") %>%
    mutate(comp=factor(comp, levels=c("Other","Debt","Equity","FDI")))
  
  totals <- df %>% select(year, dm_A=dm_A_gdp, dm_B=dm_B_gdp)
  
  ggplot() +
    geom_hline(yintercept=0, colour="grey45", linewidth=0.45) +
    geom_col(data=comp_df, aes(x=year, y=pct, fill=comp),
             position="stack", width=0.92, alpha=0.72) +
    geom_line(data=totals, aes(x=year, y=dm_A,
                               colour="DM total Scén. A", linetype="DM total Scén. A"),
              linewidth=1.00) +
    geom_line(data=totals, aes(x=year, y=dm_B,
                               colour="DM total Scén. B", linetype="DM total Scén. B"),
              linewidth=0.90) +
    scale_fill_manual(values=comp_palette, name="Composante") +
    scale_colour_manual(
      values=c("DM total Scén. A"="black","DM total Scén. B"=col_blue),
      name=NULL) +
    scale_linetype_manual(
      values=c("DM total Scén. A"="solid","DM total Scén. B"="longdash"),
      name=NULL) +
    scale_x_continuous(breaks=seq(1993, y_ext_end, 6)) +
    scale_y_continuous(labels=label_number(suffix="%")) +
    labs(title=panel_label, x=NULL, y="Dark matter (% du PIB)") +
    theme_paper +
    theme(plot.title     =element_text(size=9.5, face="bold"),
          legend.position="none",
          axis.text.x    =element_text(size=7.5),
          axis.text.y    =element_text(size=7.5),
          plot.margin    =margin(4,6,4,4))
}

dm_usa <- make_dm_panel("USA", "États-Unis")
dm_jpn <- make_dm_panel("JPN", "Japon")
dm_chn <- make_dm_panel("CHN", "Chine")
dm_eu  <- make_dm_panel("EU",  "Union européenne")

fig_dm_2x2 <- (dm_usa | dm_jpn) / (dm_chn | dm_eu) +
  plot_annotation(
    title    = "Décomposition de la dark matter par classe d'actif — USA, Japon, Chine, UE",
    subtitle = paste0(
      "Aires empilées = composantes DM, Scénario A (taux G&R 2006). ",
      "DM\\u2082 = NFA\\u2082/r\\u2082 \\u2212 stock officiel (EWN). ",
      "Ligne noire = total Scén. A. Ligne bleue = total Scén. B. % du PIB, 1993\u2013", y_ext_end, "."),
    theme=theme(plot.title=element_text(size=11, face="bold"),
                plot.subtitle=element_text(size=8.5, colour="grey35"))
  )

save_fig(fig_dm_2x2, "fig_dm_decomp_4panels", part="part_II", w=14, h=10)


# ==============================================================================
# 3.2 — Cross-country dark matter decomposition table (both scenarios)
# ==============================================================================

dm_tab_raw <- bind_rows(
  nfa_decomp %>% filter(iso3c %in% showcase), eu_agg
) %>%
  filter(year %in% summ_window) %>%
  group_by(iso3c) %>%
  summarise(
    label  = panel_labels[iso3c[1]],
    dmA    = round(mean(dm_A_gdp,       na.rm=TRUE), 1),
    fdiA   = round(mean(dm_fdi_A_gdp,   na.rm=TRUE), 1),
    eqA    = round(mean(dm_equity_A_gdp,na.rm=TRUE), 1),
    debtA  = round(mean(dm_debt_A_gdp,  na.rm=TRUE), 1),
    otherA = round(mean(dm_other_A_gdp, na.rm=TRUE), 1),
    dmB    = round(mean(dm_B_gdp,       na.rm=TRUE), 1),
    fdiB   = round(mean(dm_fdi_B_gdp,   na.rm=TRUE), 1),
    eqB    = round(mean(dm_equity_B_gdp,na.rm=TRUE), 1),
    debtB  = round(mean(dm_debt_B_gdp,  na.rm=TRUE), 1),
    otherB = round(mean(dm_other_B_gdp, na.rm=TRUE), 1),
    .groups="drop"
  )

tex_dm <- kableExtra::kbl(
  dm_tab_raw %>% select(-iso3c),
  format="latex", booktabs=TRUE, linesep="",
  col.names=c("Entité","Total","FDI","Actions","Dette","Autres",
              "Total","FDI","Actions","Dette","Autres"),
  caption=paste0("Dark matter par classe d'actif sous deux scénarios, moy. ",
                 min(summ_window),"--",max(summ_window)," (\\% du PIB)"),
  label="tab:dm_decomp", escape=FALSE,
  align=c("l","r","r","r","r","r","r","r","r","r","r")
) %>%
  kableExtra::kable_styling(latex_options="hold_position", font_size=9.5) %>%
  kableExtra::add_header_above(
    c(" "=1,"Scénario A: taux G\\\\&R universels"=5,
      "Scénario B: taux différenciés"=5),
    bold=TRUE, line=TRUE, escape=FALSE) %>%
  kableExtra::footnote(
    general=paste0(
      "DM par composante = NFA implicite (NII/r) $-$ stock officiel (EWN). ",
      "Une part FDI dominante valide le canal \\\\textit{knowledge} de H\\\\&S; ",
      "une part dette dominante valide le canal actif s\\\\^{u}r (Caballero, Farhi \\\\& Gourinchas 2017). ",
      "Pour les \\\\'{e}conomies avancées non-privilège et l'UE, Scén. A = Scén. B."),
    general_title="\\\\textit{Notes:} ", escape=FALSE)

writeLines(paste0(
  "\\documentclass[11pt]{article}\n\\usepackage{booktabs,xcolor,colortbl,caption,array,graphicx,amsmath}\n",
  "\\usepackage[landscape,margin=1cm]{geometry}\n",
  "\\begin{document}\\small\\setcounter{table}{7}\n", tex_dm, "\n\\end{document}"),
  file.path(tdir, "tableDM_decomp_both_scenarios.tex"))
old <- setwd(tdir)
tryCatch(tinytex::pdflatex("tableDM_decomp_both_scenarios.tex"),
         error=function(e) message("LaTeX: ",e$message))
setwd(old)

message("Section 3 done.")
message("\nPart II complete.")
message("Figures : code/output/figures/part_II/")
message("Tables  : code/output/tables/part_II/")

# ==============================================================================
# OUTPUT SUMMARY
# ──────────────
# FIGURES (part_II)
#   fig_us_dm_extended         US dark matter stock 1982–2022
#   fig_global_nfa_extended    Global NFA by region 1985–2022
#   fig_scatter_extended       Cross-section scatter DM vs official CA (extended)
#   fig_dm_vs_ca_extended      Dark matter exports vs official CA (extended)
#   fig_nfa_decomp_4panels     ★ MAIN: 2×2 NFA decomposition (stacked areas)
#   fig_comp_nfa_{usa,jpn,chn,eu}  Individual panels for paper
#   fig_dm_decomp_4panels      ★ MAIN: 2×2 DM decomposition by component
#
# TABLES (part_II)
#   tableE1_extended_CA_vs_DM
#   tableE2_extended_DM_vs_CA
#   tableE3a_extended_sources_79
#   tableE3b_extended_sources_99
#   tableE4_panel_CA_DM
#   tableE4b_panel_DM_exports
#   tableSumm_scenarios        Robustness: three NFA measures, two DM estimates
#   tableDM_decomp_both_scenarios  Component breakdown, both scenarios
# ==============================================================================


# ==============================================================================
#
# Summary — Part II
#
# Data coverage:
#   Extended BOP: same file as Part I (Current_account_primary_income_1975_2022.csv),
#   filtered to 1975-2022 rather than 1975-2005.
#
#   Extended EWN: same file as Part I (EWN-dataset-year-end-2024_4.9.26.xlsx).
#
#   Extended real GDP: downloaded via WDI::WDI(NY.GDP.MKTP.KD) for 1975-2022.
#   This supplements the original GDP_constant_1975_2005.csv which ends at 2005.
#
# LaTeX note: all ampersands (&) inside LaTeX strings use \\& to avoid
# the "misplaced alignment tab character" error. This applies to captions,
# notes, and coefficient labels wherever & appears.
#
# ==============================================================================































# ==============================================================================
# ==============================================================================
#
#                 Part III - Original Economic Extensions
#
# ==============================================================================
# ==============================================================================

#
# Three new tests that go beyond extending H&S's time window.
# Each targets a specific theoretical mechanism that H&S identify but either
# leave static or fail to confirm empirically:
#
#   Extension 1  — The Safe-Asset Cycle (VIX)
#   Extension 1b — Financial vs. Geopolitical Risk (VIX vs. GPR horse race)
#   Extension 2  — Intangible Capital Intensity
#   Extension 3  — Financial Development and Safe Asset Production
#   Extension 4  — NII Decomposition by Asset Class
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

# ── WDI caching helper ─────────────────────────────────────────────────────────
#
# We download WDI data once and save it as a CSV so subsequent runs do not
# require an internet connection. If the file already exists we just read it.

fetch_or_load_wdi <- function(filename, indicators, start, end) {
  path <- here("code", "data", filename)
  
  if (file.exists(path)) {
    message("  Loading from cache: ", filename)
    df <- read_csv(path, show_col_types = FALSE)
    
    if (!"iso3c" %in% names(df)) {
      message("  Cache file is not in the expected format. Deleting and re-downloading...")
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
# Extension 1 — The Safe-Asset Cycle (VIX)
#
# H&S model the insurance channel as a static cross-sectional relationship:
# countries with more volatile output pay a risk premium to stable ones.
# But if insurance is what it claims to be, its price should move over time —
# rising when global risk appetite collapses and falling when it recovers.
#
# The Global Financial Cycle literature (Rey 2013 JME; Miranda-Agrippino & Rey
# 2020 AER; Caballero, Farhi & Gourinchas 2017 QJE) gives us the instrument:
# the VIX, which captures global risk appetite in a single daily time series.
#
# Our prediction is simple: when the VIX spikes, safe-haven countries should
# export more dark matter — the insurance premium they charge goes up.
# Commodity exporters and HIPC countries should move in the opposite direction.
#
# We test this with a panel regression interacting ΔlogVIX with country group
# dummies, progressively absorbing country and year fixed effects. The cleanest
# specification (column 5) includes two-way FE so the aggregate VIX level is
# absorbed by year dummies, identifying only the differential response.
#
# Data: vix_daily.csv — FRED series VIXCLS, daily since 1990.
#       We aggregate to annual means and take log-differences.
#
# ==============================================================================

message("\n── Extension 1: Safe-Asset Cycle (VIX) ──────────────────────────────────")

# ── Load and aggregate VIX ────────────────────────────────────────────────────

vix_annual <- read_csv(
  here("code", "data", "vix_daily.csv"),
  show_col_types = FALSE
) %>%
  rename(date = observation_date, vix = VIXCLS) %>%
  mutate(date = as.Date(date),
         year = as.integer(format(date, "%Y")),
         vix  = suppressWarnings(as.numeric(vix))) %>%
  filter(!is.na(vix), year >= 1990, year <= y_ext_end) %>%
  group_by(year) %>%
  summarise(vix = mean(vix, na.rm = TRUE), .groups = "drop") %>%
  arrange(year) %>%
  mutate(d_log_vix = c(NA_real_, diff(log(vix))))

message(sprintf("  VIX: %d annual obs (%d–%d)",
                nrow(vix_annual), min(vix_annual$year), max(vix_annual$year)))

# ── Merge VIX into the extended panel ─────────────────────────────────────────

safe_havens <- c("USA", "CHE", "DEU", "GBR", "JPN", "NLD", "AUT", "DNK", "NOR")

panel_vix <- panel_ext %>%
  filter(year >= 1990, year <= y_ext_end) %>%
  left_join(vix_annual, by = "year") %>%
  mutate(
    safe_d     = as.integer(iso3c %in% safe_havens),
    vix_x_safe = d_log_vix * safe_d,
    vix_x_opec = d_log_vix * opec_d
  )

# ── Country-level VIX betas ────────────────────────────────────────────────────
#
# For each country we run a simple OLS of annual dark matter exports on ΔlogVIX,
# with heteroskedasticity-robust standard errors (HC1). We require at least
# 12 observations to get a stable estimate.

compute_vix_beta <- function(df) {
  df <- df %>% filter(!is.na(dm_exp_flow_gdp), !is.na(d_log_vix))
  
  if (nrow(df) < 12) {
    return(tibble(
      beta_vix = NA_real_,
      se_vix   = NA_real_,
      t_vix    = NA_real_,
      n_obs    = nrow(df),
      r2       = NA_real_
    ))
  }
  
  fit <- lm(dm_exp_flow_gdp ~ d_log_vix, data = df)
  
  vcv <- tryCatch(
    sandwich::vcovHC(fit, type = "HC1"),
    error = function(e) vcov(fit)
  )
  
  tibble(
    beta_vix = coef(fit)[["d_log_vix"]],
    se_vix   = sqrt(vcv["d_log_vix", "d_log_vix"]),
    t_vix    = coef(fit)[["d_log_vix"]] / sqrt(vcv["d_log_vix", "d_log_vix"]),
    n_obs    = nrow(df),
    r2       = summary(fit)$r.squared
  )
}

vix_betas <- panel_vix %>%
  filter(iso3c %in% countries_79) %>%
  group_by(iso3c) %>%
  group_modify(~ compute_vix_beta(.x)) %>%
  ungroup() %>%
  filter(!is.na(beta_vix)) %>%
  mutate(
    group = case_when(
      iso3c %in% safe_havens ~ "Safe haven",
      iso3c %in% opec        ~ "OPEC",
      iso3c %in% hipc        ~ "HIPC",
      TRUE                   ~ "Other"),
    significant = abs(t_vix) > 1.645   # 10% one-sided
  )

# ── Figure E1a — Country-level VIX betas (ranked bar chart) ──────────────────
#
# Bars are faded when the coefficient is not significant at the 10% level.
# Safe-haven countries should cluster at the top (positive betas);
# commodity exporters at the bottom (negative betas).

fig_e1_bars <- vix_betas %>%
  ggplot(aes(x = reorder(iso3c, beta_vix), y = beta_vix,
             fill = group, alpha = significant)) +
  geom_col(width = 0.75) +
  geom_hline(yintercept = 0, colour = "grey20", linewidth = 0.5) +
  scale_fill_manual(
    values = c("Safe haven" = col_blue, "OPEC" = col_red,
               "HIPC" = "grey55", "Other" = "grey78"),
    name = NULL) +
  scale_alpha_manual(values = c("TRUE" = 0.92, "FALSE" = 0.28), guide = "none") +
  coord_flip() +
  labs(
    title    = "Extension 1 — Country VIX Beta: Dark Matter Exports vs. Global Risk",
    subtitle = paste0("OLS coefficient of annual dark matter exports/GDP on \u0394log(VIX), ",
                      "1990\u2013", y_ext_end, ". Faded bars: |t| < 1.645. HC1 robust SE."),
    x = NULL, y = "OLS beta on \u0394 log(VIX)") +
  theme_paper +
  theme(legend.position = "bottom", axis.text.y = element_text(size = 6.5))

save_fig(fig_e1_bars, "E1a_vix_beta_countries", part = "part_III", w = 7.5, h = 11)

# ── Figure E1b — Group-level VIX betas ────────────────────────────────────────
#
# Average beta by country group with 95% CI of the group mean.
# A positive safe-haven beta and negative OPEC beta would confirm the
# time-varying nature of H&S's insurance channel.

vix_group <- vix_betas %>%
  group_by(group) %>%
  summarise(mu = mean(beta_vix),
            se = sd(beta_vix) / sqrt(n()),
            n  = n(), .groups = "drop")

fig_e1_groups <- vix_group %>%
  ggplot(aes(x = reorder(group, mu), y = mu, fill = group)) +
  geom_col(width = 0.5, alpha = 0.88) +
  geom_errorbar(aes(ymin = mu - 1.96*se, ymax = mu + 1.96*se),
                width = 0.18, linewidth = 0.55, colour = "grey25") +
  geom_hline(yintercept = 0, linewidth = 0.5, colour = "grey20") +
  geom_text(aes(label = paste0("n=", n), y = mu + sign(mu) * 0.015),
            size = 3, colour = "grey30", vjust = -0.3) +
  scale_fill_manual(
    values = c("Safe haven" = col_blue, "OPEC" = col_red,
               "HIPC" = "grey55", "Other" = "grey78"),
    guide = "none") +
  labs(
    title    = "Extension 1 — VIX Beta by Country Group",
    subtitle = paste0("Mean country-level OLS beta on \u0394log(VIX), 1990\u2013",
                      y_ext_end, ". Error bars: 95% CI of the group mean."),
    x = NULL, y = "Mean VIX beta") +
  theme_paper

save_fig(fig_e1_groups, "E1b_vix_beta_groups", part = "part_III", w = 6.5, h = 4.5)

# ── Table E1 — Panel regressions ───────────────────────────────────────────────
#
# Five specifications increasing in fixed effects. The critical column is (5):
# year FE absorb the aggregate VIX shock, so only the within-year differential
# response of safe havens vs. other countries is identified. If the interaction
# is positive and significant there, H&S's insurance mechanism is genuinely
# countercyclical and not just a cross-sectional regularity.

d_e1 <- panel_vix %>%
  filter(iso3c %in% countries_79,
         !is.na(dm_exp_flow_gdp), !is.na(d_log_vix),
         !is.na(fdi_assets_gdp),  !is.na(fdi_liab_gdp))

t_e1 <- list(
  "(1)"           = fixest::feols(
    dm_exp_flow_gdp ~ d_log_vix + fdi_liab_gdp + fdi_assets_gdp,
    data = d_e1, vcov = "hetero"),
  "(2) Interact." = fixest::feols(
    dm_exp_flow_gdp ~ d_log_vix + vix_x_safe + vix_x_opec +
      fdi_liab_gdp + fdi_assets_gdp,
    data = d_e1, vcov = "hetero"),
  "(3) FE"        = fixest::feols(
    dm_exp_flow_gdp ~ d_log_vix + fdi_liab_gdp + fdi_assets_gdp | iso3c,
    data = d_e1, vcov = ~iso3c),
  "(4) FE+Int."   = fixest::feols(
    dm_exp_flow_gdp ~ d_log_vix + vix_x_safe + vix_x_opec +
      fdi_liab_gdp + fdi_assets_gdp | iso3c,
    data = d_e1, vcov = ~iso3c),
  "(5) Two-way"   = fixest::feols(
    dm_exp_flow_gdp ~ vix_x_safe + vix_x_opec +
      fdi_liab_gdp + fdi_assets_gdp | iso3c + year,
    data = d_e1, vcov = ~iso3c)
)

fe_rows_e1 <- tribble(
  ~term,         ~`(1)`, ~`(2) Interact.`, ~`(3) FE`, ~`(4) FE+Int.`, ~`(5) Two-way`,
  "Country FE",  "No",   "No",             "Yes",     "Yes",          "Yes",
  "Year FE",     "No",   "No",             "No",      "No",           "Yes")
attr(fe_rows_e1, "position") <- c(9, 10)

tex_e1 <- modelsummary(
  t_e1,
  stars       = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  fmt         = "%.4f",
  gof_omit    = "AIC|BIC|Log|Adj|Within|RMSE",
  add_rows    = fe_rows_e1,
  coef_rename = c(
    "d_log_vix"      = "$\\Delta\\log(\\text{VIX})$",
    "vix_x_safe"     = "$\\Delta\\log(\\text{VIX})\\times\\text{Safe haven}$",
    "vix_x_opec"     = "$\\Delta\\log(\\text{VIX})\\times\\text{OPEC}$",
    "fdi_liab_gdp"   = "FDI liabilities / GDP",
    "fdi_assets_gdp" = "FDI assets / GDP"),
  output = "latex_tabular")

tex_e1_wrapped <- paste0(
  "\\begin{table}[htbp]\n\\centering\n",
  "\\caption{Extension 1 --- The Safe-Asset Cycle: VIX Sensitivity of Dark Matter Exports (",
  "1990--", y_ext_end, ")}\n",
  "\\label{tab:E1_vix}\n\\scriptsize\n",
  tex_e1, "\n",
  "\\begin{minipage}{0.95\\linewidth}\n",
  "\\footnotesize Notes: Dependent variable: annual dark matter exports divided by GDP. ",
  "Safe-haven countries: USA, CHE, DEU, GBR, JPN, NLD, AUT, DNK, NOR (Maggiori 2017). ",
  "Columns (1)--(2): heteroskedasticity-robust standard errors. ",
  "Columns (3)--(5): standard errors clustered by country. ",
  "In column (5), year fixed effects absorb the aggregate VIX level, ",
  "so only the differential response of safe havens relative to other countries is identified. ",
  "A positive and significant interaction confirms that H\\&S's insurance mechanism ",
  "is countercyclical, not merely a cross-sectional regularity. ",
  "* p$<$0.10, ** p$<$0.05, *** p$<$0.01.\n",
  "\\end{minipage}\n\\end{table}"
)

compile_table(tex_e1_wrapped, "tableE1_vix_panel",
              part = "part_III", landscape = TRUE, table_number = 1, fit_width = TRUE)

message("  Extension 1 done.")


# ==============================================================================
#
# Extension 1b — Financial vs. Geopolitical Risk: VIX vs. GPR Horse Race
#
# The VIX picks up financial market volatility — option-implied uncertainty
# about near-term US equity returns. But safe-haven status may reflect
# something broader: political stability and the perception that a country
# will remain a reliable counterparty even in times of geopolitical stress.
#
# To distinguish these two channels we use the Geopolitical Risk Index
# (Caldara & Iacoviello 2022, AER P&P), constructed from automated text
# searches in 10 major English-language newspapers since 1985. The GPR rises
# during wars, terrorist attacks and interstate tensions even when financial
# markets are calm — think of the Crimea annexation in 2014 or North Korea's
# missile tests in 2017, periods when the VIX barely moved.
#
# We run a horse race: include both ΔlogVIX and ΔlogGPR interacted with
# safe-haven and OPEC dummies, with two-way fixed effects throughout.
# If both interactions are positive for safe havens, the premium is driven
# by both financial and geopolitical safety. If only one survives, we can
# attribute the dark matter mechanism more precisely.
#
# Data: gpr_web_latest.xlsx — sheet "GPR", column "GPR", monthly 1985–2021.
#       We use the main GPR series (threats + acts, all newspaper sources).
#
# ==============================================================================

message("\n── Extension 1b: VIX vs. GPR Horse Race ─────────────────────────────────")

# ── Load and aggregate GPR ────────────────────────────────────────────────────

gpr_raw <- read_excel(
  here("code", "data", "gpr_web_latest.xlsx"),
  sheet = "GPR"
)

gpr_annual <- gpr_raw %>%
  mutate(
    Date = as.Date(as.numeric(Date), origin = "1899-12-30"),
    year = as.integer(format(Date, "%Y")),
    gpr  = suppressWarnings(as.numeric(GPR))
  ) %>%
  filter(!is.na(gpr), !is.na(year), year >= 1990, year <= y_ext_end) %>%
  group_by(year) %>%
  summarise(gpr = mean(gpr, na.rm = TRUE), .groups = "drop") %>%
  arrange(year) %>%
  mutate(d_log_gpr = c(NA_real_, diff(log(gpr))))

message(sprintf("  GPR: %d annual obs (%d–%d)",
                nrow(gpr_annual), min(gpr_annual$year), max(gpr_annual$year)))

# ── Merge VIX and GPR into the panel ─────────────────────────────────────────

panel_gpr <- panel_vix %>%
  left_join(gpr_annual, by = "year") %>%
  mutate(
    gpr_x_safe = d_log_gpr * safe_d,
    gpr_x_opec = d_log_gpr * opec_d
  )

d_e1b <- panel_gpr %>%
  filter(iso3c %in% countries_79,
         !is.na(dm_exp_flow_gdp),
         !is.na(d_log_vix), !is.na(d_log_gpr),
         !is.na(fdi_liab_gdp), !is.na(fdi_assets_gdp))

message(sprintf("  E1b sample: %d obs, %d countries",
                nrow(d_e1b), n_distinct(d_e1b$iso3c)))

# ── Figure E1b — VIX vs GPR: showing when they diverge ───────────────────────
#
# We z-standardise both series so they are on the same scale.
# The divergence periods (2013-2019) are where geopolitical risk rises
# while financial markets stay calm — exactly where the two channels
# can be separately identified.

fig_e1b_ts <- vix_annual %>%
  left_join(gpr_annual, by = "year") %>%
  filter(!is.na(vix), !is.na(gpr)) %>%
  mutate(vix_std = as.numeric(scale(vix)),
         gpr_std = as.numeric(scale(gpr))) %>%
  pivot_longer(c(vix_std, gpr_std), names_to = "index", values_to = "value") %>%
  mutate(index = recode(index,
                        "vix_std" = "VIX (financial volatility)",
                        "gpr_std" = "GPR (geopolitical risk)")) %>%
  ggplot(aes(x = year, y = value, colour = index, linetype = index)) +
  geom_hline(yintercept = 0, colour = col_grey, linewidth = 0.4) +
  geom_line(linewidth = 1.0, alpha = 0.9) +
  scale_colour_manual(
    values = c("VIX (financial volatility)" = col_blue,
               "GPR (geopolitical risk)"    = col_red),
    name = NULL) +
  scale_linetype_manual(
    values = c("VIX (financial volatility)" = "solid",
               "GPR (geopolitical risk)"    = "dashed"),
    name = NULL) +
  annotate("text", x = 2001.5, y = 3.1, label = "9/11",
           size = 2.8, colour = col_grey, fontface = "italic") +
  annotate("text", x = 2008.8, y = 3.5, label = "GFC",
           size = 2.8, colour = col_grey, fontface = "italic") +
  annotate("text", x = 2020.2, y = 3.7, label = "COVID",
           size = 2.8, colour = col_grey, fontface = "italic") +
  scale_x_continuous(breaks = seq(1990, y_ext_end, 4)) +
  labs(
    title    = "Extension 1b — VIX vs. GPR: Complementary Risk Measures (1990\u2013present)",
    subtitle = paste0("Both series z-standardised. Divergence periods (e.g. 2013\u20132019) ",
                      "allow separate identification of financial and geopolitical safe-haven premia."),
    x = NULL, y = "Standardised index (z-score)") +
  theme_paper + theme(legend.position = "bottom")

save_fig(fig_e1b_ts, "E1b_vix_vs_gpr_timeseries", part = "part_III", w = 10, h = 5)

# ── Table E1b — Horse race with two-way FE throughout ─────────────────────────
#
# All columns absorb country and year fixed effects. Column (3) is the key
# specification: both VIX and GPR interactions are included jointly, so each
# coefficient captures the marginal contribution of one type of risk holding
# the other constant. Column (4) excludes OPEC countries as a robustness check.

t_e1b <- list(
  "(1) VIX"        = fixest::feols(
    dm_exp_flow_gdp ~ vix_x_safe + vix_x_opec + fdi_liab_gdp + fdi_assets_gdp | iso3c + year,
    data = d_e1b, vcov = ~iso3c),
  "(2) GPR"        = fixest::feols(
    dm_exp_flow_gdp ~ gpr_x_safe + gpr_x_opec + fdi_liab_gdp + fdi_assets_gdp | iso3c + year,
    data = d_e1b, vcov = ~iso3c),
  "(3) VIX + GPR"  = fixest::feols(
    dm_exp_flow_gdp ~ vix_x_safe + gpr_x_safe + vix_x_opec + gpr_x_opec +
      fdi_liab_gdp + fdi_assets_gdp | iso3c + year,
    data = d_e1b, vcov = ~iso3c),
  "(4) OPEC excl." = fixest::feols(
    dm_exp_flow_gdp ~ vix_x_safe + gpr_x_safe + fdi_liab_gdp + fdi_assets_gdp | iso3c + year,
    data = filter(d_e1b, opec_d == 0), vcov = ~iso3c)
)

fe_rows_e1b <- tribble(
  ~term,        ~`(1) VIX`, ~`(2) GPR`, ~`(3) VIX + GPR`, ~`(4) OPEC excl.`,
  "Country FE", "Yes", "Yes", "Yes", "Yes",
  "Year FE",    "Yes", "Yes", "Yes", "Yes")
attr(fe_rows_e1b, "position") <- c(9, 10)

tex_e1b <- modelsummary(
  t_e1b,
  stars       = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  fmt         = "%.4f",
  gof_omit    = "AIC|BIC|Log|Adj|Within|RMSE",
  add_rows    = fe_rows_e1b,
  coef_rename = c(
    "vix_x_safe"     = "$\\Delta\\log(\\text{VIX})\\times\\text{Safe haven}$",
    "gpr_x_safe"     = "$\\Delta\\log(\\text{GPR})\\times\\text{Safe haven}$",
    "vix_x_opec"     = "$\\Delta\\log(\\text{VIX})\\times\\text{OPEC}$",
    "gpr_x_opec"     = "$\\Delta\\log(\\text{GPR})\\times\\text{OPEC}$",
    "fdi_liab_gdp"   = "FDI liabilities / GDP",
    "fdi_assets_gdp" = "FDI assets / GDP"),
  output = "latex_tabular")

tex_e1b_wrapped <- paste0(
  "\\begin{table}[htbp]\n\\centering\n",
  "\\caption{Extension 1b --- Horse Race: VIX vs.~GPR as Drivers of Safe-Haven Dark Matter (",
  "1990--", y_ext_end, ")}\n",
  "\\label{tab:E1b_gpr}\n\\scriptsize\n",
  tex_e1b, "\n",
  "\\begin{minipage}{0.95\\linewidth}\n",
  "\\footnotesize Notes: All columns include country and year fixed effects. ",
  "The aggregate VIX and GPR levels are absorbed by year dummies; ",
  "only the differential response of each group to a common shock is identified. ",
  "GPR = Caldara \\& Iacoviello (2022, AER P\\&P) Geopolitical Risk Index, ",
  "constructed from automated text searches in 10 major English-language newspapers. ",
  "If $\\Delta\\log(\\text{GPR})\\times\\text{Safe haven}$ is positive and significant ",
  "after controlling for VIX, geopolitical safety generates a premium independently ",
  "of financial market volatility. ",
  "Standard errors clustered by country. * p$<$0.10, ** p$<$0.05, *** p$<$0.01.\n",
  "\\end{minipage}\n\\end{table}"
)

compile_table(tex_e1b_wrapped, "tableE1b_vix_gpr",
              part = "part_III", landscape = FALSE, table_number = 2, fit_width = TRUE)

message("  Extension 1b done.")


# ==============================================================================
#
# Extension 2 — Intangible Capital Intensity
#
# H&S include R&D expenditure as a share of GDP to capture their FDI-knowledge
# channel: multinationals export blueprints and know-how through their foreign
# affiliates, but this trade is not recorded in balance of payments statistics.
# Countries that invest heavily in knowledge-based assets should therefore show
# large dark matter exports as their unrecorded knowledge exports generate
# income that official statistics attribute to the affiliate, not the parent.
#
# H&S find R&D insignificant and drop it. We argue the problem is the proxy:
# Corrado, Hulten & Sichel (2009, RIW) show that R&D is only ~30% of total
# intangible investment. Software, organisational capital, and brands matter
# equally, and these are equally invisible in BoP statistics.
#
# We construct a composite index by z-standardising four WDI indicators:
#   z1 = R&D expenditure / GDP           (H&S's original proxy)
#   z2 = log(resident patent applications) (innovation output, not just input)
#   z3 = log(high-tech exports / mfg)    (sectoral intensity of intangibles)
#   z4 = log(ICT service exports)        (most BoP-invisible intangible category)
# and taking the row mean of available components (at least 2 required).
#
# Data: WDI — downloaded once and cached locally as wdi_intangibles.csv.
#
# ==============================================================================

message("\n── Extension 2: Intangible Capital Intensity ────────────────────────────")

# ── Load WDI intangibles (download once, cache locally) ───────────────────────

wdi_int <- fetch_or_load_wdi(
  filename   = "wdi_intangibles.csv",
  indicators = c(rnd_gdp      = "GB.XPD.RSDV.GD.ZS",
                 patents_res  = "IP.PAT.RESD",
                 hitech_share = "TX.VAL.TECH.MF.ZS",
                 ict_exports  = "BX.GSR.CCIS.ZS"),
  start = 1980, end = y_ext_end
) %>%
  filter(!is.na(iso3c), year >= 1980)

# ── Build composite intangible index ─────────────────────────────────────────

intang_cs <- wdi_int %>%
  group_by(iso3c) %>%
  summarise(
    rnd_avg = mean(rnd_gdp,      na.rm = TRUE),
    pat_avg = mean(patents_res,  na.rm = TRUE),
    hit_avg = mean(hitech_share, na.rm = TRUE),
    ict_avg = mean(ict_exports,  na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    z_rnd = as.numeric(scale(rnd_avg)),
    z_pat = as.numeric(scale(log1p(pat_avg))),
    z_hit = as.numeric(scale(log1p(hit_avg))),
    z_ict = as.numeric(scale(log1p(ict_avg)))
  ) %>%
  rowwise() %>%
  mutate(
    intang_idx = {
      v <- c(z_rnd, z_pat, z_hit, z_ict)
      if (sum(!is.na(v)) < 2) NA_real_ else mean(v, na.rm = TRUE)
    }
  ) %>%
  ungroup()

# ── Cross-section regressions ─────────────────────────────────────────────────
#
# Column (1) replicates H&S's Table 3 baseline on the extended window.
# Column (2) adds R&D alone — replicating their null result.
# Column (3) substitutes the composite index — our main test.
# Column (4) is a horse race between the two.
# Column (5) restricts to industrial countries where intangible-intensive FDI
# (pharma, software, finance) is most concentrated.

cs_e2 <- cs_ext %>%
  select(-any_of("rnd_avg")) %>%
  left_join(
    intang_cs %>% select(iso3c, rnd_avg, intang_idx),
    by = "iso3c"
  ) %>%
  mutate(
    dm_exp_ratio     = dm_exp_gdp     / 100,
    fdi_assets_ratio = fdi_assets_gdp / 100,
    fdi_liab_ratio   = fdi_liab_gdp   / 100
  )

d_e2 <- cs_e2 %>%
  filter(iso3c %in% countries_79,
         !is.na(dm_exp_ratio), !is.na(fdi_assets_ratio),
         !is.na(fdi_liab_ratio), !is.na(output_vol_hp)) %>%
  mutate(across(c(dm_exp_ratio, fdi_assets_ratio, fdi_liab_ratio,
                  output_vol_hp, intang_idx, rnd_avg), winsor))



t_e2 <- list(
  "(1) Baseline"   = lm(dm_exp_ratio ~ fdi_assets_ratio + fdi_liab_ratio + output_vol_hp,
                        data = d_e2),
  "(2) + R\\&D"    = lm(dm_exp_ratio ~ fdi_assets_ratio + fdi_liab_ratio + output_vol_hp +
                          rnd_avg, data = filter(d_e2, !is.na(rnd_avg))),
  "(3) + Composite"= lm(dm_exp_ratio ~ fdi_assets_ratio + fdi_liab_ratio + output_vol_hp +
                          intang_idx, data = filter(d_e2, !is.na(intang_idx))),
  "(4) Horse race" = lm(dm_exp_ratio ~ fdi_assets_ratio + fdi_liab_ratio + output_vol_hp +
                          rnd_avg + intang_idx,
                        data = filter(d_e2, !is.na(rnd_avg), !is.na(intang_idx))),
  "(5) Industrial" = lm(dm_exp_ratio ~ fdi_assets_ratio + fdi_liab_ratio + output_vol_hp +
                          intang_idx,
                        data = filter(d_e2, iso3c %in% industrial, !is.na(intang_idx)))
)

print(sapply(t_e2, nobs))

tex_e2 <- modelsummary(
  t_e2,
  stars       = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  fmt         = "%.3f",
  gof_omit    = "AIC|BIC|Log|F|RMSE",
  coef_rename = c(
    "fdi_assets_ratio" = "FDI assets / GDP",
    "fdi_liab_ratio"   = "FDI liabilities / GDP",
    "output_vol_hp"    = "Output volatility (HP)",
    "rnd_avg"          = "R\\&D / GDP",
    "intang_idx"       = "Composite intangible index"),
  output = "latex_tabular")

tex_e2_wrapped <- paste0(
  "\\begin{table}[htbp]\n\\centering\n",
  "\\caption{Extension 2 --- Intangible Capital Intensity and Dark Matter (",
  y_ext_start, "--", y_ext_end, ")}\n",
  "\\label{tab:E2_intangibles}\n\\scriptsize\n",
  tex_e2, "\n",
  "\\begin{minipage}{0.95\\linewidth}\n",
  "\\footnotesize Notes: Dependent variable: cumulative dark matter exports over ",
  y_ext_start, "--", y_ext_end, ", divided by end-year GDP. ",
  "The composite intangible index is the row mean of four z-standardised components: ",
  "R\\&D/GDP, log resident patent applications, log high-technology export share, ",
  "and log ICT service exports (at least two components required per country). ",
  "Following Corrado, Hulten \\& Sichel (2009), R\\&D captures only $\\sim30\\%$ of ",
  "total intangible investment; the composite index recovers the remainder. ",
  "Column~(4): horse race between H\\&S's R\\&D proxy and the composite. ",
  "Column~(5): restricted to industrial countries (Keller \\& Yeaple 2013). ",
  "All variables winsorised at the 1\\% level. ",
  "* p$<$0.10, ** p$<$0.05, *** p$<$0.01.\n",
  "\\end{minipage}\n\\end{table}"
)

compile_table(tex_e2_wrapped, "tableE2_intangibles",
              part = "part_III", landscape = FALSE, table_number = 3, fit_width = TRUE)

# ── Figure E2 — Composite index vs. dark matter exports ───────────────────────

fig_e2 <- d_e2 %>%
  filter(!is.na(intang_idx)) %>%
  mutate(grp = case_when(
    iso3c %in% safe_havens ~ "Safe haven",
    iso3c %in% industrial  ~ "Other industrial",
    TRUE                   ~ "Developing")) %>%
  ggplot(aes(x = intang_idx, y = dm_exp_ratio, label = iso3c)) +
  geom_hline(yintercept = 0, colour = col_grey, linewidth = 0.4) +
  geom_vline(xintercept = 0, colour = col_grey, linewidth = 0.4, linetype = "dashed") +
  geom_smooth(method = "lm", se = TRUE, colour = col_red,
              linewidth = 0.9, fill = col_red, alpha = 0.08) +
  geom_point(aes(colour = grp, size = grp), alpha = 0.82) +
  geom_text_repel(aes(colour = grp), size = 2.4,
                  segment.colour = "grey70", segment.size = 0.3,
                  box.padding = 0.3, max.overlaps = 25, seed = 42) +
  scale_colour_manual(
    values = c("Safe haven" = col_blue, "Other industrial" = col_red,
               "Developing" = "grey55"), name = NULL) +
  scale_size_manual(
    values = c("Safe haven" = 2.8, "Other industrial" = 2.2, "Developing" = 1.6),
    guide = "none") +
  labs(
    title    = "Extension 2 — Composite Intangible Index vs. Dark Matter Exports",
    subtitle = paste0("Cross-section, ", y_ext_start, "\u2013", y_ext_end,
                      ". Index = mean z-score of R&D, patents, high-tech and ICT exports."),
    x = "Composite intangible intensity (standardised)",
    y = "Cumulative dark matter exports / end-year GDP") +
  theme_paper + theme(legend.position = "bottom")

save_fig(fig_e2, "E2_intangible_scatter", part = "part_III", w = 9, h = 6.5)

message("  Extension 2 done.")


# ==============================================================================
#
# Extension 3 — Financial Development and Safe Asset Production Capacity
#
# H&S invoke Caballero, Farhi & Gourinchas (at the time a 2005 working paper,
# published 2017 in QJE) as one theoretical rationale for the US exporting dark
# matter: financially underdeveloped countries cannot write claims on their own
# productive assets, forcing their savings into foreign securities. This demand
# for foreign safe assets allows issuers — mainly the US — to borrow at below-
# equilibrium rates. The return differential is what H&S capitalise as dark matter.
#
# H&S test this with a Rule of Law variable and find nothing. We argue the
# right proxy is financial depth, not institutions: it is the ability to issue
# liquid, transparent, deeply traded financial liabilities that confers the
# safe-asset premium (He, Krishnamurthy & Milbradt 2019, AER).
#
# We use two WDI indicators averaged over the sample window:
#   private credit / GDP     : depth of the banking sector
#   stock market cap / GDP   : depth of equity markets
# standardised and combined into a composite FinDev index.
#
# Data: WDI — downloaded once and cached locally as wdi_findev.csv.
#
# ==============================================================================

message("\n── Extension 3: Financial Development ───────────────────────────────────")

# ── Load WDI financial development (download once, cache locally) ─────────────

wdi_fin <- fetch_or_load_wdi(
  filename   = "wdi_findev.csv",
  indicators = c(private_credit = "FS.AST.PRVT.GD.ZS",
                 stock_mktcap   = "CM.MKT.LCAP.GD.ZS"),
  start = 1980, end = y_ext_end
) %>%
  filter(!is.na(iso3c), year >= 1980)

# ── Build composite financial development index ───────────────────────────────

findev_cs <- wdi_fin %>%
  group_by(iso3c) %>%
  summarise(
    credit_avg = mean(private_credit, na.rm = TRUE),
    mktcap_avg = mean(stock_mktcap,   na.rm = TRUE),
    .groups    = "drop"
  ) %>%
  mutate(
    z_credit = as.numeric(scale(log1p(credit_avg))),
    z_mktcap = as.numeric(scale(log1p(mktcap_avg)))
  ) %>%
  rowwise() %>%
  mutate(findev_idx = mean(c_across(c(z_credit, z_mktcap)), na.rm = TRUE)) %>%
  ungroup()

# ── Cross-section regressions ─────────────────────────────────────────────────
#
# We add the FinDev components progressively to show which dimension of
# financial depth matters most. Column (5) is the kitchen-sink specification
# stacking all three extensions, testing whether financial development has
# incremental explanatory power beyond H&S's original channels.

cs_e3 <- cs_ext %>%
  left_join(findev_cs,                               by = "iso3c") %>%
  left_join(intang_cs %>% select(iso3c, intang_idx), by = "iso3c") %>%
  mutate(dm_exp_ratio     = dm_exp_gdp     / 100,
         fdi_assets_ratio = fdi_assets_gdp / 100,
         fdi_liab_ratio   = fdi_liab_gdp   / 100)

d_e3 <- cs_e3 %>%
  filter(iso3c %in% countries_79,
         !is.na(dm_exp_ratio), !is.na(fdi_assets_ratio),
         !is.na(fdi_liab_ratio), !is.na(output_vol_hp)) %>%
  mutate(across(c(dm_exp_ratio, fdi_assets_ratio, fdi_liab_ratio,
                  output_vol_hp, z_credit, z_mktcap, findev_idx, intang_idx), winsor))

t_e3 <- list(
  "(1) Baseline"    = lm(dm_exp_ratio ~ fdi_assets_ratio + fdi_liab_ratio + output_vol_hp,
                         data = d_e3),
  "(2) Credit"      = lm(dm_exp_ratio ~ fdi_assets_ratio + fdi_liab_ratio + output_vol_hp +
                           z_credit, data = filter(d_e3, !is.na(z_credit))),
  "(3) Mkt cap"     = lm(dm_exp_ratio ~ fdi_assets_ratio + fdi_liab_ratio + output_vol_hp +
                           z_mktcap, data = filter(d_e3, !is.na(z_mktcap))),
  "(4) FinDev idx"  = lm(dm_exp_ratio ~ fdi_assets_ratio + fdi_liab_ratio + output_vol_hp +
                           findev_idx, data = filter(d_e3, !is.na(findev_idx))),
  "(5) Full"        = lm(dm_exp_ratio ~ fdi_assets_ratio + fdi_liab_ratio + output_vol_hp +
                           findev_idx + intang_idx + rule_of_law + opec + hipc,
                         data = filter(d_e3, !is.na(findev_idx), !is.na(intang_idx),
                                       !is.na(rule_of_law)))
)

print(sapply(t_e3, nobs))

tex_e3 <- modelsummary(
  t_e3,
  stars       = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  fmt         = "%.3f",
  gof_omit    = "AIC|BIC|Log|F|RMSE",
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
    "hipc"             = "HIPC dummy"),
  output = "latex_tabular")

tex_e3_wrapped <- paste0(
  "\\begin{table}[htbp]\n\\centering\n",
  "\\caption{Extension 3 --- Financial Development and Safe Asset Production Capacity (",
  y_ext_start, "--", y_ext_end, ")}\n",
  "\\label{tab:E3_findev}\n\\scriptsize\n",
  tex_e3, "\n",
  "\\begin{minipage}{0.95\\linewidth}\n",
  "\\footnotesize Notes: Dependent variable: cumulative dark matter exports over ",
  y_ext_start, "--", y_ext_end, ", divided by end-year GDP. ",
  "The FinDev composite index is the mean of z-standardised log(private credit/GDP) ",
  "and log(stock market cap/GDP), averaged over the sample window. ",
  "The theoretical prediction (Caballero, Farhi \\& Gourinchas 2017; ",
  "He, Krishnamurthy \\& Milbradt 2019) is a positive coefficient: ",
  "financially deeper economies produce safe assets at lower cost, ",
  "attracting capital at below-equilibrium rates --- the return differential ",
  "H\\&S capitalise as dark matter. ",
  "Column~(5) includes all three extensions jointly. ",
  "All variables winsorised at the 1\\% level. ",
  "* p$<$0.10, ** p$<$0.05, *** p$<$0.01.\n",
  "\\end{minipage}\n\\end{table}"
)

compile_table(tex_e3_wrapped, "tableE3_findev",
              part = "part_III", landscape = FALSE, table_number = 4, fit_width = TRUE)

# ── Figure E3 — Two-panel scatter: credit depth and equity market depth ────────

make_scatter_e3 <- function(df, xvar, xlabel) {
  df %>%
    filter(!is.na(.data[[xvar]])) %>%
    mutate(grp = case_when(
      iso3c %in% safe_havens ~ "Safe haven",
      iso3c %in% industrial  ~ "Other industrial",
      TRUE                   ~ "Developing")) %>%
    ggplot(aes(x = .data[[xvar]], y = dm_exp_ratio, label = iso3c)) +
    geom_hline(yintercept = 0, colour = col_grey, linewidth = 0.4) +
    geom_smooth(method = "lm", se = TRUE, colour = col_red,
                linewidth = 0.9, fill = col_red, alpha = 0.08) +
    geom_point(aes(colour = grp, size = grp), alpha = 0.82) +
    geom_text_repel(aes(colour = grp), size = 2.2,
                    segment.colour = "grey70", segment.size = 0.3,
                    box.padding = 0.28, max.overlaps = 20, seed = 7) +
    scale_colour_manual(
      values = c("Safe haven" = col_blue, "Other industrial" = col_red,
                 "Developing" = "grey55"), name = NULL) +
    scale_size_manual(
      values = c("Safe haven" = 2.8, "Other industrial" = 2.2, "Developing" = 1.6),
      guide = "none") +
    labs(x = xlabel, y = "Dark matter exports / GDP") +
    theme_paper + theme(legend.position = "bottom")
}

fig_e3 <- (make_scatter_e3(d_e3, "z_credit", "Private credit / GDP (z-score)") +
             make_scatter_e3(d_e3, "z_mktcap", "Stock market cap / GDP (z-score)")) +
  plot_annotation(
    title    = "Extension 3 — Financial Development vs. Dark Matter Exports",
    subtitle = paste0("Cross-section, ", y_ext_start, "\u2013", y_ext_end,
                      ". OLS fit with 95% CI. Blue = safe haven; red = other industrial."),
    theme = theme(plot.title    = element_text(size = 10, face = "bold"),
                  plot.subtitle = element_text(size = 8, colour = "grey40"))
  ) + plot_layout(guides = "collect") & theme(legend.position = "bottom")

save_fig(fig_e3, "E3_financial_development_scatter", part = "part_III", w = 12, h = 6)

message("  Extension 3 done.")


# ==============================================================================
#
# Extension 4 — NII Decomposition by Asset Class
#
# H&S apply a single discount rate r = 5% to total net investment income.
# This is a deliberate simplification: they want to use a constant rate so
# that changes in our dark matter measure reflect changes in actual income
# flows rather than movements in price-earnings ratios (see their footnote 7).
#
# But the choice of r matters for how large dark matter looks, and it matters
# differently for different countries depending on the composition of their
# foreign asset and liability positions. A country whose NII comes mostly from
# FDI (earning ~8%) looks very different under a flat 5% versus a component-
# specific set of rates. We test the sensitivity of H&S's main result to this
# assumption by decomposing NII into four asset classes and applying alternative
# discount rates calibrated on Gourinchas & Rey (2006, NBER).
#
# The four sub-series are already in our BOP file — no additional download:
#   NETCD_T.D_F5_D42S.USD.A  FDI income (net)
#   NETCD_T.P_F5_D4S.USD.A   Portfolio equity income (net)
#   NETCD_T.P_F3_D41.USD.A   Portfolio debt income (net)
#   NETCD_T.O_F_D4P.USD.A    Other investment income (net)
#
# Discount rate scenarios:
#   A: flat r = 5%              (H&S baseline)
#   B: FDI 8%, equity 6%, debt 3%, other 5%  (Gourinchas & Rey central estimates)
#   C: FDI 10%, equity 7%, debt 2.5%, other 4%  (wider spread — upper bound)
#
# ==============================================================================

message("\n── Extension 4: NII Decomposition by Asset Class ────────────────────────")

# ── Extract NII sub-components from the BOP file ──────────────────────────────

yr_e4 <- names(bop_raw_ext)[grepl("^\\d{4}$", names(bop_raw)) &
                          as.integer(names(bop_raw)) >= 1990 &
                          as.integer(names(bop_raw)) <= y_ext_end]

codes_e4 <- c(
  nii_total = "NETCD_T.IN1.USD.A",
  nii_fdi   = "NETCD_T.D_F5_D42S.USD.A",
  nii_pe    = "NETCD_T.P_F5_D4S.USD.A",
  nii_pd    = "NETCD_T.P_F3_D41.USD.A",
  nii_other = "NETCD_T.O_F_D4P.USD.A"
)

decomp <- map_dfr(names(codes_e4), function(vn) {
  bop_raw_ext %>%
    rename(series_code = SERIES_CODE) %>%
    mutate(
      iso3c     = str_extract(series_code, "^[^.]+"),
      indicator = str_remove(series_code, "^[^.]+\\.")) %>%
    filter(indicator == codes_e4[[vn]]) %>%
    select(iso3c, all_of(yr_e4)) %>%
    pivot_longer(-iso3c, names_to = "year", values_to = "value") %>%
    mutate(year      = as.integer(year),
           value     = suppressWarnings(as.numeric(value)),
           component = vn)
}) %>%
  pivot_wider(names_from = component, values_from = value) %>%
  left_join(EWN_ext %>% select(iso3c, year, gdp_usd), by = c("iso3c","year")) %>%
  mutate(
    # Scenario A: H&S flat r = 5% applied to total NII
    nfa_dm_A = nii_total / 0.05,
    # Scenario B: component-specific rates from Gourinchas & Rey (2006)
    nfa_dm_B = (nii_fdi / 0.08) + (nii_pe / 0.06) +
      (nii_pd  / 0.03) + (nii_other / 0.05),
    # Scenario C: wider spread — tests sensitivity to extreme assumptions
    nfa_dm_C = (nii_fdi / 0.10) + (nii_pe / 0.07) +
      (nii_pd  / 0.025) + (nii_other / 0.04),
    share_fdi   = nii_fdi   / nii_total * 100,
    share_pe    = nii_pe    / nii_total * 100,
    share_pd    = nii_pd    / nii_total * 100,
    share_other = nii_other / nii_total * 100
  )

# ── Figure E4a — NII decomposition for six key countries ─────────────────────
#
# We show the income breakdown for three safe havens (USA, GBR, CHE) and
# three countries that appear as dark matter importers in H&S (IRL, ITA, DEU).
# This reveals whether the US premium is driven by FDI (knowledge channel)
# or by portfolio debt (safe asset / liquidity channel).

showcase_e4 <- c("USA","GBR","DEU","JPN","FRA","IRL")
labels_e4   <- c(USA="United States", GBR="United Kingdom", DEU="Germany",
                 JPN="Japan",         FRA="France",         IRL="Ireland")
comp_cols <- c(
  "FDI income"       = col_blue,
  "Portfolio equity" = col_red,
  "Portfolio debt"   = col_green,
  "Other investment" = "grey50",
  "Total NII"        = "black"
)

fig_e4a <- decomp %>%
  filter(iso3c %in% showcase_e4, !is.na(nii_total)) %>%
  pivot_longer(c(nii_fdi, nii_pe, nii_pd, nii_other, nii_total),
               names_to = "component", values_to = "value") %>%
  mutate(
    value_bn  = value / 1e6,
    component = factor(
      recode(component, nii_fdi = "FDI income", nii_pe = "Portfolio equity",
             nii_pd = "Portfolio debt", nii_other = "Other investment",
             nii_total = "Total NII"),
      levels = c("FDI income","Portfolio equity","Portfolio debt",
                 "Other investment","Total NII")),
    country = factor(labels_e4[iso3c], levels = labels_e4)
  ) %>%
  ggplot(aes(x = year, y = value_bn,
             colour = component, linetype = component, linewidth = component)) +
  geom_hline(yintercept = 0, colour = "grey70", linewidth = 0.35) +
  geom_line(alpha = 0.9, na.rm = TRUE) +
  scale_colour_manual(values = comp_cols, name = NULL) +
  scale_linetype_manual(
    values = c("FDI income" = "solid", "Portfolio equity" = "longdash",
               "Portfolio debt" = "dashed", "Other investment" = "dotted",
               "Total NII" = "solid"),
    name = NULL) +
  scale_linewidth_manual(
    values = c("FDI income" = 1.3, "Portfolio equity" = 1.1,
               "Portfolio debt" = 1.1, "Other investment" = 1.0, "Total NII" = 1.8),
    name = NULL) +
  facet_wrap(~ country, scales = "free_y", ncol = 3) +
  scale_x_continuous(breaks = seq(1990, y_ext_end, 8)) +
  labs(
    title    = paste0("Extension 4 — NII Decomposition by Asset Class (1990\u2013",
                      y_ext_end, ")"),
    subtitle = paste0("Net investment income ($tn) split by asset class. ",
                      "FDI income dominance for safe havens supports the H&S knowledge channel."),
    x = NULL, y = "Net investment income (trillions USD)") +
  theme_paper +
  theme(legend.position = "bottom",
        strip.text      = element_text(face = "bold", size = 9),
        axis.text.x     = element_text(size = 7.5))

save_fig(fig_e4a, "E4a_nii_decomposition_countries", part = "part_III", w = 13, h = 9)

# ── Figure E4b — Discount rate sensitivity for the US ─────────────────────────
#
# If H&S's conclusion about the US holds under Scenarios B and C, the choice
# of r = 5% is not driving the result. If dark matter collapses under the
# component-specific rates, the flat discount rate assumption is doing
# more work than the paper acknowledges.

fig_e4b <- decomp %>%
  filter(iso3c == "USA", year >= 1990, !is.na(gdp_usd)) %>%
  mutate(
    A_gdp = nfa_dm_A / gdp_usd * 100,
    B_gdp = nfa_dm_B / gdp_usd * 100,
    C_gdp = nfa_dm_C / gdp_usd * 100
  ) %>%
  pivot_longer(c(A_gdp, B_gdp, C_gdp), names_to = "scenario", values_to = "pct") %>%
  mutate(scenario = recode(scenario,
                           "A_gdp" = "A: flat r=5% (H&S baseline)",
                           "B_gdp" = "B: FDI 8%, equity 6%, debt 3%",
                           "C_gdp" = "C: FDI 10%, equity 7%, debt 2.5%")) %>%
  filter(!is.na(pct)) %>%
  ggplot(aes(x = year, y = pct, colour = scenario, linetype = scenario)) +
  geom_hline(yintercept = 0, colour = col_grey, linewidth = 0.4) +
  geom_line(linewidth = 1.6, alpha = 0.9) +
  scale_colour_manual(
    values = c("A: flat r=5% (H&S baseline)"      = col_blue,
               "B: FDI 8%, equity 6%, debt 3%"    = col_red,
               "C: FDI 10%, equity 7%, debt 2.5%" = col_green),
    name = NULL) +
  scale_linetype_manual(
    values = c("A: flat r=5% (H&S baseline)"      = "solid",
               "B: FDI 8%, equity 6%, debt 3%"    = "dashed",
               "C: FDI 10%, equity 7%, debt 2.5%" = "dotted"),
    name = NULL) +
  scale_x_continuous(breaks = seq(1990, y_ext_end, 4)) +
  labs(
    title    = "Extension 4b — US Dark Matter Stock under Alternative Discount Rate Scenarios",
    subtitle = paste0("% of US GDP. Scenarios calibrated on Gourinchas & Rey (2006). ",
                      "Persistence across B and C confirms H&S is robust to the choice of r."),
    x = NULL, y = "NFA dark matter (% of US GDP)") +
  theme_paper + theme(legend.position = "bottom")

save_fig(fig_e4b, "E4b_discount_scenarios_usa", part = "part_III", w = 10, h = 5.5)

# ── Table E4 — NII composition summary ────────────────────────────────────────
#
# Simple descriptive table showing each country's average annual NII and how it
# breaks down across asset classes. Countries are sorted by FDI income share
# to show which economies rely most heavily on the knowledge channel.

comp_summ <- decomp %>%
  filter(iso3c %in% c(safe_havens, "FRA","ITA","CAN","AUS"),
         year >= 2000, !is.na(nii_total)) %>%
  group_by(iso3c) %>%
  summarise(
    nii_avg = mean(nii_total / 1e6, na.rm = TRUE),
    fdi_sh  = mean(share_fdi,       na.rm = TRUE),
    pe_sh   = mean(share_pe,        na.rm = TRUE),
    pd_sh   = mean(share_pd,        na.rm = TRUE),
    oth_sh  = mean(share_other,     na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(fdi_sh)) %>%
  mutate(across(where(is.numeric), ~ round(., 1)))

comp_tex <- kableExtra::kbl(
  comp_summ,
  format    = "latex", booktabs = TRUE, linesep = "",
  col.names = c("Country", "Avg NII (\\$tn)", "FDI (\\%)",
                "Port.~equity (\\%)", "Port.~debt (\\%)", "Other (\\%)"),
  caption   = paste0("Extension 4 --- NII Composition by Asset Class, ",
                     "Annual Average 2000--", y_ext_end),
  label     = "tab:E4_nii",
  escape    = FALSE,
  align     = c("l","r","r","r","r","r")
) %>%
  kableExtra::kable_styling(
    latex_options = c("hold_position","striped"),
    font_size = 10, full_width = FALSE) %>%
  kableExtra::column_spec(1, bold = TRUE, width = "2.2cm") %>%
  kableExtra::column_spec(2:6, width = "2.2cm") %>%
  kableExtra::add_header_above(
    c(" " = 2, "Share of total NII (\\\\%)" = 4),
    escape = FALSE, bold = TRUE, line = TRUE) %>%
  kableExtra::footnote(
    general = paste0(
      "Average annual net investment income and its decomposition by asset class. ",
      "Countries sorted by FDI income share. ",
      "A high FDI share is consistent with H\\\\&S's knowledge-dissemination channel. ",
      "A high portfolio debt share is consistent with the safe asset / liquidity channel ",
      "(Gourinchas \\\\& Rey 2006)."),
    general_title = "\\\\textit{Notes:} ",
    escape = FALSE)

# Compile manually — kableExtra bypasses compile_table()
full_e4 <- paste0(
  "\\documentclass[11pt]{article}\n",
  "\\usepackage{booktabs,xcolor,colortbl,caption,array,graphicx}\n",
  "\\usepackage[top=2cm,bottom=2cm,left=2.5cm,right=2.5cm]{geometry}\n",
  "\\begin{document}\\small\\setcounter{table}{4}\n",
  comp_tex, "\n\\end{document}")

tex_e4p <- file.path(here("code","output","tables","part_III"),
                     "tableE4_nii_composition.tex")
pdf_e4p <- file.path(here("code","output","tables","part_III"),
                     "tableE4_nii_composition.pdf")
writeLines(full_e4, tex_e4p)
if (file.exists(pdf_e4p)) file.remove(pdf_e4p)
old <- setwd(here("code","output","tables","part_III"))
tryCatch(tinytex::pdflatex("tableE4_nii_composition.tex"),
         error = function(e) message("LaTeX error E4: ", e$message))
setwd(old)
if (file.exists(pdf_e4p)) message("  Saved → part_III/tableE4_nii_composition.pdf")

message("  Extension 4 done.")

message("\nDone.")
message("Figures (PDF + PNG) : code/output/figures/part_III/")
message("Tables  (PDF + TEX) : code/output/tables/part_III/")

# ==============================================================================
#
# Summary — Part III Extensions
#
# DATA FILES IN code/data/  (after first run)
#
#   vix_daily.csv        Daily VIX from FRED (VIXCLS), 1990-present.
#                        Columns: observation_date, VIXCLS.
#
#   gpr_web_latest.xlsx  Caldara & Iacoviello (2022) GPR index, monthly 1985-2021.
#                        We use sheet "GPR", column "GPR" (global index).
#                        The file also contains "GPR_THREAT", "GPR_ACT" and
#                        country-specific series in "GPR_COUNTRIES" — not used
#                        here but available for robustness checks.
#
#   wdi_intangibles.csv  Downloaded from WDI on first run, cached locally.
#                        Contains R&D/GDP, resident patents, high-tech export
#                        share, and ICT service exports for all countries.
#
#   wdi_findev.csv       Downloaded from WDI on first run, cached locally.
#                        Contains private credit/GDP and stock market cap/GDP.
#
# NII sub-components (Extension 4) are extracted directly from the existing
# BOP file — no additional download required.
#
# ==============================================================================
