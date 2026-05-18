# ==============================================================================
# V1
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
# FIX: Romania est "ROU" dans le BOP (pas "ROM"). Le code original utilisait
# "ROM" ce qui empêchait la jointure avec les données BOP → Romania disparaissait.
#
# 7 pays non récupérables avec notre extrait BOP actuel :
# AUT, BFA, CIV, IRL, MOZ, RWA, YEM ont 0 observation non manquante
# pour les séries historiques de CA et de revenu primaire / investissement
# sur 1980-2003 dans notre fichier BOP brut.
# Ces pays ne peuvent donc pas être inclus dans les calculs de dark matter
# sans compléter la source de données.

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
# Scaffold countries_109 × années : seuls les pays de l'échantillon H&S
# sont dans le panel. Pour les pays sans NII dans le BOP, ca_usd peut
# être complété par ca_ewn (colonne CA de l'EWN) si disponible.
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
  # Pour les pays sans CA dans le BOP, utiliser la CA de l'EWN comme backup
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
    dm_exp_flow = ca_dm - ca_usd,
    dm_exp_flow_gdp = dm_exp_flow / gdp_usd * 100
  )

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
# Part 6 — Cross-section dataset (strict cumulative 1980-2003)
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
    n_ca = sum(!is.na(ca_usd)),
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
    
    n_ca = sum(!is.na(ca_usd)),
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
# Tous les scatters sont filtrés sur countries_109 (échantillon H&S exact).
# Figure 6c : panel séparé depuis le BOP brut + interpolation linéaire.
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
       subtitle = "Billions of US dollars",
       x = NULL, y = "Billions USD")

save_fig(fig1, "fig1_us_ca_nii")

# ── Figure 3a ──────────────────────────────────────────────────────────────────

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

save_fig(fig3a, "fig3a_scatter", h = 6.5)

# ── Figure 3b ──────────────────────────────────────────────────────────────────

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

save_fig(fig3b, "fig3b_scatter_zoom", h = 6.5)

# ── Figure 5b ──────────────────────────────────────────────────────────────────

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

save_fig(fig5b, "fig5b_dm_vs_ca", h = 6.5)

# ── Figure 6c ──────────────────────────────────────────────────────────────────
# Panel séparé depuis le BOP brut complet + interpolation intra-série.
# Protection : approx() requiert >= 2 points non-NA.

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
#
# Part 8 — Regression tables
#
# compile_table():
#   - table_number: sets the table counter so that each standalone PDF has
#     the correct table number.
#   - landscape: compiles wide tables in landscape format.
#   - fit_width: resizes the tabular to \linewidth using graphicx::\resizebox.
#
# IMPORTANT FIX:
#   This version does NOT use adjustbox.
#   The previous V1 version loaded \usepackage{adjustbox}, but adjustbox.sty
#   was missing in TinyTeX, so the .tex files were updated while the PDFs were
#   not regenerated. This version uses only graphicx, which is already loaded.
#
# ==============================================================================

compile_table <- function(tex_content, filename, landscape = FALSE,
                          table_number = 1, fit_width = FALSE) {
  
  geom <- if (landscape) {
    "\\usepackage[landscape, margin=0.7in]{geometry}\n"
  } else {
    "\\usepackage[margin=1in]{geometry}\n"
  }
  
  counter_cmd <- paste0("\\setcounter{table}{", table_number - 1L, "}\n")
  
  # Fit wide tables without adjustbox.
  # graphicx provides \\resizebox{\\linewidth}{!}{...}.
  # fixed = TRUE treats the patterns literally and avoids regex issues.
  if (fit_width) {
    tex_content <- sub(
      "\\begin{tabular}",
      "\\resizebox{\\linewidth}{!}{%\n\\begin{tabular}",
      tex_content,
      fixed = TRUE
    )
    tex_content <- sub(
      "\\end{tabular}",
      "\\end{tabular}%\n}",
      tex_content,
      fixed = TRUE
    )
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
  
  tables_dir <- here("code", "output", "tables")
  tex_path <- file.path(tables_dir, paste0(filename, ".tex"))
  pdf_path <- file.path(tables_dir, paste0(filename, ".pdf"))
  
  writeLines(full_doc, tex_path)
  
  # Remove old PDF before compiling so a stale PDF cannot be mistaken
  # for a successfully regenerated table.
  if (file.exists(pdf_path)) {
    removed <- tryCatch(
      file.remove(pdf_path),
      warning = function(w) FALSE,
      error   = function(e) FALSE
    )
    if (!isTRUE(removed)) {
      stop(
        "Cannot remove old PDF: ", pdf_path,
        "\nClose the PDF if it is open, then rerun the script."
      )
    }
  }
  
  old_wd <- setwd(tables_dir)
  on.exit(setwd(old_wd), add = TRUE)
  
  tinytex::pdflatex(paste0(filename, ".tex"))
  
  if (!file.exists(pdf_path)) {
    stop(
      "PDF was not created: ", pdf_path,
      "\nCheck the LaTeX log file: ",
      file.path(tables_dir, paste0(filename, ".log"))
    )
  }
}

# ── Table 1 ────────────────────────────────────────────────────────────────────

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
              "table1",
              table_number = 1)

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

compile_table(paste(tex2, collapse = "\n"),
              "table2",
              table_number = 2)

# ── Table 3 ────────────────────────────────────────────────────────────────────

# ==============================================================================
# Table 3 — Sources of Dark Matter: Cross-Section Evidence
# ==============================================================================

# Helper si pas déjà défini plus haut
winsor <- function(x, p = 0.01) {
  if (all(is.na(x))) return(x)
  q <- quantile(x, c(p, 1 - p), na.rm = TRUE)
  pmax(pmin(x, q[2]), q[1])
}

# Variables en ratios et non en points de pourcentage
cs <- cs %>%
  mutate(
    dm_exp_ratio = dm_exp_gdp / 100,
    fdi_assets_ratio = fdi_assets_gdp / 100,
    fdi_liab_ratio = fdi_liab_gdp / 100
  )

# Échantillon élargi utilisé dans certaines colonnes.
# Si countries_99 existe déjà dans votre script, cette ligne ne pose pas problème.
countries_99 <- setdiff(
  countries_109,
  c("BFA", "ETH", "MDG", "MWI", "MLI", "NPL", "NER", "RWA", "TGO", "UGA")
)

d3_79 <- cs %>%
  filter(
    iso3c %in% countries_79,
    !is.na(dm_exp_ratio),
    !is.na(fdi_assets_ratio),
    !is.na(fdi_liab_ratio),
    !is.na(output_vol)
  ) %>%
  mutate(across(c(dm_exp_ratio, fdi_assets_ratio, fdi_liab_ratio, output_vol), winsor))

d3_99 <- cs %>%
  filter(
    iso3c %in% countries_99,
    !is.na(dm_exp_ratio),
    !is.na(fdi_assets_ratio),
    !is.na(fdi_liab_ratio),
    !is.na(output_vol)
  ) %>%
  mutate(across(c(dm_exp_ratio, fdi_assets_ratio, fdi_liab_ratio, output_vol), winsor))

d3_ind <- cs %>%
  filter(
    iso3c %in% industrial,
    !is.na(dm_exp_ratio),
    !is.na(fdi_assets_ratio),
    !is.na(fdi_liab_ratio),
    !is.na(output_vol)
  ) %>%
  mutate(across(c(dm_exp_ratio, fdi_assets_ratio, fdi_liab_ratio, output_vol), winsor))

t3 <- list(
  "(i)" = lm(
    dm_exp_ratio ~ fdi_assets_ratio + fdi_liab_ratio + output_vol,
    data = d3_79
  ),
  
  "(ii)" = lm(
    dm_exp_ratio ~ fdi_assets_ratio + fdi_liab_ratio + output_vol +
      rule_of_law + rnd_avg,
    data = d3_79
  ),
  
  "(iii)" = lm(
    dm_exp_ratio ~ fdi_assets_ratio + fdi_liab_ratio + output_vol +
      rule_of_law + rnd_avg + opec + hipc,
    data = d3_79
  ),
  
  "(iv)" = lm(
    dm_exp_ratio ~ fdi_assets_ratio + fdi_liab_ratio + output_vol,
    data = d3_99
  ),
  
  "(v)" = lm(
    dm_exp_ratio ~ fdi_assets_ratio + fdi_liab_ratio + output_vol +
      rule_of_law + rnd_avg,
    data = d3_99
  ),
  
  "(vi)" = lm(
    dm_exp_ratio ~ fdi_assets_ratio + fdi_liab_ratio + output_vol +
      rule_of_law + rnd_avg + opec + hipc,
    data = d3_99
  ),
  
  "(vii) Industrial" = lm(
    dm_exp_ratio ~ fdi_assets_ratio + fdi_liab_ratio + output_vol,
    data = d3_ind
  )
)

print(sapply(t3, nobs))

# ==============================================================================
# Table 3 display — split into two LaTeX tables
# ==============================================================================

dir.create(here("code", "output", "tables"),
           recursive = TRUE, showWarnings = FALSE)

coef_names_t3 <- c(
  "fdi_assets_ratio" = "FDI assets / GDP",
  "fdi_liab_ratio" = "FDI liabilities / GDP",
  "output_vol" = "Output volatility",
  "rule_of_law" = "Rule of Law",
  "rnd_avg" = "R\\&D (\\% GDP)",
  "opec" = "OPEC dummy",
  "hipc" = "HIPC dummy"
)

t3_left  <- t3[1:3]
t3_right <- t3[4:7]

options("modelsummary_format_numeric_latex" = "plain")

tex3_left <- modelsummary(
  t3_left,
  stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  fmt = "%.3f",
  coef_rename = coef_names_t3,
  gof_omit = "AIC|BIC|Log|F|RMSE",
  output = "latex_tabular"
)

tex3_right <- modelsummary(
  t3_right,
  stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  fmt = "%.3f",
  coef_rename = coef_names_t3,
  gof_omit = "AIC|BIC|Log|F|RMSE",
  output = "latex_tabular"
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

writeLines(
  wrap_table(
    tex3_left,
    "Sources of Dark Matter: Cross-Section Evidence, columns (i)--(iii)",
    "tab:table3a"
  ),
  here("code", "output", "tables", "table3a.tex")
)

writeLines(
  wrap_table(
    tex3_right,
    "Sources of Dark Matter: Cross-Section Evidence, columns (iv)--(vii)",
    "tab:table3b"
  ),
  here("code", "output", "tables", "table3b.tex")
)

if (exists("compile_table")) {
  compile_table(
    wrap_table(tex3_left,
               "Sources of Dark Matter: Cross-Section Evidence, columns (i)--(iii)",
               "tab:table3a"),
    "table3a",
    landscape = FALSE,
    table_number = 3,
    fit_width = TRUE
  )
  
  compile_table(
    wrap_table(tex3_right,
               "Sources of Dark Matter: Cross-Section Evidence, columns (iv)--(vii)",
               "tab:table3b"),
    "table3b",
    landscape = FALSE,
    table_number = 3,
    fit_width = TRUE
  )
}

# ── Table 4 ────────────────────────────────────────────────────────────────────

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
              "table4",
              landscape = TRUE,
              table_number = 4,
              fit_width = TRUE)





# ── Table 4b — Panel, annual dark matter exports ───────────────────────────────

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
              "table4b",
              landscape = TRUE,
              table_number = 5,
              fit_width = TRUE)

message("\nDone.")
message("Figures (PDF + PNG) : code/output/figures/")
message("Tables  (PDF + TEX) : code/output/tables/")
message("Main replication uses cs_available; cs_strict is kept as robustness diagnostic.")

# ==============================================================================
#
# Summary — Replication of Hausmann & Sturzenegger (2006)
#
# DATA COVERAGE NOTE
#
# Bug corrigé : "ROM" → "ROU" (code ISO3C de la Roumanie) dans countries_109
# et countries_79. Le code original ne faisait pas la jointure avec le BOP
# pour la Roumanie, la perdant silencieusement.
#
# Couverture des données :
# Dans notre extrait IMF BOP actuel, 7 pays de la liste H&S 109
# (AUT, BFA, CIV, IRL, MOZ, RWA, YEM) ont 0 observation non manquante
# pour les séries historiques de compte courant et de revenu primaire /
# revenu d'investissement sur 1980-2003.
#
# Nous avons vérifié cela directement dans le fichier BOP brut, avant tout
# nettoyage ou jointure. Nous avons aussi cherché toutes les séries candidates
# liées à income / investment / primary / direct / portfolio / interest, sans
# trouver de série alternative exploitable sur 1980-2003 pour ces pays.
#
# Ces pays ne peuvent donc pas être inclus dans le calcul du dark matter sans
# changer ou compléter la source de données.
#
# Pour Table 3, les pertes supplémentaires dans les colonnes avec contrôles
# viennent principalement de la couverture limitée de la série WDI R&D.
#
# [TO BE COMPLETED: comparison with original results]
#
# ==============================================================================
# ==============================================================================
#
#                     Part II - Extension of the article
#
# ==============================================================================
# ==============================================================================

# ==============================================================================
# Extension 1 — Augmented proxy sample
#
# Goal:
# Test whether the main results are sensitive to supplementing missing BOP income
# data with WDI national-accounts net income from abroad (NY.GSR.NFCY.CD).
#
# Important:
# This is NOT the baseline replication and NOT the original H&S data source.
# It is a proxy robustness check.
# ==============================================================================


# ==============================================================================
# Augmented proxy sample — conservative version
# Uses WDI national-accounts net income only when both 1980 and 2003 are available
# Not used as baseline
# ==============================================================================

# ==============================================================================
# WDI alternative income coverage
# ==============================================================================

if (!requireNamespace("WDI", quietly = TRUE)) install.packages("WDI")
library(WDI)

missing_nii_countries <- c("AUT", "IRL", "CIV", "BFA", "MOZ", "RWA", "YEM")

wdi_grid <- tidyr::expand_grid(
  iso3c = missing_nii_countries,
  year = 1980:2003
)

fetch_wdi_indicator <- function(code, varname) {
  
  message("Downloading ", code, " as ", varname, "...")
  
  out <- tryCatch(
    {
      WDI::WDI(
        country = missing_nii_countries,
        indicator = setNames(code, varname),
        start = 1980,
        end = 2003,
        extra = FALSE
      )
    },
    error = function(e) {
      message("Failed to download ", code, ": ", e$message)
      NULL
    }
  )
  
  if (is.null(out) || nrow(out) == 0) {
    out <- wdi_grid
    out[[varname]] <- NA_real_
    return(out)
  }
  
  if (!varname %in% names(out)) {
    out[[varname]] <- NA_real_
  }
  
  out %>%
    select(iso3c, year, all_of(varname))
}

wdi_bop_net  <- fetch_wdi_indicator("BN.GSR.FCTY.CD", "nii_wdi_bop")
wdi_na_net   <- fetch_wdi_indicator("NY.GSR.NFCY.CD", "nii_wdi_na")
wdi_receipts <- fetch_wdi_indicator("BX.GSR.FCTY.CD", "income_receipts_bop")
wdi_payments <- fetch_wdi_indicator("BM.GSR.FCTY.CD", "income_payments_bop")

wdi_alt_check <- list(
  wdi_grid,
  wdi_bop_net,
  wdi_na_net,
  wdi_receipts,
  wdi_payments
) %>%
  purrr::reduce(full_join, by = c("iso3c", "year")) %>%
  arrange(iso3c, year) %>%
  mutate(
    nii_wdi_bop_constructed = if_else(
      !is.na(income_receipts_bop) & !is.na(income_payments_bop),
      income_receipts_bop - income_payments_bop,
      NA_real_
    )
  )

wdi_alt_coverage <- wdi_alt_check %>%
  group_by(iso3c) %>%
  summarise(
    n_bop_net = sum(!is.na(nii_wdi_bop)),
    n_na_net  = sum(!is.na(nii_wdi_na)),
    n_receipts = sum(!is.na(income_receipts_bop)),
    n_payments = sum(!is.na(income_payments_bop)),
    n_bop_constructed = sum(!is.na(nii_wdi_bop_constructed)),
    has_na_1980 = !is.na(nii_wdi_na[year == 1980][1]),
    has_na_2003 = !is.na(nii_wdi_na[year == 2003][1]),
    first_na_year = ifelse(
      n_na_net > 0,
      min(year[!is.na(nii_wdi_na)]),
      NA_integer_
    ),
    last_na_year = ifelse(
      n_na_net > 0,
      max(year[!is.na(nii_wdi_na)]),
      NA_integer_
    ),
    .groups = "drop"
  )

print(wdi_alt_coverage, n = Inf)

wdi_na_eligible <- wdi_alt_coverage %>%
  filter(has_na_1980, has_na_2003) %>%
  pull(iso3c)

print(wdi_na_eligible)
# Expected: AUT, BFA, CIV, RWA

wdi_na_backup_conservative <- wdi_alt_check %>%
  filter(iso3c %in% wdi_na_eligible) %>%
  transmute(
    iso3c,
    year,
    # WDI NY.GSR.NFCY.CD is in current US dollars.
    # IMF BOP extract used in the replication is in millions of US dollars.
    nii_wdi_na = nii_wdi_na / 1e6
  )
panel_augmented <- panel %>%
  left_join(wdi_na_backup_conservative, by = c("iso3c", "year")) %>%
  mutate(
    nii_usd_baseline = nii_usd,
    nii_usd_augmented = coalesce(nii_usd, nii_wdi_na),
    nii_source = case_when(
      !is.na(nii_usd) ~ "IMF_BOP",
      is.na(nii_usd) & !is.na(nii_wdi_na) ~ "WDI_NA_proxy",
      TRUE ~ NA_character_
    ),
    nfa_dm = nii_usd_augmented / r
  ) %>%
  arrange(iso3c, year) %>%
  group_by(iso3c) %>%
  mutate(
    ca_dm = nfa_dm - dplyr::lag(nfa_dm),
    ca_dm_gdp = ca_dm / gdp_usd * 100,
    dm_exp_flow = ca_dm - ca_usd,
    dm_exp_flow_gdp = dm_exp_flow / gdp_usd * 100
  ) %>%
  ungroup()

cs_augmented <- panel_augmented %>%
  filter(year >= y_cs, year <= y_cs_end) %>%
  group_by(iso3c) %>%
  summarise(
    country = safe_first(country),
    
    n_ca = sum(!is.na(ca_usd)),
    n_nii = sum(!is.na(nii_usd_augmented)),
    
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
    
    source_has_proxy = any(nii_source == "WDI_NA_proxy", na.rm = TRUE),
    
    .groups = "drop"
  )

augmented_sample_compare <- tibble(
  specification = c("baseline IMF BOP", "augmented WDI NA proxy"),
  
  table1_full_n = c(
    cs %>%
      filter(iso3c %in% countries_109,
             !is.na(cum_dm_bn), !is.na(cum_oca_bn)) %>%
      nrow(),
    
    cs_augmented %>%
      filter(iso3c %in% countries_109,
             !is.na(cum_dm_bn), !is.na(cum_oca_bn)) %>%
      nrow()
  ),
  
  table3_79_n = c(
    cs %>%
      filter(iso3c %in% countries_79,
             !is.na(dm_exp_gdp),
             !is.na(fdi_assets_gdp),
             !is.na(fdi_liab_gdp),
             !is.na(output_vol)) %>%
      nrow(),
    
    cs_augmented %>%
      filter(iso3c %in% countries_79,
             !is.na(dm_exp_gdp),
             !is.na(fdi_assets_gdp),
             !is.na(fdi_liab_gdp),
             !is.na(output_vol)) %>%
      nrow()
  ),
  
  recovered_proxy_countries = paste(wdi_na_eligible, collapse = ", ")
)

print(augmented_sample_compare)


# ==============================================================================
# Augmented Table 3 — proxy robustness
# ==============================================================================

cs_augmented <- cs_augmented %>%
  mutate(
    dm_exp_ratio = dm_exp_gdp / 100,
    fdi_assets_ratio = fdi_assets_gdp / 100,
    fdi_liab_ratio = fdi_liab_gdp / 100
  )

d3_aug_79 <- cs_augmented %>%
  filter(
    iso3c %in% countries_79,
    !is.na(dm_exp_ratio),
    !is.na(fdi_assets_ratio),
    !is.na(fdi_liab_ratio),
    !is.na(output_vol)
  ) %>%
  mutate(across(c(dm_exp_ratio, fdi_assets_ratio, fdi_liab_ratio, output_vol), winsor))

d3_aug_99 <- cs_augmented %>%
  filter(
    iso3c %in% countries_99,
    !is.na(dm_exp_ratio),
    !is.na(fdi_assets_ratio),
    !is.na(fdi_liab_ratio),
    !is.na(output_vol)
  ) %>%
  mutate(across(c(dm_exp_ratio, fdi_assets_ratio, fdi_liab_ratio, output_vol), winsor))

t3_aug <- list(
  "(i)" = lm(dm_exp_ratio ~ fdi_assets_ratio + fdi_liab_ratio + output_vol,
             data = d3_aug_79),
  
  "(ii)" = lm(dm_exp_ratio ~ fdi_assets_ratio + fdi_liab_ratio + output_vol +
                rule_of_law + rnd_avg,
              data = d3_aug_79),
  
  "(iii)" = lm(dm_exp_ratio ~ fdi_assets_ratio + fdi_liab_ratio + output_vol +
                 rule_of_law + rnd_avg + opec + hipc,
               data = d3_aug_79),
  
  "(iv)" = lm(dm_exp_ratio ~ fdi_assets_ratio + fdi_liab_ratio + output_vol,
              data = d3_aug_99),
  
  "(v)" = lm(dm_exp_ratio ~ fdi_assets_ratio + fdi_liab_ratio + output_vol +
               rule_of_law + rnd_avg,
             data = d3_aug_99),
  
  "(vi)" = lm(dm_exp_ratio ~ fdi_assets_ratio + fdi_liab_ratio + output_vol +
                rule_of_law + rnd_avg + opec + hipc,
              data = d3_aug_99)
)

print(sapply(t3_aug, nobs))


# ==============================================================================
# Augmented Table 3 — export as PDF
# ==============================================================================

dir.create(here("code", "output", "tables"),
           recursive = TRUE, showWarnings = FALSE)

coef_names_t3_aug <- c(
  "fdi_assets_ratio" = "FDI assets / GDP",
  "fdi_liab_ratio" = "FDI liabilities / GDP",
  "output_vol" = "Output volatility",
  "rule_of_law" = "Rule of Law",
  "rnd_avg" = "R\\&D (\\% GDP)",
  "opec" = "OPEC dummy",
  "hipc" = "HIPC dummy"
)

tex3_aug <- modelsummary(
  t3_aug,
  stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  fmt = "%.3f",
  coef_rename = coef_names_t3_aug,
  gof_omit = "AIC|BIC|Log|F|RMSE",
  output = "latex_tabular"
)

tex3_aug_wrapped <- paste0(
  "\\begin{table}[htbp]\n",
  "\\centering\n",
  "\\caption{Augmented Proxy Robustness: Sources of Dark Matter, Cross-Section Evidence}\n",
  "\\label{tab:table3_augmented}\n",
  "\\scriptsize\n",
  tex3_aug, "\n",
  "\\begin{minipage}{0.95\\linewidth}\n",
  "\\footnotesize Notes: Standard errors in parentheses. ",
  "Dependent variable: cumulative dark matter exports over 1980--2003, divided by 2003 GDP. ",
  "This table is a robustness check, not the baseline replication. ",
  "The augmented sample supplements missing IMF BOP income data with WDI national-accounts net income from abroad ",
  "only for countries with observations in both 1980 and 2003. ",
  "Recovered proxy countries: ", paste(wdi_na_eligible, collapse = ", "), ". ",
  "Variables are expressed as ratios where relevant and winsorised at the 1\\% level. ",
  "* p$<$0.10, ** p$<$0.05, *** p$<$0.01.\n",
  "\\end{minipage}\n",
  "\\end{table}"
)

compile_table(
  tex3_aug_wrapped,
  "table3_augmented",
  landscape = TRUE,
  table_number = 6,
  fit_width = TRUE
)

message("Augmented Table 3 created: code/output/tables/table3_augmented.pdf")


### EXTENSION - PART 2

# ==============================================================================
# Extension — Time Update (2004–2023) + Insurance Channel Decomposition
#
# Theory: H&S's output_vol conflates systematic risk (beta vs world) and
# idiosyncratic risk. The insurance channel predicts only LOW BETA countries
# sell insurance and accumulate dark matter exports.
# Idiosyncratic volatility should NOT predict dark matter once beta is controlled.
#
# New variables:
#   beta_world   : 10-year rolling OLS beta of d.log(GDP_i) on d.log(GDP_world)
#   idio_vol     : residual SD from the same regression
#   beta_cs      : country-level average beta over the cross-section window
#   idio_vol_cs  : country-level average idiosyncratic vol over the same window
# ==============================================================================

# ── Packages ──────────────────────────────────────────────────────────────────

pkgs <- c("tidyverse", "readxl", "countrycode", "here",
          "mFilter", "plm", "ggrepel", "scales", "broom",
          "stargazer", "tinytex", "modelsummary", "WDI", "zoo")
for (p in pkgs) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
invisible(lapply(pkgs, library, character.only = TRUE))

# ── Key parameters ─────────────────────────────────────────────────────────────

r          <- 0.05
y_start    <- 1975
y_ext_start <- 1980          # extension cross-section start (same as H&S)
y_ext_end   <- 2022          # extension cross-section end (latest full year)
roll_window <- 10            # rolling window for beta estimation

# Country lists — carry over from replication

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
# 1. DOWNLOAD EXTENDED GDP DATA VIA WDI PACKAGE
#    NY.GDP.MKTP.KD = real GDP constant 2015 USD
# ==============================================================================

gdp_ext_raw <- WDI::WDI(
  indicator = c(gdp_con = "NY.GDP.MKTP.KD"),
  start     = y_start,
  end       = y_ext_end,
  extra     = FALSE
) %>%
  as_tibble() %>%
  mutate(
    iso3c = if_else(
      !is.na(iso3c),
      iso3c,
      countrycode(iso2c, "iso2c", "iso3c", warn = FALSE)
    )
  ) %>%
  filter(!is.na(iso3c), !is.na(gdp_con), !is.na(year)) %>%
  select(iso3c, year, gdp_con)

# ==============================================================================
# 2. CONSTRUCT WORLD OUTPUT GROWTH SERIES
#    Use the full WDI cross-section to build a GDP-weighted world growth index.
#    This is the "market portfolio" against which we compute country betas.
# ==============================================================================

world_gdp <- gdp_ext_raw %>%
  group_by(year) %>%
  summarise(world_gdp_con = sum(gdp_con, na.rm = TRUE), .groups = "drop") %>%
  arrange(year) %>%
  mutate(d_log_world = c(NA, diff(log(world_gdp_con))))

# ==============================================================================
# 3. COMPUTE ROLLING BETAS AND IDIOSYNCRATIC VOLATILITY
#
#    For each country i and each year t, regress:
#      d.log(GDP_i)_{t-9:t} ~ d.log(GDP_world)_{t-9:t}
#    Extract beta (systematic exposure) and SD of residuals (idiosyncratic vol).
#
#    This gives a time-varying measure for the panel.
#    For the cross-section we will average over the relevant window.
# ==============================================================================

compute_rolling_beta <- function(df_country, world_df, window = roll_window) {
  
  df <- df_country %>%
    arrange(year) %>%
    mutate(d_log_gdp = c(NA, diff(log(gdp_con)))) %>%
    left_join(world_df %>% select(year, d_log_world), by = "year") %>%
    filter(!is.na(d_log_gdp), !is.na(d_log_world))
  
  n <- nrow(df)
  if (n < window) {
    df$beta_world <- NA_real_
    df$idio_vol   <- NA_real_
    return(df %>% select(iso3c, year, beta_world, idio_vol))
  }
  
  beta_vec    <- rep(NA_real_, n)
  idio_vec    <- rep(NA_real_, n)
  
  for (i in window:n) {
    idx   <- (i - window + 1):i
    y_vec <- df$d_log_gdp[idx]
    x_vec <- df$d_log_world[idx]
    if (sum(!is.na(y_vec) & !is.na(x_vec)) < (window - 2)) next
    fit <- tryCatch(
      lm(y_vec ~ x_vec),
      error = function(e) NULL
    )
    if (is.null(fit)) next
    beta_vec[i] <- coef(fit)["x_vec"]
    idio_vec[i] <- sd(resid(fit), na.rm = TRUE)
  }
  
  df$beta_world <- beta_vec
  df$idio_vol   <- idio_vec
  df %>% select(iso3c, year, beta_world, idio_vol)
}

# Apply to all countries 
beta_panel <- gdp_ext_raw %>%
  group_by(iso3c) %>%
  group_map(
    ~ compute_rolling_beta(.x %>% mutate(iso3c = .y$iso3c), world_gdp),
    .keep = TRUE
  ) %>%
  bind_rows()

message(sprintf("Beta panel: %d rows, %d countries",
                nrow(beta_panel),
                n_distinct(beta_panel$iso3c)))

# ==============================================================================
# 4. LOAD EXTENDED BOP AND EWN DATA



bop_ext_path <- here("code", "data", "Current_account_primary_income_1975_2022.csv")

if (file.exists(bop_ext_path)) {
  
  bop_ext_raw <- read_csv(bop_ext_path, show_col_types = FALSE)
  year_cols_ext <- names(bop_ext_raw)[grepl("^\\d{4}$", names(bop_ext_raw))]
  
  bop_ext <- bop_ext_raw %>%
    rename(series_code = SERIES_CODE) %>%
    mutate(
      iso3c     = str_extract(series_code, "^[^.]+"),
      indicator = str_remove(series_code, "^[^.]+\\.")
    ) %>%
    filter(indicator %in% c("NETCD_T.CAB.USD.A", "NETCD_T.IN1.USD.A")) %>%
    select(iso3c, indicator, all_of(year_cols_ext)) %>%
    pivot_longer(all_of(year_cols_ext), names_to = "year", values_to = "value") %>%
    mutate(year = as.integer(year),
           value = suppressWarnings(as.numeric(value))) %>%
    filter(year >= y_start, year <= y_ext_end) %>%
    pivot_wider(names_from = indicator, values_from = value) %>%
    rename(ca_usd  = `NETCD_T.CAB.USD.A`,
           nii_usd = `NETCD_T.IN1.USD.A`) %>%
    mutate(nii_source = "IMF_BOP")
  
} else {
  message("Extended BOP file not found — using original BOP + EWN CA backup.")
  # Fall back: use original bop object (from replication) and EWN for post-2003
  # 'bop' must exist in your environment from the replication script
  bop_ext <- bop %>%
    mutate(nii_source = "IMF_BOP_original")
}

# ==============================================================================
# 5. LOAD EXTENDED EWN DATA (already in your environment as EWN_raw)
#    Filter to extended window
# ==============================================================================

EWN_ext <- EWN_raw %>%
  rename(
    ifs_code   = IFS_Code,
    year       = Year,
    fdi_assets = `FDI assets (stock)`,
    fdi_liab   = `FDI liabilities (stock)`,
    nfa        = `Net IIP excl gold`,
    gdp_usd    = `GDP (US$)`,
    ca_ewn     = `Current account balance`
  ) %>%
  select(ifs_code, year, fdi_assets, fdi_liab, nfa, gdp_usd, ca_ewn) %>%
  filter(year >= y_start, year <= y_ext_end) %>%
  mutate(
    iso3c          = countrycode(ifs_code, "imf", "iso3c", warn = FALSE),
    nfa_gdp        = nfa        / gdp_usd * 100,
    fdi_assets_gdp = fdi_assets / gdp_usd * 100,
    fdi_liab_gdp   = fdi_liab   / gdp_usd * 100
  ) %>%
  filter(!is.na(iso3c))

# ==============================================================================
# 6. BUILD EXTENDED PANEL
# ==============================================================================

scaffold_ext <- expand.grid(
  iso3c = countries_109,
  year  = y_start:y_ext_end,
  stringsAsFactors = FALSE
) %>% as_tibble()

panel_ext <- scaffold_ext %>%
  left_join(bop_ext, by = c("iso3c", "year")) %>%
  left_join(
    EWN_ext %>% select(iso3c, year, fdi_assets_gdp, fdi_liab_gdp,
                       nfa_gdp, gdp_usd, ca_ewn),
    by = c("iso3c", "year")
  ) %>%
  left_join(gdp_ext_raw, by = c("iso3c", "year")) %>%
  left_join(beta_panel,  by = c("iso3c", "year")) %>%
  left_join(wgi,         by = "iso3c") %>%   # from replication (1996-2005 avg)
  mutate(
    ca_usd  = if_else(is.na(ca_usd)  & !is.na(ca_ewn), ca_ewn, ca_usd),
    country = countrycode(iso3c, "iso3c", "country.name"),
    opec_d  = as.integer(iso3c %in% opec),
    hipc_d  = as.integer(iso3c %in% hipc),
    nfa_dm  = nii_usd / r,
    ca_gdp  = ca_usd  / gdp_usd * 100
  ) %>%
  select(-ca_ewn) %>%
  arrange(iso3c, year)

panel_ext <- panel_ext %>%
  arrange(iso3c, year) %>%
  group_by(iso3c) %>%
  mutate(
    ca_dm           = nfa_dm - dplyr::lag(nfa_dm),
    ca_dm_gdp       = ca_dm / gdp_usd * 100,
    dm_exp_flow     = ca_dm - ca_usd,
    dm_exp_flow_gdp = dm_exp_flow / gdp_usd * 100
  ) %>%
  ungroup()

# ==============================================================================
# 7. EXTENDED CROSS-SECTION (1980–2022)
#    Mirrors H&S's cs object but over the longer window.
#    We average beta and idio_vol over the full cross-section window
#    (analogous to H&S's HP volatility computed over 1980-2003).
# ==============================================================================

safe_first  <- function(x) { x <- x[!is.na(x)]; if (!length(x)) NA else x[1] }
mean_or_na  <- function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
value_at    <- function(x, year, ty) {
  v <- x[year == ty]; v <- v[!is.na(v)]
  if (!length(v)) NA_real_ else v[1]
}

cs_ext <- panel_ext %>%
  filter(year >= y_ext_start, year <= y_ext_end) %>%
  group_by(iso3c) %>%
  summarise(
    country      = safe_first(country),
    n_ca         = sum(!is.na(ca_usd)),
    n_nii        = sum(!is.na(nii_usd)),
    
    cum_oca_bn   = if_else(n_ca > 0,
                           sum(ca_usd, na.rm = TRUE) / 1e3,
                           NA_real_),
    
    cum_dm_bn = {
      v <- nfa_dm[!is.na(nfa_dm)]
      if (length(v) < 2) NA_real_ else (last(v) - first(v)) / 1e3
    },
    
    dm_exp_bn    = cum_dm_bn - cum_oca_bn,
    gdp_end_bn   = value_at(gdp_usd, year, y_ext_end) / 1e3,
    
    cum_oca_gdp  = cum_oca_bn / gdp_end_bn * 100,
    cum_dm_gdp   = cum_dm_bn  / gdp_end_bn * 100,
    dm_exp_gdp   = dm_exp_bn  / gdp_end_bn * 100,
    dm_exp_ratio = dm_exp_gdp / 100,
    
    fdi_assets_gdp = value_at(fdi_assets_gdp, year, y_ext_end),
    fdi_liab_gdp   = value_at(fdi_liab_gdp,   year, y_ext_end),
    fdi_assets_ratio = fdi_assets_gdp / 100,
    fdi_liab_ratio   = fdi_liab_gdp   / 100,
    
    # NEW: average beta and idiosyncratic vol over the full window
    beta_avg     = mean_or_na(beta_world),
    idio_vol_avg = mean_or_na(idio_vol),
    
    # H&S's original volatility measure (HP cycle SD) for comparison
    output_vol_hp = {
      y <- gdp_con[!is.na(gdp_con)]
      if (length(y) < 10) NA_real_
      else tryCatch(
        sd(mFilter::hpfilter(log(y), freq = 100)$cycle),
        error = function(e) NA_real_
      )
    },
    
    rule_of_law  = safe_first(rule_of_law),
    rnd_avg      = mean_or_na(rnd),      # from replication panel
    opec         = safe_first(opec_d),
    hipc         = safe_first(hipc_d),
    .groups = "drop"
  )

# ==============================================================================
# 8. HORSE RACE: beta vs. idio_vol vs. H&S's output_vol
#
#    H&S: dark matter exports ~ FDI_liab + FDI_assets + output_vol
#
#    Prediction from insurance theory:
#      (a) Low beta countries (stable relative to world) export more dark matter
#          → beta_avg should enter NEGATIVELY
#      (b) Idiosyncratic volatility should NOT matter (it is uninsurable at
#          the global level) → idio_vol_avg coefficient should be small / insignificant
#      (c) H&S's output_vol is a blend of (a) and (b), so once decomposed it
#          should lose significance
#
#    We test this with a direct horse race across four specifications.
# ==============================================================================

winsor <- function(x, p = 0.01) {
  if (all(is.na(x))) return(x)
  q <- quantile(x, c(p, 1 - p), na.rm = TRUE)
  pmax(pmin(x, q[2]), q[1])
}

d_hr <- cs_ext %>%
  filter(
    iso3c %in% countries_79,
    !is.na(dm_exp_ratio),
    !is.na(fdi_assets_ratio),
    !is.na(fdi_liab_ratio),
    !is.na(output_vol_hp),
    !is.na(beta_avg),
    !is.na(idio_vol_avg)
  ) %>%
  mutate(across(
    c(dm_exp_ratio, fdi_assets_ratio, fdi_liab_ratio,
      output_vol_hp, beta_avg, idio_vol_avg),
    winsor
  ))

t_hr <- list(
  
  # Column 1: H&S baseline replication (extended window)
  "H&S baseline\n(extended)" = lm(
    dm_exp_ratio ~ fdi_assets_ratio + fdi_liab_ratio + output_vol_hp,
    data = d_hr
  ),
  
  # Column 2: Replace output_vol with beta only
  "Beta only" = lm(
    dm_exp_ratio ~ fdi_assets_ratio + fdi_liab_ratio + beta_avg,
    data = d_hr
  ),
  
  # Column 3: Replace output_vol with idio_vol only
  "Idio vol only" = lm(
    dm_exp_ratio ~ fdi_assets_ratio + fdi_liab_ratio + idio_vol_avg,
    data = d_hr
  ),
  
  # Column 4: Full decomposition — horse race
  "Full decomposition" = lm(
    dm_exp_ratio ~ fdi_assets_ratio + fdi_liab_ratio + beta_avg + idio_vol_avg,
    data = d_hr
  ),
  
  # Column 5: Full decomposition + controls
  "Full + controls" = lm(
    dm_exp_ratio ~ fdi_assets_ratio + fdi_liab_ratio +
      beta_avg + idio_vol_avg + rule_of_law + opec + hipc,
    data = d_hr
  )
)

sapply(t_hr, nobs)

# ==============================================================================
# 9. OUTPUT TABLE — Horse Race
# ==============================================================================

coef_names_hr <- c(
  "fdi_assets_ratio" = "FDI assets / GDP",
  "fdi_liab_ratio"   = "FDI liabilities / GDP",
  "output_vol_hp"    = "Output vol. (H\\&S HP)",
  "beta_avg"         = "Beta vs. world output",
  "idio_vol_avg"     = "Idiosyncratic volatility",
  "rule_of_law"      = "Rule of Law",
  "opec"             = "OPEC dummy",
  "hipc"             = "HIPC dummy"
)

tex_hr <- modelsummary(
  t_hr,
  stars       = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  fmt         = "%.3f",
  coef_rename = coef_names_hr,
  gof_omit    = "AIC|BIC|Log|F|RMSE",
  output      = "latex_tabular"
)

tex_hr_wrapped <- paste0(
  "\\begin{table}[htbp]\n",
  "\\centering\n",
  "\\caption{Insurance Channel Decomposition: Beta vs.~Idiosyncratic Volatility (1980--",
  y_ext_end, ")}\n",
  "\\label{tab:horse_race}\n",
  "\\scriptsize\n",
  tex_hr, "\n",
  "\\begin{minipage}{0.95\\linewidth}\n",
  "\\footnotesize Notes: Standard errors in parentheses. ",
  "Dependent variable: cumulative dark matter exports over 1980--", y_ext_end,
  ", divided by end-year GDP. ",
  "\\textit{Beta vs.~world output} is the country's average 10-year rolling OLS beta ",
  "of real GDP growth on world real GDP growth. ",
  "\\textit{Idiosyncratic volatility} is the average SD of residuals from the same regression. ",
  "H\\&S's \\textit{Output vol.} conflates both; columns (2)--(5) decompose it. ",
  "Theory predicts: negative beta (low-beta countries sell insurance, accumulate dark matter), ",
  "insignificant idiosyncratic volatility. ",
  "All variables winsorised at the 1\\% level. ",
  "* p$<$0.10, ** p$<$0.05, *** p$<$0.01.\n",
  "\\end{minipage}\n",
  "\\end{table}"
)

compile_table(
  tex_hr_wrapped,
  "table_horse_race",
  landscape  = FALSE,
  table_number = 7,
  fit_width  = TRUE
)

# ==============================================================================
# 10. EXTENDED TIME-SERIES FIGURES
#     Figure A: US dark matter stock updated to 2022
#     Figure B: Global NFA positions updated to 2022
# ==============================================================================

# ── US extended ───────────────────────────────────────────────────────────────

us_ext <- panel_ext %>% filter(iso3c == "USA") %>% arrange(year)

# Anchor at 1982 (329 bn) and accumulate CA forward/backward (same as replication)
us_ext$off_nfa <- NA_real_
idx82 <- which(us_ext$year == 1982)
us_ext$off_nfa[idx82] <- 329000
for (i in (idx82 + 1):nrow(us_ext))
  us_ext$off_nfa[i] <- us_ext$off_nfa[i-1] + replace_na(us_ext$ca_usd[i], 0)
for (i in seq(idx82 - 1, 1))
  us_ext$off_nfa[i] <- us_ext$off_nfa[i+1] - replace_na(us_ext$ca_usd[i+1], 0)

us_ext <- us_ext %>%
  mutate(
    dm_stock_bn  = (nfa_dm - off_nfa) / 1e3,
    dm_stock_gdp = (nfa_dm - off_nfa) / gdp_usd * 100,
    nii_bn       = nii_usd / 1e3,
    cum_ca_bn    = cumsum(replace_na(ca_usd, 0)) / 1e3
  )

fig_us_ext <- us_ext %>%
  filter(year >= 1982, !is.na(dm_stock_bn)) %>%
  ggplot(aes(x = year)) +
  geom_col(aes(y = dm_stock_bn / 1e3),
           fill = "#2166AC", alpha = 0.25, width = 0.75) +
  geom_col(aes(y = dm_stock_bn / 1e3),
           fill = NA, colour = "#2166AC", alpha = 0.7,
           width = 0.75, linewidth = 0.3) +
  geom_line(aes(y = dm_stock_gdp / 10), colour = "#B2182B", linewidth = 1) +
  geom_vline(xintercept = 2003.5, linetype = "dashed",
             colour = "grey50", linewidth = 0.5) +
  annotate("text", x = 2004, y = max(us_ext$dm_stock_gdp / 10, na.rm = TRUE) * 0.9,
           label = "H&S\nend", hjust = 0, size = 2.8, colour = "grey40") +
  scale_x_continuous(breaks = seq(1982, y_ext_end, 4)) +
  scale_y_continuous(
    name     = "Trillions USD",
    labels   = label_number(suffix = "T"),
    sec.axis = sec_axis(~ . * 10, name = "% of US GDP",
                        labels = label_number(suffix = "%"))
  ) +
  labs(
    title    = paste0("US Stock of Dark Matter (1982\u2013", y_ext_end, ")"),
    subtitle = "Updated from H&S (2006). Dashed line = original paper end-date.",
    x = NULL
  ) +
  theme_bw(base_size = 11) +
  theme(axis.title.y.right = element_text(colour = "#B2182B", size = 9.5))

save_fig(fig_us_ext, "fig_us_dm_extended")

# ── Global NFA extended ───────────────────────────────────────────────────────

global_ext <- panel_ext %>%
  filter(!is.na(nfa_dm), !is.na(gdp_usd),
         year >= 1980, year <= y_ext_end) %>%
  mutate(region = case_when(
    iso3c == "USA" ~ "United States",
    iso3c == "JPN" ~ "Japan",
    iso3c %in% eu  ~ "European Union",
    TRUE           ~ "Rest of World"
  )) %>%
  group_by(year, region) %>%
  summarise(nfa_dm_sum = sum(nfa_dm, na.rm = TRUE), .groups = "drop") %>%
  left_join(
    panel_ext %>%
      filter(!is.na(gdp_usd), year >= 1980, year <= y_ext_end) %>%
      group_by(year) %>%
      summarise(world_gdp = sum(gdp_usd, na.rm = TRUE), .groups = "drop"),
    by = "year"
  ) %>%
  mutate(nfa_pct = nfa_dm_sum / world_gdp * 100)

fig_global_ext <- ggplot(
  global_ext,
  aes(x = year, y = nfa_pct, colour = region, linetype = region)
) +
  geom_hline(yintercept = 0, colour = "grey45",
             linewidth = 0.4, linetype = "dashed") +
  geom_vline(xintercept = 2003.5, linetype = "dashed",
             colour = "grey60", linewidth = 0.4) +
  geom_line(linewidth = 0.9) +
  scale_colour_manual(values = c(
    "United States"  = "#2166AC",
    "Japan"          = "#B2182B",
    "European Union" = "grey30",
    "Rest of World"  = "grey65"
  )) +
  scale_linetype_manual(values = c(
    "United States"  = "solid",
    "Japan"          = "longdash",
    "European Union" = "dashed",
    "Rest of World"  = "dotted"
  )) +
  scale_x_continuous(breaks = seq(1980, y_ext_end, 4)) +
  scale_y_continuous(labels = label_number(suffix = "%")) +
  guides(colour   = guide_legend(nrow = 2),
         linetype = guide_legend(nrow = 2)) +
  labs(
    title    = paste0("Net Foreign Asset Positions Including Dark Matter (1980\u2013",
                      y_ext_end, ")"),
    subtitle = "% of world GDP. Dashed vertical = H&S (2006) end-date.",
    x = NULL, y = "% of world GDP"
  ) +
  theme_bw(base_size = 11) +
  theme(legend.position = "bottom",
        legend.title     = element_blank())

save_fig(fig_global_ext, "fig_global_nfa_extended")

# ==============================================================================
# 11. PANEL EXTENSION: time-varying beta in annual dark matter regressions
#
#     Extends Table 4 by replacing static output_vol with the time-varying
#     beta_world and idio_vol. Pooled and FE, same samples as H&S.
# ==============================================================================

d_panel_ext <- panel_ext %>%
  filter(
    iso3c %in% countries_79,
    year  >= y_ext_start,
    year  <= y_ext_end,
    !is.na(ca_dm_gdp),
    !is.na(fdi_liab_gdp),
    !is.na(fdi_assets_gdp),
    !is.na(beta_world),
    !is.na(idio_vol)
  )

if (!requireNamespace("fixest", quietly = TRUE)) install.packages("fixest")
library(fixest)

options("modelsummary_format_numeric_latex" = "plain")

d_panel_ext <- panel_ext %>%
  filter(
    iso3c %in% countries_79,
    year  >= y_ext_start,
    year  <= y_ext_end,
    !is.na(dm_exp_flow_gdp),
    !is.na(fdi_liab_gdp),
    !is.na(fdi_assets_gdp),
    !is.na(beta_world),
    !is.na(idio_vol)
  )

d_panel_ext_restr <- d_panel_ext %>%
  filter(opec_d == 0, hipc_d == 0)

t_panel_ext <- list(
  "Pool -- Full (beta)" = fixest::feols(
    dm_exp_flow_gdp ~ fdi_liab_gdp + fdi_assets_gdp + beta_world,
    data = d_panel_ext
  ),
  
  "Pool -- Restr (beta)" = fixest::feols(
    dm_exp_flow_gdp ~ fdi_liab_gdp + fdi_assets_gdp + beta_world,
    data = d_panel_ext_restr
  ),
  
  "FE -- Full (beta)" = fixest::feols(
    dm_exp_flow_gdp ~ fdi_liab_gdp + fdi_assets_gdp + beta_world | iso3c,
    data = d_panel_ext
  ),
  
  "FE -- Restr (beta)" = fixest::feols(
    dm_exp_flow_gdp ~ fdi_liab_gdp + fdi_assets_gdp + beta_world | iso3c,
    data = d_panel_ext_restr
  ),
  
  "Pool -- Full (decomp)" = fixest::feols(
    dm_exp_flow_gdp ~ fdi_liab_gdp + fdi_assets_gdp + beta_world + idio_vol,
    data = d_panel_ext
  ),
  
  "FE -- Full (decomp)" = fixest::feols(
    dm_exp_flow_gdp ~ fdi_liab_gdp + fdi_assets_gdp + beta_world + idio_vol | iso3c,
    data = d_panel_ext
  )
)

tex_panel_ext <- modelsummary(
  t_panel_ext,
  stars    = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  fmt      = "%.4f",
  gof_omit = "AIC|BIC|Log|Adj|Within|FE",
  coef_rename = c(
    "fdi_liab_gdp"   = "FDI liabilities (\\% GDP)",
    "fdi_assets_gdp" = "FDI assets (\\% GDP)",
    "beta_world"     = "Beta vs. world output",
    "idio_vol"       = "Idiosyncratic volatility"
  ),
  output = "latex_tabular"
)

tex_panel_ext_wrapped <- paste0(
  "\\begin{table}[htbp]\n",
  "\\centering\n",
  "\\caption{Panel Extension: Beta Decomposition of the Insurance Channel (1980--",
  y_ext_end, ")}\n",
  "\\label{tab:panel_ext}\n",
  "\\scriptsize\n",
  tex_panel_ext, "\n",
  "\\begin{minipage}{0.95\\linewidth}\n",
  "\\footnotesize Notes: Annual dark matter exports as dependent variable. ",
  "\\textit{Beta vs.~world output} computed from a 10-year rolling window. ",
  "Columns (1)--(4) use beta only; columns (5)--(6) add idiosyncratic volatility. ",
  "FE specifications include country fixed effects. ",
  "* p$<$0.10, ** p$<$0.05, *** p$<$0.01.\n",
  "\\end{minipage}\n",
  "\\end{table}"
)

writeLines(
  tex_panel_ext_wrapped,
  here("code", "output", "tables", "table_panel_extended.tex")
)

if (exists("compile_table")) {
  compile_table(
    tex_panel_ext_wrapped,
    "table_panel_extended",
    landscape    = TRUE,
    table_number = 8,
    fit_width    = TRUE
  )
}

# ==============================================================================
# EXTENDED REPLICATION TABLES — SAME TABLES AS H&S REPLICATION, 1980–2022
#
# Outputs:
#   Table E1  : Extended Table 1 — Official CA vs dark-matter-implied CA
#   Table E2  : Extended Table 2 — Dark matter exports vs official CA
#   Table E3a : Extended Table 3 — Sources of dark matter, columns (i)--(iii)
#   Table E3b : Extended Table 3 — Sources of dark matter, columns (iv)--(vii)
#   Table E4  : Extended Table 4 — Panel determinants of CA^DM
#   Table E4b : Extended Table 4b — Panel determinants of annual dark matter exports
#
# Requires objects already created earlier:
#   cs_ext, panel_ext, countries_109, countries_79, industrial,
#   y_ext_start, y_ext_end, compile_table()
# ==============================================================================

# ── Setup ─────────────────────────────────────────────────────────────────────

if (!requireNamespace("fixest", quietly = TRUE)) install.packages("fixest")
library(fixest)

options("modelsummary_format_numeric_latex" = "plain")

dir.create(here("code", "output", "tables"),
           recursive = TRUE, showWarnings = FALSE)

if (!exists("compile_table")) {
  stop("compile_table() is not defined. Run the table-compilation helper first.")
}

winsor <- function(x, p = 0.01) {
  if (all(is.na(x))) return(x)
  q <- quantile(x, c(p, 1 - p), na.rm = TRUE)
  pmax(pmin(x, q[2]), q[1])
}

countries_99 <- setdiff(
  countries_109,
  c("BFA", "ETH", "MDG", "MWI", "MLI", "NPL", "NER", "RWA", "TGO", "UGA")
)

# Make the LaTeX table number show E1, E2, E4b, etc.
set_table_id <- function(tex, id) {
  sub(
    "\\begin{table}",
    paste0("\\renewcommand{\\thetable}{", id, "}\n\\begin{table}"),
    tex,
    fixed = TRUE
  )
}

# Rebuild annual dark matter variables safely using dplyr::lag()
panel_ext <- panel_ext %>%
  arrange(iso3c, year) %>%
  group_by(iso3c) %>%
  mutate(
    ca_dm           = nfa_dm - dplyr::lag(nfa_dm),
    ca_dm_gdp       = ca_dm / gdp_usd * 100,
    dm_exp_flow     = ca_dm - ca_usd,
    dm_exp_flow_gdp = dm_exp_flow / gdp_usd * 100
  ) %>%
  ungroup()

# Ratio variables for Table E3
cs_ext <- cs_ext %>%
  mutate(
    dm_exp_ratio     = dm_exp_gdp / 100,
    fdi_assets_ratio = fdi_assets_gdp / 100,
    fdi_liab_ratio   = fdi_liab_gdp / 100
  )

# Static cross-sectional output volatility for panel tables
output_vol_ext <- cs_ext %>%
  select(iso3c, output_vol_ext = output_vol_hp)

panel_ext_aug <- panel_ext %>%
  left_join(output_vol_ext, by = "iso3c")

# Safety check
check_ca_dm <- panel_ext_aug %>%
  filter(
    iso3c %in% countries_79,
    year >= y_ext_start,
    year <= y_ext_end
  ) %>%
  summarise(
    n_ca_dm_gdp = sum(!is.na(ca_dm_gdp)),
    distinct_ca_dm_gdp = n_distinct(ca_dm_gdp, na.rm = TRUE),
    sd_ca_dm_gdp = sd(ca_dm_gdp, na.rm = TRUE),
    nonzero_ca_dm_gdp = sum(ca_dm_gdp != 0, na.rm = TRUE)
  )

print(check_ca_dm)

if (check_ca_dm$distinct_ca_dm_gdp <= 1 || check_ca_dm$sd_ca_dm_gdp == 0) {
  stop("ca_dm_gdp is constant. Check dplyr::lag() and BOP import before running extended tables.")
}

# ==============================================================================
# TABLE E1 — Extended Table 1
# Cumulative official current account vs. dark-matter-implied current account
# ==============================================================================

d1_ext <- cs_ext %>%
  filter(
    iso3c %in% countries_109,
    !is.na(cum_dm_bn),
    !is.na(cum_oca_bn)
  )

t1_ext <- list(
  "Full"                = lm(cum_oca_bn ~ cum_dm_bn, d1_ext),
  "Excl. USA"           = lm(cum_oca_bn ~ cum_dm_bn,
                             filter(d1_ext, iso3c != "USA")),
  "Excl. USA, GBR"      = lm(cum_oca_bn ~ cum_dm_bn,
                             filter(d1_ext, !iso3c %in% c("USA", "GBR"))),
  "Excl. USA, GBR, JPN" = lm(cum_oca_bn ~ cum_dm_bn,
                             filter(d1_ext, !iso3c %in% c("USA", "GBR", "JPN")))
)

tex1_ext <- capture.output(
  stargazer(
    t1_ext[[1]], t1_ext[[2]], t1_ext[[3]], t1_ext[[4]],
    column.labels    = names(t1_ext),
    title            = paste0(
      "Extended Table 1: Cumulative Current Account and Dark-Matter-Implied Current Account (",
      y_ext_start, "--", y_ext_end, ")"
    ),
    label            = "tab:E1_extended_CA_vs_DM",
    dep.var.labels   = "Cumulative official CA (\\$bn)",
    covariate.labels = c("Dark-matter-implied CA (\\$bn)", "Constant"),
    omit.stat        = c("f", "ser", "adj.rsq"),
    notes            = "Standard errors in parentheses. * p$<$0.10, ** p$<$0.05, *** p$<$0.01.",
    notes.align      = "l",
    style            = "aer",
    type             = "latex"
  )
)

tex1_ext <- set_table_id(paste(tex1_ext, collapse = "\n"), "E1")

compile_table(
  tex1_ext,
  "table_E1_extended_CA_vs_DM",
  table_number = 1,
  fit_width = FALSE
)

# ==============================================================================
# TABLE E2 — Extended Table 2
# Dark matter exports vs. official current account
# ==============================================================================

d2_ext <- cs_ext %>%
  filter(
    iso3c %in% countries_109,
    !is.na(dm_exp_gdp),
    !is.na(cum_oca_gdp)
  )

t2_ext <- list(
  "Full"           = lm(dm_exp_gdp ~ cum_oca_gdp, d2_ext),
  "Excl. USA"      = lm(dm_exp_gdp ~ cum_oca_gdp,
                        filter(d2_ext, iso3c != "USA")),
  "Excl. USA, GBR" = lm(dm_exp_gdp ~ cum_oca_gdp,
                        filter(d2_ext, !iso3c %in% c("USA", "GBR")))
)

tex2_ext <- capture.output(
  stargazer(
    t2_ext[[1]], t2_ext[[2]], t2_ext[[3]],
    column.labels    = names(t2_ext),
    title            = paste0(
      "Extended Table 2: Dark Matter Exports and the Official Current Account (",
      y_ext_start, "--", y_ext_end, ")"
    ),
    label            = "tab:E2_extended_DM_exports_vs_CA",
    dep.var.labels   = paste0("Dark matter exports (\\% of ", y_ext_end, " GDP)"),
    covariate.labels = c(paste0("Official CA (\\% of ", y_ext_end, " GDP)"), "Constant"),
    omit.stat        = c("f", "ser", "adj.rsq"),
    notes            = "Standard errors in parentheses. * p$<$0.10, ** p$<$0.05, *** p$<$0.01.",
    notes.align      = "l",
    style            = "aer",
    type             = "latex"
  )
)

tex2_ext <- set_table_id(paste(tex2_ext, collapse = "\n"), "E2")

compile_table(
  tex2_ext,
  "table_E2_extended_DM_exports_vs_CA",
  table_number = 2,
  fit_width = FALSE
)

# ==============================================================================
# TABLE E3 — Extended Table 3
# Sources of dark matter: cross-section evidence
# ==============================================================================

d3_79_ext <- cs_ext %>%
  filter(
    iso3c %in% countries_79,
    !is.na(dm_exp_ratio),
    !is.na(fdi_assets_ratio),
    !is.na(fdi_liab_ratio),
    !is.na(output_vol_hp)
  ) %>%
  mutate(across(
    c(dm_exp_ratio, fdi_assets_ratio, fdi_liab_ratio, output_vol_hp),
    winsor
  ))

d3_99_ext <- cs_ext %>%
  filter(
    iso3c %in% countries_99,
    !is.na(dm_exp_ratio),
    !is.na(fdi_assets_ratio),
    !is.na(fdi_liab_ratio),
    !is.na(output_vol_hp)
  ) %>%
  mutate(across(
    c(dm_exp_ratio, fdi_assets_ratio, fdi_liab_ratio, output_vol_hp),
    winsor
  ))

d3_ind_ext <- cs_ext %>%
  filter(
    iso3c %in% industrial,
    !is.na(dm_exp_ratio),
    !is.na(fdi_assets_ratio),
    !is.na(fdi_liab_ratio),
    !is.na(output_vol_hp)
  ) %>%
  mutate(across(
    c(dm_exp_ratio, fdi_assets_ratio, fdi_liab_ratio, output_vol_hp),
    winsor
  ))

t3_ext <- list(
  "(i)" = lm(
    dm_exp_ratio ~ fdi_assets_ratio + fdi_liab_ratio + output_vol_hp,
    data = d3_79_ext
  ),
  
  "(ii)" = lm(
    dm_exp_ratio ~ fdi_assets_ratio + fdi_liab_ratio + output_vol_hp +
      rule_of_law,
    data = d3_79_ext
  ),
  
  "(iii)" = lm(
    dm_exp_ratio ~ fdi_assets_ratio + fdi_liab_ratio + output_vol_hp +
      rule_of_law + opec + hipc,
    data = d3_79_ext
  ),
  
  "(iv)" = lm(
    dm_exp_ratio ~ fdi_assets_ratio + fdi_liab_ratio + output_vol_hp,
    data = d3_99_ext
  ),
  
  "(v)" = lm(
    dm_exp_ratio ~ fdi_assets_ratio + fdi_liab_ratio + output_vol_hp +
      rule_of_law,
    data = d3_99_ext
  ),
  
  "(vi)" = lm(
    dm_exp_ratio ~ fdi_assets_ratio + fdi_liab_ratio + output_vol_hp +
      rule_of_law + opec + hipc,
    data = d3_99_ext
  ),
  
  "(vii) Industrial" = lm(
    dm_exp_ratio ~ fdi_assets_ratio + fdi_liab_ratio + output_vol_hp,
    data = d3_ind_ext
  )
)

print(sapply(t3_ext, nobs))

coef_names_t3_ext <- c(
  "fdi_assets_ratio" = "FDI assets / GDP",
  "fdi_liab_ratio"   = "FDI liabilities / GDP",
  "output_vol_hp"    = "Output volatility",
  "rule_of_law"      = "Rule of Law",
  "opec"             = "OPEC dummy",
  "hipc"             = "HIPC dummy"
)

t3_left_ext  <- t3_ext[1:3]
t3_right_ext <- t3_ext[4:7]

tex3_left_ext <- modelsummary(
  t3_left_ext,
  stars       = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  fmt         = "%.3f",
  coef_rename = coef_names_t3_ext,
  gof_omit    = "AIC|BIC|Log|F|RMSE",
  output      = "latex_tabular"
)

tex3_right_ext <- modelsummary(
  t3_right_ext,
  stars       = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  fmt         = "%.3f",
  coef_rename = coef_names_t3_ext,
  gof_omit    = "AIC|BIC|Log|F|RMSE",
  output      = "latex_tabular"
)

wrap_table_ext <- function(tabular, caption, label, table_id) {
  paste0(
    "\\renewcommand{\\thetable}{", table_id, "}\n",
    "\\begin{table}[htbp]\n",
    "\\centering\n",
    "\\caption{", caption, "}\n",
    "\\label{", label, "}\n",
    "\\scriptsize\n",
    tabular, "\n",
    "\\begin{minipage}{0.95\\linewidth}\n",
    "\\footnotesize Notes: Standard errors in parentheses. ",
    "Dependent variable: cumulative dark matter exports over ",
    y_ext_start, "--", y_ext_end, ", divided by ", y_ext_end, " GDP. ",
    "Variables are expressed as ratios where relevant and winsorised at the 1\\% level. ",
    "Columns (i)--(iii) use the H\\&S 79-country sample when data are available. ",
    "Columns (iv)--(vi) use the H\\&S 99-country sample when data are available. ",
    "Column (vii) uses the H\\&S industrial-country sample when data are available. ",
    "R\\&D is omitted from the extended specification because the extended cross-section contains no usable R\\&D observations.",
    "* p$<$0.10, ** p$<$0.05, *** p$<$0.01.\n",
    "\\end{minipage}\n",
    "\\end{table}"
  )
}

tex3a_ext_wrapped <- wrap_table_ext(
  tex3_left_ext,
  paste0(
    "Extended Table 3a: Sources of Dark Matter, 79-Country Sample (",
    y_ext_start, "--", y_ext_end, ")"
  ),
  "tab:E3a_extended_sources_79",
  "E3a"
)

tex3b_ext_wrapped <- wrap_table_ext(
  tex3_right_ext,
  paste0(
    "Extended Table 3b: Sources of Dark Matter, 99-Country and Industrial Samples (",
    y_ext_start, "--", y_ext_end, ")"
  ),
  "tab:E3b_extended_sources_99_industrial",
  "E3b"
)

compile_table(
  tex3a_ext_wrapped,
  "table_E3a_extended_sources_79",
  landscape = FALSE,
  table_number = 3,
  fit_width = TRUE
)

compile_table(
  tex3b_ext_wrapped,
  "table_E3b_extended_sources_99_industrial",
  landscape = FALSE,
  table_number = 3,
  fit_width = TRUE
)

# ==============================================================================
# TABLE E4 — Extended Table 4
# Panel determinants of dark-matter-implied current account
#
# Dependent variable:
#   ca_dm_gdp = change in income-implied NFA / GDP
# ==============================================================================

d4_ext <- panel_ext_aug %>%
  filter(
    iso3c %in% countries_79,
    year >= y_ext_start,
    year <= y_ext_end,
    !is.na(ca_dm_gdp),
    !is.na(fdi_liab_gdp),
    !is.na(fdi_assets_gdp),
    !is.na(output_vol_ext)
  )

d4_ext_restr <- d4_ext %>%
  filter(opec_d == 0, hipc_d == 0)

d4_ext_ind <- d4_ext %>%
  filter(iso3c %in% industrial)

t4_ext <- list(
  "Pool -- Full" = fixest::feols(
    ca_dm_gdp ~ fdi_liab_gdp + fdi_assets_gdp + output_vol_ext,
    data = d4_ext
  ),
  
  "Pool -- Restr" = fixest::feols(
    ca_dm_gdp ~ fdi_liab_gdp + fdi_assets_gdp + output_vol_ext,
    data = d4_ext_restr
  ),
  
  "Pool -- Ind" = fixest::feols(
    ca_dm_gdp ~ fdi_liab_gdp + fdi_assets_gdp + output_vol_ext,
    data = d4_ext_ind
  ),
  
  "FE -- Full" = fixest::feols(
    ca_dm_gdp ~ fdi_liab_gdp + fdi_assets_gdp + output_vol_ext | iso3c,
    data = d4_ext
  ),
  
  "FE -- Restr" = fixest::feols(
    ca_dm_gdp ~ fdi_liab_gdp + fdi_assets_gdp + output_vol_ext | iso3c,
    data = d4_ext_restr
  ),
  
  "FE -- Ind" = fixest::feols(
    ca_dm_gdp ~ fdi_liab_gdp + fdi_assets_gdp + output_vol_ext | iso3c,
    data = d4_ext_ind
  )
)

print(sapply(t4_ext, nobs))

tex4_ext_ms <- modelsummary(
  t4_ext,
  stars    = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  fmt      = "%.4f",
  gof_omit = "AIC|BIC|Log|Adj|Within|FE",
  coef_rename = c(
    "fdi_liab_gdp"   = "FDI liabilities (\\% GDP)",
    "fdi_assets_gdp" = "FDI assets (\\% GDP)",
    "output_vol_ext" = "Output volatility"
  ),
  output = "latex_tabular"
)

tex4_ext_wrapped <- paste0(
  "\\renewcommand{\\thetable}{E4}\n",
  "\\begin{table}[htbp]\n",
  "\\centering\n",
  "\\caption{Extended Table 4: Panel Determinants of the Dark-Matter-Implied Current Account (",
  y_ext_start, "--", y_ext_end, ")}\n",
  "\\label{tab:E4_extended_panel_CA_DM}\n",
  "\\scriptsize\n",
  tex4_ext_ms, "\n",
  "\\begin{minipage}{0.95\\linewidth}\n",
  "\\footnotesize Notes: The dependent variable is the dark-matter-implied current account, ",
  "$CA^{DM}_{it}$, defined as the annual change in income-implied net foreign assets, divided by GDP. ",
  "Output volatility is the extended H\\&S-style HP-filtered volatility measure. ",
  "Pooled and country fixed-effects specifications are reported. ",
  "* p$<$0.10, ** p$<$0.05, *** p$<$0.01.\n",
  "\\end{minipage}\n",
  "\\end{table}"
)

compile_table(
  tex4_ext_wrapped,
  "table_E4_extended_panel_CA_DM",
  landscape = TRUE,
  table_number = 4,
  fit_width = TRUE
)

# ==============================================================================
# TABLE E4b — Extended Table 4b
# Panel determinants of annual dark matter exports
#
# Dependent variable:
#   dm_exp_flow_gdp = (CA^DM - official CA) / GDP
# ==============================================================================

d4b_ext <- panel_ext_aug %>%
  filter(
    iso3c %in% countries_79,
    year >= y_ext_start,
    year <= y_ext_end,
    !is.na(dm_exp_flow_gdp),
    !is.na(fdi_liab_gdp),
    !is.na(fdi_assets_gdp),
    !is.na(output_vol_ext)
  )

d4b_ext_restr <- d4b_ext %>%
  filter(opec_d == 0, hipc_d == 0)

d4b_ext_ind <- d4b_ext %>%
  filter(iso3c %in% industrial)

t4b_ext <- list(
  "Pool -- Full" = fixest::feols(
    dm_exp_flow_gdp ~ fdi_liab_gdp + fdi_assets_gdp + output_vol_ext,
    data = d4b_ext
  ),
  
  "Pool -- Restr" = fixest::feols(
    dm_exp_flow_gdp ~ fdi_liab_gdp + fdi_assets_gdp + output_vol_ext,
    data = d4b_ext_restr
  ),
  
  "Pool -- Ind" = fixest::feols(
    dm_exp_flow_gdp ~ fdi_liab_gdp + fdi_assets_gdp + output_vol_ext,
    data = d4b_ext_ind
  ),
  
  "FE -- Full" = fixest::feols(
    dm_exp_flow_gdp ~ fdi_liab_gdp + fdi_assets_gdp + output_vol_ext | iso3c,
    data = d4b_ext
  ),
  
  "FE -- Restr" = fixest::feols(
    dm_exp_flow_gdp ~ fdi_liab_gdp + fdi_assets_gdp + output_vol_ext | iso3c,
    data = d4b_ext_restr
  ),
  
  "FE -- Ind" = fixest::feols(
    dm_exp_flow_gdp ~ fdi_liab_gdp + fdi_assets_gdp + output_vol_ext | iso3c,
    data = d4b_ext_ind
  )
)

print(sapply(t4b_ext, nobs))

tex4b_ext_ms <- modelsummary(
  t4b_ext,
  stars    = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  fmt      = "%.4f",
  gof_omit = "AIC|BIC|Log|Adj|Within|FE",
  coef_rename = c(
    "fdi_liab_gdp"   = "FDI liabilities (\\% GDP)",
    "fdi_assets_gdp" = "FDI assets (\\% GDP)",
    "output_vol_ext" = "Output volatility"
  ),
  output = "latex_tabular"
)

tex4b_ext_wrapped <- paste0(
  "\\renewcommand{\\thetable}{E4b}\n",
  "\\begin{table}[htbp]\n",
  "\\centering\n",
  "\\caption{Extended Table 4b: Panel Determinants of Annual Dark Matter Exports (",
  y_ext_start, "--", y_ext_end, ")}\n",
  "\\label{tab:E4b_extended_panel_DM_exports}\n",
  "\\scriptsize\n",
  tex4b_ext_ms, "\n",
  "\\begin{minipage}{0.95\\linewidth}\n",
  "\\footnotesize Notes: The dependent variable is annual dark matter exports, ",
  "$CA^{DM}_{it} - CA^{official}_{it}$, divided by GDP. ",
  "Output volatility is the extended H\\&S-style HP-filtered volatility measure. ",
  "Pooled and country fixed-effects specifications are reported. ",
  "* p$<$0.10, ** p$<$0.05, *** p$<$0.01.\n",
  "\\end{minipage}\n",
  "\\end{table}"
)

compile_table(
  tex4b_ext_wrapped,
  "table_E4b_extended_panel_DM_exports",
  landscape = TRUE,
  table_number = 4,
  fit_width = TRUE
)

# ==============================================================================
# Done
# ==============================================================================

message("\nExtended replication tables done.")
message("Output files:")
message("  table_E1_extended_CA_vs_DM.pdf")
message("  table_E2_extended_DM_exports_vs_CA.pdf")
message("  table_E3a_extended_sources_79.pdf")
message("  table_E3b_extended_sources_99_industrial.pdf")
message("  table_E4_extended_panel_CA_DM.pdf")
message("  table_E4b_extended_panel_DM_exports.pdf")
message("Location: code/output/tables/")





