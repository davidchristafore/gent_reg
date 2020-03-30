# Article: The Influence of Land Use Regulation on the Probability Low-Income
# Neighborhoods Will Gentrify
# Authors: David Christafore and Susane Leguizamon
# Code by: David Christafore
# This version: March 30, 2020

library(tidyverse)
library(haven)
library(xtable)
library(texreg)
library(lmtest)
library(sandwich)
library(mfx)
select <- dplyr::select # mfx overwrites dplyr select

# Load data ----------

tract_00_join <-
  read_csv("LTDB_Std_2000_Sample.csv") %>%
  select(-state, -county, -tract, -placefp10, -cbsa10, -metdiv10, -ccflag10) %>%
  left_join(read_csv("LTDB_Std_2000_fullcount.csv"), by = "TRTID10")

sample_12 <-
  read_csv("LTDB_Std_2010_Sample.csv") %>%
  mutate(
    TRTID10 = as.numeric(str_c(statea, countya, tracta)),
    county_id = as.character(as.numeric(str_c(statea, countya)))
  )

tract_area <-
  read_csv(file = "DEC_10_SF1_G001_with_ann.csv", skip = 1) %>%
  mutate(
    TRTID10 = as.numeric(Id2),
    area = `AREA CHARACTERISTICS - Area (Land)` / 1000000 # m^2 to km^2
  ) %>%
  select(TRTID10, area)

tract_join <-
  tract_00_join %>%
  inner_join(sample_12, by = "TRTID10") %>% # Keep only tracts in 00 & 12 data
  left_join(tract_area, by = "TRTID10")

fwf_positions <-
  fwf_positions(
    c(1, 9, 25), c(4, 12, 29),
    col_names = c("msacmsa_id", "pmsa_id", "county_id")
)

# MSA codes in the regulation data are for MSAs/PMSAs, while county_to_msa
# includes separate columns for the MSA/CMSA id and PMSA id. Therefore,
# I create msa_id by using either the MSA or PMSA code for a place.

county_to_msa <-
  read_fwf("99mfips.txt", fwf_positions) %>%
  filter(!is.na(county_id)) %>% # rm blank lines & msa name holders
  mutate(msa_id = as.numeric(ifelse(!is.na(pmsa_id), pmsa_id, msacmsa_id))) %>%
  distinct(county_id, .keep_all = TRUE) %>%
  mutate(county_id = as.character(as.numeric(county_id))) %>%
  select(county_id, msa_id)

wrluri_by_msa <-
  read_csv("MSA_Data.csv") %>%
  mutate(region = as_factor(Region)) %>%
  select(msa_id = Code_99, msa_name = MSA_Name, wrluri = WRLURI, region)

# The following MSA codes are in the regulation but not land availability
# data: 875 bergen, 5945 orange, 5380 nassau, 5190 monmouth. Therefore, they
# will be dropped in combining the regulation and availability data.

dev_land <-
  read_dta("HOUSING_SUPPLY.dta") %>%
  select(msa_id = msanecma, unaval)

# dev_land uses the NECMA id for New England MSAs. I change these to the MSA ids
# used in the regulation data for Boston, Hartford, Providence, and Springfield.

dev_land[dev_land$msa_id == 1123, "msa_id"] = 1120 # Boston
dev_land[dev_land$msa_id == 3283, "msa_id"] = 3280 # Hartford
dev_land[dev_land$msa_id == 6483, "msa_id"] = 6480 # Providence
dev_land[dev_land$msa_id == 8003, "msa_id"] = 8000 # Springfield

full_wharton <-
  read_dta("WHARTON LAND REGULATION DATA_1_24_2008.dta") %>%
  filter(!is.na(WRLURI)) %>%
  select(msa99, ADI, SRI, EI, OSI, DRI, LAI, LPAI, LZAI, SCII, SPII, LPPI) %>%
  group_by(msa99) %>%
  summarise_all(mean)

# Census data on the same variables is provided in different files for MSAs and
# PMSAs. create_msa is a function that takes these separate files and combines
# them such that each row is an observation for each MSA, CMSA, or PMSA id.

create_msa <- function(msacmsa, pmsa) {
  df_msacmsa <- read_csv(msacmsa, skip = 1)
  df_pmsa <-
    read_csv(pmsa, skip = 1) %>%
    mutate(Id2 = str_extract(Id2, "\\d{4}$"))
  bind_rows(df_msacmsa, df_pmsa)
}

pop00_msa <- create_msa(
  "DEC_00_SF3_P001_with_ann.csv",
  "DEC_00_SF3_P001_with_ann_p.csv"
)

hinc00_msa <- create_msa(
  "DEC_00_SF3_P053_with_ann.csv",
  "DEC_00_SF3_P053_with_ann_p.csv"
)

unemp00_msa <- create_msa(
  "DEC_00_SF3_QTP24_with_ann.csv",
  "DEC_00_SF3_QTP24_with_ann_p.csv"
) %>%
  select(c(1:3, 37))

names(unemp00_msa)[4] <- "unemp00_msa"

msa_vars <-
  pop00_msa %>%
  left_join(hinc00_msa, by =c("Id2", "Id", "Geography")) %>%
  left_join(unemp00_msa, by = c("Id", "Id2", "Geography")) %>%
  mutate(msa_id = as.numeric(Id2)) %>%
  select(
    msa_id,
    pop00_msa = Total,
    hinc00_msa = `Median household income in 1999`,
    unemp00_msa
  )

msa_data <-
  wrluri_by_msa %>%
  inner_join(dev_land, by = "msa_id") %>% # Drops MSAs not in availability
  inner_join(full_wharton, by = c("msa_id" = "msa99")) %>%
  filter(!msa_id == 8000) %>% # Drops Springfield with no city tracts
  mutate_at(vars(-msa_id, -msa_name, -region, -unaval), list(zscore = scale))

tract_join_msa <-
  tract_join %>%
  inner_join(county_to_msa, by = "county_id") %>% # Drops non-MSA tracts
  inner_join(msa_data, by = "msa_id") %>% # 29,523 tracts in 42 MSAs remain
  inner_join(msa_vars, by = "msa_id")

# Clean data ----------

names(tract_join_msa) <- tolower(colnames(tract_join_msa))

# I only want to include populated tracts without missing home value, rent,
# or income data or 0 values for these variables. For rent, I use the cutoff of
# 100 rather than 0 as some rents are suspiciously low. Note that -999
# represents missing in 2012.

tract_data <-
  tract_join_msa %>%
  filter(
    pop00 > 49, pop12 > 49,
    !is.na(mhmval00), mhmval00 > 0, mhmval12 != -999,
    !is.na(mrent00), mrent00 > 99, mrent12 != -999,
    !is.na(hinc00), hinc12 != -999
  )

# The 2008-2012 data is in 2010 dollars, while the 2000 data is in 1999 dollars.
# I deflate the 2008-2012 dollars to 2000 dollars with the CPI ratio
# 166.6 / 218.056; the base for these CPIs is chained 1982-1984 dollars.

deflator <- 166.6 / 218.056

tract_data <-
  tract_data %>%
  mutate_at(vars(hinc12, mhmval12, incpc12, mrent12), function(x) (x * deflator))

# Create variables ----------

tract_perc <-
  tract_data %>%
  transmute(
    nhwht00 = nhwht00 / pop00,
    nhwht12 = nhwht12 / pop12,
    col00 = col00 / ag25up00,
    col12 = col12 / ag25up12,
    fb00 = fb00 / pop00sf3,
    fb12 = fb12 / pop12,
    h30old00 = h30old00 / hu00sp,
    h30old12 = h30old12 / hu12,
    vac00 = vac00 / hu00,
    vac12 = vac12 / hu12,
    own00 = own00 / ohu00,
    own12 = own12 / ohu12,
    unemp00 = unemp00 / clf00,
    unemp12 = unemp12 / clf12,
  ) %>%
  mutate_all(function(x) (x * 100))

tract_data <-
  tract_data %>%
  mutate(density00 = pop00 / area, density12 = pop12 / area)

tract_logs <-
  tract_data %>%
  transmute_at(
    vars(matches("^(pop\\d{2}$|mhmval|hinc\\d{2}|density|pop\\d{2}_)")),
    list(l = log)
  )

tile_vars <- c("hinc00", "hinc12", "mhmval00", "mhmval12")

# The quartiles and deciles are calculated within MSAs

tract_qtiles <-
  tract_data %>%
  group_by(msa_id) %>%
  transmute_at(
    tile_vars,
    list(qtile = ntile),
    n = 4
  ) %>%
  ungroup() %>%
  select(-msa_id)

tract_dtiles <-
  tract_data %>%
  group_by(msa_id) %>%
  transmute_at(
    tile_vars,
    list(dtile = ntile),
    n = 10
  ) %>%
  ungroup() %>%
  select(-msa_id)

comb_data <-
  tract_data %>%
  select(trtid10, state:ccflag10, msa_id:density12, tile_vars, mrent00, mrent12) %>%
  bind_cols(tract_perc, tract_logs, tract_qtiles, tract_dtiles)

tract_growth <-
  comb_data %>%
  mutate(
    hinc_growth = 100 * ((hinc12 - hinc00) / hinc00),
    mhmval_growth = 100 * ((mhmval12 - mhmval00) / mhmval00),
    col_change = col12 - col00,
    mrent_growth = 100 * ((mrent12 - mrent00) / mrent00)
  )

to_msa_med <- c("hinc_growth", "mhmval_growth", "col_change", "mrent_growth")

tract_set <-
  tract_growth %>%
  group_by(msa_id) %>%
  mutate_at(to_msa_med, list(msa_med = median)) %>%
  ungroup()

# Gentrification is defined as above MSA tract median growth of the college
# educated population & either above MSA tract median positive growth in
# home value or rent. The following code is applied to all tracts, but in
# the gent. regressions only gentrifiable tracts (those in bottom 25% or 40%
# of MSA income will be used).

gent_data <-
  tract_set %>%
  mutate(
    col_cond = if_else(col_change > col_change_msa_med, T, F),
    rent_cond1 = if_else(mrent_growth > mrent_growth_msa_med, T, F),
    rent_cond2 = if_else(mrent_growth > 0 & rent_cond1, T, F),
    mhmval_cond1 = if_else(mhmval_growth > mhmval_growth_msa_med, T, F),
    mhmval_cond2 = if_else(mhmval_growth > 0 & mhmval_cond1, T, F),
    hinc_cond1 = if_else(hinc_growth > hinc_growth_msa_med, T, F),
    hinc_cond2 = if_else(hinc_growth > 0 & hinc_cond1, T, F),
    gent_alt = if_else(col_cond & (rent_cond2 | mhmval_cond2), 1, 0)
  )

# Only city tracts will be considered for gentrification the rest of the
# analysis is limited to them. This results in 12,576 tracts.

cc_data <- gent_data %>% filter(ccflag10 == 1)
write_csv(cc_data, 'cc_data.csv')

# Create Table 1 ----------

msa_table_1 <-
  cc_data %>%
  group_by(msa_name) %>%
  filter(hinc00_qtile == 1) %>%
  summarise(`Prop. Gent.` = mean(gent_alt == 1)) %>%
  select(`Prop. Gent.`)

msa_table <-
  cc_data %>%
  group_by(msa_name) %>%
  summarise(
    WRLURI = mean(wrluri),
    Tracts = n(),
    Gentrifiable = sum(hinc00_qtile == 1)
  ) %>%
  rename(MSA = msa_name) %>%
  cbind(msa_table_1)

# make_xtable is a function to allow me to reuse settings to make my desired
# Latex tables from tibbles for non-results tables

make_xtable <- function(df, caption, fn, note) {
  print(
    xtable(df, caption = caption),
    digits = 2,
    file = fn,
    include.rownames = FALSE,
    add.to.row = list(pos = list(nrow(df)),
                      command = paste("\\hline \n", note)),
    hline.after = c(-1,0),
    caption.placement = "top",
    booktabs = TRUE,
    comment = FALSE
  )
}

caption <- c("Proportion of Low Income Tracts Gentrified by MSA")

note <- paste(
  "\\multicolumn{5}{l}",
  "{\\scriptsize",
  "All city tracts included in the analysis.",
  "Gentrifiable tracts are those in the first income quartile.}"
)

note <- ("\\parbox{.6\\linewidth}{\\vspace{2pt}\\scriptsize
All city tracts included in the analysis and gentrifiable tracts are those
in the first income quartile.\\\\
Prop. Gent. is the proportion of gentrifiable tracts that gentrified.}")

make_xtable(msa_table, caption, "tables/tab1.tex", note)

# Create Table 2 ----------

for_stats <- c(
  "col00", "density00_l", "fb00", "h30old00", "gent_alt", "hinc00_l",
  "hinc00_msa_l", "mhmval00_l", "nhwht00", "own00", "pop00_l", "pop00_msa_l",
  "unemp00_msa", "unemp00", "vac00", "wrluri", "lppi", "spii", "scii", "lzai",
  "lai", "lpai", "sri", "dri", "ei", "osi", "adi", "unaval"
)

# create_stat_vec is a function to return a vector of means or standard
# deviations for the variables in for_stats for a given tibble.

create_stat_vec <- function(df, stat) {
  df %>%
    summarize_at(for_stats, stat)  %>%
    as_vector()
}

Mean <- create_stat_vec(cc_data, mean)
Sd <- create_stat_vec(cc_data, sd)

Variable <- c(
  "Bachelors", "Density", "Foreign Born", "Over 30",
  "Gentrified", "HH Income", "MSA HH Income", "Home Value",
  "White", "Owner Occupied", "Population", "Msa Population",
  "MSA Unemployment", "Unemployment", "Vacant",
  "WRLURI", "LPPI", "SPII", "SCII", "LZAI", "LAI", "LPAI", "SRI", "DRI", "EI",
  "OSI", "ADI", "Unavailable"
)

Description <- c(
  "Percent of 25 and over population with a bachelor's degree.",
  "Natural log of population density (per square kilometer)",
  "Percent of population that is foreign born.",
  "Percent of housing units greater than 30 years old",
  "Equal to 1 if tract gentrified and 0 otherwise.",
  "Natural log of median household income.",
  "Natural log of MSA median household income.",
  "Natural log of median house value.",
  "Percent of population that is non-Hispanic white.",
  "Percent of occupied housing units that are owner occupied.",
  "Natural log of population.",
  "Natural log of MSA population.",
  "Percent of MSA labor force that is unemployed.",
  "Percent of labor force that is unemployed",
  "Percent of housing units that are vacant",
  "Wharton Land Use Regulation Index",
  "Local Political Pressure Index",
  "State Political Involvement Index",
  "State Court Involvement Index",
  "Local Zoning Approval Index",
  "Local Assembly Index",
  "Local Project Approval Index",
  "Supply Restrictions Index",
  "Density Restrictions Index",
  "Exactions Index",
  "Open Space Index",
  "Approval Delay Index",
  "Land Unavailability Index"
)

def_tab <- tibble(Variable, Description, Mean, Sd)

caption <- c("Variable Definitions and Statistics")
note <- paste(
  "\\multicolumn{4}{l}",
  "{\\scriptsize",
  "All city tracts included in the analysis.",
  "Gentrifiable tracts are those in the first income quartile.}"
)
make_xtable(def_tab, caption, "tables/tab2.tex", note)

# Create Table 3 ----------

low_inc_gent <-
  cc_data %>%
  filter(hinc00_qtile == 1, gent_alt == 1)

low_inc_non_gent <-
  cc_data %>%
  filter(hinc00_qtile == 1, gent_alt == 0)

Mean_Gent <- create_stat_vec(low_inc_gent, mean)
Sd_Gent <- create_stat_vec(low_inc_gent, sd)

Mean_Non_Gent <- create_stat_vec(low_inc_non_gent, mean)
Sd_Non_Gent <- create_stat_vec(low_inc_non_gent, sd)

stat_gent_tab <- tibble(Variable, Mean_Gent, Sd_Gent, Mean_Non_Gent, Sd_Non_Gent)

caption <- c("Statistics by Gentrification Status")
note <- paste(
  "\\multicolumn{5}{l}",
  "{\\scriptsize",
  "Only gentrifiable tracts included in the analysis.",
  "Gentrifiable tracts are those in the first income quartile.}"
)
make_xtable(stat_gent_tab, caption, "tables/tab3.tex", note)

# Create Table 4 ----------

# make_texreg is a function to create results tables using some already
# preset options in texreg.

make_texreg <- function(mods, fn, note, ses, pvals, caption, coef_names, mod_names) {
  texreg(
    mods,
    file = fn,
    custom.note = paste("%stars.", note),
    digits = 4,
    override.se = ses,
    override.pvalues = pvals,
    caption = caption,
    custom.model.names = mod_names,
    custom.coef.names = coef_names,
    include.rmse = FALSE,
    include.rsquared = FALSE,
    include.aic = FALSE,
    include.loglik = FALSE,
    include.bic = FALSE,
    include.deviance = FALSE,
    center = FALSE,
    caption.above = TRUE,
    booktabs = TRUE,
    stars = c(.01, .05, .10),
    use.packages = FALSE
  )
}

by_qtile <-
  cc_data %>%
  group_by(hinc00_qtile) %>%
  group_split()

formula <- "I(mhmval12_l - mhmval00_l) ~ pop00_l + hinc00_l + density00_l +
  nhwht00 + col00 + fb00 + unemp00 + h30old00 + vac00 + own00 + wrluri_zscore +
  unemp00_msa + hinc00_msa_l + pop00_msa_l + unaval"

mods <- map(by_qtile, ~ lm(as.formula(formula), data = .))

tests <- map(mods, ~ coeftest(., vcov = vcovCL(., type="HC1", cluster = ~msa_name)))
ses <- map(tests, function(x) x[, 2])
pvals <- map(tests, function(x) x[, 4])

coef_names <- c("Intercept", "Population", "HH Income", "Density", "White",
                "Bachelors", "Foreign Born", "Unemployment", "Over 30",
                "Vacant", "Owner Occupied", "WRLURI", "MSA Unemployment",
                "MSA HH Income", "MSA Population", "Unavailable")

caption <- paste("OLS Results with Change in Log Home Value",
                "between 2010 and 2000, as the Dependent Variable,",
                "by Income Quartile")

note <- "Standard errors clustered by MSA in parentheses."

mod_names <- c("Inc. Quartile 1", "Inc. Quartile 2", "Inc. Quartile 3",
               "Inc. Quartile 4")

make_texreg(mods, "tables/tab4.tex", note, ses, pvals, caption, coef_names, mod_names)

# Create Table 5 ----------

data_probit <-
  cc_data %>%
  filter(hinc00_qtile == 1)

form_msa <- gent_alt ~ wrluri_zscore + unemp00_msa + hinc00_msa_l +
  pop00_msa_l + unaval

form_region <- update(form_msa, . ~ . + region)

form_dem <- update(form_region, . ~ . + pop00_l + hinc00_l + density00_l +
                     unemp00 + fb00 + col00 + nhwht00)

form_full <- update(form_dem, . ~ . + mhmval00_l + h30old00 + vac00 + own00)

mods <- map(
  list(form_msa, form_region, form_dem, form_full),
  probitmfx,
  data = data_probit,
  clustervar1 = "msa_name",
  atmean = TRUE,
  robust = TRUE
)

coef_names <- c("WRLURI", "MSA Unemployment", "MSA HH Income", "MSA Population",
                "Unavailable", "Northeast", "South", "West", "Population",
                "HH Income", "Density",  "Unemployment", "Foreign Born",
                "Bachelors", "White", "Home Value", "Over 30", "Vacant",
                "Owner Occupied")

caption <- paste("Probit Marginal Effects on Gentrification:",
                  "Tracts in Bottom Income Quartile Eligible to Gentrify")

note <- "Standard errors clustered by MSA in parentheses."

make_texreg(mods, "tables/tab5.tex", note, 0, 0, caption, coef_names, mod_names = NULL)

# Create Table 6 ----------

data_probit_robust <-
  cc_data %>%
  filter(hinc00_dtile <= 4)

mods <- map(
  list(form_msa, form_region, form_dem, form_full),
  probitmfx,
  data = data_probit_robust,
  clustervar1 = "msa_name",
  atmean = TRUE,
  robust = TRUE
)

caption <- paste("Probit Marginal Effects on Gentrification:",
                 "Tracts in First Four Income Deciles Eligible to Gentrify")

note <- "Standard errors clustered by MSA in parentheses."

make_texreg(mods, "tables/tab6.tex", note, 0, 0, caption, coef_names, mod_names = NULL)

# Create Table 7 ----------

form_full <- "gent_alt ~ wrluri_zscore + unemp00_msa + hinc00_msa_l + pop00_msa_l +
  unaval + region + pop00_l + hinc00_l + density00_l + unemp00 + fb00 + col00 +
  nhwht00 + mhmval00_l + h30old00 + vac00 + own00"

create_form <- function(x) {
  new_form <- gsub("wrluri_zscore", x, form_full)
  new_form
}

regs <- c("adi_zscore", "sri_zscore", "ei_zscore", "osi_zscore", "dri_zscore",
          "lai_zscore", "lpai_zscore", "lzai_zscore", "scii_zscore", "spii_zscore",
          "lppi_zscore")

res_list <-
  regs %>%
  map(create_form) %>%
  map(probitmfx, data = data_probit, clustervar1 = "msa_name")

coefs <- map_dbl(res_list, ~ .$mfxest[1, 1])
ses <- map_dbl(res_list, ~ .$mfxest[1, 2])
pvals <- map_dbl(res_list, ~.$mfxest[1, 4])

coef_names <- c("ADI: Approv. Delay Ind.",
                "SRI: Supply Restr. Ind.",
                "EI: Exactions Ind.",
                "OSI: Open Space Ind.",
                "DRI: Density Restr. Ind.",
                "LAI: Local Assem. Ind.",
                "LPAI: Proj. Approv. Ind.",
                "LZAI: Zoning Approv. Ind.",
                "SCII: Court Involv. Ind.",
                "SPII: Policital Invol. Ind.",
                "LPPI: Political Press. Ind.")

regs_df <-
  data.frame(Index = coef_names, MFX = coefs, P_Values = pvals)

caption <- c("Probit Marginal Effects by Regulation Index")
note <- paste("\\scriptsize Each marginal effect from a regression including the full",
              "set of controls.")
make_xtable(regs_df, caption, "tables/tab7.tex", note)

# Finished








