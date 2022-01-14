# Load libraries ----------------------------------------------------------

library(tidyverse)
library(xtable) # To make latex results tables
library(texreg) # Also to make latex results tables
library(lmtest) # For coeftest() to return clustered errors and pvalues
library(sandwich) # For vcovCL() to calculate the clustered errors
library(mfx) # For probitmfx() to fit the probit model
select <- dplyr::select # mfx overwrites dplyr select

# Load data ---------------------------------------------------------------

cc_data <- read_csv('cc_data_good.csv')

# Create functions --------------------------------------------------------

# make_xtable is a function to allow me to reuse settings to make my desired
# Latex tables from tibbles for non-results tables.
# Latex code is saved to a tables_good folder in the working directory

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

# Create Table 1 ----------------------------------------------------------

msa_table <-
  cc_data %>%
  group_by(msa_name) %>%
  summarise(
    WRLURI = mean(wrluri),
    Tracts = n(),
    Gentrifiable = sum(hinc00_qtile == 1),
    `Prop. Gent.` = mean(gent_alt[hinc00_qtile == 1] == 1)
  ) %>%
  rename(MSA = msa_name)

caption <- c("Proportion of Low Income Tracts Gentrified by MSA")

note <- ("\\parbox{.6\\linewidth}{\\vspace{2pt}\\scriptsize
All city tracts included in the analysis and gentrifiable tracts are those
in the first income quartile.\\\\
Prop. Gent. is the proportion of gentrifiable tracts that gentrified.}")

make_xtable(msa_table, caption, "tables_good/tab1.tex", note)

# Create Table 2 ----------------------------------------------------------

for_stats <- c(
  "col00", "density00_l", "fb00", "h30old00", "gent_alt", "hinc00_l",
  "hinc00_msa_l", "mhmval00_l", "nhwht00", "own00", "pop00_l", "pop00_msa_l",
  "unemp00_msa", "unemp00", "vac00", "wrluri", "lppi", "spii", "scii", "lzai",
  "lai", "lpai", "sri", "dri", "ei", "osi", "adi", "unaval"
)

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

descrips <- tibble(Variable, Description)

def_tab <- 
  cc_data |> 
  summarize(across(all_of(for_stats), list(mean = mean, sd = sd))) |> 
  pivot_longer(
    everything(), 
    names_to = c("var", ".value"),
    names_pattern = "(.*)(mean|sd)"
  ) |> 
  bind_cols(descrips) |> 
  select(Variable, Description, Mean = mean, Sd = sd)

caption <- c("Variable Definitions and Statistics")

note <- paste(
  "\\multicolumn{4}{l}",
  "{\\scriptsize",
  "All city tracts included in the analysis.",
  "Gentrifiable tracts are those in the first income quartile.}"
)

make_xtable(def_tab, caption, "tables_good/tab2.tex", note)

# Create Table 3 ----------------------------------------------------------

stats_split_by_gent <- 
  cc_data |> 
  filter(hinc00_qtile == 1) |> 
  group_by(gent_alt) |> 
  group_split() |> 
  map(
    \(x) x |> 
      summarize(across(all_of(for_stats), list(mean = mean, sd = sd))) |>
      pivot_longer(
        everything(),
        names_to = c("var", ".value"),
        names_pattern = "(.*)(mean|sd)"
      )
  )

stat_gent_tab <- tibble(
  Variable, 
  Mean_Gent = stats_split_by_gent[[2]]$mean,
  Sd_Gent = stats_split_by_gent[[2]]$sd,
  Mean_Non_gent = stats_split_by_gent[[1]]$mean,
  Sd_Non_gent = stats_split_by_gent[[1]]$sd,
)

caption <- c("Statistics by Gentrification Status")

note <- paste(
  "\\multicolumn{5}{l}",
  "{\\scriptsize",
  "Only gentrifiable tracts included in the analysis.",
  "Gentrifiable tracts are those in the first income quartile.}"
)

make_xtable(stat_gent_tab, caption, "tables_good/tab3.tex", note)

# Create Table 4 ----------------------------------------------------------

formula <- "I(mhmval12_l - mhmval00_l) ~ pop00_l + hinc00_l + density00_l +
  nhwht00 + col00 + fb00 + unemp00 + h30old00 + vac00 + own00 + wrluri_zscore +
  unemp00_msa + hinc00_msa_l + pop00_msa_l + unaval"

mods <- 
  cc_data |> 
  group_by(hinc00_qtile) |> 
  group_split() |> 
  map(\(x) lm(as.formula(formula), data = x)) 

tests <- 
  mods |> 
  map(\(x) coeftest(x, vcov = vcovCL(x, type="HC1", cluster = ~msa_name)))

ses <- map(tests, \(x) x[, 2])
pvals <- map(tests, \(x) x[, 4])

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

make_texreg(mods, "tables_good/tab4.tex", note, ses, pvals, caption, coef_names, mod_names)

# Create Table 5 ----------------------------------------------------------

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

make_texreg(mods, "tables_good/tab5.tex", note, 0, 0, caption, coef_names, mod_names = NULL)

# Create Table 6 ----------------------------------------------------------

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

make_texreg(mods, "tables_good/tab6.tex", note, 0, 0, caption, coef_names, mod_names = NULL)

# Create Table 7 ----------------------------------------------------------

form_full <- "gent_alt ~ wrluri_zscore + unemp00_msa + hinc00_msa_l + pop00_msa_l +
  unaval + region + pop00_l + hinc00_l + density00_l + unemp00 + fb00 + col00 +
  nhwht00 + mhmval00_l + h30old00 + vac00 + own00"

regs <- c("adi_zscore", "sri_zscore", "ei_zscore", "osi_zscore", "dri_zscore",
          "lai_zscore", "lpai_zscore", "lzai_zscore", "scii_zscore", "spii_zscore",
          "lppi_zscore")

res_list <-
  regs |> 
  map(\(x) gsub("wrluri_zscore", x, form_full)) |> 
  map(probitmfx, data = data_probit, clustervar1 = "msa_name")

coefs <- map_dbl(res_list, \(x) x$mfxest[1, 1])
ses <- map_dbl(res_list, \(x) x$mfxest[1, 2])
pvals <- map_dbl(res_list, \(x) x$mfxest[1, 4])

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

regs_df <- data.frame(Index = coef_names, MFX = coefs, P_Values = pvals)

caption <- c("Probit Marginal Effects by Regulation Index")

note <- paste("\\scriptsize Each marginal effect from a regression including the full",
              "set of controls.")

make_xtable(regs_df, caption, "tables_good/tab7.tex", note)
