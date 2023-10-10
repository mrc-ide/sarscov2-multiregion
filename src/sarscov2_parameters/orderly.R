orderly2::orderly_parameters(multiregion = FALSE,
                             multi_pars_from_single = FALSE,
                             assumptions = "central",
                             deterministic = FALSE)

orderly2::orderly_shared_resource(global_util.R = "util_new.R")

orderly2::orderly_resource(c("pars/single/central/deterministic/info.csv",
                             "pars/single/central/deterministic/proposal.csv",
                             "pars/single/central/stochastic/info.csv",
                             "pars/single/central/stochastic/proposal.csv",
                             "data/vaccine_efficacy_alpha.csv",
                             "data/vaccine_efficacy_delta.csv",
                             "data/vaccine_efficacy_omicron.csv",
                             "data/vaccine_uptake.csv",
                             "data/support_severity.csv"))

orderly2::orderly_dependency(
  "sarscov2_data",
  "latest",
  c(data_vaccination.csv = "outputs/data_vaccination.csv",
    weighted_prior_ranges.csv = "outputs/weighted_prior_ranges.csv"))

orderly2::orderly_artefact(
  "fitted hyperparameters for priors",
  c("parameters_base.rds",
    "parameters_info.csv",
    "parameters_prior.csv",
    "parameters_proposal.csv",
    "parameters_transform.R"))

library(sircovid)
library(spimalot)
library(tidyr)
library(dplyr)
library(forcats)
library(magrittr)

orderly2::orderly_resource("R/support.R")
orderly2::orderly_resource("R/priors.R")
orderly2::orderly_resource("R/baseline.R")
orderly2::orderly_resource("R/transform.R")
orderly2::orderly_resource("R/vaccine.R")
source("R/support.R")
source("R/priors.R")
source("R/baseline.R")
source("R/transform.R")
source("R/vaccine.R")
source("R/multiregion.R")

source("global_util.R")

version_check("sircovid", "0.14.11")
version_check("spimalot", "0.8.22")

date <- "2022-02-24"

## Five epochs after starting with a single strain model (without vaccination)
## * mid August 2020: Alpha appears, expand strains
## * early December 2020: vaccination starts, expand vaccine classes 
##                        but without boosters
## * early March 2021: delta appears, expand strains
## * mid September 2021: booster programme starts, expand vaccine classes
## * early November 2021: omicron appears, rotate strains
epoch_dates <- c("2020-09-17", "2020-12-07", "2021-03-08", "2021-09-14",
                 "2021-11-01")

## No restart dat
restart_date <- NULL

## Fixed parameter names for multiregion
fixed_pars <- c("ta_alpha", "rel_p_H_alpha", "rel_p_ICU_alpha", "rel_p_D_alpha",
                "ta_delta", "rel_p_H_delta", "rel_p_ICU_delta", "rel_p_D_delta",
                "ta_omicron", "rel_p_H_omicron", "rel_p_ICU_omicron",
                "rel_p_D_omicron", "p_G_D", "p_G_D_2", "p_H", "p_H_2", "p_H_D",
                "p_ICU", "p_ICU_2", "p_ICU_D", "p_W_D", "mu_D", "mu_D_2",
                "mu_D_3", "mu_D_4", "mu_D_5", "mu_gamma_H", "mu_gamma_H_2",
                "mu_gamma_H_3", "mu_gamma_H_4",
                "p_NC_15_24", "p_NC_25_49", "p_NC_50_64", 
                "p_NC_65_79", "p_NC_80_plus", "p_NC_weekend_15_24",
                "p_NC_weekend_25_49", "p_NC_weekend_50_64",
                "p_NC_weekend_65_79", "p_NC_weekend_80_plus",
                "alpha_D", "alpha_H", "alpha_admission", "alpha_death_hosp",
                "rho_pillar2_tests")

## Load all parameters from the last run; creates priors, and updates
## new entries into the proposal matrix as needed.
pars <- load_mcmc_parameters(assumptions, deterministic,
                             multiregion, multi_pars_from_single,
                             fixed_pars)

## The baselines are always region-specific
regions <- sircovid::regions("england")

baseline <- lapply(regions, create_baseline,
                   date, NULL, # setting restart date to NULL
                   epoch_dates, pars$info, assumptions)
names(baseline) <- regions

message("Writing parameters_info.csv")
write_csv(pars$info, "parameters_info.csv")
message("Writing parameters_proposal.csv")
write_csv(pars$proposal, "parameters_proposal.csv")
message("Writing parameters_prior.csv")
write_csv(pars$prior, "parameters_prior.csv")

message("Writing parameters_base.rds")
saveRDS(baseline, "parameters_base.rds")

message("Writing parameters_transform.R")
fs::file_copy("R/transform.R",
              "parameters_transform.R", overwrite = TRUE)
