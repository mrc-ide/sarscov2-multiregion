orderly2::orderly_parameters(region = "london",
                             multiregion = FALSE,
                             deterministic = FALSE,
                             short_run = FALSE,
                             assumptions = "central",
                             rt_severity = TRUE,
                             severity = TRUE)

orderly2::orderly_shared_resource(global_util.R = "util_new.R")

orderly2::orderly_artefact(
  description = "Fitting outputs",
  files = "outputs/fit.rds"
)

orderly2::orderly_dependency(
  "sarscov2_data",
  "latest",
  c("data/england_region_data.csv" = "outputs/england_region_data.csv",
    "data/serology.csv" = "outputs/serology_for_inference.csv"))
orderly2::orderly_dependency(
  "sarscov2_parameters",
  "latest(parameter:assumptions == this:assumptions && parameter:deterministic == this:deterministic && parameter:multiregion == this:multiregion)",
  c("parameters/base.rds" = "parameters_base.rds",
    "parameters/info.csv" = "parameters_info.csv",
    "parameters/prior.csv" = "parameters_prior.csv",
    "parameters/proposal.csv" = "parameters_proposal.csv",
    "parameters/transform.R" = "parameters_transform.R"))

library(sircovid)
library(spimalot)
library(dplyr)
library(tidyr)

orderly2::orderly_resource("data.R")
source("data.R")

source("global_util.R")

version_check("sircovid", "0.15.1")
version_check("spimalot", "0.8.31")

date <- "2022-02-24"

trim_deaths <- 0
trim_pillar2 <- 0
trim_pillar2_date <- FALSE
adm_backfill_date <- as.Date(date)

## MCMC control (only applies if short_run = FALSE)
if (deterministic) {
  burnin <- 5000
  n_mcmc <- 15000
  n_sample <- 1000
  chains <- 4
  kernel_scaling <- 0.1
  if (multiregion) {
    burnin <- 2 * burnin
    n_mcmc <- 2 * n_mcmc
  } 
  adaptive_proposal <- mcstate::adaptive_proposal_control(initial_vcv_weight = 100)
} else {
  burnin <- 500
  n_mcmc <- 1500
  n_sample <- 1000
  chains <- 4
  kernel_scaling <- 0.2
  adaptive_proposal <- FALSE
}

region <- spimalot::spim_check_region(region, multiregion)

pars <- spimalot::spim_fit_pars_load("parameters", region, assumptions,
                                     kernel_scaling)

restart_date <- readRDS("parameters/base.rds")[[region[[1]]]]$restart_date

## NOTE: only currently using compiled compare for the deterministic
## model as it has a bigger increase in speed here.
control <- spimalot::spim_control(
  short_run, chains, deterministic = deterministic,
  multiregion = multiregion,
  date_restart = restart_date,
  n_mcmc = n_mcmc, burnin = burnin, n_sample = n_sample,
  adaptive_proposal = adaptive_proposal,
  compiled_compare = deterministic, rt = rt_severity,
  severity = severity, intrinsic_severity = rt_severity)

data_rtm <- read_csv("data/england_region_data.csv")
data_serology <- read_csv("data/serology.csv")

data <- spim_data(
  date, region, data_rtm, data_serology, trim_deaths, trim_pillar2,
  adm_backfill_date, trim_pillar2_date, full_data = FALSE)

filter <- spimalot::spim_particle_filter(data, pars$mcmc,
                                         control$particle_filter,
                                         deterministic)

## To run the model at this point, we just need to run:
##
## > filter$run(pars$mcmc$model(pars$mcmc$initial()))

## This bit takes ages, of course
samples <- spimalot::spim_fit_run(pars, filter, control$pmcmc)

## This is the data set including series that we do not fit to, and
## with the full series of carehomes deaths.
data_full <- spim_data(
  date, region, data_rtm, data_serology,
  trim_deaths, trim_pillar2,
  adm_backfill_date, trim_pillar2_date, full_data = TRUE)

## This is new, and used only in sorting out the final outputs. Some
## explanation would be useful.
data_inputs <- list(rtm = data_rtm,
                    full = data_full,
                    fitted = data)

saveRDS(date, "date.rds")

dat <- spimalot::spim_fit_process(samples, pars, data_inputs,
                                  control$particle_filter)

dir.create("outputs", FALSE, TRUE)
saveRDS(dat$fit, "outputs/fit.rds")
