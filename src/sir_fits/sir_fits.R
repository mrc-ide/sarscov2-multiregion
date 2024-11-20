orderly2::orderly_dependency(
  "sir_data",
  'latest()',
  c("inputs/data.rds" = "outputs/data.rds",
    "sir.R" = "sir.R",
    "outputs/true_history.rds" = "outputs/true_history.rds",
    "outputs/true_pars.rds" = "outputs/true_pars.rds"))

orderly2::orderly_parameters(short_run = FALSE,
                             region = "all")

orderly2::orderly_artefact(description = "Fit object",
                           files = c("outputs/fit.rds",
                                     "outputs/true_pars.rds",
                                     "outputs/true_history.rds"))

orderly2::orderly_resource("support.R")
source("support.R")

library(monty)
library(dust2)
library(odin2)

deterministic <- TRUE

if (short_run) {
  n_burnin <- 50
  n_steps <- 150
  n_sample <- 100
  n_chains <- 4
  n_particles <- 8
} else {
  n_burnin <- 5000
  n_steps <- 15000
  n_sample <- 1000
  n_chains <- 4
  n_particles <- 200
}

data <- readRDS("inputs/data.rds")
data <- get_data(data, region)

control <- fit_control(region, deterministic, n_steps, n_burnin, n_sample,
                       n_chains, n_particles)

filter <- create_filter(sir, data, deterministic, control)

groups <- if (region == "all") unique(data$region) else NULL

packer <- create_packer(groups)

prior <- create_prior(region, packer$names())

fit <- run_fit(filter, packer, prior, control, deterministic, region)

dir.create("outputs", FALSE, TRUE)
saveRDS(fit, "outputs/fit.rds")
