orderly2::orderly_dependency(
  "sir_data",
  'latest()',
  c("inputs/data.rds" = "outputs/data.rds",
    "sir.R" = "sir.R",
    "outputs/true_history.rds" = "outputs/true_history.rds",
    "outputs/true_pars.rds" = "outputs/true_pars.rds"))

orderly_pars <- orderly2::orderly_parameters(short_run = FALSE,
                                             region = "all",
                                             beta_hyperprior = FALSE)

orderly2::orderly_artefact(description = "Fit object",
                           files = c("outputs/fit.rds",
                                     "outputs/true_pars.rds",
                                     "outputs/true_history.rds",
                                     "outputs/traceplots.png"))

orderly2::orderly_resource("support.R")
orderly2::orderly_resource("plot.R")
source("support.R")
source("plot.R")

library(monty)
library(dust2)
library(odin2)
library(ggplot2)
library(bayesplot)

deterministic <- TRUE

if (orderly_pars$short_run) {
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
data <- get_data(data, orderly_pars$region)

control <- fit_control(orderly_pars$region, deterministic, n_steps, n_burnin,
                       n_sample, n_chains, n_particles)

sir <- odin2::odin("sir.R")

filter <- create_filter(sir, data, deterministic, control)

groups <- if (orderly_pars$region == "all") unique(data$region) else NULL

packer <- create_packer(groups, beta_hyperprior)

prior <- create_prior(orderly_pars$region, packer$names())

fit <- run_fit(filter, packer, prior, control, deterministic,
               orderly_pars$region)

dir.create("outputs", FALSE, TRUE)
ggsave(traceplots(fit, control), 
       filename = "outputs/traceplots.png",
       width = 15, height = 9, bg = "white")


fit <- thin_samples(fit, control)

saveRDS(fit, "outputs/fit.rds")
