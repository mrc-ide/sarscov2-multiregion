orderly_pars <- orderly2::orderly_parameters(short_run = FALSE,
                                             n_regions = 5)

regions <- LETTERS[seq_len(orderly_pars$n_regions)]

for (r in regions) {
  orderly2::orderly_dependency("sir_fits",
                               quote(latest(parameter:region == environment:r && parameter:short_run == this:short_run)),
                               c("inputs/fit_single_${r}.rds" = "outputs/fit.rds",
                                 "figs/traceplots_single_${r}.png" = "outputs/traceplots.png"))
}
orderly2::orderly_dependency("sir_fits",
                             quote(latest(parameter:region == "all" && parameter:short_run == this:short_run)),
                             c("inputs/fit_multi.rds" = "outputs/fit.rds",
                               "figs/traceplots_multi.png" = "outputs/traceplots.png",
                               "inputs/true_history.rds" = "outputs/true_history.rds",
                               "inputs/true_pars.rds" = "outputs/true_pars.rds"))

orderly2::orderly_artefact(description = "Fit plots",
                           files = c("figs/trajectories_single.png",
                                     "figs/trajectories_multi.png"))

# orderly2::orderly_artefact(description = "Convergence diagnostics",
#                            files = c("outputs/diag_single.rds",
#                                      "outputs/diag_multi.rds"))

orderly2::orderly_resource("support.R")
orderly2::orderly_resource("plot.R")
source("plot.R")
source("support.R")

library(tidyverse)
library(patchwork)

fits <- load_fits(regions)

true_history <- readRDS("inputs/true_history.rds")
true_pars <- readRDS("inputs/true_pars.rds")

dir.create("figs", FALSE, TRUE)
dir.create("outputs", FALSE, TRUE)

write_png("figs/trajectories_single.png", width = 2400, height = 1800,
          res = 200,
          plot_trajectories(
            fits, true_history, regions, c("S", "I", "cases_inc"), FALSE))


write_png("figs/trajectories_multi.png", width = 2400, height = 1800,
          res = 200,
          plot_trajectories(
            fits, true_history, regions, c("S", "I", "cases_inc"), TRUE))


write_png("figs/forest_plot.png", width = 1200, height = 1200,
          res = 200,
          forest_plot(fits, true_pars))

#saveRDS(fit_single_regions$convergence_diagnostics, "outputs/diag_single.rds")
#saveRDS(fit_multiregion$convergence_diagnostics, "outputs/diag_multi.rds")

#pars_table <- get_pars_table(fit_single_regions, fit_multiregion, true_pars)
