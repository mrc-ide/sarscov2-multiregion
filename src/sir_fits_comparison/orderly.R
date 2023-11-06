orderly2::orderly_parameters(short_run = FALSE,
                             n_regions = 5)

regions <- LETTERS[seq_len(n_regions)]

for (r in regions) {
  orderly2::orderly_dependency("sir_fits",
                               quote(latest(parameter:region == environment:r && parameter:short_run == this:short_run && parameter:multiregion == FALSE)),
                               c("regional_results/single/${r}/fit.rds" = "outputs/fit.rds"))
}
orderly2::orderly_dependency("sir_fits",
                             quote(latest(parameter:region == "all" && parameter:short_run == this:short_run && parameter:multiregion == TRUE)),
                             c("regional_results/multi/fit.rds" = "outputs/fit.rds",
                               "regional_results/multi/true_history.rds" = "outputs/true_history.rds",
                               "regional_results/multi/true_pars.rds" = "outputs/true_pars.rds"))

orderly2::orderly_artefact("Traceplots",
                           c("figs/traceplot_A_single.png",
                             "figs/traceplot_B_single.png",
                             "figs/traceplot_C_single.png",
                             "figs/traceplot_D_single.png",
                             "figs/traceplot_E_single.png",
                             "figs/traceplot_A_multi.png",
                             "figs/traceplot_B_multi.png",
                             "figs/traceplot_C_multi.png",
                             "figs/traceplot_D_multi.png",
                             "figs/traceplot_E_multi.png"))

orderly2::orderly_artefact("Fit plots",
                           c("figs/trajectories_single.png"))#,
                             #"figs/trajectories_multi.png"))

orderly2::orderly_artefact("Convergence diagnostics",
                           c("outputs/diag_single.rds",
                             "outputs/diag_multi.rds"))

orderly2::orderly_resource("support.R")
orderly2::orderly_resource("plot.R")
source("plot.R")
source("support.R")

library(mcstate)

fit_single_regions <- load_single_region(regions)

fit_multiregion <- load_multiregion(regions)

true_history <- readRDS("regional_results/multi/true_history.rds")
true_pars <- readRDS("regional_results/multi/true_pars.rds")

dir.create("figs", FALSE, TRUE)
dir.create("outputs", FALSE, TRUE)

for (i in seq_along(regions)) {
  fig_name <- paste0("figs/traceplot_", regions[i], "_single.png")
  write_png(fig_name, width = 3000, height = 1800, res = 200,
            plot_traceplots(fit_single_regions, i))
  
  fig_name <- paste0("figs/traceplot_", regions[i], "_multi.png")
  write_png(fig_name, width = 3000, height = 1800, res = 200,
            plot_traceplots(fit_multiregion, i))
}

write_png("figs/trajectories_single.png", width = 2400, height = 1200,
          res = 200,
          plot_trajectories(
            fit_single_regions, true_history, regions, c("S", "I")))

saveRDS(fit_single_regions$convergence_diagnostics, "outputs/diag_single.rds")
saveRDS(fit_multiregion$convergence_diagnostics, "outputs/diag_multi.rds")

#pars_table <- get_pars_table(fit_single_regions, fit_multiregion, true_pars)
