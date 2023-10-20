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
                             c("regional_results/multi/fit.rds" = "outputs/fit.rds"))

orderly2::orderly_artefact("Traceplots",
                           c("figs/traceplot_A.png",
                             "figs/traceplot_B.png",
                             "figs/traceplot_C.png",
                             "figs/traceplot_D.png",
                             "figs/traceplot_E.png",
                             "figs/traceplot_A_multi.png",
                             "figs/traceplot_B_multi.png",
                             "figs/traceplot_C_multi.png",
                             "figs/traceplot_D_multi.png",
                             "figs/traceplot_E_multi.png"))

orderly2::orderly_artefact("Convergence diagnostics",
                           c("outputs/diag_single.rds",
                             "outputs/diag_multi.rds"))

orderly2::orderly_resource("support.R")
orderly2::orderly_resource("plot.R")
source("plot.R")
source("support.R")

fit_single_regions <- lapply(regions, function (r) readRDS(paste0("regional_results/single/", r, "/fit.rds")))
names(fit_single_regions) <- regions

fit_multiregion <- readRDS("regional_results/multi/fit.rds")

dir.create("figs", FALSE, TRUE)
dir.create("outputs", FALSE, TRUE)

for (i in seq_along(regions)) {
  fig_name <- paste0("figs/traceplot_", regions[i], ".png")
  write_png(fig_name, width = 3000, height = 1800, res = 200,
            plot_traceplots(fit_single_regions[[regions[i]]]))
  
  fig_name <- paste0("figs/traceplot_", regions[i], "_multi.png")
  write_png(fig_name, width = 3000, height = 1800, res = 200,
            plot_traceplots(fit_multiregion, i))
}

diag_single <- lapply(fit_single_regions, get_convergence_diagnostic)
diag_multi <- get_convergence_diagnostic(fit_multiregion)

saveRDS(diag_single, "outputs/diag_single.rds")
saveRDS(diag_multi, "outputs/diag_multi.rds")
