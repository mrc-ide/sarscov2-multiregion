orderly2::orderly_parameters(short_run = FALSE, deterministic = FALSE, assumptions = "central")

orderly2::orderly_shared_resource(global_util.R = "util_new.R")

orderly2::orderly_artefact(
  "Comparison plots",
  c("figs/compare_intrinsic_severity.png")
)

orderly2::orderly_dependency(
  "sarscov2_fits_combined",
  'latest(parameter:assumptions == this:assumptions && parameter:short_run == this:short_run && parameter:deterministic == this:deterministic && parameter:multiregion == TRUE)',
  c("regional_results/multi/fit.rds" = "regional_results/fit.rds",
    "figs/forest_plot_betas_multi.png" = "figs/forest_plot_betas.png",
    "figs/forest_plot_misc_multi.png" = "figs/forest_plot_misc.png",
    "figs/forest_plot_tv_severity_multi.png" = "figs/forest_plot_tv_severity.png",
    "figs/forest_plot_variants_multi.png" = "figs/forest_plot_variants.png", 
    "figs/paper_figure_1_multi.png" = "paper_plots/paper_figure_1.png",
    "figs/paper_figure_2_multi.png" = "paper_plots/paper_figure_2.png"))
orderly2::orderly_dependency(
  "sarscov2_fits_combined",
  'latest(parameter:assumptions == this:assumptions && parameter:short_run == this:short_run && parameter:deterministic == this:deterministic && parameter:multiregion == FALSE)',
  c("regional_results/single/east_of_england/fit.rds" = "regional_results/east_of_england/fit.rds",
    "regional_results/single/london/fit.rds" = "regional_results/london/fit.rds",
    "regional_results/single/midlands/fit.rds" = "regional_results/midlands/fit.rds",
    "regional_results/single/north_east_and_yorkshire/fit.rds" = "regional_results/north_east_and_yorkshire/fit.rds",
    "regional_results/single/north_west/fit.rds" = "regional_results/north_west/fit.rds",
    "regional_results/single/south_east/fit.rds" = "regional_results/south_east/fit.rds",
    "regional_results/single/south_west/fit.rds" = "regional_results/south_west/fit.rds",
    "figs/forest_plot_betas_single.png" = "figs/forest_plot_betas.png",
    "figs/forest_plot_misc_single.png" = "figs/forest_plot_misc.png",
    "figs/forest_plot_tv_severity_single.png" = "figs/forest_plot_tv_severity.png",
    "figs/forest_plot_variants_single.png" = "figs/forest_plot_variants.png", 
    "figs/paper_figure_1_single.png" = "paper_plots/paper_figure_1.png",
    "figs/paper_figure_2_single.png" = "paper_plots/paper_figure_2.png"))

library(sircovid)
library(spimalot)
library(dplyr)
library(tidyr)
library(ggplot2)
library(data.table)
library(jtools)
library(patchwork)
library(reshape2)
library(DescTools)
library(ggrepel)
library(stringr)
library(lubridate)
library(ggtext)
library(car)

orderly2::orderly_resource("support_paper_plots.R")
orderly2::orderly_resource("support_data.R")
orderly2::orderly_resource("supplement_plots.R")
orderly2::orderly_resource("support_plots.R")
source("support_paper_plots.R")
source("support_data.R")
source("supplement_plots.R")
source("support_plots.R")

source("global_util.R")

version_check("sircovid", "0.14.11")
version_check("spimalot", "0.8.22")

date <- "2022-02-24"

dat_multi <- spim_combined_load_multiregion("regional_results/multi",
                                            get_severity = TRUE)
dat_single <- spimalot::spim_combined_load("regional_results/single",
                                           regions = "england",
                                           get_onward = FALSE,
                                           get_severity = TRUE)

png("figs/compare_intrinsic_severity.png", units = "in", width = 15, height = 15, res = 300)
plot_compare_intrinsic_severity(dat_single, dat_multi, "Emergence3")
dev.off()
