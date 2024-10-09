orderly2::orderly_parameters(short_run = FALSE, deterministic = FALSE, assumptions = "central", multiregion = FALSE, rt_severity = TRUE)

orderly2::orderly_shared_resource(global_util.R = "util_new.R")

if (multiregion) {
  orderly2::orderly_dependency(
    "sarscov2_fits",
    'latest(parameter:region == "england" && parameter:assumptions == this:assumptions && parameter:short_run == this:short_run && parameter:deterministic == this:deterministic && parameter:multiregion == TRUE && (parameter:rt_severity == this:rt_severity || parameter:rt_severity == TRUE))',
    c("regional_results/fit.rds" = "outputs/fit.rds"))
  orderly2::orderly_artefact(
    "Fit object for onward comparison",
    c("regional_results/fit.rds")
  )
  
} else {
  for (r in sircovid::regions("england")) {
    orderly2::orderly_dependency("sarscov2_fits",
                                 quote(latest(parameter:region == environment:r && parameter:assumptions == this:assumptions && parameter:short_run == this:short_run && parameter:deterministic == this:deterministic && parameter:multiregion == FALSE && (parameter:rt_severity == this:rt_severity || parameter:rt_severity == TRUE))),
                                 c("regional_results/${r}/fit.rds" = "outputs/fit.rds"))
  }
  orderly2::orderly_artefact(
    "Fit object for onward comparison",
    paste0("regional_results/", sircovid::regions("england"), "/fit.rds"))
}

orderly2::orderly_artefact(
  description = "Files for external reviews",
  files = c("outputs/parameters/proposal.csv",
            "outputs/parameters/prior.csv",
            "outputs/parameters/info.csv",
            "outputs/aggregated_data.rds"))
orderly2::orderly_artefact(
  description = "regional fitting plots and projections for comparison",
  files = c("figs/age_deaths_hosp.png",
            "figs/age_deaths_comm.png",
            "figs/age_admissions.png",
            "figs/age_react_prevalence.png",
            "figs/age_pillar2.png",
            "figs/cumulative_attack_rate.png",
            "figs/data_fits_regional.png",
            "figs/forest_plot_betas.png",
            "figs/forest_plot_misc.png",
            "figs/forest_plot_variants.png",
            "figs/forest_plot_tv_severity.png",
            "figs/incidence.png",
            "figs/incidence_per_1000.png",
            "figs/infections_per_strain.png",
            "figs/pillar2_all_ages.png",
            "figs/pillar2_over25.png",
            "figs/prevalence_ons.png",
            "figs/prevalence_react.png",
            "figs/serology_euroimmun.png",
            "figs/serology_roche_n.png",
            "figs/status_effective_susceptible.png",
            "figs/status_infection.png",
            "figs/status_vaccine.png",
            "figs/variant_Wildtype_Alpha.png",
            "figs/variant_Alpha_Delta.png",
            "figs/variant_Delta_Omicron.png",
            "figs/mu_D.png",
            "figs_by_age/pillar2_0_14.png",
            "figs_by_age/pillar2_15_24.png",
            "figs_by_age/pillar2_25_49.png",
            "figs_by_age/pillar2_50_64.png",
            "figs_by_age/pillar2_65_79.png",
            "figs_by_age/pillar2_80_plus.png",
            "figs_by_age/admissions_0_9.png",
            "figs_by_age/admissions_10_19.png",
            "figs_by_age/admissions_20_29.png",
            "figs_by_age/admissions_30_39.png",
            "figs_by_age/admissions_40_49.png",
            "figs_by_age/admissions_50_59.png",
            "figs_by_age/admissions_60_69.png",
            "figs_by_age/admissions_70_79.png",
            "figs_by_age/admissions_80_plus.png",
            "figs_by_age/deaths_comm_0_49.png",
            "figs_by_age/deaths_comm_50_54.png",
            "figs_by_age/deaths_comm_55_59.png",
            "figs_by_age/deaths_comm_60_64.png",
            "figs_by_age/deaths_comm_65_69.png",
            "figs_by_age/deaths_comm_70_74.png",
            "figs_by_age/deaths_comm_75_79.png",
            "figs_by_age/deaths_comm_80_plus.png",
            "figs_by_age/deaths_hosp_0_49.png",
            "figs_by_age/deaths_hosp_50_54.png",
            "figs_by_age/deaths_hosp_55_59.png",
            "figs_by_age/deaths_hosp_60_64.png",
            "figs_by_age/deaths_hosp_65_69.png",
            "figs_by_age/deaths_hosp_70_74.png",
            "figs_by_age/deaths_hosp_75_79.png",
            "figs_by_age/deaths_hosp_80_plus.png",
            "figs_by_age/react_5_24.png",
            "figs_by_age/react_25_34.png",
            "figs_by_age/react_35_44.png",
            "figs_by_age/react_45_54.png",
            "figs_by_age/react_55_64.png",
            "figs_by_age/react_65_plus.png"))

if (rt_severity) {
  orderly2::orderly_artefact(
    "Plots of Rt or severity",
    c("figs/beta.png",
      "figs/Rt_eff_general.png",
      "figs/Rt_general.png",
      "paper_plots/paper_figure_1.png",
      "paper_plots/paper_figure_2.png",
      "paper_plots/suppl_age_heatmaps.png",
      "paper_plots/suppl_regional_intrinsic.png"))
}

if (multiregion) {
  orderly2::orderly_artefact(
    "Traceplots",
    c("traceplots/traceplot_fixed.png",
      "traceplots/traceplot_varied_east_of_england.png",
      "traceplots/traceplot_varied_london.png",
      "traceplots/traceplot_varied_midlands.png",
      "traceplots/traceplot_varied_north_east_and_yorkshire.png",
      "traceplots/traceplot_varied_north_west.png",
      "traceplots/traceplot_varied_south_east.png",
      "traceplots/traceplot_varied_south_west.png"))
} else {
  orderly2::orderly_artefact(
    "Traceplots",
    c("traceplots/traceplot_east_of_england.png",
      "traceplots/traceplot_london.png",
      "traceplots/traceplot_midlands.png",
      "traceplots/traceplot_north_east_and_yorkshire.png",
      "traceplots/traceplot_north_west.png",
      "traceplots/traceplot_south_east.png",
      "traceplots/traceplot_south_west.png"))
}

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
orderly2::orderly_resource("plot_mu_D.R")
source("support_paper_plots.R")
source("support_data.R")
source("supplement_plots.R")
source("plot_mu_D.R")

source("global_util.R")

version_check("sircovid", "0.15.1")
version_check("spimalot", "0.8.30")

date <- "2022-02-24"

if (multiregion) {
  dat <- spim_combined_load_multiregion("regional_results", get_severity = rt_severity)
  saveRDS(dat$samples[[1]]$adaptive, "adaptive.rds")
} else {
  dat <- spimalot::spim_combined_load("regional_results", regions = "england",
                                      get_onward = FALSE, get_severity = rt_severity)
  adaptive <- spimalot:::list_transpose(
    lapply(dat$samples[sircovid::regions("england")], "[[", "adaptive"))
  saveRDS(adaptive, "adaptive.rds")
}


dir.create("outputs", FALSE, TRUE)
dir.create("figs", FALSE, TRUE)
dir.create("figs_by_age", FALSE, TRUE)
dir.create("spim_view", FALSE, TRUE)
dir.create("paper_plots", FALSE, TRUE)
dir.create("traceplots", FALSE, TRUE)

saveRDS(dat$data, "outputs/aggregated_data.rds")

spimalot::spim_pars_pmcmc_save(dat$parameters, "outputs/parameters")

par_names <- unique(dat$parameters$proposal$name)
subset_variants <- c("ta_alpha", "rel_p_H_alpha",
                     "rel_p_ICU_alpha", "rel_p_D_alpha",
                     "ta_delta", "rel_p_H_delta",
                     "rel_p_ICU_delta",  "rel_p_D_delta",
                     "ta_omicron", "rel_p_ICU_omicron",
                     "rel_p_H_omicron", "rel_p_D_omicron")
subset_tv_severity <- c("mu_D", "mu_D_5", "p_H",
                        "mu_D_2", "p_H_D", "p_H_2",
                        "mu_D_3", "p_ICU_D", "p_ICU",
                        "mu_D_4", "p_W_D", "p_ICU_2")
subset_misc <-   
  setdiff(grep("^beta", par_names, value = TRUE, invert = TRUE),
          c(subset_variants, subset_tv_severity))
seed_dates <- c("start_date", "seed_date_alpha", "seed_date_delta",
                "seed_date_omicron")
subset_misc <- c(seed_dates, setdiff(subset_misc, seed_dates))
par_labels <- forest_plot_labels(dat)

write_png("figs/forest_plot_variants.png", width = 1600, height = 1600, res = 200,
          spim_plot_forest(
            dat, plot_type = "subset", nrow = 3,
            subset = subset_variants,
            par_labels = par_labels))

write_png("figs/forest_plot_tv_severity.png", width = 1600, height = 1600, res = 200,
          spim_plot_forest(dat, plot_type = "subset",
                           subset = subset_tv_severity,
                           par_labels = par_labels))

write_png("figs/forest_plot_misc.png",
          width = 2400, height = 1600, res = 200,
          spim_plot_forest(dat, plot_type = "subset",
                           subset = subset_misc,
                           par_labels = par_labels))

write_png("figs/forest_plot_betas.png", width = 2400, height = 1600, res = 200,
          spim_plot_forest(dat, plot_type = "betas",
                           par_labels = par_labels))
if (multiregion) {
  write_png("traceplots/traceplot_fixed.png", width = 3000, height = 1800, res = 200,
            plot_traceplots(dat, "england", FALSE))
  for (r in sircovid::regions("england")) {
    fig_name <- paste0("traceplots/traceplot_varied_", r, ".png")
    write_png(fig_name, width = 3000, height = 1800, res = 200,
              plot_traceplots(dat, r, TRUE))
  }  
} else {
  for (r in sircovid::regions("england")) {
    fig_name <- paste0("traceplots/traceplot_", r, ".png")
    write_png(fig_name, width = 3000, height = 1800, res = 200,
              plot_traceplots(dat, r))
  }
}


write_png("figs/mu_D.png", width = 2400, height = 1200, res = 200,
          plot_mu_D(
            dat, sircovid::regions("england")))

write_png("figs/data_fits_regional.png", width = 2400 / 5 * 7, height = 1800,
          res = 200,
          spimalot::spim_plot_trajectories(
            dat, sircovid::regions("england"),
            c("deaths_hosp", "deaths_comm", "icu",
              "general", "hosp", "all_admission"), age_band = "all",
            with_forecast = FALSE, add_betas = FALSE))

write_png("figs/serology_euroimmun.png", width = 2400, height = 1200, res = 200,
           spimalot::spim_plot_serology(dat, sircovid::regions("england"), 1, 60))

write_png("figs/serology_roche_n.png", width = 2400, height = 1200, res = 200,
           spimalot::spim_plot_serology(dat, sircovid::regions("england"), 2, 60))

write_png("figs/status_effective_susceptible.png",
          width = 2400, height = 1200, res = 200,
          spimalot::spim_plot_effective_susceptible(
            dat, sircovid::regions("england"),
            strain_names = c("Wildtype", "Alpha", "Delta", "Omicron"),
            as.Date(c("2019-12-31", "2020-09-18", "2021-03-09", "2021-11-02"))))

write_png("figs/status_vaccine.png",
          width = 2400, height = 1200, res = 200,
          spimalot::spim_plot_vaccine_status(
            dat, sircovid::regions("england"),
            c("Unvaccinated", "First dose", "Second dose", "Waned", "Booster")))

write_png("figs/status_infection.png",
          width = 2400, height = 1200, res = 200,
          spimalot::spim_plot_infection_status(
            dat, sircovid::regions("england")))

write_png("figs/infections_per_strain.png",
          width = 2400, height = 1200, res = 200,
          spimalot::spim_plot_infections_per_strain(
            dat, sircovid::regions("england"),
            strain_names = c("Wildtype", "Alpha", "Delta", "Omicron"),
            as.Date(c("2019-12-31", "2020-09-18", "2021-03-09", "2021-11-02"))))

write_png("figs/cumulative_attack_rate.png",
          width = 2400, height = 1200, res = 200,
          spimalot::spim_plot_cumulative_attack_rate(
            dat, sircovid::regions("england")))


pillar2_age_bands <-
  c("over25", "under15", "15_24", "25_49", "50_64", "65_79", "80_plus")
write_png("figs/pillar2_all_ages.png", width = 2400, height = 1200, res = 200,
          spimalot::spim_plot_pillar2_positivity(
            dat, sircovid::regions("england"), "all",
            date_min = as.Date("2020-05-15"), ymax = 50))
for (i in pillar2_age_bands) {
  if (i == "over25") {
    fig_name <- "figs/pillar2_over25.png"
  } else if (i == "under15") {
    fig_name <- "figs_by_age/pillar2_0_14.png"
  } else {
    fig_name <- paste0("figs_by_age/pillar2_", i, ".png")
  }
  write_png(fig_name, width = 2400, height = 1200, res = 200,
            spimalot::spim_plot_pillar2_positivity(
              dat, sircovid::regions("england"), i,
              date_min = as.Date("2020-05-15"), ymax = 50))
}


write_png("figs/prevalence_react.png", width = 2400, height = 1200, res = 200,
          spimalot::spim_plot_react(
            dat, sircovid::regions("england"), date_min = as.Date("2020-05-15"),
            ymax = 10))

write_png("figs/prevalence_ons.png", width = 2400, height = 1200, res = 200,
          spimalot::spim_plot_ons(
            dat, sircovid::regions("england"), date_min = as.Date("2020-05-15"),
            ymax = 10))

write_png("figs/variant_Wildtype_Alpha.png", width = 2400, height = 1200, res = 200,
          spimalot::spim_plot_variant(
            dat, sircovid::regions("england"), "Alpha",
            date_min = as.Date("2020-09-17"),
            date_max = as.Date("2021-03-01")))

write_png("figs/variant_Alpha_Delta.png", width = 2400, height = 1200, res = 200,
          spimalot::spim_plot_variant(
            dat, sircovid::regions("england"), "Delta",
            date_min = as.Date("2021-03-01"),
            date_max = as.Date("2021-08-15")))

write_png("figs/variant_Delta_Omicron.png", width = 2400, height = 1200, res = 200,
          spimalot::spim_plot_variant(
            dat, sircovid::regions("england"), "Omicron",
            date_min = as.Date("2021-11-03"),
            date_max = as.Date("2022-02-01")))

write_png("figs/incidence.png", width = 2400, height = 1200, res = 200,
          spimalot::spim_plot_incidence(
            dat, c(sircovid::regions("england"))))

write_png("figs/incidence_per_1000.png", width = 2400, height = 1200, res = 200,
          spimalot::spim_plot_incidence(
            dat, c(sircovid::regions("england")), per_1000 = TRUE))

if (rt_severity) {
  write_png("figs/Rt_eff_general.png", width = 2400, height = 1200, res = 200,
            spimalot::spim_plot_Rt(
              dat, c(sircovid::regions("england")),
              "eff_Rt_general"))
  
  write_png("figs/Rt_general.png", width = 2400, height = 1200, res = 200,
            spimalot::spim_plot_Rt(
              dat, c(sircovid::regions("england")), "Rt_general"))
  
  write_png("figs/beta.png", width = 2400, height = 1200, res = 200,
            spimalot::spim_plot_Rt(
              dat, c(sircovid::regions("england")), "beta"))  
}


## Plot outputs by age

deaths_age_bands <- c("0_49", "50_54", "55_59", "60_64", "65_69", "70_74",
                      "75_79", "80_plus")
for (i in deaths_age_bands) {
  fig_name <- paste0("figs_by_age/deaths_hosp_", i, ".png")
  write_png(fig_name, width = 2400, height = 1200, res = 200,
            spimalot::spim_plot_trajectories_by_age(
              dat, sircovid::regions("england"), "deaths_hosp", age_band = i,
              with_forecast = FALSE, add_betas = FALSE))

  
  fig_name <- paste0("figs_by_age/deaths_comm_", i, ".png")
  write_png(fig_name, width = 2400, height = 1200, res = 200,
            spimalot::spim_plot_trajectories_by_age(
              dat, sircovid::regions("england"), "deaths_comm", age_band = i,
              with_forecast = FALSE, add_betas = FALSE))
}

admissions_age_bands <- c("0_9", "10_19", "20_29", "30_39", "40_49",
                          "50_59", "60_69", "70_79", "80_plus")
for (i in admissions_age_bands) {
  fig_name <- paste0("figs_by_age/admissions_", i, ".png")
  write_png(fig_name, width = 2400, height = 1200, res = 200,
            spimalot::spim_plot_trajectories_by_age(
              dat, sircovid::regions("england"), "all_admission", age_band = i,
              with_forecast = FALSE, add_betas = FALSE))
}

react_age_bands <- c("5_24", "25_34", "35_44", "45_54", "55_64", "65_plus")
for (i in react_age_bands) {
  fig_name <- paste0("figs_by_age/react_", i, ".png")
  write_png(fig_name, width = 2400, height = 1200, res = 200,
            spimalot::spim_plot_react(
              dat, sircovid::regions("england"), date_min = as.Date("2020-05-15"),
              ymax = 10, age_band = i))
}


## Nationally aggregated plots by age

write_png("figs/age_deaths_hosp.png", width = 2400, height = 1200, res = 200,
          spimalot::spim_plot_trajectories_by_age(
            dat, regions = "england", "deaths_hosp", age_band = deaths_age_bands,
            with_forecast = FALSE, add_betas = FALSE,
            title = "Hospital deaths by age - England"))

write_png("figs/age_deaths_comm.png", width = 2400, height = 1200, res = 200,
          spimalot::spim_plot_trajectories_by_age(
            dat, regions = "england", "deaths_comm", age_band = deaths_age_bands,
            with_forecast = FALSE, add_betas = FALSE,
            title = "Community deaths by age - England"))

write_png("figs/age_admissions.png", width = 2400, height = 1200, res = 200,
          spimalot::spim_plot_trajectories_by_age(
            dat, regions = "england", "all_admission", age_band = admissions_age_bands,
            with_forecast = FALSE, add_betas = FALSE,
            title = "Hospital admissions by age - England"))

write_png("figs/age_react_prevalence.png", width = 2400, height = 1200, res = 200,
          spimalot::spim_plot_react(
            dat, "england", date_min = as.Date("2020-05-15"),
            ymax = 10, age_band = react_age_bands,
            title = "Infection prevalence (REACT-1 study) - England"))

write_png("figs/age_pillar2.png", width = 2400, height = 1200, res = 200,
          spimalot::spim_plot_pillar2_positivity(
            dat, "england", pillar2_age_bands[-c(1, 2)],
            date_min = as.Date("2020-05-15"), ymax = 60,
            title = "Pillar 2 positivity (new infections) - England"))


if (rt_severity) {
  vam_data <- get_strain_timelines(dat$data)
  strain_epochs <- sircovid::sircovid_date_as_date(
    dat$parameters$base[[1]]$epoch_dates)[dat$parameters$base[[1]]$strain_epochs[-1L]]
  
  # Paper plots
  write_png("paper_plots/paper_figure_1.png", units = "in", width = 16.5,
            height = 10, res = 300,
            paper_plot_1(dat, "england", vam_data))
  
  write_png("paper_plots/paper_figure_2.png", units = "in", width = 16.5, 
            height = 10, res = 300,
            paper_plot_2(dat, "england", strain_epochs, vam_data, age_bands_select = TRUE))
  
  
  ## Supplement plots
  write_png("paper_plots/suppl_age_heatmaps.png", units = "in", width = 18,
            height = 15, res = 300,
            suppl_age_heatmaps(dat, "england", vam_data))
  
  write_png("paper_plots/suppl_regional_intrinsic.png", units = "in", width = 11,
            height = 11, res = 300,
            suppl_regional_intrinsic(dat, "england", "Emergence3", strain_epochs, vam_data))
}
