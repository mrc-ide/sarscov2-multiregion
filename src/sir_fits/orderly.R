orderly2::orderly_dependency(
  "sir_data",
  'latest()',
  c("inputs/data.rds" = "outputs/data.rds"))

orderly2::orderly_parameters(short_run = FALSE,
                             multiregion = TRUE,
                             region = "all")

orderly2::orderly_artefact("Fit object",
                           "outputs/fit.rds")

orderly2::orderly_shared_resource(sir.R = "sir.R")

orderly2::orderly_resource("support.R")
source("support.R")

library(mcstate)
library(dust)
library(odin.dust)

data <- readRDS("inputs/data.rds")

sir <- odin.dust::odin_dust("sir.R")

fit <- run_fit(data, sir, region, short_run)


dir.create("outputs", FALSE, TRUE)
saveRDS(fit, "outputs/fit.rds")
