
orderly2::orderly_parameters(n_regions = 5)

orderly2::orderly_artefact("Compiled SIR model and simulated data",
                           c("outputs/sir.rds",
                             "outputs/data.rds"))

orderly2::orderly_shared_resource(sir.R = "sir.R")

orderly2::orderly_resource("support.R")
source("support.R")

library(odin.dust)
library(dust)

sir <- odin.dust::odin_dust("sir.R")

regions <- LETTERS[seq_len(n_regions)]

set.seed(1)

pars_region_1 <- function() {
  list(beta = rgamma(1, shape = 100, scale = 0.2 / 100),
       gamma = 0.1,
       alpha = 0.2,
       rho = 0.01,
       N = 10000)
}

pars <- lapply(regions, function(x) pars_region_1())

mod <- sir$new(pars, 0, 1, seed = 1L, pars_multi = TRUE)

n_days <- 100

y <- mod$simulate(seq(0, 4 * n_days, by = 4))
rownames(y) <- names(mod$info()[[1]]$index)

data <- simulate_data(y, pars)

dir.create("outputs", FALSE, TRUE)

saveRDS(sir, "outputs/sir.rds")
saveRDS(data, "outputs/data.rds")
