
orderly_pars <- orderly2::orderly_parameters(n_regions = 5,
                                             deterministic_data = FALSE)


orderly2::orderly_artefact(description = "Simulated data and model code",
                           files = c("outputs/data.rds",
                                     "outputs/true_pars.rds",
                                     "outputs/true_history.rds",
                                     "sir.R"))

orderly2::orderly_resource("support.R")
orderly2::orderly_resource("sir.R")
source("support.R")

library(odin2)
library(dust2)

sir <- odin2::odin("sir.R")

regions <- LETTERS[seq_len(orderly_pars$n_regions)]

set.seed(1)

pars_region_1 <- function() {
  list(beta = rgamma(1, shape = 100, scale = 0.2 / 100),
       gamma = 0.1,
       alpha = 0.2,
       lambda = rpois(1, 10),
       N = 1000)
}

pars <- lapply(regions, function(x) pars_region_1())

sys <- 
  dust2::dust_system_create(sir, pars, dt = 0.25,
                            n_groups = orderly_pars$n_regions,
                            deterministic = orderly_pars$deterministic_data)
dust2::dust_system_set_state_initial(sys)
time <- 0:100
y <- dust2::dust_system_simulate(sys, time)

rownames(y) <- unlist(names(dust2::dust_unpack_index(sys)))
colnames(y) <- regions


data <- simulate_data(y, pars)

dir.create("outputs", FALSE, TRUE)

saveRDS(data, "outputs/data.rds")

names(pars) <- regions

saveRDS(pars, "outputs/true_pars.rds")
saveRDS(y, "outputs/true_history.rds")
