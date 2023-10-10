## run script: multiregion and region specific deperministic models.
# If not already run you will need to run these

orderly2::orderly_run("sarscov2_data")


##----------------------------------
orderly2::orderly_run(
  "sarscov2_parameters",
  parameters = list(deterministic = TRUE,
                    multiregion = TRUE,
                    multi_pars_from_single = FALSE,
                    assumptions = "central"))

#reg specific param run 
orderly2::orderly_run(
  "sarscov2_parameters",
  parameters = list(deterministic = TRUE,
                    multiregion = FALSE,
                    multi_pars_from_single = TRUE,
                    assumptions = "central"))

### ---------------------------------------------------------------------------
setwd(orderly::orderly_config()$root)
packages <- c("sircovid", "lubridate", "coda", "tidyr", "ggplot2",
              "viridisLite", "orderly2", 'vaultr', 'readxl', "ggtext",
              'abind', 'here', "mcstate", "dust", "spimalot", "purrr",
              "stringr", "ggrepel", "naniar", "desplot", "rmarkdown",
              "jtools", "DescTools", "car")
src <- conan::conan_sources(NULL,
                            repos = c("https://ncov-ic.github.io/drat",
                                      "https://raphaels1.r-universe.dev"))
ctx <- context::context_save("contexts",
                             packages = packages,
                             package_sources = src)
cfg <- didehpc::didehpc_config(cluster = "wpia-hn",
                               template = 'AllNodes',
                               cores = 32)
obj <- didehpc::queue_didehpc(ctx, config = cfg)

regions <- sircovid::regions("england")

t <- obj$enqueue(packageVersion("sircovid"))
t$wait(timeout = 100)


##--------------------
## Short runs multiregion deterministic
##--------------------

multiregion_fits <- obj$enqueue(
  orderly2::orderly_run('sarscov2_fits',
                        parameters = list(short_run = TRUE,
                                          multiregion = TRUE,
                                          region = "england",
                                          deterministic = TRUE,
                                          assumptions = "central"))
)
multiregion_fits_result <- multiregion_fits$result()

# combine draft runs
combined_multiregion <-
  obj$enqueue(orderly2::orderly_run('sarscov2_fits_combined',
                       parameters = list(short_run = TRUE,
                                         multiregion = TRUE,
                                         deterministic = TRUE,
                                         assumptions = "central")))


## Short runs region specific deterministic
single_region_fits <- 
  obj$lapply(X = regions,
             FUN = function(x) {
               orderly2::orderly_run('sarscov2_fits',
                                    parameters = list(short_run = TRUE,
                                                      multiregion = FALSE,
                                                      region = x,
                                                      deterministic = TRUE,
                                                      assumptions = "central"))
               })

## make note of your bundle names and what they refer to!
batch_single_region_fits <- single_region_fits$name
res_single_region_fits <- obj$task_bundle_get(batch_single_region_fits)$results()

combined_single_region <-
  obj$enqueue(orderly2::orderly_run('sarscov2_fits_combined',
                                    parameters = list(short_run = TRUE,
                                                      deterministic = TRUE,
                                                      multiregion = FALSE,
                                                      assumptions = "central")))

comparison <-
  obj$enqueue(orderly2::orderly_run('sarscov2_fits_comparison',
                                    parameters = list(short_run = FALSE,
                                                      deterministic = TRUE,
                                                      assumptions = "central")))

##----------------------------Long run------------------------------------------------
##--------------------
## Long runs multiregion
##--------------------
multiregion_fits <- obj$enqueue(orderly2::orderly_run('sarscov2_fits',
                                                      parameters = list(short_run = FALSE,
                                                                        multiregion = TRUE,
                                                                        region = "england",
                                                                        deterministic = TRUE,
                                                                        assumptions = "central")))

multiregion_fits_result <- multiregion_fits$result()

# combine
combined_multiregion <-
  obj$enqueue(orderly2::orderly_run('sarscov2_fits_combined',
                                    parameters = list(short_run = FALSE,
                                                      multiregion = TRUE,
                                                      deterministic = TRUE,
                                                      assumptions = "central")))


##--------------------
## Long runs single region
##--------------------
single_region_fits <- 
  obj$lapply(X = regions,
             FUN = function(x) {
               orderly2::orderly_run('sarscov2_fits',
                                     parameters = list(short_run = FALSE,
                                                       multiregion = FALSE,
                                                       region = x,
                                                       deterministic = TRUE,
                                                       assumptions = "central"))})

## make note of your bundle names and what they refer to!
batch_single_region_fits <- single_region_fits$name
res_single_region_fits <- obj$task_bundle_get(batch_single_region_fits)$results()

combined_single_region <-
  obj$enqueue(orderly2::orderly_run('sarscov2_fits_combined',
                                    parameters = list(short_run = FALSE,
                                                      multiregion = FALSE,
                                                      deterministic = TRUE,
                                                      assumptions = "central")))

combined_single_region_result <- combined_single_region$result()


comparison <-
  obj$enqueue(orderly2::orderly_run('sarscov2_fits_comparison',
                                    parameters = list(short_run = FALSE,
                                                      deterministic = TRUE,
                                                      assumptions = "central")))
