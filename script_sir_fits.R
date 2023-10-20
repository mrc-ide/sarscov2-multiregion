
n_regions <- 5
regions <- LETTERS[seq_len(n_regions)]

orderly2::orderly_run("sir_data",
                      parameters = list(n_regions = n_regions))


### ---------------------------------------------------------------------------
setwd(orderly::orderly_config()$root)
packages <- c("sircovid", "lubridate", "coda", "tidyr", "ggplot2",
              "viridisLite", "orderly2", 'vaultr', 'readxl', "ggtext",
              'abind', 'here', "mcstate", "dust", "spimalot", "purrr",
              "stringr", "ggrepel", "naniar", "desplot", "rmarkdown",
              "jtools", "DescTools", "car", "odin.dust")
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


##----------------------------Long run------------------------------------------------
##--------------------
## Long runs multiregion
##--------------------
multiregion_fits <- obj$enqueue(orderly2::orderly_run('sir_fits',
                                                      parameters = list(short_run = FALSE,
                                                                        multiregion = TRUE,
                                                                        region = "all")))

multiregion_fits_result <- multiregion_fits$result()

##--------------------
## Long runs single region
##--------------------
single_region_fits <- 
  obj$lapply(X = regions,
             FUN = function(x) {
               orderly2::orderly_run('sir_fits',
                                     parameters = list(short_run = FALSE,
                                                       multiregion = FALSE,
                                                       region = x))})

## make note of your bundle names and what they refer to!
batch_single_region_fits <- single_region_fits$name
res_single_region_fits <- obj$task_bundle_get(batch_single_region_fits)$results()


comparison <-
  obj$enqueue(orderly2::orderly_run('sir_fits_comparison',
                                    parameters = list(short_run = FALSE,
                                                      n_regions = n_regions)))
