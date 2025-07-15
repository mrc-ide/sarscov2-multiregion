
n_regions <- 5
regions <- LETTERS[seq_len(n_regions)]
deterministic <- FALSE

orderly2::orderly_run("sir_data",
                      parameters = list(n_regions = n_regions))


##----------------------------Run on cluster------------------------------------
hipercow::hipercow_init()
hipercow::hipercow_configure(driver = "dide-windows")
hipercow::hipercow_provision()

##--------------------
## Long runs multiregion
##--------------------
multiregion_fits <- hipercow::task_create_expr(
  orderly2::orderly_run('sir_fits',
                        parameters = list(short_run = FALSE,
                                          deterministic = deterministic,
                                          region = "multi",
                                          n_regions = n_regions)),
  resources = hipercow::hipercow_resources(queue = 'AllNodes',
                                           cores = 32)
)
multiregion_fits_result <- hipercow::task_result(multiregion_fits)


##--------------------
## Long runs single region
##--------------------
single_region_fits <- hipercow::task_create_bulk_expr(
  orderly2::orderly_run('sir_fits',
                        parameters = list(short_run = FALSE,
                                          deterministic = deterministic,
                                          region = region,
                                          n_regions = 1)),
  data.frame(region = regions),
  resources = hipercow::hipercow_resources(queue = 'AllNodes',
                                           cores = if (deterministic) 4 else 32)
)
single_region_fits_result <- 
  hipercow::hipercow_bundle_result(single_region_fits$name)




comparison <- obj$enqueue(
  orderly2::orderly_run('sir_fits_comparison',
                        parameters = list(short_run = FALSE,
                                          deterministic = deterministic,
                                          n_regions = n_regions)))




##----------------------------Run locally---------------------------------------
##--------------------
## Long runs multiregion
##--------------------
orderly2::orderly_run('sir_fits',
                      parameters = list(short_run = FALSE,
                                        deterministic = deterministic,
                                        region = "multi",
                                        n_regions = n_regions))

##--------------------
## Long runs single region
##--------------------
single_region_fits <- 
  lapply(regions,
         function(r) orderly2::orderly_run('sir_fits',
                                           parameters =
                                             list(short_run = FALSE,
                                                  deterministic = deterministic,
                                                  region = r,
                                                  n_regions = 1)))

comparison <-
  orderly2::orderly_run('sir_fits_comparison',
                        parameters = list(short_run = FALSE,
                                          deterministic = deterministic,
                                          n_regions = n_regions))
