
n_regions <- 5
regions <- LETTERS[seq_len(n_regions)]

orderly2::orderly_run("sir_data",
                      parameters = list(n_regions = n_regions))


### ---------------------------------------------------------------------------
hipercow::hipercow_init()
hipercow::hipercow_configure(driver = "dide-windows")
hipercow::hipercow_provision()


##----------------------------Long run------------------------------------------------
##--------------------
## Long runs multiregion
##--------------------
multiregion_fits <- hipercow::task_create_expr(
  orderly2::orderly_run('sir_fits',
                        parameters = list(short_run = FALSE,
                                          region = "all")),
  resources = hipercow::hipercow_resources(queue = 'AllNodes',
                                           cores = 20)
)
multiregion_fits_result <- hipercow::task_result(multiregion_fits)

##--------------------
## Long runs single region
##--------------------
single_region_fits <- hipercow::task_create_bulk_expr(
  orderly2::orderly_run('sir_fits',
                        parameters = list(short_run = FALSE,
                                          region = region)),
  data.frame(region = regions),
  resources = hipercow::hipercow_resources(queue = 'AllNodes',
                                           cores = 4)
)
single_region_fits_result <- 
  hipercow::hipercow_bundle_result(single_region_fits$name)




comparison <-
  obj$enqueue(orderly2::orderly_run('sir_fits_comparison',
                                    parameters = list(short_run = FALSE,
                                                      n_regions = n_regions)))
