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
## Basic cluster setup
hipercow::hipercow_init(driver = "windows")
hipercow::hipercow_provision(method = "pkgdepends")

regions <- sircovid::regions("england")

short_run <- TRUE

##--------------------
## multiregion deterministic
##--------------------

multiregion_fits <- hipercow::task_create_expr(
  orderly2::orderly_run('sarscov2_fits',
                        parameters = list(region = "england",
                                          multiregion = TRUE,
                                          short_run = short_run,
                                          deterministic = TRUE,
                                          assumptions = "central")),
  resources = hipercow::hipercow_resources(queue = 'AllNodes',
                                           cores = 32)
)
res <- hipercow::task_result(multiregion_fits)

# combine runs
multiregion_combined <- hipercow::task_create_expr(
  orderly2::orderly_run('sarscov2_fits_combined',
                        parameters = list(short_run = short_run,
                                          multiregion = TRUE,
                                          deterministic = TRUE,
                                          assumptions = "central")),
  resources = hipercow::hipercow_resources(queue = 'AllNodes',
                                           cores = 32)
)
res <- hipercow::task_result(multiregion_combined)


##--------------------
## single region deterministic
##--------------------

single_region_fits <- 
  hipercow::task_create_bulk_expr(
    orderly2::orderly_run('sarscov2_fits',
                          parameters = list(region = region,
                                            multiregion = FALSE,
                                            short_run = short_run,
                                            deterministic = TRUE,
                                            assumptions = "central")),
    data.frame(region = regions),
    resources = hipercow::hipercow_resources(queue = 'AllNodes',
                                             cores = 4))
res <- hipercow::hipercow_bundle_result(single_region_fits$name)

# combine runs
single_region_combined <- hipercow::task_create_expr(
  orderly2::orderly_run('sarscov2_fits_combined',
                        parameters = list(short_run = short_run,
                                          multiregion = FALSE,
                                          deterministic = TRUE,
                                          assumptions = "central")),
  resources = hipercow::hipercow_resources(queue = 'AllNodes',
                                           cores = 32)
)
res <- hipercow::task_result(single_region_combined)


##--------------------
## comparison
##--------------------

comparison <- hipercow::task_create_expr(
  orderly2::orderly_run('sarscov2_fits_comparison',
                        parameters = list(short_run = short_run,
                                          deterministic = TRUE,
                                          assumptions = "central")),
  resources = hipercow::hipercow_resources(queue = 'AllNodes',
                                           cores = 32)
)
res <- hipercow::task_result(comparison)
