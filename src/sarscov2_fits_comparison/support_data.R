aggregate_maxvar <- function(dat) {
  
  regions <- sircovid::regions("england")
  get_reg_pop <- function(r) sum(sircovid:::sircovid_population(r))
  wts <- vapply(regions, get_reg_pop, numeric(1))
  
  agg_maxvar1 <- function(x) {
    ## Take the regions, sort each and bind into an array
    x_regions <- dplyr::bind_rows(lapply(x[regions], sort))
    x$england <- apply(x_regions, 1, weighted.mean, w = wts)
    x
  }
  
  lapply(dat, function(w) lapply(w, agg_maxvar1))
}
