traceplots <- function(samples, control) {
  samples$pars <- abind::abind(samples$pars, samples$density, along = 1)
  rownames(samples$pars)[nrow(samples$pars)] <- "log_posterior"
  rownames(samples$pars) <- 
    gsub(">", "", gsub("<", "_", rownames(samples$pars)))
  
  samples <- posterior::as_draws_df(samples)
  
  color_scheme <- unname(unlist(rev(bayesplot::color_scheme_get("viridis"))))
  color_scheme <- gsub("*FF", "", color_scheme)
  
  bayesplot::color_scheme_set(color_scheme)
  p <- bayesplot::mcmc_trace(samples, n_warmup = control$pmcmc$n_burnin,
                             facet_args = list(nrow = 3,
                                               labeller = label_parsed)) +
    theme(legend.position = "none")
  p
}
