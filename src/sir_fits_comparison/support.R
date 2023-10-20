get_convergence_diagnostic <- function(fit) {
  
  samples <- fit$samples
  info <- fit$info
  
  multiregion <- samples$nested
  
  pars <- samples$pars_full
  n_pars_full <- nrow(pars)
  n_chains <- max(samples$chain)
  
  burnin <- info$n_burnin
  
  chain_full <- rep(seq_len(n_chains), each = n_pars_full / n_chains)
  
  if (multiregion) {
    regions <- info$region
    n_regions <- length(regions)
    
    nms_fixed <- info$pars$fixed
    nms_varied <- info$pars$varied
    
    pars_fixed <- pars[, , 1]
    pars_fixed <- pars_fixed[, nms_fixed, drop = FALSE]
    pars_varied <- pars[, nms_varied, , drop = FALSE]
    pars_varied <- array(pars_varied,
                         c(n_pars_full, n_regions * length(nms_varied)))
    colnames(pars_varied) <-
      apply(expand.grid(nms_varied, regions), 1, paste, collapse = "_")
    
    pars <- cbind(pars_fixed, pars_varied)
  }
  
  clean_chains <- function(x) {
    ## Remove burnin
    out <- x[-seq_len(burnin), ]
    
    if (multiregion) {
      ## Thin by 2
      out <- x[seq(1, nrow(out), by = 2), ]
    }
    
    out
  }
  
  chains <- lapply(
    lapply(unname(split(data.frame(pars), chain_full)), clean_chains),
    coda::as.mcmc)
  
  rhat <- tryCatch(coda::gelman.diag(chains, multivariate = FALSE),
                   error = function(e) NULL)
  if (!is.null(rhat)) {
    rhat <- round(max(rhat$psrf[, "Point est."]), 3)
  } else {
    rhat <- NA_real_
  }
  
  calc_ess <- function(p) {
    traces <- matrix(p, ncol = n_chains)
    traces <- traces[-seq_len(burnin), ]
    if (multiregion) {
      traces <- traces[seq(1, nrow(traces), by = 2), ]
    }
    sum(coda::effectiveSize(coda::as.mcmc(traces)))
  }
  
  nms <- colnames(pars)
  ess <- lapply(nms, function (nm) {
    calc_ess(pars[, nm])
  })
  ess <- round(min(unlist(ess)))
  
  data.frame(rhat, ess)
}
