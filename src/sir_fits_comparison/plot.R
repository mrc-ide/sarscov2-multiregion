plot_traceplots <- function(fit, r = NULL) {
  
  samples <- fit$samples
  
  n_pars_full <- nrow(samples$pars_full)
  n_chains <- max(samples$chain)
  cols <- rev(viridisLite::viridis(n_chains))
  samples$chain_full <- rep(seq_len(n_chains), each = n_pars_full / n_chains)
  
  stopifnot(
    identical(samples$chain_full,
              rep(seq_len(n_chains),
                  each = length(samples$chain_full) / n_chains)))
  
  if (is.null(r)) {
    pars <- samples$pars_full
    probs <- samples$probabilities_full
  } else {
    pars <- samples$pars_full[, , r]
    probs <- samples$probabilities_full[, , r]
  }
  
  nms <- colnames(pars)
  
  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  
  new_grid <- function(n, title) {
    n_rows <- ceiling(sqrt(n + 1))
    if (n + 1 <= n_rows * (n_rows - 1)) {
      n_cols <- n_rows - 1
    } else {
      n_cols <- n_rows
    }
    
    par(mfrow = c(n_rows, n_cols),
        mar = c(3, 3, 2, 1),
        mgp = c(2, 0.5, 0),
        oma = c(1, 1, 1 + as.integer(title), 1))
  }
  
  plot_traces1 <- function(p, name) {
    traces <- matrix(p, ncol = n_chains)
    ess <- coda::effectiveSize(coda::as.mcmc(traces))
    
    matplot(traces, type = "l", lty = 1,
            xlab = "Iteration", bty = "n",
            ylab = name, col = cols,
            main = "",
            font.main = 1)
  }
  
  new_grid(length(nms), FALSE)
  for (nm in nms) {
    plot_traces1(pars[, nm], nm)
  }
  plot_traces1(probs[, "log_likelihood"], "log_likelihood")
  
}

write_device <- function(dev, filename, code, ...) {
  dev(filename, ...)
  on.exit(dev.off())
  res <- force(code)
  if (inherits(res, "ggplot")) {
    print(res)
    NULL
  }
}

write_png <- function(filename, code, ...) {
  write_device(png, filename, code, ...)
}

