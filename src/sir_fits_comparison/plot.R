plot_traceplots <- function(fit, r) {
  
  samples <- fit$samples[[r]]
  
  n_pars_full <- nrow(samples$pars_full)
  n_chains <- max(samples$chain)
  cols <- rev(viridisLite::viridis(n_chains))
  samples$chain_full <- rep(seq_len(n_chains), each = n_pars_full / n_chains)
  
  stopifnot(
    identical(samples$chain_full,
              rep(seq_len(n_chains),
                  each = length(samples$chain_full) / n_chains)))
  
  pars <- samples$pars_full
  probs <- samples$probabilities_full
  
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

plot_trajectories <- function(fit, true_history, regions, what) {

  n_regions <- length(regions)
  op <- par(mfcol = c(length(what), n_regions),
            oma = c(1, 1, 4, 1),
            mar = c(3, 3, 0.5, 0.5))
  on.exit(par(op))
  
  
  for (r in regions) {
    plot_trajectories_region(r, fit, true_history, what)
  }
  
  
  mtext(side = 3, text = toupper(regions),
        line = 0.5, outer = TRUE, cex = 0.8,
        at = seq(1 / n_regions / 2, by = 1 / n_regions, length.out = n_regions))
}

plot_trajectories_region <- function(region, fit, true_history, what) {
  for (w in what) {
    plot_trajectories_region1(w, region, fit, true_history)
  }
}

plot_trajectories_region1 <- function(what, region, fit, true_history) {
  
  trajectories <- fit$samples[[region]]$trajectories
  date <- trajectories$time / trajectories$rate
  cols <- plot_colours()
  
  res_fit <- trajectories$state[what, , ]
  
  res_true <- true_history[what, , region, ]
  
  ps <- seq(0.025, 0.975, 0.005)
  qs <- apply(res_fit, MARGIN = 2, FUN = quantile, ps, na.rm = TRUE)
  
  oo <- par(mgp = c(1.7, 0.5, 0), bty = "n")
  on.exit(oo)
  
  
  xlim <- c(min(date), max(date))
  
  
  ylim <- c(0, max(res_fit, res_true))
  plot(xlim[1], 0, type = "n",
       xlim = xlim,
       ylim = ylim,
       xlab = "t", ylab = what)
  fit_cols <- c(mix_cols(cols$blue, "white", 0.7),
                mix_cols(cols$blue, "white", 0.495))
  
  ci_bands(qs[c("2.5%", "25.0%", "75.0%", "97.5%"), ], date,
           cols = fit_cols, horiz = FALSE, leg = FALSE)
  lines(date, qs["50.0%", ], col = cols$now, lty = 1, lwd = 1.5,
        lend = 1)
  points(date, res_true, pch = 23, bg = cols$orange, col = cols$brown, cex = 0.7,
         lwd = 0.6)
  
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

ci_bands <- function(quantiles, y, palette = NULL, cols = NULL, leg = TRUE,
                     leg_y = 0, leg_x = 1, horiz = TRUE, ...) {
  yy <- c(y, rev(y))
  yy <- c(yy, yy[1])
  n_bands <- (nrow(quantiles) - 1) / 2 + 1
  if (!is.null(palette)) {
    cols <- do.call(what = palette,
                    args = list(n = n_bands))
  }
  
  
  for (band in seq_len(n_bands)) {
    x1 <- quantiles[band, ]
    x2 <- quantiles[nrow(quantiles) + 1 - band, ]
    
    
    x2 <- rev(x2)
    x2 <- c(x2, x1[1])
    if (horiz) {
      polygon(y = yy,
              x = c(x1, x2),
              col = cols[band],
              border = NA)
    } else {
      polygon(x = yy,
              y = c(x1, x2),
              col = cols[band],
              border = NA)
    }
    
    
  }
  if (leg) {
    leg_cols <- which(row.names(quantiles) %in% leg)
    leg <- c(row.names(quantiles)[1], seq(5, 50, 5), "%")
    leg[seq(2, 10, 2)] <- ""
    legend(y = leg_y,
           x = leg_x,
           pch = 15,
           col = cols[leg_cols],
           legend = leg,
           border = NA,
           bty = "n", ...)
  }
}

ci_bands <- function(quantiles, y, palette = NULL, cols = NULL, leg = TRUE,
                     leg_y = 0, leg_x = 1, horiz = TRUE, ...) {
  yy <- c(y, rev(y))
  yy <- c(yy, yy[1])
  n_bands <- (nrow(quantiles) - 1) / 2 + 1
  if (!is.null(palette)) {
    cols <- do.call(what = palette,
                    args = list(n = n_bands))
  }
  
  
  for (band in seq_len(n_bands)) {
    x1 <- quantiles[band, ]
    x2 <- quantiles[nrow(quantiles) + 1 - band, ]
    
    
    x2 <- rev(x2)
    x2 <- c(x2, x1[1])
    if (horiz) {
      polygon(y = yy,
              x = c(x1, x2),
              col = cols[band],
              border = NA)
    } else {
      polygon(x = yy,
              y = c(x1, x2),
              col = cols[band],
              border = NA)
    }
    
    
  }
  if (leg) {
    leg_cols <- which(row.names(quantiles) %in% leg)
    leg <- c(row.names(quantiles)[1], seq(5, 50, 5), "%")
    leg[seq(2, 10, 2)] <- ""
    legend(y = leg_y,
           x = leg_x,
           pch = 15,
           col = cols[leg_cols],
           legend = leg,
           border = NA,
           bty = "n", ...)
  }
}

mix_cols <- function(a, b, weight = 0.5) {
  a_rgb <- vapply(a, grDevices::col2rgb, numeric(3))
  b_rgb <- vapply(b, grDevices::col2rgb, numeric(3))
  ret <- a_rgb * (1 - weight) + b_rgb * weight
  grDevices::rgb(ret[1, ], ret[2, ], ret[3, ], maxColorValue = 255)
}




plot_colours <- function() {
  list(
    green = "#58A449",
    green2 = "#228B22",
    blue = "#278B9A",
    sky_blue = "#C0DDE1",
    sky_blue2 = "#7EBAC2",
    orange = "#E48C2A",
    brown = "#724615",
    puce = "#AF9699",
    purple = "#5C5992",
    cyan = "#699196",
    cyan2 = "#405D61")
}

plot_adaptive_scaling <- function(fit) {
  multiregion <- fit$samples[[1]]$nested
  
  regions <- names(fit$samples)
  
  if (multiregion) {
    n_rows <- ceiling((length(fit$samples) + 1) / 2)
  } else {
    n_rows <- ceiling(length(fit$samples) / 2)
  }
  n_cols <- 2
  
  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  
  par(mfrow = c(n_rows, n_cols),
      mar = c(3, 3, 2, 1),
      mgp = c(2, 0.5, 0),
      oma = c(1, 1, 1, 1))
  
  if (multiregion) {
    for (r in regions) {
      matplot(fit$samples[[1]]$adaptive$scaling$varied[[r]], 
              type = "l", main = r, ylab = "scaling")
    }
    matplot(fit$samples[[1]]$adaptive$scaling$fixed, 
            type = "l", main = "Fixed", ylab = "scaling")
  } else {
    for (r in regions) {
      matplot(fit$samples[[r]]$adaptive$scaling, 
              type = "l", main = r, ylab = "scaling")
    }
  }
}
