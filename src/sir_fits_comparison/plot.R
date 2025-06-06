
plot_trajectories <- function(fit, true_history, regions, what,
                              multiregion = FALSE, beta_hyperprior = FALSE) {
  
  n_regions <- length(regions)
  op <- par(mfcol = c(length(what), n_regions),
            oma = c(1, 1, 4, 1),
            mar = c(3, 3, 0.5, 0.5))
  
  on.exit(par(op))
  for (r in regions) {
    plot_trajectories_region(r, fit, true_history, what, multiregion,
                             beta_hyperprior)
  }
  
  
  mtext(side = 3, text = toupper(regions),
        line = 0.5, outer = TRUE, cex = 0.8,
        at = seq(1 / n_regions / 2, by = 1 / n_regions, length.out = n_regions))
}

plot_trajectories_region <- function(region, fit, true_history, what,
                                     multiregion = FALSE,
                                     beta_hyperprior = FALSE) {
  for (w in what) {
    plot_trajectories_region1(w, region, fit, true_history, multiregion,
                              beta_hyperprior)
  }
}

plot_trajectories_region1 <- function(what, region, fit, true_history,
                                      multiregion = FALSE,
                                      beta_hyperprior = FALSE) {
  
  if (multiregion) {
    if (beta_hyperprior) {
      trajectories <- fit$multi_beta_hp$observations$trajectories
    } else {
      trajectories <- fit$multi$observations$trajectories
    }
    res_fit <- trajectories[what, region, , ]
  } else {
    trajectories <- fit[[region]]$observations$trajectories
    res_fit <- trajectories[what, , ]
  }
  
  if (what == "cases_inc") {
    res_true <- fit[[region]]$data$cases
  } else {
    res_true <- true_history[what, region, -1L]
  }
  date <- fit[[region]]$data$day
  
  cols <- plot_colours()
  
  
  
  ps <- seq(0.025, 0.975, 0.005)
  qs <- apply(res_fit, MARGIN = 1, FUN = quantile, ps, na.rm = TRUE)
  
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
  lines(date, qs["50.0%", ], col = cols$blue, lty = 1, lwd = 1.5,
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


forest_plot <- function(fits, true_pars) {
  
  plot1 <- function(par, shared = TRUE) {
    
    summarise <- function(region, multi_varied = FALSE) {
      if (multi_varied) {
        p <- fits$multi$pars[paste0(par, "<", region, ">"), ]
      } else {
        p <- fits[[region]]$pars[par, ]
      }
      data.frame(region = region,
                 mean = mean(p),
                 lb = quantile(p, 0.025, names = FALSE),
                 ub = quantile(p, 0.975, names = FALSE))
    }
    
    df <- lapply(regions, summarise) %>%
      dplyr::bind_rows()
    
    if (shared) {
      df <- rbind(df, summarise("multi"))
      df$region <- factor(df$region, c("multi", rev(regions)))
      
      true_value <- true_pars[[1]][[par]]
      
      ggplot(df, aes(y = region, x = mean, xmin = lb, xmax = ub)) +
        theme_classic() +
        geom_point(color = 'black', shape = 18, size = 3) +
        geom_linerange() +
        geom_point(data = subset(df, region == 'multi'),
                   color = 'red', shape = 18, size = 3) +
        geom_linerange(data = subset(df, region == 'multi'), color = 'red') +
        geom_vline(xintercept = true_value, color = "blue",
                   linetype = "dashed") +
        xlab(par)
    } else {
      df$fit <- "single"
      df_multi <- lapply(regions, function(r) summarise(r, TRUE)) %>%
        dplyr::bind_rows()
      df_multi$fit <- "multi"
      df <- rbind(df, df_multi)
      df$region <- factor(df$region, rev(regions))
      
      true_value <- 
        vapply(rev(regions), function(r) true_pars[[r]][[par]], numeric(1))
      
      g <- ggplot(df, aes(y = region, x = mean, xmin = lb, xmax = ub, col = fit,
                     fill = fit)) +
        theme_classic() +
        geom_point(shape = 18, size = 3, 
                   position = position_dodge(width = 0.5)) +
        geom_linerange(position = position_dodge(width = 0.5)) +
        scale_fill_manual(values = c("red", "black"), guide = "none") +
        scale_color_manual(values = c("red", "black"), guide = "none") +
        xlab(par)
      
      for (i in 1:length(true_value)) {
        g <- g + geom_segment(y = i - 0.5, yend = i + 0.5, x = true_value[i],
                              xend = true_value[i],
                              linetype = "dashed", color = "blue")
      }
      g
    }
    
    
  }
  
  alpha <- plot1("alpha")
  gamma <- plot1("gamma")
  beta <- plot1("beta", shared = FALSE)
  lambda <- plot1("lambda", shared = FALSE)
  
  (alpha | gamma) / (beta | lambda)
}
