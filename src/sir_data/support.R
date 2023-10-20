simulate_data <- function(y, pars) {
  n_regions <- dim(y)[3]
  n_days <- dim(y)[4] - 1
  
  simulate_data_region <- function(i) {
    alpha <- pars[[i]]$alpha
    cases_model <- y["cases_inc", , i, seq_len(n_days) + 1]
    cases <- rnbinom(n_days, 1 / alpha,
                     mu = cases_model + rexp(n_days, rate = 1e6))
    
    rho <- pars[[i]]$rho
    prob_pos <- y["I", , i, seq_len(n_days) + 1] / pars[[i]]$N
    n_positives <- rbetabinom(n_days, 50, prob_pos, rho)
    data.frame(day = seq_len(n_days),
               cases = cases,
               n_positives = n_positives,
               n_tests = 50)
    
  }
  
  data <- lapply(seq_len(n_regions), simulate_data_region)
  names(data) <- regions
  
  ret <- dplyr::bind_rows(data, .id = "population") 
  ret$population <- factor(ret$population)
  
  ret
}

rbetabinom <- function(n, size, mu, rho) {
  a <- mu * (1 / rho - 1)
  b <- (1 - mu) * (1 / rho - 1)
  
  p <- rbeta(n, a, b)
  rbinom(n, size, p)
}
