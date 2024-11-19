simulate_data <- function(y, pars) {
  n_regions <- dim(y)[2]
  n_days <- dim(y)[3] - 1
  
  simulate_data_region <- function(i) {
    alpha <- pars[[i]]$alpha
    cases_model <- y["cases_inc", i, seq_len(n_days) + 1]
    cases <- rnbinom(n_days, 1 / alpha,
                     mu = cases_model + rexp(n_days, rate = 1e6))
    
    data.frame(day = seq_len(n_days),
               cases = cases)
    
  }
  
  data <- lapply(seq_len(n_regions), simulate_data_region)
  names(data) <- regions
  
  ret <- dplyr::bind_rows(data, .id = "region") 
  
  ret
}
