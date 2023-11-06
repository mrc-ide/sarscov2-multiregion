index <- function(info) {
  list(run = unlist(info$index["cases_inc"]), state = unlist(info$index))
}

run_fit <- function(data, model, region, short_run) {
  
  multiregion <- region == "all"
  if (short_run) {
    burnin <- 50
    n_steps <- 150
    n_sample <- 100
    n_chains <- 4
  } else {
    burnin <- 5000
    n_steps <- 15000
    n_sample <- 1000
  }
  if (multiregion) {
    burnin <- 2 * burnin
    n_steps <- 2 * n_steps
  }
  n_chains <- 4
  
  n_steps_retain <- ceiling(n_sample / n_chains)
  
  parallel <- control_parallel(n_chains, multiregion)
  
  adaptive_proposal <- mcstate::adaptive_proposal_control(initial_vcv_weight = 100,
                                                          min_scaling = 1)
  control <- 
    mcstate::pmcmc_control(n_steps = n_steps, n_burnin = burnin,
                           n_threads_total = parallel$n_threads_total,
                           n_workers = parallel$n_workers, n_chains = n_chains,
                           n_steps_retain = n_steps_retain, save_state = TRUE,
                           adaptive_proposal = adaptive_proposal,
                           save_trajectories = TRUE, progress = TRUE)
  
  if (multiregion) {
    pf_data <- 
      mcstate::particle_filter_data(data, time = "day", rate = 4,
                                    initial_time = 0, population = "population")
    regions <- levels(pf_data$population)
    n_regions <- length(regions)
    
    beta <- mcstate::pmcmc_varied_parameter(
      name = "beta",
      populations = regions,
      initial = rep(0.5, n_regions),
      min = 0, max = 1)
    gamma <- mcstate::pmcmc_parameter("gamma", 0.5, min = 0, max = 1)
    alpha <- mcstate::pmcmc_parameter("alpha", 0.5, min = 0, max = 1)
    rho <- mcstate::pmcmc_parameter("rho", 0.05, min = 0, max = 1)
    
    proposal_fixed <- diag(c(0.001, 0.001, 0.00001))
    row.names(proposal_fixed) <- colnames(proposal_fixed) <-
      c("gamma", "alpha", "rho")
    proposal_varied <- array(rep(0.001, n_regions), c(1, 1, n_regions),
                             dimnames = list("beta", "beta",
                                             regions))
    
    transform <- lapply(regions, function(r) transform_pars)
    names(transform) <- regions
    
    pars <- mcstate::pmcmc_parameters_nested$new(
      parameters = list(beta = beta, gamma = gamma, alpha = alpha,
                        rho = rho),
      proposal_varied = proposal_varied,
      proposal_fixed = proposal_fixed,
      populations = regions,
      transform = transform)
    
    initial <- replicate(control$n_chains,
                         pars$propose(pars$initial(), "both", scale = 10))
    
    message("Running multiregion fit")
  } else {
    pf_data <- 
      mcstate::particle_filter_data(data[data$population == region, ],
                                    time = "day", rate = 4, initial_time = 0)
    
    pars <- mcstate::pmcmc_parameters$new(
      list(mcstate::pmcmc_parameter("beta", 0.5, min = 0, max = 1),
           mcstate::pmcmc_parameter("gamma", 0.5, min = 0, max = 1),
           mcstate::pmcmc_parameter("alpha", 0.5, min = 0, max = 1),
           mcstate::pmcmc_parameter("rho", 0.05, min = 0, max = 1)),
      proposal = diag(c(0.001, 0.001, 0.001, 0.00001)),
      transform = transform_pars)
    
    initial <- replicate(control$n_chains,
                         pars$propose(pars$initial(), scale = 10))
    
    message(sprintf("Running single region fit for region %s", region))
  }
  
  n_threads <- parallel$n_threads_total / parallel$n_workers
  p <- mcstate::particle_deterministic$new(pf_data, model, compare = NULL,
                                           index = index, n_threads = n_threads)
  
  samples <-  mcstate::pmcmc(pars, p, initial = initial, control = control)
  
  
  if (multiregion) {
    info <- list(region = levels(data$population),
                 pars = list(fixed = pars$names("fixed"),
                             varied = pars$names("varied")),
                 n_burnin = burnin,
                 n_steps = n_steps,
                 n_chains = n_chains,
                 n_sample = n_sample)
  } else {
    info <- list(region = region,
                 pars = pars$names(),
                 n_burnin = burnin,
                 n_steps = n_steps,
                 n_chains = n_chains,
                 n_sample = n_sample)
  }
  
  list(samples = samples,
       data = pf_data,
       info = info)
  
}

control_parallel <- function(n_chains, multiregion, verbose = TRUE) {
  n_threads <- control_cores()
  max_workers <- 4
  
  if (!multiregion) {
    n_workers <- min(n_chains, n_threads)
    n_threads <- n_workers
  } else {
    n_workers <- min(n_chains, n_threads, max_workers)
    n_threads_given <- n_threads
    n_threads <- ceiling(n_threads / n_workers) * n_workers
    if (verbose && n_threads > n_threads_given) {
      message(sprintf("Increasing total threads from %d to %d",
                      n_threads_given, n_threads))
    }
  }
  
  if (verbose) {
    message(sprintf("Running on %d workers with %d threads",
                    n_workers, n_threads))
  }
  list(n_threads_total = n_threads, n_workers = n_workers)
}

control_cores <- function() {
  as.integer(Sys.getenv("CONTEXT_CORES",
                        Sys.getenv("MC_CORES",
                                   getOption("mc.cores", 1))))
}

