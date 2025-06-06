
get_data <- function(data, region) {
  if (region == "all") {
    dust2::dust_filter_data(data, time = "day", group = "region")
  } else {
    data <- data[data$region == region, ]
    data$region <- NULL
    dust2::dust_filter_data(data, time = "day")
  }
}

fit_control <- function(region, deterministic, n_steps, n_burnin,
                        n_sample, n_chains, n_particles) {
  
  multiregion <- region == "all"
  
  adaptive_proposal <- deterministic
  
  thinning_factor <- floor((n_steps - n_burnin) / (n_sample / n_chains))
  
  parallel <- control_parallel(n_chains, multiregion)
  
  rerun_every <- if (deterministic) Inf else 100
  
  n_particles <- if (deterministic) 1 else n_particles
  
  filter <- list(n_particles = n_particles,
                 n_threads = parallel$n_threads)
  
  pmcmc <- list(n_steps = n_steps,
                n_chains = n_chains,
                n_burnin = n_burnin,
                thinning_factor = thinning_factor,
                n_threads_total = parallel$n_threads_total,
                n_workers = parallel$n_workers,
                rerun_every = rerun_every,
                rerun_random = TRUE, 
                adaptive_proposal = adaptive_proposal,
                progress = TRUE)
  
  list(
    filter = filter,
    pmcmc = pmcmc
  )
}

create_filter <- function(sys, data, deterministic, control) {
  
  if (deterministic) {
    dust2::dust_unfilter_create(sys, 0, data, dt = 0.25)
    
  } else {
    dust2::dust_filter_create(sys, 0, data, dt = 0.25,
                              n_particles = control$filter$n_particles,
                              n_threads = control$filter$n_threads)
  }
}

create_packer <- function(groups = NULL, beta_hyperprior = FALSE) {
  
  fitted_pars <- c("alpha",
                   "beta",
                   "gamma",
                   "lambda")
  
  if (is.null(groups)) {
    
    fixed <- list(N = 1000)
    packer <- monty::monty_packer(scalar = fitted_pars, fixed = fixed)
  
  } else {
    
    shared <- c("alpha", "gamma")
    
    if (beta_hyperprior) {
      fitted_pars <- c(fitted_pars, c("beta_cv", "beta_mean"))
      shared <- c(shared, c("beta_cv", "beta_mean"))
    }
    
    fixed <- list(N = 1000)
    packer <- monty::monty_packer_grouped(
      groups, scalar = fitted_pars, fixed = fixed, shared = shared)
    
  }
  
  packer
}

create_prior <- function(region, names){
  ## We will use the monty DSL for priors, but it is not currently setup
  ## for nested models so we will have to write that version manually
  if (region == "all") {
    domain <- array(0, c(length(names), 2))
    rownames(domain) <- names
    domain[, 2] <- ifelse(grepl("^alpha|^beta_cv", names), 1, 1000)
    monty_model(
      list(
        parameters = names,
        density = function(x) {
          names(x) <- names
          dens <- sum(dbeta(x[grepl("^alpha", names(x))], 1, 1, log = TRUE)) +
            sum(dunif(x[grepl("^gamma", names(x))], 0, 1000, log = TRUE)) +
            sum(dunif(x[grepl("^lambda", names(x))], 0, 1000, log = TRUE))
          if (all(c("beta_mean", "beta_cv") %in% names(x))) {
            beta_scale <- x["beta_mean"] * x["beta_cv"]^2
            dens <- dens +
              dunif(x["beta_mean"], 0, 1000, log = TRUE) +
              dunif(x["beta_cv"], 0, 1, log = TRUE) +
              sum(dgamma(x[grepl("^beta<", names(x))], 
                         shape = 1 / x["beta_cv"]^2, 
                         scale = beta_scale, log = TRUE))
          } else {
            dens <- dens + 
              sum(dunif(x[grepl("^beta<", names(x))], 0, 1000, log = TRUE))
          }
        },
        domain = domain
      ))
  } else {
    monty::monty_dsl({
      alpha ~ Beta(1, 1)
      beta ~ Uniform(0, 1000)
      gamma ~ Uniform(0, 1000)
      lambda ~ Uniform(0, 1000)
    })
  }
}

run_fit <- function(filter, packer, prior, control, deterministic, region) {
  
  likelihood <- dust2::dust_likelihood_monty(filter, packer,
                                             save_trajectories = TRUE)
  
  density <- likelihood + prior 
  
  names <- packer$names()
  initial <- rep(0, length(names))
  initial[grepl("^alpha|^beta|^gamma", names)] <- 0.5
  initial[grepl("^beta_cv", names)] <- 0.2
  initial[grepl("^lambda", names)] <- 5
  
  vcv <- array(0, c(length(names), length(names)))
  diag(vcv[grepl("^alpha|^beta|^gamma", names),
           grepl("^alpha|^beta|^gamma", names)]) <- 0.001
  if (sum(grepl("^lambda", names)) == 1) {
    vcv[grepl("^lambda", names), grepl("^lambda", names)] <- 1
  } else {
    diag(vcv[grepl("^lambda", names), grepl("^lambda", names)]) <- 1
  }
  
  
  
  n_steps <- control$pmcmc$n_steps
  n_chains <- control$pmcmc$n_chains
  
  if (deterministic) {
    sampler <- monty::monty_sampler_adaptive(vcv)
  } else {
    sampler <- monty::monty_sampler_random_walk(vcv)
  }
  
  runner <- monty::monty_runner_serial()
  samples <- monty::monty_sample(density, sampler, n_steps,
                                 initial = initial, n_chains = n_chains,
                                 runner = runner)
  
  rownames(samples$observations$trajectories) <- filter$packer_state$names()
  if (region == "all") {
    colnames(samples$observations$trajectories) <- filter$groups
  }
  ## save the packer and data for downstream use
  samples$packer <- packer
  samples$data <- data
  samples
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


thin_samples <- function(samples, control) {
  burnin <- control$pmcmc$n_burnin
  thinning_factor <- control$pmcmc$thinning_factor
  
  traj_names <- dimnames(samples$observations$trajectories)
  
  samples <- monty::monty_samples_thin(samples, thinning_factor, burnin)
  dimnames(samples$observations$trajectories) <- traj_names
  
  
  samples$pars <- array_flatten(samples$pars, c(2, 3))
  
  samples$density <- c(samples$density)
  
  if (region == "all") {
    samples$observations$trajectories <- 
      array_flatten(samples$observations$trajectories, c(4, 5))
  } else {
    samples$observations$trajectories <- 
      array_flatten(samples$observations$trajectories, c(3, 4))
  }
  
  samples
}


array_flatten <- function(x, i) {
  dx <- dim(x)
  if (any(i < 1 | i > length(dx))) {
    stop(sprintf("Values of 'i' must be in [1, %d]", length(dx)))
  }
  if (length(i) < 2) {
    stop("i must be vector of at least length 2")
  }
  if (any(diff(i) != 1)) {
    stop("All values of 'i' must be consecutive integers")
  }
  dx[[i[[1L]]]] <- prod(dx[i])
  dx_new <- dx[seq_along(dx)[-i[-1]]]
  if (length(dx_new) == 1L) {
    dx_new <- NULL
  }
  nms_x <- dimnames(x)
  if (!is.null(nms_x)) {
    nms_x[i[1]] <- list(NULL)
    nms_x[i[-1]] <- NULL
  }
  dim(x) <- dx_new
  dimnames(x) <- nms_x
  x
}
