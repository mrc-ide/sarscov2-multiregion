## Need some utilities here; these functions likely headed to spimalot
## at some point:
assert_has_names <- spimalot:::assert_has_names
assert_unique <- spimalot:::assert_unique
assert_setequal <- spimalot:::assert_setequal


multiregion_info <- function(info, fixed_pars) {
  regions <- setdiff(unique(info$region), NA)
  assert_has_names(info, c("region", "include",
                           "name", "initial", "max", "integer"))

  info <- info[info$include, ]
  rownames(info) <- NULL
  
  full_pars <- unique(info$name)
  current_fixed <- info$name[is.na(info$region)]
  
  ## More work to do for fixed things:
  f <- function(x) {
    name <- x$name[[1]]
    
    for (v in c("min", "max", "integer")) {
      if (!all(x[[v]] == x[[v]][[1]])) {
        stop(sprintf("Inconsistent '%s' across regions for '%s'",
                     v, name))
      }
    }
    is_current_fixed <- name %in% current_fixed
    is_new_fixed <- name %in% fixed_pars
    if (is_current_fixed != is_new_fixed) {
      if (is_new_fixed) {
        x$region <- NA
        x$initial <- mean(x$initial)
        
        x <- x[1, ]
      } else {
        x <- x[rep(1, length(regions)), ]
        x$region <- regions
      }
    } 
    x
  }
  ret <- do.call(rbind, lapply(split(info, info$name), f))
  rownames(ret) <- NULL

  ret
}

multiregion_prior <- function(info, prior) {
  prior_cols <- c("region", "type", "name", "gamma_scale", "gamma_shape",
                  "beta_shape1", "beta_shape2")
  assert_has_names(prior, prior_cols)

  prior <- prior[prior$region %in% info$region, ]

  nms_varied <- unique(info$name[!is.na(info$region)])
  nms_fixed <- info$name[is.na(info$region)]
  assert_setequal(prior$name, c(nms_varied, nms_fixed), unique = FALSE)

  nms <- split(prior$name, prior$region)
  if (length(unique(nms)) != 1) {
    stop("Names differ across regions")
  }
  
  prior_varied <- prior[prior$name %in% nms_varied, ]

  ## Then for the fixed, more complicated:
  f <- function(x) {
    is_same <- function(x) {
      all(is.na(x)) || (!anyNA(x) && all(x == x[[1]]))
    }
    nm <- x$name[[1]]
    ok <- (nm %in% c("m_CHR", "m_CHW")) ||
      all(vapply(x[names(x) != "region"], is_same, TRUE))
    if (!ok) {
      stop(sprintf("prior disagreement for '%s'", nm))
    }
    x[1, ]
  }

  prior_fixed <- prior[prior$name %in% nms_fixed , ]
  prior_fixed <- dplyr::bind_rows(
    lapply(split(prior_fixed, prior_fixed$name), f))
  prior_fixed$region[] <- NA

  prior <- rbind(prior_fixed, prior_varied)
  rownames(prior) <- NULL

  prior
}


multiregion_proposal <- function(info, proposal) {
  
  nms_varied <- unique(info$name[!is.na(info$region)])
  nms_fixed <- info$name[is.na(info$region)]
  nms_all <- c(nms_fixed, nms_varied)
  assert_setequal(proposal$name, nms_all, unique = FALSE)
  assert_setequal(setdiff(names(proposal), c("name", "region")), nms_all)

  regions <- unique(info$region[!is.na(info$region)])
  
  current_fixed <- proposal$name[is.na(proposal$region)]
  new_proposal <- proposal
  for (i in nms_all) {
    if (i %in% nms_fixed) {
      if (!(i %in% current_fixed)) {
        ## Move from varied to fixed
        mean_prop <- mean(proposal[[i]][proposal$name == i])
        new_proposal <- new_proposal[new_proposal$name != i, ]
        new_proposal[[i]] <- 0
        i_prop <- as.data.frame(array(0, c(1, ncol(new_proposal))))
        names(i_prop) <- names(new_proposal)
        i_prop$region <- NA_character_
        i_prop$name <- i
        i_prop[[i]] <- mean_prop
        new_proposal <- rbind(i_prop, new_proposal)
      }
    } else {
      if (i %in% current_fixed) {
        ## Move from fixed to varied
        prop <- proposal[[i]][proposal$name == i]
        new_proposal <- new_proposal[new_proposal$name != i, ]
        new_proposal[[i]] <- 0
        i_prop <- 
          as.data.frame(array(0, c(length(regions), ncol(new_proposal))))
        names(i_prop) <- names(new_proposal)
        i_prop$region <- regions
        i_prop$name <- i
        i_prop[[i]] <- prop
        new_proposal <- rbind(i_prop, new_proposal)
      }
    }
  }

  new_proposal <- dplyr::arrange(new_proposal, !is.na(region), region, name)
  rownames(new_proposal) <- NULL
  new_proposal
}
