plot_compare_intrinsic_severity <- function(dat_single, dat_multi) {
  
  variant_names <- c("Wildtype", "Alpha", "Delta", "Omicron")
  source_names <- c("IHR", "HFR", "IFR", "R0")
  region_names <- sircovid::regions("england")
  fit_names <- c("Single regions fit", "Multiregion fit")
  
  get_sev <- function(dat, fit_type) {
    
    get_sev_what <- function(w) {
      
      get_sev_variant <- function(v) {
        x_r <- lapply(region_names, function(r) mean_ci(dat[[w]][[v]][[r]]))
        names(x_r) <- region_names
        dplyr::bind_rows(x_r, .id = "region")
      }
      
      x_v <- lapply(variant_names, get_sev_variant)
      names(x_v) <- variant_names
      dplyr::bind_rows(x_v, .id = "variant")
    }
    
    x <- lapply(source_names, get_sev_what)
    names(x) <- source_names
    x <- dplyr::bind_rows(x, .id = "source") %>%
      mutate(fit = fit_type)
  }
  
  
  
  df <- dplyr::bind_rows(get_sev(dat_single, "Single regions fit"),
                         get_sev(dat_multi, "Multiregion fit")) %>%
    mutate(region = case_when(
      region == "east_of_england" ~ "EE",
      region == "london" ~ "LON",
      region == "midlands" ~ "MID",
      region == "north_east_and_yorkshire" ~ "NEY",
      region == "north_west" ~ "NW",
      region == "south_east" ~ "SE",
      region == "south_west" ~ "SW")) %>%
    mutate(variant = factor(variant, levels = variant_names), 
           region = factor(region),
           source = factor(source, levels = source_names),
           fit = factor(fit, levels = fit_names)) 
  
  box_bounds <- df %>% group_by(variant, source, fit) %>% summarise(ymin = min(lb), ymax = max(ub)) %>%
    mutate(xmin = case_when(
      variant == "Wildtype" ~ 0.5,
      variant == "Alpha" ~ 1.5,
      variant == "Delta" ~ 2.5,
      variant == "Omicron" ~ 3.5)) %>%
    mutate(xmax = xmin + 1)
                       
  scale_y_IHR <- scale_y_continuous(labels = scales::percent_format(accuracy = 0.1))
  scale_y_HFR <- scale_y_continuous(labels = scales::percent_format(accuracy = 1))
  scale_y_IFR <- scale_y_continuous(labels = scales::percent_format(accuracy = 0.1))
  scale_y_R0 <- scale_y_continuous()
  
  out <- ggplot(df, aes(x = variant, y = mean, col = variant, fill = variant, group = region)) +
    geom_point(size = 3, shape = 18, position = position_dodge(width = 0.5)) +
    geom_linerange(aes(ymin = lb, ymax = ub), position = position_dodge(width = 0.5)) +
    ggrepel::geom_text_repel(aes(label = region), size = 4, show.legend = FALSE, position = position_dodge(width = 0.5)) +
    geom_rect(data = box_bounds, aes(x = NULL, y = NULL, group = NULL, xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), alpha = 0.1, lty = 3) +
    labs(y = "", x = "") +
    scale_color_manual(values = all_variant_colours) +
    scale_fill_manual(values = all_variant_colours) +
    scale_x_discrete(breaks = NULL) +
    facet_grid(rows = vars(source), cols = vars(fit), scales = "free_y")
  
  out
}


plot_compare_aggregate_severity <- function(dat_single, dat_multi) {
  dat_single_maxvar <- aggregate_maxvar(dat_single)
  
  variant_names <- c("Wildtype", "Alpha", "Delta", "Omicron")
  source_names <- c("IHR", "HFR", "IFR", "R0")
  fit_names <- c("Multiregion fit", "Single regions fit paired", 
                 "Single regions fit ranked")
  
  get_sev <- function(dat, fit_type) {
    
    get_sev_what <- function(w) {
      x_v <- lapply(variant_names, function (v) mean_ci(dat[[w]][[v]]$england))
      names(x_v) <- variant_names
      dplyr::bind_rows(x_v, .id = "variant")
    }
    
    x <- lapply(source_names, get_sev_what)
    names(x) <- source_names
    x <- dplyr::bind_rows(x, .id = "source") %>%
      mutate(fit = fit_type)
  }
  
  
  
  df <- dplyr::bind_rows(get_sev(dat_single, "Single regions fit paired"),
                         get_sev(dat_single_maxvar, "Single regions fit ranked"),
                         get_sev(dat_multi, "Multiregion fit")) %>%
    mutate(variant = factor(variant, levels = variant_names), 
           source = factor(source, levels = source_names),
           fit = factor(fit, levels = fit_names)) 
  
  scale_y_IHR <- scale_y_continuous(labels = scales::percent_format(accuracy = 0.1))
  scale_y_HFR <- scale_y_continuous(labels = scales::percent_format(accuracy = 1))
  scale_y_IFR <- scale_y_continuous(labels = scales::percent_format(accuracy = 0.1))
  scale_y_R0 <- scale_y_continuous()
  
  out <- ggplot(df, aes(x = variant, y = mean, col = variant, fill = variant)) +
    geom_point(size = 3, shape = 18, position = position_dodge(width = 0.5)) +
    geom_linerange(aes(ymin = lb, ymax = ub), position = position_dodge(width = 0.5)) +
    labs(y = "", x = "") +
    scale_color_manual(values = all_variant_colours) +
    scale_fill_manual(values = all_variant_colours) +
    scale_x_discrete(breaks = NULL) +
    facet_grid(rows = vars(source), cols = vars(fit), scales = "free_y")
  
  out
}


mean_ci <- function(x) {
  mean <- mean(x)
  lb <- quantile(x, 0.025)
  ub <- quantile(x, 0.975)
  
  out <- c(mean, lb, ub)
  names(out) <- c("mean", "lb", "ub")
  out
}
