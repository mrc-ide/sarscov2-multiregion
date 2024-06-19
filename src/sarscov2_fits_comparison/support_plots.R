plot_compare_intrinsic_severity <- function(dat_single, dat_multi, when) {
  
  get_sev <- function(dat, fit_type) {
    sev <- dat$intrinsic_severity %>%
      filter(region %in% sircovid::regions("england")) %>%
      pivot_wider(names_from = name, values_from = value) %>%
      filter(period == when) %>%
      mutate(source = paste0("Basic ", source)) %>%
      select(!period)
    
    # r0 <- dplyr::bind_rows(
    #   lapply(sircovid::regions("england"), function (r) get_r0_region(dat, r))) %>%
    #   mutate(source = "R0") %>%
    #   rename(estimate = value)
    # 
    # dplyr::bind_rows(sev, r0) %>%
    #   mutate(fit = fit_type)
    
    sev %>%
      mutate(fit = fit_type)
  }
  
  variant_names <- c("Wildtype", "Alpha", "Delta", "Omicron")
  source_names <- c("Basic IHR", "Basic HFR", "Basic IFR")#, "R0")
  fit_names <- c("Single regions fit", "Multi-region fit")
  
  df <- dplyr::bind_rows(get_sev(dat_single, "Single regions fit"),
                         get_sev(dat_multi, "Multi-region fit")) %>%
    mutate(region = case_when(
      region == "east_of_england" ~ "EE",
      region == "london" ~ "LON",
      region == "midlands" ~ "MID",
      region == "north_east_and_yorkshire" ~ "NEY",
      region == "north_west" ~ "NW",
      region == "south_east" ~ "SE",
      region == "south_west" ~ "SW")) %>%
    pivot_longer(!c(region, estimate, source, fit), names_to = "variant") %>%
    pivot_wider(names_from = estimate, values_from = value) %>%
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
    scale_y_continuous(labels = scales::percent) +
    facet_grid(rows = vars(source), cols = vars(fit), scales = "free_y") +
    theme(strip.text = element_text(size = 20),
          legend.text = element_text(size = 20),
          legend.title = element_text(size = 20),
          legend.key.size = unit(1, 'cm'),
          legend.position = "bottom",
          axis.text.y = element_text(size = 13))
  
  out
}
