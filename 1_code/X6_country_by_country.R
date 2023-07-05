country_plot <- function(data, num, country, lower_limit) {
  col <- better_colors[names(better_colors) == country]
  print(col)
  p <- ggplot(data,
         aes(
           x = mid_year,
           y = estimate,
           ymin = lwr,
           ymax = upr, 
           color = col,
           fill = col
         )) +
    facet_rep_grid(name ~ sex + age, scales = "free_y", switch = "y") +
    # geom_point() +
    # geom_line() +
    # geom_ribbon(alpha = 0.4) +
    geom_pointrange(size = 0.2, color = col) +
    scale_x_continuous(breaks=scales::breaks_extended(n = 4), n.breaks = 4, limits = c(lower_limit, 2020)) +
    # if (country == "Italy") {
    #   p + scale_x_continuous(breaks=c(2005, 2010, 2015, 2020), n.breaks = 4, limits = c(lower_limit, 2020)) 
    # } else {
    #   p + scale_x_continuous(breaks=scales::breaks_pretty(n = 4), n.breaks = 4, limits = c(lower_limit, 2020)) 
    # }

    # p + 
    scale_y_continuous(labels=scales::percent_format(accuracy = 1)) +
    theme_tufte(base_size = 12) +
    theme(
      axis.line = element_line(),
      legend.position = "none",
      strip.placement = "outside",
      axis.text.x = element_text(size = 8),
      axis.text.y = element_text(size = 8),
      panel.spacing = unit(0, "lines")
      # strip.switch.pad.grid = unit(1, "cm")
      # axis.title.y = element_text(vjust = -15)
    ) +
    labs(
      title = NULL,
      subtitle = NULL,
      caption = NULL,
      x = NULL,
      y = NULL
    ) 
}

c_by_c_vars <- c(
  "eligible",
  "eligible_low",
  "eligible_upp",
  "treated",
  "treated_low",
  "treated_upp",
  "controlled",
  "controlled_low",
  "controlled_upp"
)

c_by_c <-
  age_sex_results %>%
  ungroup() %>%
  pivot_longer(all_of(c_by_c_vars)) %>%
  mutate(
    type = case_when(
      str_detect(name, "_low") ~ "lwr",
      str_detect(name, "_upp") ~ "upr",
      TRUE ~ "estimate"
    ),
    name = str_replace(name, "(_low)|(_upp)", ""),
    name = factor(
      name, 
      levels = c("eligible", "treated", "controlled"),
      labels = c("Prevalence (%)", "Treated (%)", "Controlled (%)")
    )
  ) %>%
  pivot_wider(names_from = type, values_from = value) %>%
  mutate(num = as.numeric(factor(Country))) %>%
  group_by(Country) %>%
  mutate(lower = min(mid_year) - 2) %>%
  ungroup() %>%
  nest_by(Country, num, lower) %>%
  mutate(p = list(country_plot(data, num, Country, lower)))

c_by_c <- filter(c_by_c, !Country %in% c("Belgium", "Malta"))
map2(
  c_by_c$Country,
  c_by_c$p,
  function(country, p) {
    ggsave(
      plot = p,
      filename = paste0("3_figures/countries/fig_", tolower(country), ".pdf"),
      device = "pdf",
      width = 10,
      height = 6.5
    )
  }
)
