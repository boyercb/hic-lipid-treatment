country_plot <- function(data, num, country) {
  col <- better_colors[num]
  
  ggplot(data,
         aes(
           x = mid_year,
           y = estimate,
           ymin = lwr,
           ymax = upr
         )) +
    facet_rep_grid(name ~ sex + age, scales = "free_y") +
    geom_pointrange(size = 0.2, color = col) +
    scale_x_continuous(breaks=scales::pretty_breaks(n = 4), n.breaks = 4) +
    theme_tufte(base_size = 8) +
    theme(
      axis.line = element_line(),
      legend.position = "none"
    ) +
    labs(
      x = "",
      y = ""
    ) 
}

c_by_c_vars <- c(
  "nonhdl",
  "nonhdl_low",
  "nonhdl_upp",
  "eligible",
  "eligible_low",
  "eligible_upp",
  "treated",
  "treated_low",
  "treated_upp"
)

c_by_c <-
  results %>%
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
      levels = c("nonhdl", "eligible", "treated"),
      labels = c("Non-HDL-C", "Eligible", "Treated")
    )
  ) %>%
  pivot_wider(names_from = type, values_from = value) %>%
  mutate(num = as.numeric(factor(Country))) %>%
  nest_by(Country, num) %>%
  mutate(p = list(country_plot(data, num, Country)))

map2(
  c_by_c$Country,
  c_by_c$p,
  function(country, p) {
    ggsave(
      plot = p,
      filename = paste0("3_figures/countries/fig_", tolower(country), ".pdf"),
      device = "pdf",
      width = 10,
      height = 6
    )
  }
)
