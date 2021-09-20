
# colourCount <- length(unique(results$Country)) # number of levels
# getPalette <- colorRampPalette(RColorBrewer::brewer.pal(9, "Set1"))

g1 <-
  ggplot(results,
         aes(
           x = mid_year,
           y = nonhdl,
           ymin = nonhdl_low,
           ymax = nonhdl_upp,
           color = Country
         )) +
    facet_rep_grid(age ~ sex) +
    geom_pointrange(size = 0.2) +
    scale_color_brewer(name = "", palette = "Set3") +
    theme_tufte(base_size = 12) +
    theme(
      axis.line = element_line(),
      legend.position = "bottom"
    ) +
    labs(
      x = "",
      y = "Non-HDL Cholesterol (mmol/L)\n"
    ) 

ggsave(
  plot = g1,
  filename = "3_figures/fig1_mean_chol.pdf",
  device = "pdf",
  width = 7.5,
  height = 10
)


# could do 15 year increments.

g2 <- 
  ggplot(
    results,
    aes(
      x = mid_year,
      y = elevated,
      ymin = elevated_low,
      ymax = elevated_upp,
      color = Country
    )
  ) +
    facet_rep_grid(age ~ sex) +
    geom_pointrange(size = 0.2) +
    scale_color_brewer(name = "", palette = "Set3") +
    theme_tufte(base_size = 12) +
    theme(
      axis.line = element_line(),
      legend.position = "bottom"
    ) +
    labs(
      x = "",
      y = "Prevalence of elevated cholesterol (%) \n"
    ) 

ggsave(
  plot = g2,
  filename = "3_figures/fig2_elevated.pdf",
  device = "pdf",
  width = 7.5,
  height = 10
)

g3 <- 
  ggplot(
    results,
    aes(
      x = mid_year,
      y = treated,
      ymin = treated_low,
      ymax = treated_upp,
      color = Country
    )
  ) +
    facet_rep_grid(age ~ sex) +
    geom_pointrange(size = 0.2) +
    scale_color_brewer(name = "", palette = "Set3") +
    theme_tufte(base_size = 12) +
    theme(
      axis.line = element_line(),
      legend.position = "bottom"
    ) +
    labs(
      x = "",
      y = "Proportion of those with elevated\nwho are treated (%)\n"
    ) 

ggsave(
  plot = g3,
  filename = "3_figures/fig3_treated.pdf",
  device = "pdf",
  width = 7.5,
  height = 10
)