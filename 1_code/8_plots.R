
# colourCount <- length(unique(results$Country)) # number of levels
# getPalette <- colorRampPalette(RColorBrewer::brewer.pal(9, "Set1"))

better_colors <- c(
  '#e6194B',
  '#3cb44b',
  '#ffe119',
  '#4363d8',
  '#f58231',
  '#911eb4',
  '#42d4f4',
  '#f032e6',
  '#bfef45',
  '#fabed4',
  '#469990',
  '#dcbeff',
  '#9A6324',
  '#fffac8',
  '#800000',
  '#aaffc3',
  '#808000',
  '#ffd8b1',
  '#000075',
  '#a9a9a9',
  '#ffffff',
  '#000000'
)

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
    scale_color_manual(name = "", values = better_colors) +
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
      y = eligible,
      ymin = eligible_low,
      ymax = eligible_upp,
      color = Country
    )
  ) +
    facet_rep_grid(age ~ sex) +
    geom_pointrange(size = 0.2) +
    scale_color_manual(name = "", values = better_colors) +
    theme_tufte(base_size = 12) +
    theme(
      axis.line = element_line(),
      legend.position = "bottom"
    ) +
    labs(
      x = "",
      y = "Proportion of population eligible for cholesterol treatment (%) \n"
    ) 

ggsave(
  plot = g2,
  filename = "3_figures/fig2_eligible.pdf",
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
    scale_color_manual(name = "", values = better_colors) +
    theme_tufte(base_size = 12) +
    theme(
      axis.line = element_line(),
      legend.position = "bottom"
    ) +
    labs(
      x = "",
      y = "Proportion of those eligible\nwho are treated (%)\n"
    ) 

ggsave(
  plot = g3,
  filename = "3_figures/fig3_treated.pdf",
  device = "pdf",
  width = 7.5,
  height = 10
)

g4 <- 
  ggplot(
    results,
    aes(
      x = mid_year,
      y = severe,
      ymin = severe_low,
      ymax = severe_upp,
      color = Country
    )
  ) +
  facet_rep_grid(age ~ sex) +
  geom_pointrange(size = 0.2) +
  scale_color_manual(name = "", values = better_colors) +
  theme_tufte(base_size = 12) +
  theme(
    axis.line = element_line(),
    legend.position = "bottom"
  ) +
  labs(
    x = "",
    y = "Proportion with uncontrolled dyslipidemia (%)\n"
  ) 

ggsave(
  plot = g4,
  filename = "3_figures/fig4_severe.pdf",
  device = "pdf",
  width = 7.5,
  height = 10
)


g5 <- 
  ggplot(
    results,
    aes(
      x = mid_year,
      y = uncontrolled,
      ymin = uncontrolled_low,
      ymax = uncontrolled_upp,
      color = Country
    )
  ) +
  facet_rep_grid(age ~ sex) +
  geom_pointrange(size = 0.2) +
  scale_color_manual(name = "", values = better_colors) +
  theme_tufte(base_size = 12) +
  theme(
    axis.line = element_line(),
    legend.position = "bottom"
  ) +
  labs(
    x = "",
    y = "Proportion with uncontrolled dyslipidemia,\n among those who are treated (%)\n"
  ) 

ggsave(
  plot = g5,
  filename = "3_figures/fig5_uncontrolled.pdf",
  device = "pdf",
  width = 7.5,
  height = 10
)



g6 <- 
  ggplot(
    results,
    aes(
      x = mid_year,
      y = statins,
      ymin = statins_low,
      ymax = statins_upp,
      color = Country
    )
  ) +
  facet_rep_grid(age ~ sex) +
  geom_pointrange(size = 0.2) +
  scale_color_manual(name = "", values = better_colors) +
  theme_tufte(base_size = 12) +
  theme(
    axis.line = element_line(),
    legend.position = "bottom"
  ) +
  labs(
    x = "",
    y = "Proportion on statin,\n among those who are treated (%)\n"
  ) 

ggsave(
  plot = g6,
  filename = "3_figures/fig6_statin.pdf",
  device = "pdf",
  width = 7.5,
  height = 10
)

g7 <- 
  ggplot(
    results,
    aes(
      x = mid_year,
      y = risk_score,
      ymin = risk_score_low,
      ymax = risk_score_upp,
      color = Country
    )
  ) +
  facet_rep_grid(age ~ sex) +
  geom_pointrange(size = 0.2) +
  scale_color_manual(name = "", values = better_colors) +
  theme_tufte(base_size = 12) +
  theme(
    axis.line = element_line(),
    legend.position = "bottom"
  ) +
  labs(
    x = "",
    y = "Mean risk score (Globorisk)\n"
  ) 

ggsave(
  plot = g7,
  filename = "3_figures/figS3_risk_score.pdf",
  device = "pdf",
  width = 7.5,
  height = 10
)
