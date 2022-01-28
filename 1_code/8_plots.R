results <- filter(
  results, !id_study %in% c("GBR_2010_NDNS", "GBR_2014_NDNS", "GBR_2016_NDNS", "GBR_2017_NDNS")
)
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
      y = eligible,
      ymin = eligible_low,
      ymax = eligible_upp,
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
    scale_color_brewer(name = "", palette = "Set3") +
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
  scale_color_brewer(name = "", palette = "Set3") +
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
  scale_color_brewer(name = "", palette = "Set3") +
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
  scale_color_brewer(name = "", palette = "Set3") +
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
  scale_color_brewer(name = "", palette = "Set3") +
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
