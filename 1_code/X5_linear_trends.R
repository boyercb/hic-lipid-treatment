g <- 
  ggplot(results,
         aes(
           x = mid_year,
           y = nonhdl,
           ymin = nonhdl_low,
           ymax = nonhdl_upp,
           color = Country
         )) +
  facet_rep_grid(age ~ sex) +
  geom_smooth(se = FALSE, method = "lm") +
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
  plot = g,
  filename = "3_figures/figS_mean_chol_trends.pdf",
  device = "pdf",
  width = 7.5,
  height = 10
)



linear_results <- 
  results %>%
  ungroup() %>%
  select(id_study, Country, mid_year, data, srvy) %>%
  unnest(c(data)) %>%
  filter(mid_year > 1999) %>%
  nest_by(Country, sex, age) %>%
  mutate(
    linear_model = list(
      lm(nonhdl ~ mid_year, data = data)
    ),
    linear_coef = coef(linear_model)[2],
    linear_lwr = confint(linear_model)[2, 1],
    linear_upr = confint(linear_model)[2, 2],
    linear_stat = coef(linear_model)[2] / sqrt(vcov(linear_model)[2, 2]),
    linear_pval = 2 * pt(abs(linear_stat), linear_model$df.residual, lower.tail = FALSE)
  )


