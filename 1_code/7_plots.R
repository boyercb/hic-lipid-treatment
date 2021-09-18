
colourCount <- length(unique(results$Country)) # number of levels
getPalette <- colorRampPalette(RColorBrewer::brewer.pal(9, "Set1"))

ggplot(results, aes(x = mid_year, y = nonhdl, ymin = nonhdl_low, ymax = nonhdl_upp, color = Country)) +
  lemon::facet_rep_grid(age ~ sex) +
  geom_pointrange() +
  scale_color_brewer(name = "", palette = "Set3") +
  theme_tufte(base_size = 16) +
  theme(
    axis.line = element_line(),
    legend.position = "bottom"
  ) +
  labs(
    x = "",
    y = "Non-HDL Cholesterol (mmol/L)\n"
  ) 

ggplot(results, aes(x = mid_year, y = nonhdl, ymin = nonhdl_low, ymax = nonhdl_upp, color = Country)) +
  lemon::facet_rep_grid(age ~ sex) +
  geom_smooth(se = FALSE, method = "lm") +
  scale_color_brewer(name = "", palette = "Set3") +
  theme_tufte(base_size = 16) +
  theme(
    axis.line = element_line(),
    legend.position = "bottom"
  ) +
  labs(
    x = "",
    y = "Non-HDL Cholesterol (mmol/L)\n"
  ) 
# could do 15 year increments.

ggplot(results, aes(x = mid_year, y = dyslipidemia, ymin = dyslipidemia_low, ymax = dyslipidemia_upp, color = Country)) +
  lemon::facet_rep_grid(age ~ sex) +
  geom_pointrange() +
  scale_color_brewer(name = "", palette = "Set3") +
  theme_tufte(base_size = 16) +
  theme(
    axis.line = element_line(),
    legend.position = "bottom"
  ) +
  labs(
    x = "",
    y = "Prevalence of elevated cholesterol (%) \n"
  ) 

ggplot(results, aes(x = mid_year, y = treated, ymin = treated_low, ymax = treated_upp, color = Country)) +
  facet_grid(age ~ sex) +
  geom_pointrange() +
  scale_color_brewer(name = "", palette = "Set3") +
  theme_tufte(base_size = 16) +
  theme(
    axis.line = element_line(),
    legend.position = "bottom"
  ) +
  labs(
    x = "",
    y = "Proportion of those with dyslipidemia\nwho are treated (%)\n"
  ) 