g <- ggplot(hic, aes(x = ldl_cleaned, y = nonhdl)) +
  geom_hex(bins = 200, na.rm = TRUE) +
  scale_fill_distiller() +
  # scale_color_distiller() +
  geom_smooth(
    mapping = aes(color = "gam"),
    method = "gam",
    se = FALSE
  ) +
  geom_smooth(
    mapping = aes(color = "linear"),
    method = "lm",
    se = FALSE
  ) +
  scale_color_manual(
    name = "",
    values = c("pink", "red"),
    aesthetics = c("color", "color")
  ) +
  annotate(
    "text",
    x = 7,
    y = 16,
    label = lm_eqn(hic),
    parse = TRUE,
    family =  "serif",
    size = 5
  ) +
  #scale_fill_viridis_c(option = "D") +
  theme_tufte() +
  theme(
    axis.line = element_line(),
    legend.position = c(0.9, 0.25)
  ) +
  labs(
    x = "\nLDL-C (mmol/L)",
    y = "Non-HDL-C (mmol/L)\n"
  )


ggsave(
  plot = g,
  filename = "3_figures/figS1_calibration.pdf",
  device = "pdf",
  width = 7.5,
  height = 7.5
)
