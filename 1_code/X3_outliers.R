pp <- wrap_plots(pairs_plots, guides = "collect")

ggsave(
  plot = pp,
  filename = "3_figures/figS2_maha_outliers.pdf",
  device = "pdf",
  width = 10,
  height = 7.5
)

