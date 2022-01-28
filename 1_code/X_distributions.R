pal <- colorRampPalette(RColorBrewer::brewer.pal(9, "RdYlBu"))

g <- ggplot(filter(hic, Country == "United States of America"), aes(fill = factor(mid_year), x = nonhdl)) +
  facet_grid(age ~ sex) +
  #geom_density(alpha = 0.4, stat = KernSmooth::bkde) +
  ggalt::geom_bkde(alpha = 0.4) +
  theme_tufte(base_size = 12) +
  xlim(c(0, 12)) +
  scale_fill_manual(name = "", values = pal(12)) +
  labs(
    x = "Non-HDL-C (mmol/L)",
    y = "density"
  ) +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
  theme(
    axis.line = element_line(),
    legend.position = "bottom"
  ) 

ggsave(
  plot = g,
  filename = "3_figures/figS4_densities.pdf",
  device = "pdf",
  width = 10,
  height = 7
)

# ggplot(filter(hic, Country == "United States of America"), aes(fill = factor(mid_year), x = nonhdl)) +
#   facet_grid(age ~ fct_cross(sex, factor(treated))) +
#   #geom_density(alpha = 0.4, stat = KernSmooth::bkde) +
#   ggalt::geom_bkde(alpha = 0.4) +
#   theme_tufte(base_size = 12) +
#   xlim(c(0, 12)) +
#   scale_fill_manual(name = "", values = pal(12)) +
#   labs(
#     x = "Non-HDL-C (mmol/L)",
#     y = "density"
#   ) +
#   guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
#   theme(
#     axis.line = element_line(),
#     legend.position = "bottom"
#   ) 
