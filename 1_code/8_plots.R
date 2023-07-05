
better_colors <- c(
'United States of America' = '#e6194B',
'United Kingdom' = '#3cb44b',
'Spain' = '#ffe119',
'South Korea' = '#4363d8',
'Slovakia' = '#f58231',
'Poland' = '#911eb4',
'Malta' = '#42d4f4',
'Italy' = '#f032e6',
'Ireland' = '#bfef45',
'Finland' = '#fabed4',
'Czech Republic' = '#469990',
'Chile' = '#dcbeff',
'Belgium' = '#9A6324',
'Australia' = '#fffac8'
# '#800000',
# '#aaffc3',
# '#808000',
# '#ffd8b1',
# '#000075',
# '#a9a9a9',
# '#ffffff',
# '#000000'
)


plot_trends <- function(
  variable, 
  label, 
  units = "percent", 
  breaks = c(0, 0.2, 0.4, 0.6, 0.8),
  limits = c(0, 0.90),
  exclude = c("POL_2009_PolSenior", "GBR_2005_HSE"),
  color_scheme
) {

variable_low <- paste0(variable, "_low")
variable_upp <- paste0(variable, "_upp")

# national plot
p1 <- ggplot(
  filter(national_results, !id_study %in% exclude),
  aes_string(
    x = "mid_year",
    y = variable,
    ymin = variable_low,
    ymax = variable_upp,
    color = "Country",
    fill = "Country",
    group = "Country"
  )
) +
  # geom_ribbon(alpha = 0.15, color = NA) +
  # geom_vline(xintercept = 2004, linetype = "dashed") +
  annotate("rect", 
           xmin = 2005.5, 
           xmax = 2020, 
           ymin = 0, 
           ymax = 0.95,
           fill = if (variable %in% c("treated", "controlled")) {
             "grey70" 
           } else {
             NULL
           },
           alpha = .2) +
  geom_line() +
  geom_linerange(alpha = 0.4) +
  geom_point() +
  scale_color_manual(name = "", values = color_scheme) +
  scale_fill_manual(name = "", values = color_scheme) +
  scale_y_continuous(
    labels = if (units == "percent") {
      scales::label_percent(accuracy = 1) 
    } else {
      scales::label_number_auto()
    }, 
    breaks = breaks,
    limits = limits
  ) +
  theme_tufte(base_size = 12) +
  theme(
    axis.line = element_line(),
    legend.position = "bottom"
  ) +
  labs(
    x = "",
    y = label
  ) 
  
if (variable %in% c("treated")) {
  p1 <- p1 + 
    annotate("text",
                x = 2013.25,
                y = 0.1,
                label = "late adopters", family = "Palatino") +
    annotate("text",
             x = 1997.75,
             y = 0.8,
             label = "early adopters", family = "Palatino")
}
  

# sex and age-stratified plot
p2 <- ggplot(
  filter(age2_sex_results, !id_study %in% exclude),
  aes_string(
    x = "mid_year",
    y = variable,
    ymin = variable_low,
    ymax = variable_upp,
    color = "Country",
    fill = "Country"
  )
) +
  facet_rep_wrap(sex~age2, repeat.tick.labels = TRUE) +
  #geom_pointrange(size = 0.2) +
  # geom_ribbon(alpha = 0.15, color = NA, show.legend = FALSE) +
  annotate("rect", 
           xmin = 2005.5, 
           xmax = 2020, 
           ymin = 0, 
           ymax = 0.95,
           fill = if (variable %in% c("treated", "controlled")) {
             "grey70" 
            } else {
              NULL
            },
           alpha = .2) +
  geom_line(show.legend = F) +
  geom_linerange(show.legend = F, alpha = 0.4) +
  geom_point(size = 0.9, show.legend = FALSE) +
  scale_color_manual(name = "", values = color_scheme) +
  scale_fill_manual(name = "", values = color_scheme) +
  scale_y_continuous(
    labels = if (units == "percent") {
      scales::percent_format(accuracy = 1) 
    } else {
      scales::label_number_auto()
    }, 
    breaks = breaks,
    limits = limits
  ) +
  theme_tufte(base_size = 12) +
  theme(
    axis.line = element_line(),
    legend.position = "none"
  ) +
  labs(
    x = "",
    y = NULL
  ) 

g3 <- 
  p1 + p2 + 
  plot_layout(guides = "collect") & 
  theme(legend.position = 'bottom')

g3
}

national_results <-
national_results %>%
mutate(
  eligible = replace(eligible, id_study == "FIN_2008_Athletes", 0.431),
  eligible = replace(eligible, id_study == "FIN_2011_YFS_rural", 0.316),
  eligible = replace(eligible, id_study == "FIN_2011_YFS_urban", 0.316),
  eligible_low = replace(eligible_low, id_study == "FIN_2008_Athletes", 0.37),
  eligible_low = replace(eligible_low, id_study == "FIN_2011_YFS_rural", 0.28),
  eligible_low = replace(eligible_low, id_study == "FIN_2011_YFS_urban", 0.28),
  eligible_upp = replace(eligible_upp, id_study == "FIN_2008_Athletes", 0.48),
  eligible_upp = replace(eligible_upp, id_study == "FIN_2011_YFS_rural", 0.35),
  eligible_upp = replace(eligible_upp, id_study == "FIN_2011_YFS_urban", 0.35),
  treated = replace(treated, id_study == "FIN_2008_Athletes", 0.33),
  treated = replace(treated, id_study == "FIN_2011_YFS_rural", 0.42),
  treated = replace(treated, id_study == "FIN_2011_YFS_urban", 0.42),
  treated_low = replace(treated_low, id_study == "FIN_2008_Athletes", 0.28),
  treated_low = replace(treated_low, id_study == "FIN_2011_YFS_rural", 0.35),
  treated_low = replace(treated_low, id_study == "FIN_2011_YFS_urban", 0.49),
  treated_upp = replace(treated_upp, id_study == "FIN_2008_Athletes", 0.38),
  treated_upp = replace(treated_upp, id_study == "FIN_2011_YFS_rural", 0.35),
  treated_upp = replace(treated_upp, id_study == "FIN_2011_YFS_urban", 0.49),
  controlled = replace(controlled, id_study == "FIN_2008_Athletes", 0.18),
  controlled = replace(controlled, id_study == "FIN_2011_YFS_rural", 0.23),
  controlled = replace(controlled, id_study == "FIN_2011_YFS_urban", 0.23),
  controlled_low = replace(controlled_low, id_study == "FIN_2008_Athletes", 0.15),
  controlled_low = replace(controlled_low, id_study == "FIN_2011_YFS_rural", 0.18),
  controlled_low = replace(controlled_low, id_study == "FIN_2011_YFS_urban", 0.18),
  controlled_upp = replace(controlled_upp, id_study == "FIN_2008_Athletes", 0.21),
  controlled_upp = replace(controlled_upp, id_study == "FIN_2011_YFS_rural", 0.28),
  controlled_upp = replace(controlled_upp, id_study == "FIN_2011_YFS_urban", 0.28),
  risk_score = replace(risk_score, id_study == "FIN_2008_Athletes", 0.11),
  risk_score = replace(risk_score, id_study == "FIN_2011_YFS_rural", 0.09),
  risk_score = replace(risk_score, id_study == "FIN_2011_YFS_urban", 0.09),
  risk_score_low = replace(risk_score_low, id_study == "FIN_2008_Athletes", 0.09),
  risk_score_low = replace(risk_score_low, id_study == "FIN_2011_YFS_rural", 0.06),
  risk_score_low = replace(risk_score_low, id_study == "FIN_2011_YFS_urban", 0.06),
  risk_score_upp = replace(risk_score_upp, id_study == "FIN_2008_Athletes", 0.13),
  risk_score_upp = replace(risk_score_upp, id_study == "FIN_2011_YFS_rural", 0.12),
  risk_score_upp = replace(risk_score_upp, id_study == "FIN_2011_YFS_urban", 0.12),
  smoker = replace(smoker, id_study == "FIN_2008_Athletes", 0.32),
  smoker = replace(smoker, id_study == "FIN_2011_YFS_rural", 0.28),
  smoker = replace(smoker, id_study == "FIN_2011_YFS_urban", 0.28),
  smoker_low = replace(smoker_low, id_study == "FIN_2008_Athletes", 0.28),
  smoker_low = replace(smoker_low, id_study == "FIN_2011_YFS_rural", 0.22),
  smoker_low = replace(smoker_low, id_study == "FIN_2011_YFS_urban", 0.22),
  smoker_upp = replace(smoker_upp, id_study == "FIN_2008_Athletes", 0.36),
  smoker_upp = replace(smoker_upp, id_study == "FIN_2011_YFS_rural", 0.32),
  smoker_upp = replace(smoker_upp, id_study == "FIN_2011_YFS_urban", 0.32),
  diab = replace(diab, id_study == "FIN_2008_Athletes", 0.13),
  diab = replace(diab, id_study == "FIN_2011_YFS_rural", 0.15),
  diab = replace(diab, id_study == "FIN_2011_YFS_urban", 0.15),
  diab_low = replace(diab_low, id_study == "FIN_2008_Athletes", 0.10),
  diab_low = replace(diab_low, id_study == "FIN_2011_YFS_rural", 0.11),
  diab_low = replace(diab_low, id_study == "FIN_2011_YFS_urban", 0.11),
  diab_upp = replace(diab_upp, id_study == "FIN_2008_Athletes", 0.16),
  diab_upp = replace(diab_upp, id_study == "FIN_2011_YFS_rural", 0.19),
  diab_upp = replace(diab_upp, id_study == "FIN_2011_YFS_urban", 0.19),
  treated_pop = replace(treated_pop, id_study == "FIN_2008_Athletes", 0.03),
  treated_pop = replace(treated_pop, id_study == "FIN_2011_YFS_rural", 0.05),
  treated_pop = replace(treated_pop, id_study == "FIN_2011_YFS_urban", 0.05),
  treated_pop_low = replace(treated_pop_low, id_study == "FIN_2008_Athletes", 0.01),
  treated_pop_low = replace(treated_pop_low, id_study == "FIN_2011_YFS_rural", 0.03),
  treated_pop_low = replace(treated_pop_low, id_study == "FIN_2011_YFS_urban", 0.03),
  treated_pop_upp = replace(treated_pop_upp, id_study == "FIN_2008_Athletes", 0.05),
  treated_pop_upp = replace(treated_pop_upp, id_study == "FIN_2011_YFS_rural", 0.07),
  treated_pop_upp = replace(treated_pop_upp, id_study == "FIN_2011_YFS_urban", 0.07),
)


plts <- 
pmap(
  list(
    variable = list(
      "eligible",
      "treated",
      "controlled",
      "untreated_severe",
      "untreated_high_risk",
      "treated_pop",
      "controlled_pop",
      "controlled_treated",
      "diab",
      "smoker",
      "risk_score",
      "nonhdl",
      "nonhdl_untreated",
      "hdl",
      "tc",
      "ldl"
    ),
    label = list(
      "Proportion with elevated serum cholesterol (%)\n",
      "Proportion eligible who are treated (%)\n",
      "Proportion eligible with controlled non-HDL-C levels (%)\n",
      "Proportion eligible with\nsevere untreated hypercholesterolemia (%)\n",
      "Proportion eligible who\n are high risk and untreated (%)\n",
      "Proportion treated, population-wide (%)\n",
      "Proportion controlled, population-wide (%)\n",
      "Proportion controlled, among treated (%)\n",
      "Prevalence of diabetes (%)\n",
      "Prevalence of smoking (%)\n",
      "Risk score\n",
      "Non-HDL-C levels (mmol/L)",
      "Untreated non-HDL-C levels (mmol/L)",
      "HDL-C levels (mmol/L)",
      "TC-C levels (mmol/L)",
      "LDL-C levels (mmol/L)"
    ),
    units = list(
      "percent",
      "percent",
      "percent",
      "percent",
      "percent",
      "percent",
      "percent",
      "percent",
      "percent",
      "percent",
      "percent",
      "mmol/l",
      "mmol/l",
      "mmol/l",
      "mmol/l",
      "mmol/l"
    ),
    breaks = list(
      c(0, 0.2, 0.4, 0.6, 0.8),
      c(0, 0.2, 0.4, 0.6, 0.8),
      c(0, 0.2, 0.4, 0.6, 0.8),
      c(0, 0.25, 0.5),
      c(0, 0.25, 0.5),
      c(0, 0.2, 0.4, 0.6),
      c(0, 0.2, 0.4, 0.6),
      c(0, 0.2, 0.4, 0.6, 0.8),
      c(0, 0.1, 0.2, 0.3),
      c(0, 0.25, 0.5),
      c(0, 0.1, 0.2, 0.3),
      c(3, 4, 5),
      c(3, 4, 5),
      c(1, 1.5, 2),
      c(4, 5, 6),
      c(2.5, 3, 3.5, 4)
    ),
    limits = list(
      c(0, 0.90),
      c(0, 0.95),
      c(0, 0.95),
      c(0, 0.60),
      c(0, 0.60),
      c(0, 0.70),
      c(0, 0.70),
      c(0, 1),
      c(0, 0.40),
      c(0, 0.60),
      c(0, 0.40),
      c(2.5, 5.5),
      c(2.5, 5.5),
      c(0.5, 2.5),
      c(3.5, 6.5),
      c(2, 4.5)
    ), 
    exclude = list(
      c(no_rf_data, small_samples,"POL_2009_PolSenior", "GBR_2005_HSE"),
      c(no_rf_data, small_samples,"POL_2009_PolSenior", "GBR_2005_HSE"),
      c(no_rf_data, small_samples,"POL_2009_PolSenior", "GBR_2005_HSE"),
      c(no_rf_data, small_samples,"POL_2009_PolSenior", "GBR_2005_HSE"),
      c(no_rf_data, small_samples,"POL_2009_PolSenior", "GBR_2005_HSE"),
      c("POL_2009_PolSenior", "GBR_2005_HSE"),
      c("POL_2009_PolSenior", "GBR_2005_HSE"),
      c(no_rf_data, small_samples, "POL_2009_PolSenior", "GBR_2005_HSE"),
      c(no_diab_data,"POL_2009_PolSenior", "GBR_2005_HSE"),
      c(no_smoking_data,"POL_2009_PolSenior", "GBR_2005_HSE"),
      c(no_rf_data, small_samples,"POL_2009_PolSenior", "GBR_2005_HSE"),
      c(small_samples,"POL_2009_PolSenior", "GBR_2005_HSE"),
      c(small_samples,"POL_2009_PolSenior", "GBR_2005_HSE"),
      c(small_samples,"POL_2009_PolSenior", "GBR_2005_HSE"),
      c(small_samples,"POL_2009_PolSenior", "GBR_2005_HSE"),
      c(no_ldl_data, small_samples, "POL_2009_PolSenior", "GBR_2005_HSE")
    )
  ),
  plot_trends,
  color_scheme = better_colors
)

walk2(plts, 
    list(
      "3_figures/eligible.pdf",
      "3_figures/treated.pdf",
      "3_figures/controlled.pdf",
      "3_figures/severe.pdf",
      "3_figures/high_risk.pdf",
      "3_figures/treated_pop.pdf",
      "3_figures/controlled_pop.pdf",
      "3_figures/controlled_treated.pdf",
      "3_figures/diab.pdf",
      "3_figures/smoke.pdf",
      "3_figures/risk.pdf",
      "3_figures/nonhdl.pdf",
      "3_figures/nonhdl_untreated.pdf",
      "3_figures/hdl.pdf",
      "3_figures/tc.pdf",
      "3_figures/ldl.pdf"
    ),
    function(x, y) {
      ggsave(
        plot = x,
        filename = y,
        device = "pdf",
        width = 9,
        height = 5.5
      )
    })


most_recent_survey <- 
  age_sex_results %>%
  group_by(Country, sex, age) %>%
  slice(n()) %>%
  ungroup() 
  
  
  
  
p <- 
  most_recent_survey %>%
  mutate(
    label = paste0(Country, " (", mid_year, ")"),
  ) %>%
  rename(
    treated_value = treated,
    controlled_value = controlled,
    severe_value = untreated_severe
  ) %>%
  select(Country,
         sex,
         age,
         label,
         treated_value,
         controlled_value, severe_value) %>%
  pivot_longer(
    -c(Country, sex, age, label),
    names_to = c("name", ".value"),
    names_pattern = "(.*)_(.*)"
  ) %>%
  mutate(
    name = factor(name, 
                  levels = c("treated", "controlled", "severe"),
                  labels = c(
                    "Treated (%)",
                    "Controlled (%)",
                    "Severe untreated\nhypercholesterolemia (%)"
                  ))
  ) %>%
  ggplot(., aes(x = age, y = value)) +
  geom_jitter(aes(color = Country), width = 0.2) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 5)) +
  facet_rep_grid(sex ~ name, repeat.tick.labels = T) +
  theme_tufte(base_size = 14) +
  scale_color_manual(name = "", values = better_colors) +
  labs(
    y = NULL,
    x = NULL
  ) +
  theme(
    axis.line = element_line(),
    legend.position = "bottom"
  ) 

ggsave(
  plot = p,
  filename = "3_figures/age_gaps.pdf",
  device = "pdf",
  width = 9,
  height = 5.5
)


p1 <- sex_results %>%
  group_by(Country, sex) %>%
  slice(n()) %>%
  ungroup() %>%
  mutate(
    label = paste0(Country, " (", mid_year, ")"),
    label = fct_reorder2(factor(label), sex, -treated)
  ) %>%
  select(Country, sex, label,
         eligible,
         eligible_low,
         eligible_upp) %>%
  rename(
    eligible_value = eligible
  ) %>%
  ggplot(., aes(x = eligible_value, y = label)) +
  geom_bar(stat = 'identity', fill = "#3182BD") +
  facet_wrap(~fct_rev(sex)) +
  theme_tufte(base_size = 14) +
  geom_text(aes(label = paste0(round(eligible_value * 100), "%")),
            position = position_stack(vjust = 0.5),
            family = "Palatino"
  ) + 
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    y = NULL,
    x = NULL
  ) +
  theme(legend.position = 'bottom')

p2 <- sex_results %>%
  group_by(Country, sex) %>%
  slice(n()) %>%
  ungroup() %>%
  mutate(
    label = paste0(Country, " (", mid_year, ")"),
    label = fct_reorder2(factor(label), sex, -treated),
    treated = treated - controlled
  ) %>%
  select(Country, sex, label,
         treated,
         treated_low,
         treated_upp,
         controlled,
         controlled_low,
         controlled_upp) %>%
  rename(
    treated_value = treated,
    controlled_value = controlled
  ) %>%
  pivot_longer(
    -c(Country, sex, label),
    names_to = c("name", ".value"),
    names_pattern = "(.*)_(.*)"
  ) %>%
  mutate(
    name = factor(name, 
                  levels = c("treated", "controlled"),
                  labels = c("uncontrolled", "controlled"))
  ) %>%
  ggplot(., aes(x = value, y = label, fill = name)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~fct_rev(sex)) +
  theme_tufte(base_size = 14) +
  geom_text(aes(label = paste0(round(value * 100), "%")),
            position = position_stack(vjust = 0.5),
            family = "Palatino"
  ) + 
  scale_fill_brewer(name = "", palette = "Blues") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    y = NULL,
    x = NULL
  ) +
  theme(legend.position = 'bottom')

ggsave(
  plot = p1 / p2,
  filename = "3_figures/recent.pdf",
  device = "pdf",
  width = 7,
  height = 9
)


