national_sensitivity <- 
  national_results %>%
  filter(!id_study %in% c(no_rf_data, small_samples)) |>
  mutate(
    eligible_ci = print_ci(eligible, eligible_low, eligible_upp),
    eligible_diab_ci = print_ci(
      est = eligible_diab, 
      lwr = eligible_diab_low, 
      upr = eligible_diab_upp
    ),
    eligible_aha_ci = print_ci(
      est = eligible_aha, 
      lwr = eligible_aha_low, 
      upr = eligible_aha_upp
      ),
    eligible_esc_ci = print_ci(
      est = eligible_esc, 
      lwr = eligible_esc_low, 
      upr = eligible_esc_upp
    ),
    treated_ci = print_ci(treated, treated_low, treated_upp),
    treated_diab_ci = print_ci(
      est = treated_diab, 
      lwr = treated_diab_low, 
      upr = treated_diab_upp
    ),
    treated_aha_ci = print_ci(
      est = treated_aha, 
      lwr = treated_aha_low, 
      upr = treated_aha_upp
    ),
    treated_esc_ci = print_ci(
      est = treated_esc, 
      lwr = treated_esc_low, 
      upr = treated_esc_upp
    ),
    controlled_ci = print_ci(controlled, controlled_low, controlled_upp),
    controlled_diab_ci = print_ci(
      est = controlled_diab, 
      lwr = controlled_diab_low, 
      upr = controlled_diab_upp
    ),
    controlled_aha_ci = print_ci(
      est = controlled_aha, 
      lwr = controlled_aha_low, 
      upr = controlled_aha_upp
    ),
    controlled_esc_ci = print_ci(
      est = controlled_esc, 
      lwr = controlled_esc_low, 
      upr = controlled_esc_upp
    )
  ) %>%
  ungroup() %>%
  arrange(Country, mid_year) %>%
  select(
    Country,
    mid_year,
    # sex,
    # age, 
    eligible,
    eligible_diab,
    eligible_aha,
    eligible_esc,
    treated,
    treated_diab,
    treated_aha,
    treated_esc,
    controlled,
    controlled_diab,
    controlled_aha,
    controlled_esc
  ) %>%
  mutate(
    across(c(
      starts_with("eligible"),
      starts_with("treated"),
      starts_with("controlled")
    ), scales::percent, accuracy = 1)
  )


kable(
    x = national_sensitivity, 
    format = "latex",
    align = "lccccccccccccc",
    booktabs = TRUE,
    escape = TRUE,
    longtable = TRUE,
    linesep = linesep(as.vector(table(national_sensitivity$Country))),
    label = "sensitivity_national",
    #digits = 2,
    col.names = c("Country", "Year", rep(c("NCEP", "DM  ", "AHA ", "ESC "), 3)),
    caption = "National trends in prevalence, treatment, and control of serum cholesterol using different eligibility criteria."
    ) %>%
  kable_styling(latex_options = c("repeat_header"),
                font_size = 7) %>%
  add_header_above(
    c(" " = 2, "Prevalence" = 4, "Treated" = 4, "Controlled" = 4)
  ) %>%
  footnote(
    alphabet = c(
      "NCEP = National Cholesterol Education Program (NCEP) Adult Treatment Panel III guidelines",
      "DM = NCEP plus all patients with diabetes eligible for treatment",
      "AHA = American Heart Association 2018-2019 guidelines",
      "ESC = European Society for Cardiology 2019 guidelines"
    ),
    threeparttable = T
  ) %>%
  save_kable("2_tables/sensitivity_national.tex")


# control -----------------------------------------------------------------


control_sensitivity <- 
  national_results %>%
  filter(!id_study %in% c(no_rf_data, small_samples)) |>
  mutate(
    controlled_ci = print_ci(controlled, controlled_low, controlled_upp),
    controlled_min_ci = print_ci(
      est = controlled_min, 
      lwr = controlled_min_low, 
      upr = controlled_min_upp
    ),
    controlled_max_ci = print_ci(
      est = controlled_max, 
      lwr = controlled_max_low, 
      upr = controlled_max_upp
    )
  ) %>%
  ungroup() %>%
  arrange(Country, mid_year) %>%
  select(
    Country,
    mid_year,
    # sex,
    # age, 
    controlled,
    controlled_max,
    controlled_min
  ) %>%
  mutate(
    across(c(
      starts_with("controlled")
    ), scales::percent, accuracy = 1)
  )


kable(
  x = control_sensitivity, 
  format = "latex",
  align = "lcccc",
  booktabs = TRUE,
  escape = TRUE,
  longtable = TRUE,
  linesep = linesep(as.vector(table(control_sensitivity$Country))),
  label = "control_sensitivity",
  #digits = 2,
  col.names = c("Country", "Year", "<130 mg/dL", "<100 mg/dL", "<190 mg/dL"),
  caption = "National trends in control of serum cholesterol using different control definitions."
) %>%
  kable_styling(latex_options = c("repeat_header"),
                font_size = 9) %>%
  add_header_above(
    c(" " = 2, "Controlled Non-HDL-C level" = 3)
  ) %>%
  save_kable("2_tables/control_sensitivity.tex")

