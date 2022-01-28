missing_chol_data <- 
  hic %>%
  mutate(missing_tc = is.na(tc_cleaned),
         missing_self_chol = is.na(self_chol),
         missing_drug_chol = is.na(drug_chol)
  ) %>%
  filter(!missing_tc) %>%
  group_by(Country, id_study) %>%
  summarise(
    across(.cols = c("missing_self_chol", "missing_drug_chol"), sum, na.rm = TRUE, .names = "n_{.col}"),
    across(.cols = c("missing_self_chol", "missing_drug_chol"), mean, na.rm = TRUE, .names = "pct_{.col}"),
    across(.cols = c("self_chol", "drug_chol"), mean, na.rm = TRUE),
    n = n()
  ) %>%
  mutate(
    n_missing_self_chol = paste0(n_missing_self_chol, " (", trimws(format(round(100 * pct_missing_self_chol, 2), nsmall = 2)), "%)"),
    n_missing_drug_chol = paste0(n_missing_drug_chol, " (", trimws(format(round(100 * pct_missing_drug_chol, 2), nsmall = 2)), "%)")
  ) %>%
  select(Country, id_study, n, n_missing_self_chol, n_missing_drug_chol, self_chol, drug_chol) %>% ungroup()


  kable(
    x = missing_chol_data,
    format = "latex",
    digits = 2, 
    col.names = c("Country",
                  "Study ID",
                  "N",
                  "self_chol, N (%)",
                  "drug_chol, N (%)",
                  "self_chol",
                  "drug_chol"),
    booktabs = TRUE,
    longtable = TRUE,
    linesep = linesep(as.vector(table(missing_chol_data$Country))),
  ) %>%
    kable_styling(latex_options = c("repeat_header"),
                  font_size = 7) %>%
    add_header_above(c(" " = 3, "Missing" = 2, "Mean" = 2)) %>%
    save_kable("2_tables/missing_chol_data.tex")
  
  
  missing_smk_data <- 
    hic %>%
    mutate(missing_tc = is.na(tc_cleaned),
           missing_smoker = is.na(smoker),
           missing_smoke_ever = is.na(smoke_ever)
    ) %>%
    filter(!missing_tc) %>%
    group_by(Country, id_study) %>%
    summarise(
      across(.cols = c("missing_smoker", "missing_smoke_ever"), sum, na.rm = TRUE, .names = "n_{.col}"),
      across(.cols = c("missing_smoker", "missing_smoke_ever"), mean, na.rm = TRUE, .names = "pct_{.col}"),
      across(.cols = c("smoker", "smoke_ever"), mean, na.rm = TRUE),
      n = n()
    ) %>%
    mutate(
      n_missing_smoker = paste0(n_missing_smoker, " (", trimws(format(round(100 * pct_missing_smoker, 2), nsmall = 2)), "%)"),
      n_missing_smoke_ever = paste0(n_missing_smoke_ever, " (", trimws(format(round(100 * pct_missing_smoke_ever, 2), nsmall = 2)), "%)")
    ) %>%
    select(Country, id_study, n, n_missing_smoke_ever, n_missing_smoker, smoke_ever, smoker) %>% ungroup()
  
  
  kable(
    x = missing_smk_data,
    format = "latex",
    digits = 2, 
    col.names = c("Country",
                  "Study ID",
                  "N",
                  "smoke_ever, N (%)",
                  "smoker, N (%)",
                  "smoke_ever",
                  "smoker"),
    booktabs = TRUE,
    longtable = TRUE,
    linesep = linesep(as.vector(table(missing_smk_data$Country))),
  ) %>%
    kable_styling(latex_options = c("repeat_header"),
                  font_size = 7) %>%
    add_header_above(c(" " = 3, "Missing" = 2, "Mean" = 2)) %>%
    save_kable("2_tables/missing_smk_data.tex")
