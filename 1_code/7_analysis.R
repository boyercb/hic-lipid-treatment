# set survey options
options(survey.adjust.domain.lonely = TRUE)
options(survey.lonely.psu = "adjust")

if (rerun_analysis) {
  results_nonhdl <- 
    hic %>%
    nest_by(id_study, Country, mid_year) %>%
    mutate(
      # create survey design objects to appropriately handle survey weights
      srvy = list(
        survey_design(
          data,
          psu = "psu",
          strata = "stratum",
          weights = "samplewt_chol"
        ) %>%
          filter(!(exclude_age | exclude_age_range | missing_chol))
      ),
      
      # calculate relevant survey statistics
      stats = list(
        srvy %>%
          group_by(sex, age) %>%
          summarise(
            nonhdl = survey_mean(
              x = nonhdl, 
              vartype = "ci",
              na.rm = TRUE
            ),
            nonhdl_untreated = survey_mean(
              x = nonhdl_untreated, 
              vartype = "ci",
              na.rm = TRUE
            ),
            .groups = "drop"
          ) 
      )
    )
  
  no_rf_data <- c(
    "AUS_1980_RFPS",
    "AUS_1989_RFPS",
    "CZE_1992_MONICA",
    "ESP_2005_PREVICTUS",
    "FIN_2007_YFS_rural",
    "FIN_2007_YFS_urban",
    "GBR_1987_DNS",
    "POL_2004_LIPIDOGRAM",
    "POL_2006_LIPIDOGRAM"
  )
  
  results_rf <- 
    hic %>%
    filter(!id_study %in% no_rf_data) %>%
    nest_by(id_study, Country, mid_year) %>%
    mutate(
      # create survey design objects to appropriately handle survey weights
      srvy = list(
        survey_design(
          data,
          psu = "psu",
          strata = "stratum",
          weights = "samplewt_chol"
        ) %>%
          filter(!(exclude_age | exclude_age_range | missing_chol))
      ),
      
      # calculate relevant survey statistics
      stats = list(
        srvy %>%
          group_by(sex, age) %>%
          summarise(
            risk_score = survey_mean(
              x = risk_score, 
              vartype = "ci",
              na.rm = TRUE
            ),
            pct_miss_risk_score = sum(is.na(risk_score)) / n(),
            diab = survey_mean(
              x = self_diab, 
              vartype = "ci",
              proportion = TRUE,
              prop_method = "beta",
              na.rm = TRUE
            ),
            smoker = survey_mean(
              x = smoker, 
              vartype = "ci",
              na.rm = TRUE
            ),
            severe = survey_mean(
              x = severe, 
              vartype = "ci",
              proportion = TRUE,
              prop_method = "beta",
              na.rm = TRUE
            ),
            eligible = survey_mean(
              x = eligible, 
              vartype = "ci",
              proportion = TRUE,
              prop_method = "beta",
              na.rm = TRUE
            ),
            pct_miss_eligible = sum(is.na(eligible)) / n(),
            treated = survey_mean(
              x = treated,
              vartype = "ci",
              proportion = TRUE,
              prop_method = "beta",
              na.rm = TRUE
            ),
            pct_miss_treated = sum(is.na(treated)) / n(),
            statins = survey_mean(
              x = statins,
              vartype = "ci",
              proportion = TRUE,
              prop_method = "beta",
              na.rm = TRUE
            ),
            uncontrolled = survey_mean(
              x = uncontrolled,
              vartype = "ci",
              proportion = TRUE,
              prop_method = "beta",
              na.rm = TRUE
            ),
            .groups = "drop"
          )
      )
    )
  
  results_nonhdl <- unnest(results_nonhdl, stats)
  results_rf <- unnest(results_rf, stats)
  
  results <- left_join(
    x = results_nonhdl,
    y = select(results_rf, -srvy, -data),
    by = c("id_study", "Country", "mid_year", "sex", "age")
  )
  
  write_rds(results, "0_data/results.rds")
  
} else {
  results <- read_rds("0_data/results.rds")
  
}

