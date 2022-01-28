# set survey options
options(survey.adjust.domain.lonely = TRUE)
options(survey.lonely.psu = "adjust")

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
      )
    ),
    
    # calculate relevant survey statistics
    stats = list(
      srvy %>%
        group_by(sex, age) %>%
        summarise(
          n_nonhdl = survey_total(),
          nonhdl = survey_mean(
            x = nonhdl, 
            vartype = "ci",
            na.rm = TRUE
          )
        )
    )
  )

results <-
  hic %>%
  filter(!missing_chol & !missing_rf) %>%
  nest_by(id_study, Country, mid_year) %>%
  mutate(
    # create survey design objects to appropriately handle survey weights
    srvy = list(
      survey_design(
        data,
        psu = "psu",
        strata = "stratum",
        weights = "samplewt_chol"
      )
    ),
    
    # calculate relevant survey statistics
    stats = list(
      srvy %>%
        group_by(sex, age) %>%
        summarise(
          n_other = survey_total(),
          nonhdl_untreated = survey_mean(
            x = nonhdl_untreated, 
            vartype = "ci",
            na.rm = TRUE
          ),
          risk_score = survey_mean(
            x = risk_score, 
            vartype = "ci",
            na.rm = TRUE
          ),
          pct_miss_risk_score = sum(is.na(risk_score)) / n(),
          diab = survey_mean(
            x = self_diab, 
            vartype = "ci",
            # proportion = TRUE,
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
          )
        )
    )
  )

results_nonhdl <- results_nonhdl %>%
  unnest(stats) 

results <- results %>%
  unnest(stats) 

results <- bind_rows(results_nonhdl, results)

