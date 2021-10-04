# set survey options
options(survey.adjust.domain.lonely = TRUE)
options(survey.lonely.psu = "adjust")

results <-
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
          n = survey_total(),
          nonhdl = survey_mean(
            x = nonhdl, 
            vartype = "ci",
            na.rm = TRUE
          ),
          risk_score = survey_mean(
            x = risk_score, 
            vartype = "ci",
            na.rm = TRUE
          ),
          diab = survey_mean(
            x = self_diab, 
            vartype = "ci",
            na.rm = TRUE
          ),
          smoker = survey_mean(
            x = smoker, 
            vartype = "ci",
            na.rm = TRUE
          ),
          elevated = survey_mean(
            x = elevated, 
            vartype = "ci",
            proportion = TRUE,
            na.rm = TRUE
          ),
          # aware = survey_mean(
          #   x = aware, 
          #   vartype = "ci",
          #   proportion = TRUE,
          #   na.rm = TRUE
          # ),
          treated = survey_mean(
            x = treated,
            vartype = "ci",
            na.rm = TRUE
          )
        )
    )
  )

results <- results %>%
  unnest(stats) 


