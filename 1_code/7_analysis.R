# set survey options
options(survey.adjust.domain.lonely = TRUE)
options(survey.lonely.psu = "adjust")

no_diab_data <- c(
  "CZE_1992_MONICA",
  "FIN_2007_YFS_rural",
  "FIN_2007_YFS_urban",
  "POL_2004_LIPIDOGRAM",
  "POL_2006_LIPIDOGRAM",
  "GBR_2017_BCS70"
)

no_smoking_data <- c(
  "BEL_2018_EHES",
  "ESP_2005_PREVICTUS",
  "MLT_2015_SAHHTEK"
)

no_rf_data <- c(
  no_diab_data,
  no_smoking_data
)

small_samples <- c(
  "GBR_2010_NDNS",   
  "GBR_2014_NDNS",
  "GBR_2016_NDNS",
  "GBR_2018_NDNS"
)

no_ldl_data <- c(
  "BEL_2018_EHES",
  "CZE_1992_MONICA",
  "CZE_2015_EHES",
  "GBR_1998_HSE",
  "GBR_2005_HSE",
  "GBR_2006_HSE",
  "GBR_2008_HSE",
  "GBR_2009_HSE",
  "GBR_2010_HSE",
  "GBR_2011_HSE",
  "GBR_2012_HSE",
  "GBR_2013_HSE",
  "GBR_2014_HSE",
  "GBR_2015_HSE",
  "GBR_2016_HSE",
  "GBR_2017_BCS70",
  "GBR_2017_HSE",
  "GBR_2018_HSE",
  "GBR_2019_HSE",
  "ITA_2000_OEC",
  "SVK_2012_EHES"
)

awareness_data <- c(
  "AUS_2000_AusDiab",
  "AUS_2005_AusDiab",
  "AUS_2012_AusDiab",
  "ESP_2015_ENRICA",
  "GRC_2014_HNNHS",
  "IRL_2010_TILDA",
  "POL_2004_LIPIDOGRAM",
  "POL_2006_LIPIDOGRAM",
  "POL_2014_WOBASZ",
  "USA_1991_NHANES",
  "USA_2000_NHANES",
  "USA_2002_NHANES",
  "USA_2004_NHANES",
  "USA_2006_NHANES",
  "USA_2008_NHANES",
  "USA_2010_NHANES",
  "USA_2012_NHANES",
  "USA_2014_NHANES",
  "USA_2016_NHANES",
  "USA_2018_NHANES"
)

calc_stats <- function(data, type, study) {
  print(study)
  
  if (type == "nonhdl") {
    num_ticks <- n_groups(data) * 10
    pb <- progress_bar$new(
      format = ":outcome [:bar] :current/:total (:percent) elapsed :elapsed eta :eta",
      total = num_ticks
    )
    
    track_survey_mean <- function(x, ...) {
      pb$tick(tokens = list(outcome = rlang::as_name(enquo(x))))
      survey_mean(x, ...)
    }
    
    data %>%
    summarise(
      nonhdl = track_survey_mean(
        x = nonhdl, 
        vartype = "ci",
        na.rm = TRUE
      ),
      nonhdl_untreated = track_survey_mean(
        x = nonhdl_untreated, 
        vartype = "ci",
        na.rm = TRUE
      ),
      hdl = track_survey_mean(
        x = hdl_cleaned, 
        vartype = "ci",
        na.rm = TRUE
      ),
      ldl = track_survey_mean(
        x = ldl_cleaned, 
        vartype = "ci",
        na.rm = TRUE
      ),
      tc = track_survey_mean(
        x = tc_cleaned, 
        vartype = "ci",
        na.rm = TRUE
      ),
      aware = track_survey_mean(
        x = aware, 
        vartype = "ci",
        na.rm = TRUE
      ),
      diab = track_survey_mean(
        x = self_diab, 
        vartype = "ci",
        proportion = TRUE,
        prop_method = "beta",
        na.rm = TRUE
      ),
      smoker = track_survey_mean(
        x = smoker, 
        vartype = "ci",
        na.rm = TRUE
      ),
      treated_pop = track_survey_mean(
        x = treated_pop,
        vartype = "ci",
        proportion = TRUE,
        prop_method = "beta",
        na.rm = TRUE
      ),
      controlled_pop = track_survey_mean(
        x = controlled_pop,
        vartype = "ci",
        proportion = TRUE,
        prop_method = "beta",
        na.rm = TRUE
      ),
      .groups = "drop"
    ) 
  } else if (type == "rf") {
    num_ticks <- n_groups(data) * 18
    pb <- progress_bar$new(
      format = ":outcome [:bar] :current/:total (:percent) elapsed :elapsed eta :eta",
      total = num_ticks
    )
    
    track_survey_mean <- function(x, ...) {
      pb$tick(tokens = list(outcome = rlang::as_name(enquo(x))))
      survey_mean(x, ...)
    }
    
    
    data %>%
      summarise(
        risk_score = track_survey_mean(
          x = risk_score, 
          vartype = "ci",
          na.rm = TRUE
        ),
        eligible = track_survey_mean(
          x = eligible, 
          vartype = "ci",
          proportion = TRUE,
          prop_method = "beta",
          na.rm = TRUE
        ),
        treated = track_survey_mean(
          x = treated,
          vartype = "ci",
          proportion = TRUE,
          prop_method = "beta",
          na.rm = TRUE
        ),
        controlled = track_survey_mean(
          x = controlled,
          vartype = "ci",
          proportion = TRUE,
          prop_method = "beta",
          na.rm = TRUE
        ),
        controlled_treated = track_survey_mean(
          x = controlled_treated,
          vartype = "ci",
          proportion = TRUE,
          prop_method = "beta",
          na.rm = TRUE
        ),
        controlled_min = track_survey_mean(
          x = controlled_min,
          vartype = "ci",
          proportion = TRUE,
          prop_method = "beta",
          na.rm = TRUE
        ),
        controlled_max = track_survey_mean(
          x = controlled_max,
          vartype = "ci",
          proportion = TRUE,
          prop_method = "beta",
          na.rm = TRUE
        ),
        untreated_severe = track_survey_mean(
          x = untreated_severe, 
          vartype = "ci",
          proportion = TRUE,
          prop_method = "beta",
          na.rm = TRUE
        ),
        untreated_high_risk = track_survey_mean(
          x = untreated_high_risk, 
          vartype = "ci",
          proportion = TRUE,
          prop_method = "beta",
          na.rm = TRUE
        ),
        eligible_diab = track_survey_mean(
          x = eligible_diab, 
          vartype = "ci",
          proportion = TRUE,
          prop_method = "beta",
          na.rm = TRUE
        ),
        treated_diab = track_survey_mean(
          x = treated_diab,
          vartype = "ci",
          proportion = TRUE,
          prop_method = "beta",
          na.rm = TRUE
        ),
        controlled_diab = track_survey_mean(
          x = controlled_diab,
          vartype = "ci",
          proportion = TRUE,
          prop_method = "beta",
          na.rm = TRUE
        ),
        eligible_esc = track_survey_mean(
          x = eligible_esc, 
          vartype = "ci",
          proportion = TRUE,
          prop_method = "beta",
          na.rm = TRUE
        ),
        treated_esc = track_survey_mean(
          x = treated_esc,
          vartype = "ci",
          proportion = TRUE,
          prop_method = "beta",
          na.rm = TRUE
        ),
        controlled_esc = track_survey_mean(
          x = controlled_esc,
          vartype = "ci",
          proportion = TRUE,
          prop_method = "beta",
          na.rm = TRUE
        ),
        eligible_aha = track_survey_mean(
          x = eligible_aha, 
          vartype = "ci",
          proportion = TRUE,
          prop_method = "beta",
          na.rm = TRUE
        ),
        treated_aha = track_survey_mean(
          x = treated_aha,
          vartype = "ci",
          proportion = TRUE,
          prop_method = "beta",
          na.rm = TRUE
        ),
        controlled_aha = track_survey_mean(
          x = controlled_aha,
          vartype = "ci",
          proportion = TRUE,
          prop_method = "beta",
          na.rm = TRUE
        ),
        .groups = "drop"
      )
  } else {
    stop("Error: incorrect -type- specified!")
  }
  
}

# create survey design objects to appropriately handle survey weights
results <- 
  hic %>%
  nest_by(id_study, Country, mid_year) %>%
  mutate(
    srvy = list(
      survey_design(
        data,
        psu = "psu",
        strata = "stratum",
        weights = "samplewt_chol"
      ) %>%
        filter(!(exclude_age | exclude_age_range | missing_chol))
    )
  )


# rerun results -----------------------------------------------------------

if (rerun_analysis) {
  # plan(multisession, workers = 8)
  
  results_nonhdl <- 
    results %>%
    mutate(
      national_stats = list(
        srvy %>%
          calc_stats("nonhdl", id_study)
      ),
      
      # calculate relevant survey statistics
      age_sex_stats = list(
        srvy %>%
          group_by(sex, age) %>%
          calc_stats("nonhdl", id_study)
      ),
      
      age2_sex_stats = list(
        srvy %>%
          group_by(sex, age2) %>%
          calc_stats("nonhdl", id_study)
      )
    )
  
  results_rf <- 
    results %>%
    filter(!id_study %in% c(no_rf_data, small_samples)) %>%
    mutate(
      # calculate relevant survey statistics
      # national statistics
      national_stats = list(
        srvy %>%
          calc_stats("rf", id_study)
      ),
      
      # sex stratified 
      sex_stats = list(
        srvy %>%
          group_by(sex) %>%
          calc_stats("rf", id_study)
      ),
      
      # age stratified
      age_stats = list(
        srvy %>%
          group_by(age) %>%
          calc_stats("rf", id_study)
      ),
      
      # sex stratified, age standardized
      # sex_std_stats = list(
      #   srvy %>%
      #     svystandardize(design = .,
      #                    by = ~ age,
      #                    over = ~ sex, 
      #                    # WHO 2000 - 2025 standard
      #                    # population = c(126256, 99165, 66777, 37287, 0),
      #                    # US 2000 standard
      #                    population = c(153969, 111170, 73057, 58772, 0)
      #                    ) %>%
      #     group_by(sex) %>%
      #     calc_stats("rf", id_study)
      # ),
      
      # age and sex stratified
      age_sex_stats = list(
        srvy %>%
          group_by(sex, age) %>%
          calc_stats("rf", id_study)
      ),
      
      age2_sex_stats = list(
        srvy %>%
          group_by(sex, age2) %>%
          calc_stats("rf", id_study)
      )
      
    )
  
  results_rf <- select(results_rf, -data, -srvy)
  results_nonhdl <- select(results_nonhdl, -data, -srvy)
  
  write_rds(results_rf, "0_data/results_rf.rds")
  write_rds(results_nonhdl, "0_data/results_nonhdl.rds")

} else {
  results_rf <- read_rds("0_data/results_rf.rds")
  results_nonhdl <- read_rds("0_data/results_nonhdl.rds")
}

national_results <- left_join(
  x = unnest(results_nonhdl, national_stats),
  y = unnest(results_rf, national_stats),
  by = c("id_study", "Country", "mid_year")
) |>
  ungroup() |>
  arrange(Country, mid_year)

sex_results <- unnest(results_rf, sex_stats) |>
  ungroup() |>
  arrange(Country, mid_year)
age_results <- unnest(results_rf, age_stats) |>
  ungroup() |>
  arrange(Country, mid_year)


age2_sex_results <- left_join(
  x = unnest(results_nonhdl, age2_sex_stats),
  y = unnest(results_rf, age2_sex_stats),
  by = c("id_study", "Country", "mid_year", "age2", "sex")
) |>
  ungroup() |>
  arrange(Country, mid_year)

age_sex_results <- left_join(
  x = unnest(results_nonhdl, age_sex_stats),
  y = unnest(results_rf, age_sex_stats),
  by = c("id_study", "Country", "mid_year", "age", "sex")
) |>
  ungroup() |>
  arrange(Country, mid_year)

