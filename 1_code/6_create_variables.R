hic <- 
  hic %>%
  mutate(
    sex = factor(sex, labels = c("Men", "Women")),
    
    agecont = ifelse(exclude_age | exclude_age_range, NA, age),
    
    # create 10-year age categories
    age = cut(
      x = agecont,
      breaks = seq(40, 80, by = 10), 
      labels = c("40-49 years", "50-59 years", "60-69 years", "70-79 years"),
      exclude = NULL,
      right = FALSE
    ),
    
    age = addNA(age),
    
    # calculate nonhdl total serum cholesterol
    nonhdl = tc_cleaned - hdl_cleaned,
    
    # untreated nonhdl
    nonhdl_untreated = replace(nonhdl, drug_chol == 1, NA),
      
    # calculate 10-year risk score for CVD using globorisk
    risk_score = globorisk(
      sex = as.numeric(sex) - 1,
      age = agecont,
      sbp = sbp_final_cleaned,
      tc = tc_cleaned,
      dm = self_diab,
      smk = smoker,
      iso = iso,
      year = replace(mid_year, mid_year <= 1999, 2000),
      version = "lab",
      type = "risk"
    ),
      
    # severe hypercholesterolemia indicator
    severe = as.numeric(nonhdl >= 5.69),
    
    # define elevated serum cholesterol (i.e. treatment eligibility)
    eligible = case_when(
      missing_rf | missing_chol ~ NA_real_,
      (nonhdl >= 5.69 | 
        (nonhdl >= 4.92 & risk_score >= 0.05) | 
        (nonhdl >= 3.76 & risk_score >= 0.10) | 
        (nonhdl >= 2.98 & risk_score >= 0.20) | 
        (drug_chol == 1 & !is.na(drug_chol))) ~ 1,
      TRUE ~ 0
    ),
    
    # define awareness of elevated serum cholesterol
    aware = case_when(
      self_chol == 1 & eligible == 1 ~ 1,
      self_chol == 0 & eligible == 1 ~ 0,
      TRUE ~ NA_real_
    ),
    
    # create indicator for treatment with any lipid-lowering drug among those eligible
    treated = case_when(
      drug_chol == 1 & eligible == 1 ~ 1,
      drug_chol == 0 & eligible == 1 ~ 0,
      TRUE ~ NA_real_
    ),
    
    # create indicator for those using statins among those using any lipid-lowering drug
    statins = case_when(
      is.na(drug_chol_stat) | is.na(treated) ~ NA_real_,
      drug_chol_stat == 1 & treated == 1 ~ 1,
      drug_chol_stat == 1 & treated == 1 ~ 0
    ),
    
    # identify those with dyslipidemia who are untreated
    uncontrolled = as.numeric(nonhdl >= 5.69 & drug_chol == treated)
  ) 


