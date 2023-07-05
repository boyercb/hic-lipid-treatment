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
    
    age2 = cut(
      x = agecont,
      breaks = seq(40, 80, by = 20), 
      labels = c("40-59 years", "60-79 years"),
      exclude = NULL,
      right = FALSE
    ),
    
    age2 = addNA(age2),
    
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
      type = "risk",
      updated_lac = TRUE
    ),
      
    # risk_aha = ascvd_10yr_accaha(
    #   
    # ),
      
    risk_framingham = ascvd_10yr_frs(
      gender = ifelse(sex == 2, "female", "male"),
      age = agecont,
      hdl = hdl_cleaned,
      totchol = tc_cleaned,
      sbp = sbp_final_cleaned,
      bp_med = drug_hyper,
      smoker = smoker,
      diabetes = self_diab
    ), 
    
    # severe hypercholesterolemia indicator
    severe = as.numeric(nonhdl >= convert_to_mmol(220)),
    
    # define elevated serum cholesterol (i.e. treatment eligibility)
    eligible = case_when(
      missing_rf | missing_chol ~ NA_real_,
      (nonhdl >= convert_to_mmol(220) | 
        (nonhdl >= convert_to_mmol(190) & risk_score >= 0.05) | 
        (nonhdl >= convert_to_mmol(160) & risk_score >= 0.10) | 
        (nonhdl >= convert_to_mmol(130) & risk_score >= 0.20) | 
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
    
    treated_pop = case_when(
      drug_chol == 1 ~ 1,
      drug_chol == 0 ~ 0,
      TRUE ~ NA_real_
    ),
    
    # create indicator for those using statins among those using any lipid-lowering drug
    statins = case_when(
      is.na(drug_chol_stat) | is.na(treated) ~ NA_real_,
      drug_chol_stat == 1 & treated == 1 ~ 1,
      drug_chol_stat == 1 & treated == 1 ~ 0
    ),
    
    controlled_pop = case_when(
      nonhdl < convert_to_mmol(130) ~ 1,
      # eligible == 1 ~ 0,
      TRUE ~ 0
    ),
    
    controlled_treated = case_when(
      nonhdl < convert_to_mmol(130) & treated == 1 ~ 1,
      treated == 1 ~ 0,
      TRUE ~ NA_real_
    ),
    
    controlled = case_when(
      nonhdl < convert_to_mmol(130) & treated == 1 ~ 1,
      eligible == 1 ~ 0,
      TRUE ~ NA_real_
    ),
    
    controlled_min = case_when(
      nonhdl < convert_to_mmol(190) & treated == 1 ~ 1,
      eligible == 1 ~ 0,
      TRUE ~ NA_real_
    ),
    
    controlled_max = case_when(
      nonhdl < convert_to_mmol(100) & treated == 1 ~ 1,
      eligible == 1 ~ 0,
      TRUE ~ NA_real_
    ),
    
    # identify those with severe hypercholesterolemia who are untreated
    untreated_severe = case_when(
      nonhdl >= convert_to_mmol(220) & treated == 0 & eligible == 1 ~ 1,
      eligible == 1 ~ 0,
      TRUE ~ NA_real_
    ),
    
    untreated_high_risk = case_when(
      risk_score >= 0.2 & treated == 0 & eligible == 1 ~ 1,
      eligible == 1 ~ 0,
      TRUE ~ NA_real_
    ),
    
    # alternative eligibility definitions
    # ensure everyone with diabetes is eligible
    eligible_diab = case_when(
      missing_rf | missing_chol ~ NA_real_,
      self_diab == 1 ~ 1,
      (nonhdl >= convert_to_mmol(220) | 
         (nonhdl >= convert_to_mmol(190) & risk_score >= 0.05) | 
         (nonhdl >= convert_to_mmol(160) & risk_score >= 0.10) | 
         (nonhdl >= convert_to_mmol(130) & risk_score >= 0.20) | 
         (drug_chol == 1 & !is.na(drug_chol))) ~ 1,
      TRUE ~ 0
    ),
    
    # use esc guidelines
    eligible_esc = case_when(
      missing_rf | missing_chol ~ NA_real_,
      self_diab == 1 ~ 1,
      (nonhdl >= convert_to_mmol(220) | 
         (nonhdl >= convert_to_mmol(130) & risk_score >= 0.05) | 
         (nonhdl >= convert_to_mmol(100) & risk_score >= 0.10) | 
         (drug_chol == 1 & !is.na(drug_chol))) ~ 1,
      TRUE ~ 0
    ),
    
    # use aha guidelines
    eligible_aha = case_when(
      missing_rf | missing_chol ~ NA_real_,
      self_diab == 1 ~ 1,
      (nonhdl >= convert_to_mmol(220) | 
         (nonhdl >= convert_to_mmol(100) & risk_score >= 0.075) |
         (drug_chol == 1 & !is.na(drug_chol))) ~ 1,
      TRUE ~ 0
    ),
    
    # create indicator for treatment with any lipid-lowering drug among those eligible
    treated_diab = case_when(
      drug_chol == 1 & eligible_diab == 1 ~ 1,
      drug_chol == 0 & eligible_diab == 1 ~ 0,
      TRUE ~ NA_real_
    ),
    
    # create indicator for treatment with any lipid-lowering drug among those eligible
    treated_esc = case_when(
      drug_chol == 1 & eligible_esc == 1 ~ 1,
      drug_chol == 0 & eligible_esc == 1 ~ 0,
      TRUE ~ NA_real_
    ),
    
    # create indicator for treatment with any lipid-lowering drug among those eligible
    treated_aha = case_when(
      drug_chol == 1 & eligible_aha == 1 ~ 1,
      drug_chol == 0 & eligible_aha == 1 ~ 0,
      TRUE ~ NA_real_
    ),
    
    controlled_diab = case_when(
      nonhdl < convert_to_mmol(130) & treated_diab == 1 ~ 1,
      eligible_diab == 1 ~ 0,
      TRUE ~ NA_real_
    ),
    
    controlled_esc = case_when(
      nonhdl < convert_to_mmol(130) & treated_esc == 1 ~ 1,
      eligible_esc == 1 ~ 0,
      TRUE ~ NA_real_
    ),
    
    controlled_aha = case_when(
      nonhdl < convert_to_mmol(130) & treated_aha == 1 ~ 1,
      eligible_aha == 1 ~ 0,
      TRUE ~ NA_real_
    )
  ) 


