hic <- 
  hic %>%
  mutate(
    sex = factor(sex, labels = c("Men", "Women")),
    
    agecont = age,
    
    # create 10-year age categories
    age = cut(
      x = age,
      breaks = seq(40, 80, by = 10), 
      labels = c("40-49 years", "50-59 years", "60-69 years", "70-79 years"),
      right = FALSE
    ),
    
    # calculate nonhdl total serum cholesterol
    nonhdl = tc_cleaned - hdl_cleaned,
    
    # calculate 10-year risk score for CVD using globorisk
    risk_score = globorisk::globorisk(
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
      
    # define elevated serum cholesterol (i.e. treatment eligibility)
    elevated = as.numeric(
      ((nonhdl >= 5.689 | drug_chol == 1) & !is.na(drug_chol)) | 
        (nonhdl >= 5.689 & is.na(drug_chol))
    ),
    
    # define awareness of elevated serum cholesterol
    aware = case_when(
      self_chol == 1 & elevated == 1 ~ 1,
      self_chol == 0 & elevated == 1 ~ 0,
      TRUE ~ NA_real_
    ),
    
    # create indicator for treatment with any lipid-lowering drug among those eligible
    treated = case_when(
      drug_chol == 1 & elevated == 1 ~ 1,
      drug_chol == 0 & elevated == 1 ~ 0,
      TRUE ~ NA_real_
    ),
    
    # create indicator for those using statins among those using any lipid-lowering drug
    
    
    # identify those with dyslipidemia who are untreated
    #uncontrolled = 
  ) 

# subset to only relevant analytical variables
# hic <-
#   hic %>%
#   select(
#     
#   )

