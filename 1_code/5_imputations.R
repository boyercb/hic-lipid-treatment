hic_to_impute <-
  select(hic,
         id_study,
         iso,
         Country,
         Superregion,
         Region,
         survey, 
         survey_type,
         urban_rural,
         mid_year,
         all_of(c(anthro, lipids, bp)),
         sex,
         age,
         smoker,
         smoke_ever,
         self_hyper,
         drug_hyper,
         fgl_cleaned,
         ppg_cleaned,
         hba1c_cleaned,
         self_diab,
         drug_diab_final,
         drug_chol)

ignore <-
  c("hic",
    "id_study",
    "iso",
    "Country",
    "Superregion",
    "Region",
    "survey", 
    "survey_type",
    "urban_rural",
    "mid_year",
    "hip_cleaned",
    "fgl_cleaned",
    "ppg_cleaned",
    "hba1c_cleaned")

targets <- hic_to_impute
targets[, !colnames(targets) %in% ignore] <- TRUE
targets[, colnames(targets) %in% ignore] <- FALSE

if (rerun_imputations) {
  set.seed(13431325)
  
  imps <- 
    mice(
    data = hic_to_impute,
    where = targets,
    m = 5
  )
  
  imps <- write_rds(imps, "0_data/imps.rds")
  
} else {
  imps <- read_rds("0_data/imps.rds")
}

#md.pattern(hic_to_impute, plot = TRUE, rotate.names = TRUE)

