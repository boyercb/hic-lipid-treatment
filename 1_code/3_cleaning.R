# variable lists
anthro <- c(
  "height_cleand",
  "weight_cleaned",
  "bmi_cleaned",
  "waist_cleaned",
  "hip_cleaned"
)

lipids <- c(
  "tc_cleaned",
  "ldl_cleaned",
  "hdl_cleaned"
)

bp <- c(
  "sbp_final_cleaned", 
  "dbp_final_cleaned"
)

nologs <- c("dbp_final_cleaned", "height_cleand")


# variable recoding based on conversation w Bin ---------------------------

hic <-
  hic %>%
  mutate(
    # recode smoking variable in UK surveys with never smoker = 0 to 0
    smoker = replace(smoker, Country == "United Kingdom" & is.na(smoker) & smoke_ever == 0, 0),
    # recode smoking variable in US NHANES to 0 if missing
    smoker = replace(smoker, Country == "United States of America" & is.na(smoker), 0),
    # unknown issues with smoking data (remove)
    smoker = replace(smoker, id_study %in% c("DEU_2007_HNRS", "AUS_1989_RFPS"), 0),
    # recode ‘drug_chol == NA & self_chol == 0’ to ‘drug_chol = 0’
    drug_chol = replace(drug_chol, id_study == "AUS_2012_AusDiab" & is.na(drug_chol) & self_chol == 0, 0),
    # consider all ‘drug_chol == NA’ as ‘drug_chol = 0’ for the following 
    drug_chol = replace(drug_chol, 
                        (id_study %in% c("ESP_2009_ENRICA", "POL_2004_WOBASZ") | 
                           str_detect(id_study, "(GBR_[0-9]+_NDNS)|(USA_[0-9]+_NHANES)")) & 
                          is.na(drug_chol), 
                        0),
    drug_chol = ifelse(is.na(drug_chol) & !is.na(drug_chol_stat), drug_chol_stat, drug_chol),
    drug_chol = ifelse(is.na(drug_chol) & drug_chol_fibr == 1 & drug_chol_stat == 0, drug_chol_fibr, drug_chol)
  )


# summary statistics ------------------------------------------------------

# summarise all relevant variables
hic %>%
  select(all_of(c("age", anthro, lipids, bp))) %>%
  pivot_longer(cols = everything()) %>% 
  group_by(name) %>%
  summarise(
    n    = sum(!is.na(value)),
    miss = mean(is.na(value)),
    mean = mean(value, na.rm = TRUE),
    sd   = sd(value, na.rm = TRUE),
    skew = skewness(value, na.rm = TRUE),
    kur  = kurtosis(value, na.rm = TRUE),
    min  = min(value, na.rm = TRUE),
    q1   = quantile(value, probs = 0.01, na.rm = TRUE),
    q5   = quantile(value, probs = 0.05, na.rm = TRUE),
    q10  = quantile(value, probs = 0.10, na.rm = TRUE),
    q20  = quantile(value, probs = 0.20, na.rm = TRUE),
    q50  = median(value, na.rm = TRUE),
    q80  = quantile(value, probs = 0.80, na.rm = TRUE),
    q90  = quantile(value, probs = 0.90, na.rm = TRUE),
    q95  = quantile(value, probs = 0.95, na.rm = TRUE),
    q99  = quantile(value, probs = 0.99, na.rm = TRUE),
    max  = max(value, na.rm = TRUE)
  ) %>%
  separate(name, "name", sep = "_", extra = "drop") %>%
  kable(
    x = .,
    digits = 1,
    format = "latex",
    booktabs = TRUE,
    linesep = ""
  )
  

# summarise availability of lipid measures more specifically
hic %>%
  select(all_of(lipids), id_study) %>%
  group_by(id_study) %>%
  summarise(across(lipids, ~sum(!is.na(.x)) > 0)) %>%
  summarise(
    tc_nonmiss = sum(tc_cleaned),
    hdl_nonmiss = sum(hdl_cleaned),
    ldl_nonmiss = sum(ldl_cleaned),
    tc_hdl_nonmiss = sum(tc_cleaned & hdl_cleaned),
    tc_ldl_nonmiss = sum(tc_cleaned & ldl_cleaned),
    hdl_ldl_nonmiss = sum(hdl_cleaned & ldl_cleaned),
    all_nonmiss = sum(tc_cleaned & hdl_cleaned & ldl_cleaned),
    tc_pct = mean(tc_cleaned),
    hdl_pct = mean(hdl_cleaned),
    ldl_pct = mean(ldl_cleaned),
    tc_hdl_pct = mean(tc_cleaned & hdl_cleaned),
    tc_ldl_pct = mean(tc_cleaned & ldl_cleaned),
    hdl_ldl_pct = mean(hdl_cleaned & ldl_cleaned),
    all_pct = mean(tc_cleaned & hdl_cleaned & ldl_cleaned)
  ) %>%
  pivot_longer(everything()) %>%
  mutate(
    type = if_else(str_detect(name, "nonmiss"), "n", "pct"),
    name = str_remove(name, "(_nonmiss)|(_pct)")
  ) %>%
  pivot_wider(
    names_from = "type",
    values_from = "value"
  ) %>%
  kable(
    x = .,
    digits = 2,
    format = "latex",
    booktabs = TRUE,
    linesep = ""
  )


# multivariate constraints ------------------------------------------------


# apply multivariate constraints to cholesterol and bp variables to identify
# possible outliers
hic <-
  hic %>%
  mutate(
    DBP_GREATER_THAN_SBP = dbp_final_cleaned > sbp_final_cleaned,
    LDL_GREATER_THAN_TC = ldl_cleaned > tc_cleaned,
    HDL_GREATER_THAN_TC = hdl_cleaned > tc_cleaned,
    CHOL_DIFF_GREATER_THAN_ME = 
      (tc_cleaned - (hdl_cleaned + ldl_cleaned)) <=
        margin_of_error(tc_cleaned, hdl_cleaned, ldl_cleaned)
  )

mv_constraint_vars <- c(
  "DBP_GREATER_THAN_SBP",
  "LDL_GREATER_THAN_TC",
  "HDL_GREATER_THAN_TC",
  "CHOL_DIFF_GREATER_THAN_ME"
  #colnames(pairs_outliers)
)

hic %>%
  summarise(
    across(mv_constraint_vars, mean, na.rm = TRUE, .names = "{.col}-pct"),
    across(mv_constraint_vars, sum, na.rm = TRUE, .names = "{.col}-outliers"),
    across(mv_constraint_vars, function (x) sum(!is.na(x)), .names = "{.col}-n")
  ) %>%
  pivot_longer(cols = everything()) %>%
  separate(name, c("name", "type"), sep = "-") %>%
  arrange(type) %>%
  pivot_wider(names_from = "type", values_from = "value")  %>%
  mutate(pct = pct * 100) %>%
  kable(
    x = .,
    format = "latex",
    booktabs = TRUE,
    linesep = ""
  )

# set identified outliers to missing
hic <- 
  hic %>%
  mutate(
    dbp_final_cleaned = replace(dbp_final_cleaned, DBP_GREATER_THAN_SBP, NA),
    sbp_final_cleaned = replace(sbp_final_cleaned, DBP_GREATER_THAN_SBP, NA),
    ldl_cleaned = replace(ldl_cleaned, LDL_GREATER_THAN_TC | HDL_GREATER_THAN_TC | CHOL_DIFF_GREATER_THAN_ME, NA),
    hdl_cleaned = replace(hdl_cleaned, LDL_GREATER_THAN_TC | HDL_GREATER_THAN_TC | CHOL_DIFF_GREATER_THAN_ME, NA),
    tc_cleaned = replace(tc_cleaned, LDL_GREATER_THAN_TC | HDL_GREATER_THAN_TC | CHOL_DIFF_GREATER_THAN_ME, NA)
  )


# variable transformations ------------------------------------------------

# make qq plots
# qq <-
#   hic %>% 
#   qq_plots(
#     vars = c(
#       all_of(anthro),
#       all_of(bp),
#       all_of(lipids)
#     ))
# 
# qq_logged <- 
#   hic %>% 
#   qq_plots(
#     vars = c(
#       all_of(anthro),
#       all_of(bp),
#       all_of(lipids)
#     ),
#     trans = log
#   )


# pairwise plots ----------------------------------------------------------

# apply pairwise mahalanobis distance detection algorithm to variable families
pairs <- t(cbind(combn(anthro, 2), combn(bp, 2), combn(lipids, 2)))
colnames(pairs) <- c("var1", "var2")

pairs <- 
  as_tibble(pairs) %>%
  mutate(
    trans1 = if_else(var1 %in% nologs, "identity", "log"),
    trans2 = if_else(var2 %in% nologs, "identity", "log")
  )

# generate plots
pairs_plots <- 
  pmap(as.list(pairs), 
     function(var1, var2, trans1, trans2) {
       rmaha_plot(
         data = hic, 
         var1 = var1, 
         var2 = var2, 
         trans = c(trans1, trans2),
         limits = c(0, 1800)
       )
     })

wrap_plots(pairs_plots, guides = "collect")


# replace outliers based on Mahalanobis distance --------------------------

# identify pairwise outliers
pairs_outliers <- 
  pmap_dfc(as.list(pairs), 
     function(var1, var2, trans1, trans2) {
       outlier <- 
         rmaha_detect(
           data = hic, 
           vars = c(var1, var2),
           trans = c(trans1, trans2),
           drop = TRUE
         )
       
       stub1 <- str_split(var1, "_")
       stub2 <- str_split(var2, "_")
       tb <- tibble(outlier)
       colnames(tb) <- paste0("MAHA_", toupper(stub1[[1]]), "_", toupper(stub2[[1]]))
       
       tb
     })

hic <- bind_cols(hic, pairs_outliers)

mv_constraint_vars <- c(
  "DBP_GREATER_THAN_SBP",
  "LDL_GREATER_THAN_TC",
  "HDL_GREATER_THAN_TC",
  "CHOL_DIFF_GREATER_THAN_ME",
  colnames(pairs_outliers)
)

# summarise mahalanobis outliers
hic %>%
  summarise(
    across(mv_constraint_vars, mean, na.rm = TRUE, .names = "{.col}-pct"),
    across(mv_constraint_vars, sum, na.rm = TRUE, .names = "{.col}-outliers"),
    across(mv_constraint_vars, function (x) sum(!is.na(x)), .names = "{.col}-n")
  ) %>%
  pivot_longer(cols = everything()) %>%
  separate(name, c("name", "type"), sep = "-") %>%
  arrange(type) %>%
  pivot_wider(names_from = "type", values_from = "value")  %>%
  mutate(pct = pct * 100) %>%
  kable(
    x = .,
    format = "latex",
    booktabs = TRUE,
    linesep = ""
  )

# summarise mahalanobis outliers by study
hic %>%
  group_by(id_study) %>%
  summarise(
    across(mv_constraint_vars, sum, na.rm = TRUE),
    n = n(),
    .groups = "keep"
  ) %>%
  summarise(
    across(mv_constraint_vars, function(x) x / n),
  ) %>%
  pivot_longer(cols = -id_study) %>%
  ggplot(., aes(y = fct_rev(id_study), x = name, fill = value)) +
  geom_tile() +
  scale_fill_viridis_c() +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  coord_cartesian(expand = FALSE) +
  labs(
    x = "",
    y = ""
  ) +
  theme_bw() 

hic %>%
  summarise(
    across(mv_constraint_vars, mean, na.rm = TRUE, .names = "{.col}-pct"),
    across(mv_constraint_vars, sum, na.rm = TRUE, .names = "{.col}-outliers"),
    across(mv_constraint_vars, function (x) sum(!is.na(x)), .names = "{.col}-n")
  ) %>%
  pivot_longer(cols = everything()) %>%
  separate(name, c("name", "type"), sep = "-") %>%
  arrange(type) %>%
  pivot_wider(names_from = "type", values_from = "value")  %>%
  mutate(pct = pct * 100) %>%
  kable(
    x = .,
    format = "latex",
    booktabs = TRUE,
    linesep = ""
  )

# set identified outliers to missing
hic <- 
  hic %>%
  mutate(
    dbp_final_cleaned = replace(dbp_final_cleaned, DBP_GREATER_THAN_SBP, NA),
    sbp_final_cleaned = replace(sbp_final_cleaned, DBP_GREATER_THAN_SBP, NA),
    ldl_cleaned = replace(ldl_cleaned, LDL_GREATER_THAN_TC | HDL_GREATER_THAN_TC | CHOL_DIFF_GREATER_THAN_ME, NA),
    hdl_cleaned = replace(hdl_cleaned, LDL_GREATER_THAN_TC | HDL_GREATER_THAN_TC | CHOL_DIFF_GREATER_THAN_ME, NA),
    tc_cleaned = replace(tc_cleaned, LDL_GREATER_THAN_TC | HDL_GREATER_THAN_TC | CHOL_DIFF_GREATER_THAN_ME, NA)
  )


