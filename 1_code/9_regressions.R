d <- filter(hic, !(exclude_age | exclude_age_range | missing_chol))
d$post2010 = ifelse(d$mid_year >= 2010, 1, 0)
d$woman = as.numeric(d$sex) - 1


# long term trends --------------------------------------------------------

fits <- pmap(
  list(
    outcome = list(
      "tc_cleaned",
      "hdl_cleaned",
      "nonhdl",
      "eligible",
      "treated",
      "controlled",
      "untreated_severe"
    )
  ), 
  function(outcome) {

    f1 <- feols(
      fml = as.formula(paste0(outcome, " ~ woman + age + mid_year | iso")),
      data = d,
      subset = if (outcome %in% c("tc_cleaned", "hdl_cleaned", "nonhdl")) {
          rep(TRUE, nrow(d))
        } else if (outcome %in% c("eligible", "treated_pop")) {
          !d$missing_rf & !d$missing_chol
        } else {
          d$eligible == 1
        },
      vcov = "twoway"
    )
    
    f2 <- feols(
      fml = as.formula(paste0(outcome, " ~ woman + age + mid_year + woman:mid_year | iso")), 
      data = d,
      subset = if (outcome %in% c("tc_cleaned", "hdl_cleaned", "nonhdl")) {
        rep(TRUE, nrow(d))
      } else if (outcome %in% c("eligible", "treated_pop")) {
        !d$missing_rf & !d$missing_chol
      } else {
        d$eligible == 1
      },
      vcov = "twoway"
    )
    
    f3 <- feols(
      fml = as.formula(paste0(outcome, "~ woman + age * mid_year | iso")), 
      data = d,
      subset = if (outcome %in% c("tc_cleaned", "hdl_cleaned", "nonhdl")) {
        rep(TRUE, nrow(d))
      } else if (outcome %in% c("eligible", "treated_pop")) {
        !d$missing_rf & !d$missing_chol
      } else {
        d$eligible == 1
      },
      vcov = "twoway"
    )
    
    list(f1, f2, f3)
  }
)

modelsummary(
  models = list(
    "TC" = fits[[1]][[1]], 
    "HDL-C" = fits[[2]][[1]], 
    "Non-HDL-C" = fits[[3]][[1]],
    "TC" = fits[[1]][[2]], 
    "HDL-C" = fits[[2]][[2]], 
    "Non-HDL-C" = fits[[3]][[2]],
    "TC" = fits[[1]][[3]], 
    "HDL-C" = fits[[2]][[3]], 
    "Non-HDL-C" = fits[[3]][[3]]
    ),
  coef_rename = c(
    "mid_year" = "year",
    "woman:mid_year" = "year $\\times$ woman",
    "age50-59 years" = "age 50-59",
    "age60-69 years" = "age 60-69",
    "age70-79 years" = "age 70-79",
    "age50-59 years:mid_year" = "year $\\times$ age 50-59",
    "age60-69 years:mid_year" = "year $\\times$ age 60-69",
    "age70-79 years:mid_year" = "year $\\times$ age 70-79"
    ),
  output = "latex_tabular",
  stars = TRUE,
  escape = FALSE,
  gof_omit = "(R2 Within)|(R2 Pseudo)|(AIC)|(BIC)|(Log.Lik.)|(Std.Errors)|(RMSE)|(FE)",
  gof_map = tribble(
    ~raw, ~clean, ~fmt, ~omit,
    "nobs", "Observations", 0, F
  ),
  add_rows = data.frame(
    "Country FE", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"
  )
) %>%
  add_header_above(
    c(" " = 1, "Overall" = 3, "By gender" = 3, "By age" = 3)
  ) %>%
  save_kable("2_tables/chol_regs.tex")


modelsummary(
  models = list(
    "Eligible" = fits[[5]][[1]], 
    "Treated" = fits[[6]][[1]], 
    "Controlled" = fits[[7]][[1]],
    "Eligible" = fits[[5]][[2]], 
    "Treated" = fits[[6]][[2]], 
    "Controlled" = fits[[7]][[2]],
    "Eligible" = fits[[5]][[3]], 
    "Treated" = fits[[6]][[3]], 
    "Controlled" = fits[[7]][[3]]
  ),
  coef_rename = c(
    "mid_year" = "year",
    "woman:mid_year" = "year $\\times$ woman",
    "age50-59 years" = "age 50-59",
    "age60-69 years" = "age 60-69",
    "age70-79 years" = "age 70-79",
    "age50-59 years:mid_year" = "year $\\times$ age 50-59",
    "age60-69 years:mid_year" = "year $\\times$ age 60-69",
    "age70-79 years:mid_year" = "year $\\times$ age 70-79"
  ),
  output = "latex_tabular",
  stars = TRUE,
  escape = FALSE,
  gof_omit = "(R2 Within)|(R2 Pseudo)|(AIC)|(BIC)|(Log.Lik.)|(Std.Errors)|(RMSE)|(FE)",
  gof_map = tribble(
    ~raw, ~clean, ~fmt, ~omit,
    "nobs", "Observations", 0, F
  ),
  add_rows = data.frame(
    "Country FE", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"
  )
) %>%
  add_header_above(
    c(" " = 1, "Overall" = 3, "By gender" = 3, "By age" = 3)
  ) %>%
  save_kable("2_tables/treat_regs.tex")
