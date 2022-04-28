d <- hic
d$post2010 = ifelse(d$mid_year >= 2010, 1, 0)
d$woman = as.numeric(d$sex) - 1

fe <- list(
  "Non-HDL-C" = feols(
    fml = nonhdl ~ woman + age + woman:post2010 + age:post2010 + woman:age:post2010 | iso + mid_year, 
    data = d,
    vcov = "twoway"
  ),
  "Eligible" = feols(
    fml = eligible ~ woman + age + woman:post2010 + age:post2010 + woman:age:post2010 | iso + mid_year, 
    data = d,
    subset = !d$missing_rf & !d$missing_chol,
    vcov = "twoway"
  ),
  "Treated" = feols(
    fml = treated ~ woman + age + woman:post2010 + age:post2010 + woman:age:post2010 | iso + mid_year, 
    data = d,
    subset = d$eligible == 1,
    vcov = "twoway"
  )
)

modelsummary(models = fe,
             output = "latex",
             stars = TRUE,
             gof_omit = "(R2 Within)|(R2 Pseudo)|(AIC)|(BIC)|(Log.Lik.)")
