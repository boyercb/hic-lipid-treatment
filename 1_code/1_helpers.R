# calculate the robust mahalanobis distance 
rmaha <- function(x, alpha = 0.95) {
  if (ncol(x) < 2) stop("matrix must have at least 2 columns to calculate covariance")
  cov <- covMcd(x, alpha)  # robust estimate of the covariance
  mahalanobis(x, cov$center, cov$cov)
}

# tidy wrapper for rmaha
rmaha_calc <- function(data, vars) {
  df <- select(data, {{ vars }})
  rmaha(df)
}

# tidy function for detecting outliers
rmaha_detect <- function(data, vars, trans = NULL, sd = 6, nu = Inf, name = ".outlier", drop = FALSE) {
  df <- select(data, {{ vars }})
  level <- (1 - pnorm(sd)) * 2
  threshold <- 2 * qf(1 - level, 2, nu)
    
  # apply transformation if not null
  if (!is.null(trans)) {
    if (is.function(trans) || is_formula(trans)) {
      df <- mutate(df, across(.cols = {{ vars }}, .fns = trans))
    } 
    if (length(trans) == 1) {
      fn <- rlang::as_function(trans)
      df <- mutate(df, across(.cols = {{ vars }}, .fns = fn))
    } else {
      for (i in seq_along(trans)) {
        if (trans[[i]] != "identity") {
          fn <- rlang::as_function(trans[[i]])
          df <- mutate(df, across(.cols = all_of(i), .fns = fn))
        }
      }
    }
  } 
  
  if (drop) {
    data %>%
      mutate(
        "{name}" := rmaha(df) > threshold
      ) %>%
      pull(name)
  } else {
    data %>%
      mutate(
        "{name}" := rmaha(df) > threshold
      ) 
  }

}

# tidy function for cleaning outliers
rmaha_clean <- function(data, vars, trans = NULL, sd = 6, nu = Inf) {
  data %>%
    rmaha_detect({{ vars }}, sd = sd, nu = nu, trans = trans) %>%
    mutate(across(.cols = {{ vars }}, .fns = ~replace(.x, .outlier == TRUE, NA))) %>%
    select(-.outlier)
}

# tidy function for plotting outliers
rmaha_plot <- function(data, var1, var2, trans = NULL, sd = 6, nu = Inf, bins = 130, limits = NULL) {
  var1 <- tidyselect::eval_select(enquo(var1), data)
  var2 <- tidyselect::eval_select(enquo(var2), data)
  
  df <- rmaha_detect(data, c(names(var1), names(var2)), sd = sd, nu = nu, trans = trans)
  
  v1 <- names(var1)
  v2 <- names(var2)
  
  # apply transformation if not null
  if (!is.null(trans)) {
    if (is.function(trans) || is_formula(trans)) {
      f1 <- deparse(substitute(trans))
      f2 <- f1
    } 
    
    if (length(trans) == 1) {
      f1 <- ifelse(trans == "identity", "", trans)
      f2 <- f1
    } else {
      f1 <- trans[[1]]
      f2 <- trans[[2]]
    }
    
    n1 <- str_remove(v1, "_clean[ed]+")
    n2 <- str_remove(v2, "_clean[ed]+")
    n1 <- str_remove(n1, "_final")
    n2 <- str_remove(n2, "_final")
    
    v1 <- ifelse(f1 == "identity", v1, paste0(f1, "(", v1, ")"))
    v2 <- ifelse(f2 == "identity", v2, paste0(f2, "(", v2, ")"))
    
    n1 <- ifelse(f1 == "identity", n1, paste0(f1, "(", n1, ")"))
    n2 <- ifelse(f2 == "identity", n2, paste0(f2, "(", n2, ")"))
  } 
  

  ggplot(df, aes_string(x = v1, y = v2)) +
    geom_bin2d(bins = bins, na.rm = TRUE) +
    scale_fill_continuous(type = "viridis", limits = limits) +
    theme_bw() +
    geom_point(
      data = filter(df, .outlier == TRUE),
      aes_string(x = v1, y = v2, color = ".outlier"),
      shape = 21,
      size = 3
    )  +
    scale_color_manual(
      name = "",
      values = c("red", "red"),
      labels = c("outliers")
    ) + 
    labs(
      x = n1,
      y = n2
    )
}
  
qq_plots <- function(data, vars, trans = NULL) {
  
  df <- 
    pivot_longer(
      data = data,
      cols = {{ vars }},
      names_to = ".names",
      values_to = ".values"
    ) 
  
  # apply transformation if not null
  if (!is.null(trans)) {
    f <- deparse(substitute(trans))
    df <- mutate(df, .values = trans(.values))
    df <- mutate(df, .names = paste0(f, "(", .names, ")"))
  } 
  
  ggplot(df, aes(sample = .values)) +
    facet_wrap(~.names, scales = "free_y") +
    stat_qq(na.rm = TRUE) +
    stat_qq_line(na.rm = TRUE) +
    theme_bw()
}

test <- function(data, v1, v2) {
  tidyselect::eval_select(rlang::enquos(v1, v2), data)
}

# calculate worst possible error for difference between total cholesterol and
# sum of hdl and ldl
margin_of_error <- function(tc, hdl, ldl) {
  -(0.089 * tc + (0.13 * hdl + 0.12 * ldl))
}

# function for creating kable linesep string based on numeric inputs
linesep <- function(x, y = character()) {
  if (!length(x))
    return(y)
  linesep(x[-length(x)], c(rep('', x[length(x)] - 1), '\\addlinespace', y))
}

# function to apply survey design to pooled data where survey variables may be missing
survey_design <- function(data, psu, strata, weights) {
  if(!any(is.na(data[[psu]]))) {
    psuvar <- quo(psu)
  } else {
    psuvar <- NULL
  }
  
  if(!any(is.na(data[[strata]]))) {
    stratavar <- quo(strata)
  } else {
    stratavar <- NULL
  }
  
  if(!any(is.na(data[[weights]]))) {
    wtvar <- quo(weights)
  } else {
    wtvar <- NULL
  }
  
  as_survey_design(
    data,
    ids = {{ psuvar }},
    strata = {{ stratavar }},
    weights = {{ wtvar }},
    nest = TRUE
  ) 
}

lm_eqn <- function(df) {
  m <- lm(nonhdl ~ ldl_cleaned, df)
  
  eq <- substitute(
    expr = italic(nonhdl) == a + b %.% italic(ldl)*","
      ~~italic(R)^2~"="~r2*","~~italic(rho)~"="~r, 
    env = list(
      a = format(unname(coef(m)[1]), digits = 2),
      b = format(unname(coef(m)[2]), digits = 2),
      r2 = format(summary(m)$r.squared, digits = 3),
      r = format(sqrt(summary(m)$r.squared), digits = 3)
    )
  )
  as.character(as.expression(eq));
}

convert_to_mg <- function(x) {
  x * 38.67
}

convert_to_mmol <- function(x) {
  x / 38.67
}

# specifies the number of decimal places to print, i.e. doesn't chop off 
# trailing zeros like round() does
specd <- function(x, k)
  trimws(format(round(x, k), nsmall = k))

# pretty print confidence intervals, depends on specd()
print_ci <-
  function(est, lwr, upr, k = 2)
    paste0(specd(est, k), " (", specd(lwr, k), ", ", specd(upr, k), ")")


#' ACC/AHA 2013 ASCVD risk score
#'
#' Computes 10-year risk for hard ASCVD event (defined as first occurrence of
#' non-fatal myocardial infarction (MI), congestive heart disease (CHD) death,
#' or fatal or nonfatal stroke).
#'
#' @param race patient race (white, aa)
#' @param gender patient gender (male, female)
#' @param age patient age (years)
#' @param totchol Total cholesterol (mg/dL)
#' @param hdl HDL cholesterol (mg/dL)
#' @param sbp Systolic blood pressure (mm Hg)
#' @param bp_med Patient is on a blood pressure medication (1=Yes, 0=No)
#' @param smoker Current smoker (1=Yes, 0=No)
#' @param diabetes Diabetes (1=Yes, 0=No)
#' @param ... Additional predictors can be passed and will be ignored
#'
#'
#' @return Estimated 10-Y Risk for hard ASCVD (percent)
#'
#' @export
#'
#' @examples
#' library(CVrisk)
#' ascvd_10yr_accaha(
#'   race = "aa", gender = "male", age = 55,
#'   totchol = 213, hdl = 50, sbp = 140,
#'   bp_med = 0, smoker = 0, diabetes = 0
#' )
#' @references
#' Goff, David C., et al. "2013 ACC/AHA guideline on the assessment of
#' cardiovascular risk: a report of the American College of
#' Cardiology/American Heart Association Task Force on Practice
#' Guidelines." Journal of the American College of Cardiology 63.25
#' Part B (2014): 2935-2959.
ascvd_10yr_accaha <- function(race = "white", gender = c("male", "female"),
                              age, totchol, hdl, sbp,
                              bp_med, smoker, diabetes, 
                              baseline_survival = c(0.9665, 0.9533, 0.9144, 0.8954),  ...) {
  if (any(!race %in% c("aa", "white") | missing(race))) {
    stop("race must be either 'aa' or 'white'")
  }
  
  if (any(!gender %in% c("male", "female") | missing(gender))) {
    stop("gender must be either 'male' or 'female'")
  }
  
  if (any(!is.numeric(age) | age < 1 | age > 120 | missing(age))) {
    stop("age must be a valid numeric value'")
  }
  
  if (any(!is.numeric(totchol) | totchol < 1 | totchol > 999 | missing(totchol))) {
    stop("totchol must be a valid numeric value'")
  }
  
  
  d <- tribble(
    ~race, ~gender, ~ln_age, ~ln_age_squared, ~ln_totchol, ~ln_age_totchol, ~ln_hdl, ~ln_age_hdl, ~ln_treated_sbp, ~ln_age_treated_sbp, ~ln_untreated_sbp, ~ln_age_untreated_sbp, ~smoker, ~ln_age_smoker, ~diabetes, ~group_mean, ~baseline_survival,
    "white", "female", -29.799, 4.884, 13.54, -3.114, -13.578, 3.149, 2.019, 0, 1.957, 0, 7.574, -1.665, 0.661, -29.18, baseline_survival[1],
    "aa", "female", 17.114, 0, 0.94, 0, -18.92, 4.475, 29.291, -6.432, 27.82, -6.087, 0.691, 0, 0.874, 86.61, baseline_survival[2],
    "white", "male", 12.344, 0, 11.853, -2.664, -7.99, 1.769, 1.797, 0, 1.764, 0, 7.837, -1.795, 0.658, 61.18, baseline_survival[3],
    "aa", "male", 2.469, 0, 0.302, 0, -0.307, 0, 1.916, 0, 1.809, 0, 0.549, 0, 0.645, 19.54, baseline_survival[4]
  )
  
  r <- mapply(
    FUN = function(race, gender)
      which(d$race == race & d$gender == gender),
    race = race,
    gender = gender
  )
  
  pooled_coef <- d[r, ]
  
  sbp_treated <- ifelse(bp_med == 1, sbp, 1)
  sbp_untreated <- ifelse(bp_med == 0, sbp, 1)
  
  indv_sum <- log(age) * pooled_coef$ln_age +
    log(age)^2 * pooled_coef$ln_age_squared +
    log(totchol) * pooled_coef$ln_totchol +
    log(age) * log(totchol) * pooled_coef$ln_age_totchol +
    log(hdl) * pooled_coef$ln_hdl +
    log(age) * log(hdl) * pooled_coef$ln_age_hdl +
    log(sbp_treated) * pooled_coef$ln_treated_sbp +
    log(sbp_treated) * log(age) * pooled_coef$ln_age_treated_sbp +
    log(sbp_untreated) * pooled_coef$ln_untreated_sbp +
    log(sbp_untreated) * log(age) * pooled_coef$ln_age_untreated_sbp +
    smoker * pooled_coef$smoker +
    smoker * log(age) * pooled_coef$ln_age_smoker +
    diabetes * pooled_coef$diabetes
  
  risk_score <- (1 - (pooled_coef$baseline_survival ^ exp(indv_sum - pooled_coef$group_mean)))
  
  return(risk_score)
}



#' Framingham 2008 ASCVD risk score (with lab measurement)
#'
#' Computes 10-year risk for ASCVD event (coronary death, myocardial
#' infarction (MI), coronary insufficiency, angina, ischemic stroke,
#' hemorrhagic stroke, transient ischemic attack, peripheral artery disease,
#' or heart failure).
#'
#' @param gender patient gender (male, female)
#' @param age patient age (years), between 30 and 74
#' @param hdl HDL cholesterol (mg/dL)
#' @param totchol Total cholesterol (mg/dL)
#' @param sbp Systolic blood pressure (mm Hg)
#' @param bp_med Patient is on a blood pressure medication (1=Yes, 0=No)
#' @param smoker Current smoker (1=Yes, 0=No)
#' @param diabetes Diabetes (1=Yes, 0=No)
#' @param ... Additional predictors can be passed and will be ignored
#'
#' @return Estimated 10-Y Risk for hard ASCVD event (percent)
#'
#' @export
#'
#' @examples
#' library(CVrisk)
#' ascvd_10y_frs(
#'   gender = "male", age = 55,
#'   hdl = 50, totchol = 213, sbp = 140,
#'   bp_med = 0, smoker = 0, diabetes = 0
#' )
#'
#' # 16.7
#' @references
#' D’agostino, R.B., Vasan, R.S., Pencina, M.J., Wolf, P.A., Cobain, M.,
#' Massaro, J.M. and Kannel, W.B., 2008. General cardiovascular risk
#' profile for use in primary care: the Framingham Heart Study.
#' Circulation, 117(6), pp.743-753.
ascvd_10yr_frs <- function(gender = c("male", "female"),
                           age, hdl, totchol, sbp,
                           bp_med, smoker, diabetes,
                           baseline_survival = c(0.88936, 0.95012), ...) {
  if (any(!gender %in% c("male", "female") | missing(gender))) {
    stop("gender must be either 'male' or 'female'")
  }
  
  if (any(!is.numeric(age) | missing(age))) {
    stop("age must be a valid numeric value'")
  }
  
  # if (any(age < 1 | age > 120)) {
  #   return(NA)
  # }
  
  
  # retrieve model coefficients
  d <- tribble(
    ~gender,
    ~ln_age,
    ~ln_totchol,
    ~ln_hdl,
    ~ln_untreated_sbp,
    ~ln_treated_sbp,
    ~smoker,
    ~diabetes,
    ~group_mean,
    ~baseline_survival,
    "male",
    3.06117,
    1.12370,
    -0.93263,
    1.93303,
    1.99881,
    0.65451,
    0.57367,
    23.9802,
    baseline_survival[1],
    "female",
    2.32888,
    1.20904,
    -0.70833,
    2.76157,
    2.82263,
    0.52873,
    0.69154,
    26.1931,
    baseline_survival[2]
  )
  
  r <- mapply(
    FUN = function(race, gender)
      which(d$gender == gender),
    gender = gender
  )
  
  model_coef <- d[r, ]
  
  sbp_treated <- ifelse(bp_med == 1, sbp, 1)
  sbp_untreated <- ifelse(bp_med == 0, sbp, 1)
  
  indv_sum <- log(age) * model_coef$ln_age +
    log(hdl) * model_coef$ln_hdl +
    log(totchol) * model_coef$ln_totchol +
    log(sbp_treated) * model_coef$ln_treated_sbp +
    log(sbp_untreated) * model_coef$ln_untreated_sbp +
    smoker * model_coef$smoker +
    diabetes * model_coef$diabetes
  
  risk_score <- (1 - (model_coef$baseline_survival ^ exp(indv_sum - model_coef$group_mean)))
  
  return(risk_score)  
}

#' Framingham 2008 ASCVD risk score (no lab measurement)
#'
#' Computes 10-year risk for ASCVD event (coronary death, myocardial
#' infarction (MI),coronary insufficiency, angina, ischemic stroke,
#' hemorrhagic stroke, transient ischemic attack, peripheral artery
#' disease, or heart failure).
#'
#' @param gender patient gender (male, female)
#' @param age patient age (years), between 30 and 74
#' @param bmi Body mass index (kg/m2)
#' @param sbp Systolic blood pressure (mm Hg)
#' @param bp_med Patient is on a blood pressure medication (1=Yes, 0=No)
#' @param smoker Current smoker (1=Yes, 0=No)
#' @param diabetes Diabetes (1=Yes, 0=No)
#' @param ... Additional predictors can be passed and will be ignored
#'
#' @return Estimated 10-Y Risk for hard ASCVD (percent)
#'
#' @export
#'
#' @examples
#' library(CVrisk)
#' ascvd_10y_frs_simple(
#'   gender = "male", age = 55,
#'   bmi = 30, sbp = 140,
#'   bp_med = 0, smoker = 0, diabetes = 0
#' )
#'
#' # 16.7
#' @references
#' D’agostino, R.B., Vasan, R.S., Pencina, M.J., Wolf, P.A., Cobain, M.,
#' Massaro, J.M. and Kannel, W.B., 2008. General cardiovascular risk
#' profile for use in primary care: the Framingham Heart Study.
#' Circulation, 117(6), pp.743-753.
ascvd_10yr_frs_simple <- function(gender = c("male", "female"),
                                  age, bmi, sbp,
                                  bp_med, smoker, diabetes,
                                  baseline_survival = c(0.88431, 0.94833), ...) {
  
  if (any(!gender %in% c("male", "female") | missing(gender))) {
    stop("gender must be either 'male' or 'female'")
  }
  
  if (any(!is.numeric(age) | missing(age))) {
    stop("age must be a valid numeric value'")
  }
  
  if (any(age < 1 | age > 120)) {
    return(NA)
  }
  
  # retrieve model coefficients
  d <- tribble(
    ~gender,
    ~ln_age,
    ~ln_bmi,
    ~ln_untreated_sbp,
    ~ln_treated_sbp,
    ~smoker,
    ~diabetes,
    ~group_mean,
    ~baseline_survival,
    "male",
    3.11296,
    0.79277,
    1.85508,
    1.92672,
    0.70953,
    0.53160,
    23.9388,
    baseline_survival[1],
    "female",
    2.72107,
    0.51125,
    2.81291,
    2.88267,
    0.61868,
    0.77763,
    26.0145,
    baseline_survival[2]
  )
  
  r <- mapply(
    FUN = function(race, gender)
      which(d$gender == gender),
    gender = gender
  )
  
  model_coef <- d[r, ]
  
  sbp_treated <- ifelse(bp_med == 1, sbp, 1)
  sbp_untreated <- ifelse(bp_med == 0, sbp, 1)
  
  indv_sum <- log(age) * model_coef$ln_age +
    log(bmi) * model_coef$ln_bmi +
    log(sbp_treated) * model_coef$ln_treated_sbp +
    log(sbp_untreated) * model_coef$ln_untreated_sbp +
    smoker * model_coef$smoker +
    diabetes * model_coef$diabetes
  
  risk_score <- (1 - (model_coef$baseline_survival ^ exp(indv_sum - model_coef$group_mean)))
  
  return(risk_score)
}

# facet_rep_grid <- function (..., repeat.tick.labels = FALSE) {
#   f <- ggplot2::facet_grid(...)
#   rtl <- reduce.ticks.labels.settings(repeat.tick.labels)
#   params <- append(f$params, list(repeat.tick.labels = rtl))
#   
#   ggplot2::ggproto(NULL,
#                    FacetGridRepeatLabels,
#                    shrink = f$shrink,
#                    params = params)
# }
# 
# reduce.ticks.labels.settings <- function (ticks) {
#   if (length(ticks) == 0) 
#     return(character(0))
#   if (length(ticks) == 1 && ticks == FALSE) 
#     return(character(0))
#   if (length(ticks) == 1 && ticks == TRUE) 
#     return(c("top", "right", "bottom", "left"))
#   ticks <- tolower(ticks)
#   ticks <- match.arg(
#     tolower(ticks),
#     c("none", "all", "x",
#       "y", "left", "top", "bottom", "right"),
#     several.ok = TRUE
#   )
#   if ("none" %in% ticks) 
#     return(character(0))
#   if ("all" %in% ticks) 
#     return(c("top", "right", "bottom", "left"))
#   if ("x" %in% ticks) 
#     ticks <- c(ticks[ticks != "x"], "top", "bottom")
#   if ("y" %in% ticks) 
#     ticks <- c(ticks[ticks != "y"], "right", "left")
#   return(ticks)
# }
# 
# FacetGridRepeatLabels <- ggplot2::ggproto(
#   'FacetGridRepeatLabels',
#   `_inherit` = ggplot2::FacetGrid,
#   draw_panels  = function(panels,
#                           layout,
#                           x_scales,
#                           y_scales,
#                           ranges,
#                           coord,
#                           data,
#                           theme,
#                           params) {
#     table <-
#       ggplot2::FacetGrid$draw_panels(panels,
#                                      layout,
#                                      x_scales,
#                                      y_scales,
#                                      ranges,
#                                      coord,
#                                      data,
#                                      theme,
#                                      params)
#     
#     # Add axes across all panels
#     panels <-
#       table$layout[grepl("^panel", table$layout$name), , drop = FALSE]
#     panels$col <-
#       as.integer(as.factor(panels$l))
#     panels$row <-
#       as.integer(as.factor(panels$t))
#     
#     axis_b <-
#       table$grobs[grepl('axis-b-[[:digit:]]+', table$layout$name)]
#     axis_l <-
#       table$grobs[grepl('axis-l-[[:digit:]]+', table$layout$name)]
#     axis_t <-
#       table$grobs[grepl('axis-t-[[:digit:]]+', table$layout$name)]
#     axis_r <-
#       table$grobs[grepl('axis-r-[[:digit:]]+', table$layout$name)]
#     
#     if (!'bottom' %in% params$repeat.tick.labels)
#       axis_b <- lapply(axis_b, remove_labels_from_axis)
#     if (!'left' %in% params$repeat.tick.labels)
#       axis_l <- lapply(axis_l, remove_labels_from_axis)
#     if (!'top' %in% params$repeat.tick.labels)
#       axis_t <- lapply(axis_t, remove_labels_from_axis)
#     if (!'right' %in% params$repeat.tick.labels)
#       axis_r <- lapply(axis_r, remove_labels_from_axis)
#     
#     
#     panel_range <-
#       find_panel(table)
#     panel_range[, c('col', 'row')] <-
#       c(max(panels$col), max(panels$row))
#     
#     l_axis_column_added <-
#       logical(panel_range$col)
#     r_axis_column_added <-
#       logical(panel_range$col)
#     t_axis_row_added <-
#       logical(panel_range$row)
#     b_axis_row_added <-
#       logical(panel_range$row)
#     
#     for (i in nrow(panels):1) {
#       p <- panels[i, ]
#       # Bottom
#       if (p$row != panel_range$row &
#           !inherits(axis_b[[p$col]], 'zeroGrob')) {
#         # panel not in bottom row, (add row), add axis
#         coord <-
#           table$layout[table$layout$name == p$name,]
#         if (b_axis_row_added[p$row] == FALSE) {
#           b_axis_row_added[p$row] <- TRUE
#           table <-
#             gtable::gtable_add_rows(table, max_height(axis_b), coord$b)
#         }
#         table <-
#           gtable::gtable_add_grob(
#             table,
#             axis_b[[p$col]],
#             t = coord$b + 1,
#             l = coord$l,
#             r = coord$r,
#             clip = 'off',
#             name = sprintf('axis-b-%d-%d', p$col, p$row)
#           )
#       }
#       # Left
#       if (p$col > 1 &
#           !inherits(axis_l[[p$row]], 'zeroGrob')) {
#         # panel is not left-most column, (add column), add axis
#         coord <-
#           table$layout[table$layout$name == p$name,]
#         if (l_axis_column_added[p$col] == FALSE) {
#           l_axis_column_added[p$col] <- TRUE
#           table <-
#             gtable::gtable_add_cols(table, max_width(axis_l), coord$l - 1)
#           table <-
#             gtable::gtable_add_grob(
#               table,
#               axis_l[[p$row]],
#               t = coord$t,
#               b = coord$b,
#               l = coord$l,
#               clip = 'off',
#               name = sprintf('axis-l-%d-%d', p$row, p$col)
#             )
#         } else {
#           table <-
#             gtable::gtable_add_grob(
#               table,
#               axis_l[[p$row]],
#               t = coord$t,
#               b = coord$b,
#               l = coord$l - 1,
#               clip = 'off',
#               name = sprintf('axis-l-%d-%d', p$row, p$col)
#             )
#         }
#       }
#       # Top
#       if (p$row > 1 &
#           !inherits(axis_t[[p$col]], 'zeroGrob')) {
#         # panel not in top row, (add row), add axis
#         coord <-
#           table$layout[table$layout$name == p$name,]
#         if (t_axis_row_added[p$row] == FALSE) {
#           t_axis_row_added[p$row] <- TRUE
#           table <-
#             gtable::gtable_add_rows(table, max_height(axis_t), coord$t - 1)
#           table <-
#             gtable::gtable_add_grob(
#               table,
#               axis_t[[p$col]],
#               t = coord$t,
#               l = coord$l,
#               r = coord$r,
#               clip = 'off',
#               name = sprintf('axis-t-%d-%d', p$col, p$row)
#             )
#         } else {
#           table <-
#             gtable::gtable_add_grob(
#               table,
#               axis_t[[p$col]],
#               t = coord$t - 1,
#               l = coord$l,
#               r = coord$r,
#               clip = 'off',
#               name = sprintf('axis-t-%d-%d', p$col, p$row)
#             )
#         }
#         
#       }
#       # Right
#       if (p$col != panel_range$col &
#           !inherits(axis_l[[p$row]], 'zeroGrob')) {
#         # panel is not right-most, (add colun), add axis
#         coord <-
#           table$layout[table$layout$name == p$name,]
#         if (r_axis_column_added[p$col] == FALSE) {
#           r_axis_column_added[p$col] <- TRUE
#           table <-
#             gtable::gtable_add_cols(table, max_width(axis_r), coord$r)
#         }
#         table <-
#           gtable::gtable_add_grob(
#             table,
#             axis_r[[p$row]],
#             t = coord$t,
#             b = coord$b,
#             l = coord$r + 1,
#             clip = 'off',
#             name = sprintf('axis-r-%d-%d', p$row, p$col)
#           )
#       }
#       
#     }
#     
#     table
#   }
# )
# 
# remove_labels_from_axis <- function(axisgrob) {
#   if (inherits(axisgrob, 'zeroGrob')) return(axisgrob)
#   a <- which(sapply(axisgrob$children, `[[`, i='name') == 'axis')
#   d <- grepl('titleGrob', sapply(axisgrob$children[[a]]$grobs, `[[`, i='name'))
#   if (sum(d) > 0) {
#     axisgrob$children[[a]] <- do.call(gList, axisgrob$children[[a]]$grobs[!d])
#     if (length(axisgrob$width$arg1) == 2) axisgrob$width$arg1 <- axisgrob$width$arg1[attr(axisgrob$width$arg1, 'unit') != 'grobwidth']
#     if (length(axisgrob$height$arg1) == 2) axisgrob$height$arg1 <- axisgrob$height$arg1[attr(axisgrob$height$arg1, 'unit') != 'grobheight']
#     #if (length(axisgrob$children[[a]]$heights) == 2) axisgrob$children[[a]]$heights <- axisgrob$children[[a]]$heights[!d]
#   }
#   axisgrob
# }
