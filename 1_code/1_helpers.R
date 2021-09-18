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
    
    v1 <- ifelse(f1 == "identity", v1, paste0(f1, "(", v1, ")"))
    v2 <- ifelse(f2 == "identity", v2, paste0(f2, "(", v2, ")"))
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
