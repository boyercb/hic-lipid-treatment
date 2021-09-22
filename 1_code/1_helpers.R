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


source("1_code/globorisk/globorisk.R")

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
