n_surveys <- length(unique(hic$id_study))
original_size <- nrow(hic)

# limit to those between 40 and 79 years of age
hic <- mutate(hic, exclude_age = as.numeric(age < 40 | age > 79))

exclude_age <- sum(hic$exclude_age)

# exclude if less than 5 years within a particular age range
hic <- 
  hic %>%
  group_by(id_study) %>%
  mutate(
    maxage = max(age, na.rm = TRUE),
    minage = min(age, na.rm = TRUE),
    exclude_age_range = as.numeric(
      (age >= 40 & age <= 49 & (maxage < 45 | minage > 45)) |
      (age >= 50 & age <= 59 & (maxage < 55 | minage > 55)) |
      (age >= 60 & age <= 69 & (maxage < 65 | minage > 65)) |
      (age >= 70 & age <= 79 & (maxage < 75 | minage > 75))
    )
  ) %>%
  ungroup()

exclude_age_range <- sum(hic$exclude_age_range & !hic$exclude_age)  

# exclude if no data on non-hdl tc
hic <- 
  hic %>%
  mutate(missing_chol = (is.na(tc_cleaned) | is.na(hdl_cleaned)))

exclude_missing_chol <- sum(hic$missing_chol & !hic$exclude_age_range & !hic$exclude_age)  

# count number for which there are missing risk factors
hic <- 
  hic %>%
  mutate(missing_rf = (
    is.na(sex) |
      is.na(age) |
      is.na(sbp_final_cleaned) |
      is.na(self_diab) | is.na(smoker) | is.na(drug_chol)
  ))

exclude_missing_rf <- sum(hic$missing_rf & !hic$missing_chol & !hic$exclude_age_range & !hic$exclude_age)  

sample_size <- original_size - exclude_age - exclude_age_range - exclude_missing_chol - exclude_missing_rf

a1 <-
  paste0(
    'Participants from ',
    n_surveys,
    '\nnational health surveys\n(N = ',
    format(original_size, big.mark = ","),
    ')'
  )
b1 <- ''
c1 <- ''
d1 <- ''
e1 <- ''
f1 <- paste0('Final analytic dataset\n(N = ',
             format(sample_size, big.mark = ","),
             ')')
a2 <- ''
b2 <-
  paste0('Age outside of\n 40-79 year target\n(N = ',
         format(exclude_age, big.mark = ","),
         ')')
c2 <-
  paste0('Survey includes <5 years\nof 10-year age group\n(N = ',
         format(exclude_age_range, big.mark = ","),
         ')')
d2 <-
  paste0(
    'Missing values on\nTC or HDL-C\n(N = ',
    format(exclude_missing_chol, big.mark = ","),
    ')'
  )

e2 <- paste0(
  'Missing values on\nother risk factors\n(N = ',
  format(exclude_missing_rf, big.mark = ","),
  ')'
)

f2 <- ''

  
# Create a node dataframe
ndf <- create_node_df(
  n = 12,
  label = c(a1, b1, c1, d1, e1, f1,  # Column 1
            a2, b2, c2, d2, e2, f2), # Column 2
  style = c('solid', 'invis', 'invis', 'invis', 'invis', 'solid',  # Column 1
            'invis', 'solid', 'solid', 'solid', 'solid', 'invis'), # Column 2
  shape = c('box', 'point', 'point', 'point', 'point', 'box',  # Column 1 
            'plaintext', 'box', 'box', 'box', 'box', 'point'), # Column 2
  width = c(3, 0.001, 0.001, 0.001, 0.001, 3, # Column 1
            3, 3, 3, 3, 3, 3), # Column 2
  height = c(1, 0.001, 0.001, 0.001, 0.001, 1, # Column 1
             1, 1, 1, 1, 1, 1), # Column 2
  color = 'black',
  fontcolor = 'black',
  fontsize = c(rep(14, 12)),
  fontname = c(rep('Helvetica', 12)),
  penwidth = 1.5,
  fixedsize = 'true',
  x = c(0, 0, 0, 0, 0, 0,
        3.5, 3.5, 3.5, 3.5, 3.5, 3.5),
  y = c(6.25, 5, 3.75, 2.5, 1.25, 0, 
        6.25, 5, 3.75, 2.5, 1.25, 0)
  )

# Create an edge dataframe
edf <- create_edge_df(
  from = c(1, 2, 3, 4, 5,   # Column 1
           7, 8, 9, 10, 11, # Column 2
           2, 3, 4, 5       # Horizontals
  ),
  to = c(2, 3, 4, 5, 6,    # Column 1
         8, 9, 10, 11, 12, # Column 2
         8, 9, 10, 11      # Horizontals
  ),
  arrowhead = c('none', 'none', 'none', 'none', 'normal', # Column 1
                'none', 'none', 'none', 'none', 'none',   # Column 2
                'normal', 'normal', 'normal', 'normal'    # Horizontals
  ),
  color = c('black', 'black', 'black', 'black', 'black', # Column 1
            '#00000000', '#00000000', '#00000000', '#00000000', '#00000000', # Column 2
            'black', 'black', 'black', 'black' # Horizontals
  ),
  constraint = c(rep('true', 10), # Columns
                 rep('false', 4)  # Horizontals
  )
)

g <- create_graph(ndf,
                  edf)

g

render_graph(g, layout = "neato")

export_graph(graph = g,
             file_type = "pdf",
             file_name = "3_figures/figS_STROBE.pdf")



