n_surveys <- length(unique(hic$id_study))
original_size <- nrow(hic)

# limit to those between 40 and 79 years of age
hic <- filter(hic, age >= 40 & age <= 79)

exclude_age <- original_size - nrow(hic)

# exclude if less than 5 years within a particular age range
hic <- 
  hic %>%
  group_by(id_study) %>%
  mutate(
    maxage = max(age, na.rm = TRUE),
    minage = min(age, na.rm = TRUE)
  ) %>%
  filter(!(age >= 40 & age <= 49 & (maxage < 45 | minage > 45))) %>%
  filter(!(age >= 50 & age <= 59 & (maxage < 55 | minage > 55))) %>%
  filter(!(age >= 60 & age <= 69 & (maxage < 65 | minage > 65))) %>%
  filter(!(age >= 70 & age <= 79 & (maxage < 75 | minage > 75))) %>%
  ungroup()

exclude_age_range <- original_size - exclude_age - nrow(hic)  

# exclude if no data on non-hdl tc
hic <- 
  hic %>%
  filter(!(is.na(tc_cleaned) | is.na(hdl_cleaned)))

exclude_missing_data <- original_size - exclude_age - exclude_age_range - nrow(hic)  

sample_size <- nrow(hic)

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
e1 <- paste0('Final analytic cohort\n(N = ',
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
    format(exclude_missing_data, big.mark = ","),
    ')'
  )
e2 <- ''

  
# Create a node dataframe
ndf <- create_node_df(
  n = 10,
  label = c(a1, b1, c1, d1, e1, # Column 1
            a2, b2, c2, d2, e2), # Column 2
  style = c('solid', 'invis', 'invis', 'invis', 'solid', # Column 1
            'invis', 'solid', 'solid', 'solid', 'invis'), # Column 2
  shape = c('box', 'point', 'point', 'point', 'box', # Column 1 
            'plaintext', 'box', 'box', 'box', 'point'), # Column 2
  width = c(3, 0.001, 0.001, 0.001, 3, # Column 1
            3, 3, 3, 3, 3), # Column 2
  height = c(1, 0.001, 0.001, 0.001, 1, # Column 1
             1, 1, 1, 1, 1), # Column 2
  fontsize = c(rep(14, 10)),
  fontname = c(rep('Helvetica', 10)),
  penwidth = 1.5,
  fixedsize = 'true',
  x = c(0, 0, 0, 0, 0,
        3.5, 3.5, 3.5, 3.5, 3.5),
  y = c(5, 3.75, 2.5, 1.25, 0, 
        5, 3.75, 2.5, 1.25, 0)
  )

# Create an edge dataframe
edf <- create_edge_df(
  from = c(1, 2, 3, 4, # Column 1
           6, 7, 8, 9, # Column 2
           2, 3, 4 # Horizontals
  ),
  to = c(2, 3, 4, 5, # Column 1
         7, 8, 9, 10, # Column 2
         7, 8, 9 # Horizontals
  ),
  arrowhead = c('none', 'none', 'none', 'normal', # Column 1
                'none', 'none', 'none', 'none', # Column 2
                'normal', 'normal', 'normal' # Horizontals
  ),
  color = c('black', 'black', 'black', 'black', # Column 1
            '#00000000', '#00000000', '#00000000', '#00000000', # Column 2
            'black', 'black', 'black' # Horizontals
  ),
  constraint = c(rep('true', 8), # Columns
                 rep('false', 3) # Horizontals
  )
)

g <- create_graph(ndf,
                  edf)

g

#Not run: but to run this in R Studio, uncomment below
render_graph(g, layout = "neato")

#export_graph(g, file_name = "STROBE.png")



