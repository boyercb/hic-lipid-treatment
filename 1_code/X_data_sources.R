
# combine hic sample with meta-data from NCDRisC database
data_sources <- select(hic, id_study)
data_sources <- distinct(data_sources)
data_sources <- left_join(data_sources, ncdrisc)

# subset to relevant information for Appendix table
data_sources <- 
  data_sources %>% 
  filter(!is.na(Country)) %>%
  arrange(Country, start_year) %>%
  mutate(
    no = row_number(),
    survey = if_else(
      str_detect(survey, survey_short), 
      survey, 
      paste0(survey, " (", survey_short, ")")
    ),
    survey = if_else(
      survey == "National Health and Nutrition Examination Survey (NHANES)", 
      "US NHANES 2017-2018",
      survey
    ),
    survey = if_else(
      survey == "Epidemiological study of the chances of prevention, early recognition and optimal treatment of chronic diseases in an elderly population (ESTHER)", 
      "ESTHER",
      survey
    ),
    survey = if_else(
      survey == "Epidemiological study of the chances of prevention, early recognition and optimal treatment of chronic diseases in an elderly population (ESTHER)", 
      "ESTHER",
      survey
    ),
    survey = if_else(
      str_detect(survey, "CVDRF"), 
      "Cardiovascular Risk Factors Survey in Murcia (CVDRF)",
      survey
    ),
    survey = str_replace(survey, "cArdiovascular", "Cardiovascular"),
    survey = str_replace(survey, "�", "ó"),
    survey = str_replace(survey, "\\(SHIPTREND\\)", ""),
    survey = str_replace(survey, "Study for the Evaluation of Prevalence of Hypertension and Cardiovascular Risk among the Adult Population of Romania - ", ""),
    survey = str_replace(survey, "- National epidemiological study of lipid disorders and selected risk factors of cardiovascular disease in primary health care in Poland", "")
  ) %>%
  select(
    no,
    Country,
    start_year,
    end_year,
    survey,
    age_range_F,
    age_range_M,
    N_tc_F,
    N_tc_M
    # is_ldl_calc,
    # is_ldl_fasting,
    # device_tc
  )

# create latex table
kable(
  x = data_sources,
  format = "latex",
  booktabs = TRUE,
  longtable = TRUE,
  linesep = linesep(as.vector(table(data_sources$Country))),
  caption = "Data sources from 12 high-income countries with laboratory lipid values",
  col.names = c(
    " ",
    "Country",
    "Start",
    "End",
    "Survey name",
    "Women",
    "Men",
    "Women",
    "Men"
    # "LDL calc.",
    # "LDL fasting",
    # "Device"
  )
) %>%
  kable_styling(latex_options = c("repeat_header"),
                font_size = 7) %>%
  add_header_above(c(" " = 5, "Age range" = 2, "Sample size" = 2)) %>%
  save_kable("2_tables/data_sources.tex")
