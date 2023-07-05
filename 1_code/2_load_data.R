# read in data from high income countries from Bin
hic <- read_rds("0_data/Selected studies with TC and HDL or LDL and treatment_20230628.RDS")
hic <- as_tibble(hic)

# read in meta-data for all NCDRisC surveys
ncdrisc <-
  read_csv("0_data/metadata_lipids_20210206_wNmed.csv",
           show_col_types = FALSE)

# drop surveys that we won't use
drop_list <- c(
  "GBR_2000_HSE",      # home care survey, weird population and too small
  # "GBR_1987_DNS",
  # "GBR_2010_NDNS",   # NDNS are super small lipid numbers
  # "GBR_2014_NDNS", 
  # "GBR_2016_NDNS", 
  # "GBR_2018_NDNS", 
  "GBR_2015_NSHD",      # longitudinal birth cohort among those born in 1948
  "GBR_2017_BCS70",     # longitudinal birth cohort among those born in 1970
  "FIN_2001_YFS_rural", # no one in age range 40 - 79 
  "FIN_2001_YFS_urban",
  "CRI_2005_CRELES",    # Costa Rica (not high income)
  "CRI_2007_CRELES",
  "CRI_2011_CRELES",
  "CRI_2010_CRFS",
  "CRI_2014_CRFS",
  "MEX_2006_ENSANUT",   # Mexico (not high income)
  "MEX_2019_ENSANUT", 
  # "MLT_2015_SAHHTEK",   # weird values
  "PER_2005_ENIN",      # Peru (not high income)
  "PER_2018_VIANEV", 
  "ROU_2012_SEPHAR",    # Romania (no longer high income)
  "ROU_2016_SEPHAR"
)

hic <- filter(hic, !id_study %in% drop_list)

hic <- 
  hic %>%
  filter(mid_year >= 1990) %>%
  mutate(
    Country = replace(Country, id_study == "GBR_1987_DNS", "United Kingdom"),
    Country = replace(Country, id_study == "CHL_2010_ENS", "Chile"),
    Country = replace(Country, id_study == "AUS_1983_RFPS", "Australia"),
    Country = replace(Country, id_study == "AUS_1989_RFPS", "Australia"),
    Country = replace(Country, id_study == "IRL_2010_TILDA", "Ireland"),
    hip_cleaned = replace(hip_cleaned, hip_cleaned == 999, NA),
    waist_cleaned = replace(waist_cleaned, is.nan(waist_cleaned), NA)
  )
