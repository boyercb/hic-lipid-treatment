# read in data from high income countries from Bin
hic <- read_rds("0_data/Selected studies with TC and HDL or LDL and treatment.RDS")
hic <- as_tibble(hic)

# read in meta-data for all NCDRisC surveys
ncdrisc <- read_csv("0_data/metadata_lipids_20210206_wNmed.csv")

# drop home care dataset
hic <- filter(hic, id_study != "GBR_2000_HSE")