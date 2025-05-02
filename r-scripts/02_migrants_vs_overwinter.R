## cr birding data

observation_data <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQaf_n9GnQzbDAerYHyjA1zz9t8DjZR_OMHJpxEUK4WY_msNlJEq38bIeSjfd9ZOw/pub?gid=183403795&single=true&output=csv")

## Categorize the bird code into three different categories depending on when their observation_date. spring_migrants when observation_data is before june, autumn_migrants when observation_date was between september and november and lastly overwinterers when observation_date was between june and august. I want this for every year present in the dataframe
# Convert date and extract year
observation_data$observation_date <- as.Date(observation_data$observation_date, format = "%Y-%m-%d")
observation_data$year <- as.integer(format(observation_data$observation_date, "%Y"))
# Add month for logic clarity
observation_data$month <- as.integer(format(observation_data$observation_date, "%m"))

# Define seasonal flags

# Define overwinterers correctly
observation_data$overwinterers <- ifelse(
  (observation_data$month == 11 | observation_data$month == 12) |
    (observation_data$month == 1 | observation_data$month == 2 & observation_data$observation_date <= as.Date(paste0(observation_data$year, "-02-15"))),
  1, 0
)

observation_data$spring_migrants <- ifelse(
  observation_data$observation_date >= as.Date(paste0(observation_data$year, "-02-16")) &
    observation_data$observation_date <= as.Date(paste0(observation_data$year, "-06-15")),
  1, 0
)

observation_data$breeding <- ifelse(
  observation_data$observation_date >= as.Date(paste0(observation_data$year, "-06-16")) &
    observation_data$observation_date <= as.Date(paste0(observation_data$year, "-08-15")),
  1, 0
)

observation_data$autumn_migrants <- ifelse(
  observation_data$observation_date >= as.Date(paste0(observation_data$year, "-08-16")) &
    observation_data$observation_date <= as.Date(paste0(observation_data$year, "-10-31")),
  1, 0
)


observation_data$category <- ifelse(observation_data$spring_migrants == 1, "spring_migrants",
                                    ifelse(observation_data$breeding == 1, "breeding",
                                           ifelse(observation_data$autumn_migrants == 1, "autumn_migrants",
                                                  ifelse(observation_data$overwinterers == 1, "overwinterers", NA))))




## add video information table
factvideo <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRTzpl8cLIfyyycDNx_vbnulWr64YsPvfg0a4R9-q3hWTFq87Znl-odcKJX6LHaKgpCODNia0WpgQwy/pub?gid=2044682262&single=true&output=csv")
list(factvideo$Notes)

ringed_info <- observation_data |>
  filter(bird_code %in% c("Of-CCU/R","Of-HPV/R","Of-JAE/R", "Of-JHM/R","Of-JHY/R","Of-JJV/R","Of-JLO/R","Of-JLU/R","Of-JTN/R","Of-JXM/R","Of-JXN/R","Of-JXY/R","Of-JYL/R","Of-MYV/R","Of-LMN/R","Of-KAH/R","Of-KCY/R", "Of-KET/R","Of-KKH/R","Of-KMC/R", "Of-KMP/R", "Of-KNP/R", "Of-KNX/R", "Of-KMP/R", "Of-KTP/R","Of-KTM/R", "Of-KUL/R", "Of-KUX/R", "Of-KVV/R", "Of-KXM/R", "Of-KYC/R", "Of-LPT/R", "Of-LYK/R", "Of-MJA/R", "Of-MCH/R", "Of-MLC/R", "Of-MTJ/R", "Of-NKE/R", "Of-NUK/R", "Of-NTV/R", "Of-PAE/R", "Of-PKN/r", "Of-PMP/R", "Of-PVK/R", "Of-PLA/R", "Of-KKT/R" )) 

ringed_info <- ringed_info |>
  dplyr::select(bird_code, observation_date, year, category)
