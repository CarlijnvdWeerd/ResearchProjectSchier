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
  filter(bird_code %in% c(
    "Of-CCU/R", "Of-HPL/R", "Of-HPT/R", "Of-HNW/R", "Of-HPV/R", "Of-JAE/R", "Of-JHM/R",
    "Of-JHY/R", "Of-JJV/R", "Of-JLH/R", "Of-JLO/R", "Of-JLU/R", "Of-JNH/R", "Of-JTL/R",
    "Of-JTN/R", "Of-JVY/R", "Of-JXM/R", "Of-JXN/R", "Of-JXY/R", "Of-JYL/R", "Of-KAH/R",
    "Of-KCY/R", "Of-KET/R", "Of-KJU/R", "Of-KKH/R", "Of-KKM/R", "Of-KKT/R", "Of-KMC/R",
    "Of-KMY/R", "Of-KNP/R", "Of-KNX/R", "Of-KPM/R", "Of-KTE/R", "Of-KTM/R", "Of-KTP/R",
    "Of-KUH/R", "Of-KUL/R", "Of-KUX/R", "Of-KVV/R", "Of-KXM/R", "Of-LCT/R", "Of-LMN/R",
    "Of-LPT/R", "Of-LYK/R", "Of-MAU/R", "Of-MCH/R", "Of-MCP/R", "Of-MJA/R", "Of-MLC/R",
    "Of-MTJ/R", "Of-MYV/R", "Of-NEL/R", "Of-NKE/R", "Of-NLJ/R", "Of-NLY/R", "Of-NTV/R",
    "Of-NUK/R", "Of-PAE/R", "Of-PAJ/R", "Of-PHT/R", "Of-PKN/R", "Of-PMP/R", "Of-PNL/R",
    "Of-PLA/R", "Of-PVK/R", "Of-RTH/R"
  ))


ringed_info <- ringed_info |>
  dplyr::select(bird_code, observation_date, year, category)

all_ringed_info <- observation_data |>
  dplyr::select(bird_code, observation_date, year, category)

## Can you make a histogram of the observations_data per year and count the number of breeding
histogram_data <- observation_data |>
group_by(year, category) |>
  summarise(count = n()) |>
  ungroup()

ggplot2::ggplot(histogram_data, aes(x = year, y = count, fill = category)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("spring_migrants" = "#FF9999", "breeding" = "#66B3FF", "autumn_migrants" = "#99FF99", "overwinterers" = "#FFCC99")) +
  labs(title = "Bird Observations by Year and Category",
       x = "Year",
       y = "Count",
       fill = "Category") +
  theme_minimal() +
  theme(legend.position = "top")



