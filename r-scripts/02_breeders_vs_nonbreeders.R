## cr birding data

observation_data <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQaf_n9GnQzbDAerYHyjA1zz9t8DjZR_OMHJpxEUK4WY_msNlJEq38bIeSjfd9ZOw/pub?gid=183403795&single=true&output=csv")

# Convert date and extract year
observation_data$observation_date <- as.Date(observation_data$observation_date, format = "%Y-%m-%d")
observation_data$year <- as.integer(format(observation_data$observation_date, "%Y"))
# Add month for logic clarity
observation_data$month <- as.integer(format(observation_data$observation_date, "%m"))

# Define seasonal flags
observation_data$prepare <- ifelse(
  observation_data$observation_date >= as.Date(paste0(observation_data$year, "-01-01")) &
    observation_data$observation_date <= as.Date(paste0(observation_data$year, "-05-15")),
  1, 0
)

observation_data$nonbreeding <- ifelse(
  observation_data$observation_date >= as.Date(paste0(observation_data$year, "-05-16")) &
    observation_data$observation_date <= as.Date(paste0(observation_data$year, "-07-31")),
  1, 0
)

observation_data$comeback <- ifelse(
  observation_data$observation_date >= as.Date(paste0(observation_data$year, "-08-01")) &
    observation_data$observation_date <= as.Date(paste0(observation_data$year, "-12-31")),
  1, 0
)


observation_data$category <- ifelse(observation_data$prepare == 1, "prepare",
                                    ifelse(observation_data$nonbreeding == 1, "nonbreeding",
                                           ifelse(observation_data$comeback == 1, "comeback", NA)))

all_ringed_info <- observation_data |>
  dplyr::select(bird_code, observation_date, year, category)

## I want to know which bird_code was in the category nonbreeding more than one year
nonbreeding_info <- all_ringed_info |>
  filter(category == "nonbreeding") |>
  group_by(bird_code) |>
  summarise(years = n_distinct(year)) |>
  filter(years > 1)

## in breeding_info i want also the years in which they were breeding
nonbreeding_years <- all_ringed_info |>
  filter(bird_code %in% breeding_info$bird_code) |>
  filter(category == "nonbreeding") |>
  group_by(bird_code, year) |>
  summarise(count = n()) |>
  ungroup()

## Can you make a histogram of the observations_data per year and count the number of nonbreeding
histogram_data <- observation_data |>
group_by(year, category) |>
  summarise(count = n()) |>
  ungroup()

ggplot2::ggplot(histogram_data, aes(x = year, y = count, fill = category)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("prepare" = "#FF9999", "nonbreeding" = "#66B3FF", "comeback" = "#99FF99")) +
  labs(title = "Bird Observations by Year and Category",
       x = "Year",
       y = "Count",
       fill = "Category") +
  theme_minimal() +
  theme(legend.position = "top")

nonbreeding_years |>
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
    "Of-PLA/R", "Of-PVK/R"
  ))

### So only 4 birds seen in 2025 have been nonbreeders before. HPL, JAE & KPM did not breed in 2024
