## cr birding data

observation_data <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQaf_n9GnQzbDAerYHyjA1zz9t8DjZR_OMHJpxEUK4WY_msNlJEq38bIeSjfd9ZOw/pub?gid=183403795&single=true&output=csv")

# Convert date and extract year
observation_data$observation_date <- as.Date(observation_data$observation_date, format = "%Y-%m-%d")
observation_data$year <- as.integer(format(observation_data$observation_date, "%Y"))
# Add month for logic clarity
observation_data$month <- as.integer(format(observation_data$observation_date, "%m"))

# Define seasonal flags
observation_data$prepare <- ifelse(
  observation_data$observation_date >= as.Date(paste0(observation_data$year, "-03-01")) &
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
    observation_data$observation_date <= as.Date(paste0(observation_data$year, "-10-31")),
  1, 0
)


observation_data$overwinterers <- ifelse(
  (observation_data$month %in% c(11, 12)) |
    (observation_data$month == 1 | 
       (observation_data$month == 2 & observation_data$observation_date <= as.Date(paste0(observation_data$year, "-02-15")))),
  1, 0
)


# Define overwinterers correctly
# observation_data$overwinterers <- ifelse(
#  (observation_data$month == 11 | observation_data$month == 12) |
#    (observation_data$month == 1 | observation_data$month == 2 & observation_data$observation_date <= as.Date(paste0(observation_data$year, "-02-15"))),



observation_data$category <- ifelse(observation_data$prepare == 1, "prepare",
                                    ifelse(observation_data$nonbreeding == 1, "nonbreeding",
                                           ifelse(observation_data$comeback == 1, "comeback",
                                                  ifelse(observation_data$overwinterers == 1, "overwinterers", NA))))

all_ringed_info <- observation_data |>
  dplyr::select(bird_code, observation_date, year, category)


# I want to know which bird_code was a overwinterer in the winter of  2024-11 till 2025-02
overwinterers_2024_winter <- all_ringed_info |>
  filter(category == "overwinterers") |>
  filter(year == 2024 & observation_date >= as.Date("2024-11-01") & observation_date <= as.Date("2025-02-15")) |>
  group_by(bird_code) |>
  summarise(count = n()) |>
  ungroup()


## I want to know which bird_code was in the category overwinterers more than one year
overwintering_info <- all_ringed_info |>
  filter(category == "overwinterers") |>
  group_by(bird_code) |>
  summarise(years = n_distinct(year)) |>
  filter(years > 1)

## in overwintering_info i want also the years in which they were overwintering
overwintering_years <- all_ringed_info |>
  filter(bird_code %in% overwintering_info$bird_code) |>
  filter(category == "overwinterers") |>
  group_by(bird_code, year) |>
  summarise(count = n()) |>
  ungroup()


overwintering_years |>
  filter(bird_code %in% c("Of-CCU/R", "Of-HPL/R", "Of-HPT/R", "Of-HNW/R", "Of-HPV/R", 
                          "Of-JAE/R", "Of-JCH/R", "Of-JHM/R", "Of-JHY/R", "Of-JJV/R", 
                          "Of-JLH/R", "Of-JLO/R", "Of-JLU/R", "Of-JNH/R", "Of-JTL/R", 
                          "Of-JTN/R", "Of-JVY/R", "Of-JXM/R", "Of-JXN/R", "Of-JXY/R", 
                          "Of-JYL/R",  "Of-KAH/R", "Of-KCY/R", "Of-KET/R", "Of-KHK/R", 
                          "Of-KJU/R", "Of-KKH/R", "Of-KKM/R", "Of-KKT/R", "Of-KLP/R", 
                          "Of-KMC/R", "Of-KMY/R", "Of-KNP/R", "Of-KNX/R", "Of-KPM/R", 
                          "Of-KTE/R",  "Of-KTM/R",  "Of-KTP/R", "Of-KUH/R", "Of-KUL/R", 
                          "Of-KUX/R", "Of-KVV/R", "Of-KXM/R", "Of-KXV/R",   "Of-LCT/R", 
                          "Of-LEA/R", "Of-LMN/R", "Of-LPT/R", "Of-LYK/R", "Of-MAU/R", 
                          "Of-MCH/R", "Of-MCV/R", "Of-MJA/R", "Of-MLC/R", 
                          "Of-MTJ/R", "Of-MYV/R", "Of-NEL/R", "Of-NKE/R", "Of-NLJ/R", 
                          "Of-NLY/R", "Of-NPA/R", "Of-NPP/R", "Of-NPY/R", "Of-NTV/R", 
                          "Of-NUK/R",  "Of-NVH/R", "Of-NYA/R", "Of-PAE/R", "Of-PAJ/R", 
                          "Of-PHT/R", "Of-PJU/R", "Of-PKN/R", "Of-PMP/R", "Of-PNL/R", 
                          "Of-PLA/R",   "Of-PVK/R"))

### So 4 birds filmed in 2025 have been overwinterers before. JTN in 2023/25, KKH in 2022 & 2024, KPM in 2023 & 2024 and KUX in 2022 & 2023

# When looking at 2024 and 2025 only: 


overwinterers_2024_winter |>
  filter(bird_code %in% c("Of-CCU/R", "Of-HPL/R", "Of-HPT/R", "Of-HNW/R", "Of-HPV/R", 
                          "Of-JAE/R", "Of-JCH/R", "Of-JHM/R", "Of-JHY/R", "Of-JJV/R", 
                          "Of-JLH/R", "Of-JLO/R", "Of-JLU/R", "Of-JNH/R", "Of-JTL/R", 
                          "Of-JTN/R", "Of-JVY/R", "Of-JXM/R", "Of-JXN/R", "Of-JXY/R", 
                          "Of-JYL/R",  "Of-KAH/R", "Of-KCY/R", "Of-KET/R", "Of-KHK/R", 
                          "Of-KJU/R", "Of-KKH/R", "Of-KKM/R", "Of-KKT/R", "Of-KLP/R", 
                          "Of-KMC/R", "Of-KMY/R", "Of-KNP/R", "Of-KNX/R", "Of-KPM/R", 
                          "Of-KTE/R",  "Of-KTM/R",  "Of-KTP/R", "Of-KUH/R", "Of-KUL/R", 
                          "Of-KUX/R", "Of-KVV/R", "Of-KXM/R", "Of-KXV/R",   "Of-LCT/R", 
                          "Of-LEA/R", "Of-LMN/R", "Of-LPT/R", "Of-LYK/R", "Of-MAU/R", 
                          "Of-MCH/R", "Of-MCV/R", "Of-MJA/R", "Of-MLC/R", 
                          "Of-MTJ/R", "Of-MYV/R", "Of-NEL/R", "Of-NKE/R", "Of-NLJ/R", 
                          "Of-NLY/R", "Of-NPA/R", "Of-NPP/R", "Of-NPY/R", "Of-NTV/R", 
                          "Of-NUK/R",  "Of-NVH/R", "Of-NYA/R", "Of-PAE/R", "Of-PAJ/R", 
                          "Of-PHT/R", "Of-PJU/R", "Of-PKN/R", "Of-PMP/R", "Of-PNL/R", 
                          "Of-PLA/R",   "Of-PVK/R" ))

## 18 birds that we filmed overwintered in 2024/2025!   
# "Of-JAE/R", "Of-JHY/R", "Of-JTN/R", "Of-JXM/R", "Of-JYL/R", "Of-KCY/R", "Of-KKH/R", "Of-KNX/R", "Of-KPM/R", "Of-KUL/R", "Of-MCH/R", "Of-MLC/R", "Of-NKE/R", "Of-PAE/R", "Of-PHT/R", "Of-PKN/R"



