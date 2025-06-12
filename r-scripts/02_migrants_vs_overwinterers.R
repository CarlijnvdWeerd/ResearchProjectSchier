## cr birding data

observation_data <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQl5pI4xRYpDKPVxhR_9XzzAtZetAqFbsSrpdtAzhqFHmvlBKZD0m9s-5jymajZGA/pub?gid=746495893&single=true&output=csv")

# Convert date and extract year
observation_data$observation_date <- as.Date(observation_data$observation_date, format = "%Y-%m-%d")
observation_data$year <- as.integer(format(observation_data$observation_date, "%Y"))
# Add month for logic clarity
observation_data$month <- as.integer(format(observation_data$observation_date, "%m"))

# Define seasonal flags
observation_data$early_northward_migration <- ifelse(
  observation_data$observation_date >= as.Date(paste0(observation_data$year,
                                                      "-03-16")) &
    observation_data$observation_date <= as.Date(paste0(observation_data$year,
                                                        "-04-10")),1, 0)
observation_data$late_northward_migration <- ifelse(
  observation_data$observation_date >= as.Date(paste0(observation_data$year,
                                                      "-04-11")) &
    observation_data$observation_date <= as.Date(paste0(observation_data$year,
                                                        "-06-15")),1, 0)
observation_data$nonbreeding <- ifelse(
  observation_data$observation_date >= as.Date(paste0(observation_data$year,
                                                      "-06-16")) &
    observation_data$observation_date <= as.Date(paste0(observation_data$year,
                                                        "-06-30")), 1, 0)
observation_data$southward_migration <- ifelse(
  observation_data$observation_date >= as.Date(paste0(observation_data$year,
                                                      "-07-01")) &
    observation_data$observation_date <= as.Date(paste0(observation_data$year,
                                                        "-11-30")), 1, 0)
observation_data$overwinterers <- ifelse(
  (observation_data$month == 11) |
    (observation_data$month == 12) |
    (observation_data$month == 1) |
    (observation_data$month == 2) |
    (observation_data$month == 3 & observation_data$observation_date <= 
       as.Date(paste0(ifelse(observation_data$month %in% c(11,12), observation_data$year + 1, observation_data$year), "-03-15"))),
  1, 0)


observation_data$category <- ifelse(observation_data$early_northward_migration == 1,                                      "early_northward_migration",
                             ifelse(observation_data$late_northward_migration 
                                    == 1, "late_northward_migration",
                             ifelse(observation_data$nonbreeding == 1,
                                    "nonbreeding",
                             ifelse(observation_data$southward_migration == 1,                                     "southward_migration",
                             ifelse(observation_data$overwinterers == 1, 
                                    "overwinterers", NA)))))


filmed_birds <- observation_data |>
filter(bird_code %in% c(
  "Of-CCU/R", "Of-HPL/R", "Of-HPT/R", "Of-HNW/R", "Of-HPV/R", 
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
  "Of-PLA/R",   "Of-PVK/R" 
))

#### Making a plot that shows the observations of 1th of october 2024 till 15th of june 2025

library(dplyr)
library(ggplot2)

# Define date range for 2024 / 2025
start_date2024 <- as.Date("2024-04-01")
end_date2025 <- as.Date("2025-06-15")

# Filter observations
filtered_data <- observation_data %>%
  filter(observation_date >= start_date2024 & observation_date <= end_date2025) |>
  mutate(day_number = as.numeric(observation_date - as.Date("2024-04-01")) + 1)

ggplot(filtered_data, aes(x = day_number, y = observation_lat)) +
  geom_point(aes(color = category), size = 2, alpha = 0.7) +
  facet_wrap(~ bird_code) +
  labs(title = "Latitude of Observations over Time per Bird",
       x = "Day since Oct 1, 2024",
       y = "Latitude",
       color = "Category") +
  theme_minimal()

winter_data <- filtered_data %>%
  group_by(bird_code) %>%
  filter(any(category == "overwinterers") & 
           any(category %in% c("northward_migration", "southward_migration"))) %>%
  ungroup()


ggplot(winter_data, aes(x = day_number, y = observation_lat)) +
  geom_point(aes(color = category), size = 2, alpha = 0.7) +
  facet_wrap(~ bird_code) +
  labs(title = "Latitude of Observations over Time per Bird",
       x = "Day since Oct 1, 2024",
       y = "Latitude",
       color = "Category") +
  theme_minimal()

### JEL, JLT, JPU, JTN, JYL, KJU, KKN, KNP, KPM, KTJ, KXM, LCP, LCT, LMN, LPL, LPT, LTV, LYK, MAU, MCH, MPC, MTJ, MYV, NJM, NLJ, NTV, PAE, PAJ, PHN, PKN, PMP, PNL, PPT and PXX

# Filter observations
filtered_data2024_2025 <- filmed_birds %>%
  filter(observation_date >= start_date2024 & observation_date <= end_date2025) |>
  mutate(day_number = as.numeric(observation_date - as.Date("2024-10-01")) + 1)

p_allobserve <- ggplot(filtered_data2024_2025, aes(x = day_number, y = observation_lat)) +
  geom_point(aes(color = category), size = 2, alpha = 0.7) +
  facet_wrap(~ bird_code) +
  labs(title = "Latitude of Observations over Time per Bird",
       x = "Day since Oct 1, 2024",
       y = "Latitude",
       color = "Category") +
  theme_minimal()
p_allobserve

ggsave("all_observation_over_time.png", plot = p_allobserve, width = 18, height = 18, dpi = 300)

library(dplyr)

winter_data2024_2025 <- filtered_data2024_2025 %>%
  group_by(bird_code) %>%
  filter(any(category == "overwinterers") & 
           any(category %in% c("northward_migration", "southward_migration"))) %>%
  ungroup()


p_timeline <- ggplot(winter_data2024_2025, aes(x = day_number, y = observation_lat)) +
  geom_point(aes(color = category), size = 2, alpha = 0.7) +
  facet_wrap(~ bird_code, nrow = 7, ncol = 3) +
  labs(title = "Latitude of Observations over Time per Bird 2024/2025",
       x = "Day since Oct 1, 2024",
       y = "Latitude",
       color = "Category") +
  theme_minimal()  +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 10),
    strip.text = element_text(size = 13),       # facet labels
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  )
p_timeline

## 21 birds that we filmed overwintered in 2024/2025. JTN, JYL, KJU, KNP, KPM
#                     , KXM, LCT, LMN, LPT, LYK, MAU, MCH, MTJ, MYV, NLJ, NTV #                     , PAE, PAJ, PKN, PMP, PNL
# LCT, LMN, LPT, LYK, MAU, MTJ, MYV, NLJ, NTV and PNL were ringed in 2025 

ggsave("observation_over_time.png", plot = p_timeline, width = 18, height = 15, dpi = 300)

# Define date range for 2023 / 2024
start_date2023 <- as.Date("2023-10-01")
end_date2024 <- as.Date("2024-06-15")

# Filter observations
filtered_data2023_2024 <- filmed_birds %>%
  filter(observation_date >= start_date2023 & observation_date <= end_date2024) |>
  mutate(day_number = as.numeric(observation_date - as.Date("2023-10-01")) + 1)

ggplot(filtered_data2023_2024, aes(x = day_number, y = observation_lat)) +
  geom_point(aes(color = category), size = 2, alpha = 0.7) +
  facet_wrap(~ bird_code) +
  labs(title = "Latitude of Observations over Time per Bird",
       x = "Day since Oct 1, 2023",
       y = "Latitude",
       color = "Category") +
  theme_minimal()

library(dplyr)

winter_data2023_2024 <- filtered_data2023_2024 %>%
  group_by(bird_code) %>%
  filter(any(category == "overwinterers") & 
           any(category %in% c("northward_migration", "southward_migration"))) %>%
  ungroup()


ggplot(winter_data2023_2024, aes(x = day_number, y = observation_lat)) +
  geom_point(aes(color = category), size = 2, alpha = 0.7) +
  facet_wrap(~ bird_code) +
  labs(title = "Latitude of Observations over Time per Bird",
       x = "Day since Oct 1, 2023",
       y = "Latitude",
       color = "Category") +
  theme_minimal()

### 5 birds that we filmed overwintered in 2023/2024: JTN, JXM, KHK, KPM, KUX

# Define date range for 2022 / 2023
start_date2022 <- as.Date("2022-10-01")
end_date2023 <- as.Date("2023-06-15")

# Filter observations
filtered_data2022_2023 <- filmed_birds %>%
  filter(observation_date >= start_date2022 & observation_date <= end_date2023) |>
  mutate(day_number = as.numeric(observation_date - as.Date("2022-10-01")) + 1)

ggplot(filtered_data2022_2023, aes(x = day_number, y = observation_lat)) +
  geom_point(aes(color = category), size = 2, alpha = 0.7) +
  facet_wrap(~ bird_code) +
  labs(title = "Latitude of Observations over Time per Bird",
       x = "Day since Oct 1, 2022",
       y = "Latitude",
       color = "Category") +
  theme_minimal()

library(dplyr)

winter_data2022_2023 <- filtered_data2022_2023 %>%
  group_by(bird_code) %>%
  filter(any(category == "overwinterers") & 
           any(category %in% c("northward_migration", "southward_migration"))) %>%
  ungroup()


ggplot(winter_data2022_2023, aes(x = day_number, y = observation_lat)) +
  geom_point(aes(color = category), size = 2, alpha = 0.7) +
  facet_wrap(~ bird_code) +
  labs(title = "Latitude of Observations over Time per Bird",
       x = "Day since Oct 1, 2022",
       y = "Latitude",
       color = "Category") +
  theme_minimal()


