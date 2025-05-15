install.packages(c("dplyr", "ggplot2", "signal"))

library(dplyr)
library(ggplot2)
library(signal)

odba_data_3a1 <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQbC8HZliMJ3n3fpKuI0aZCOiWq_WKZtbxV2rgLU_u0cUSBYNdTDptJ1r43gbr_TLHIuKZxvEvEo0W5/pub?gid=1949364362&single=true&output=csv")

install.packages("lubridate")
library(lubridate)

# Convert timestamps
odba_data_3a1 <- odba_data_3a1 %>%
  mutate(Collecting_time = ymd_hms(Collecting_time),
         Transmitting_time = ymd_hms(Transmitting_time))

# Visualize ODBA data over time
p1 <- ggplot(odba_data_3a1, aes(x = Collecting_time, y = ODBA)) +
  geom_line() +
  facet_wrap(~ UUID) +
  labs(title = "ODBA Over Time", x = "Time", y = "ODBA") +
  theme_minimal()

# Compute daily or hourly averages
mean_3a1 <- odba_data_3a1 %>%
  mutate(date = as.Date(Collecting_time)) %>%
  group_by(UUID, date) %>%
  summarize(
    mean_odba = mean(ODBA, na.rm = TRUE),
    max_odba = max(ODBA),
    sd_odba = sd(ODBA),
    .groups = "drop"
  )

# Visualize daily averages
ggplot(mean_3a1, aes(x = date, y = mean_odba)) +
  geom_line() +
  facet_wrap(~ UUID) +
  labs(title = "Daily ODBA Averages", x = "Date", y = "Mean ODBA") +
  theme_minimal()

odba_data_3a2 <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQbC8HZliMJ3n3fpKuI0aZCOiWq_WKZtbxV2rgLU_u0cUSBYNdTDptJ1r43gbr_TLHIuKZxvEvEo0W5/pub?gid=1613773180&single=true&output=csv")


# Convert timestamps
odba_data_3a2 <- odba_data_3a2 %>%
  mutate(Collecting_time = ymd_hms(Collecting_time),
         Transmitting_time = ymd_hms(Transmitting_time))

# Visualize ODBA data over time
p2 <- ggplot(odba_data_3a2, aes(x = Collecting_time, y = ODBA)) +
  geom_line() +
  facet_wrap(~ UUID) +
  labs(title = "ODBA Over Time", x = "Time", y = "ODBA") +
  theme_minimal()

# Compute daily or hourly averages
mean_3a2 <- odba_data_3a2 %>%
  mutate(date = as.Date(Collecting_time)) %>%
  group_by(UUID, date) %>%
  summarize(
    mean_odba = mean(ODBA, na.rm = TRUE),
    max_odba = max(ODBA),
    sd_odba = sd(ODBA),
    .groups = "drop"
  )

# Visualize daily averages
ggplot(mean_3a2, aes(x = date, y = mean_odba)) +
  geom_line() +
  facet_wrap(~ UUID) +
  labs(title = "Daily ODBA Averages", x = "Date", y = "Mean ODBA") +
  theme_minimal()

odba_data_3ae <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQbC8HZliMJ3n3fpKuI0aZCOiWq_WKZtbxV2rgLU_u0cUSBYNdTDptJ1r43gbr_TLHIuKZxvEvEo0W5/pub?gid=613070440&single=true&output=csv")

# Convert timestamps
odba_data_3ae <- odba_data_3ae %>%
  mutate(Collecting_time = ymd_hms(Collecting_time),
         Transmitting_time = ymd_hms(Transmitting_time))

# Visualize ODBA data over time
p3 <- ggplot(odba_data_3ae, aes(x = Collecting_time, y = ODBA)) +
  geom_line() +
  facet_wrap(~ UUID) +
  labs(title = "ODBA Over Time", x = "Time", y = "ODBA") +
  theme_minimal()

# Compute daily or hourly averages
mean_3ae <- odba_data_3ae %>%
  mutate(date = as.Date(Collecting_time)) %>%
  group_by(UUID, date) %>%
  summarize(
    mean_odba = mean(ODBA, na.rm = TRUE),
    max_odba = max(ODBA),
    sd_odba = sd(ODBA),
    .groups = "drop"
  )

# Visualize daily averages
ggplot(mean_3ae, aes(x = date, y = mean_odba)) +
  geom_line() +
  facet_wrap(~ UUID) +
  labs(title = "Daily ODBA Averages", x = "Date", y = "Mean ODBA") +
  theme_minimal()

odba_data_39a <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQbC8HZliMJ3n3fpKuI0aZCOiWq_WKZtbxV2rgLU_u0cUSBYNdTDptJ1r43gbr_TLHIuKZxvEvEo0W5/pub?gid=440870806&single=true&output=csv")

# Convert timestamps
odba_data_39a <- odba_data_39a %>%
  mutate(Collecting_time = ymd_hms(Collecting_time),
         Transmitting_time = ymd_hms(Transmitting_time))

# Visualize ODBA data over time
p4 <- ggplot(odba_data_39a, aes(x = Collecting_time, y = ODBA)) +
  geom_line() +
  facet_wrap(~ UUID) +
  labs(title = "ODBA Over Time", x = "Time", y = "ODBA") +
  theme_minimal()

# Compute daily or hourly averages
mean_39a <- odba_data_39a %>%
  mutate(date = as.Date(Collecting_time)) %>%
  group_by(UUID, date) %>%
  summarize(
    mean_odba = mean(ODBA, na.rm = TRUE),
    max_odba = max(ODBA),
    sd_odba = sd(ODBA),
    .groups = "drop"
  )

# Visualize daily averages
ggplot(mean_39a, aes(x = date, y = mean_odba)) +
  geom_line() +
  facet_wrap(~ UUID) +
  labs(title = "Daily ODBA Averages", x = "Date", y = "Mean ODBA") +
  theme_minimal()
### Not usueful yet, because not enough data

odba_data_39e <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQbC8HZliMJ3n3fpKuI0aZCOiWq_WKZtbxV2rgLU_u0cUSBYNdTDptJ1r43gbr_TLHIuKZxvEvEo0W5/pub?gid=33799828&single=true&output=csv")

# Convert timestamps
odba_data_39e <- odba_data_39e %>%
  mutate(Collecting.time = ymd_hms(Collecting.time),
         Transmitting.time = ymd_hms(Transmitting.time))

# Visualize ODBA data over time
p5 <- ggplot(odba_data_39e, aes(x = Collecting.time, y = ODBA)) +
  geom_line() +
  facet_wrap(~ UUID) +
  labs(title = "ODBA Over Time", x = "Time", y = "ODBA") +
  theme_minimal()

# facetwrap p1, p2, p3, p4 and p5

p_odba <- p1 + p2 + p3 + p4 + p5 +
  plot_layout(ncol = 1) +
  plot_annotation(title = "ODBA Over Time for Different UUIDs")
p_odba
# Save the plot
ggsave("odba_over_time.png", plot = p_odba, width = 10, height = 15, dpi = 300)


### Adding environmental data 
env_data_3a1 <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTCa8cXZ2gkaCs0_EyUfptN5kfP0B0gypfNxtS13yeF87MYpOIWefFEmqpgpbGR-p1ArYEjgICtY91u/pub?gid=129685321&single=true&output=csv")

env_data_3a1 <- env_data_3a1 %>%
  mutate(Collecting_time = ymd_hms(Collecting_time),
         Transmitting_time = ymd_hms(Transmitting_time))

install.packages("fuzzyjoin")
library(fuzzyjoin)

fuzzy_joined <- fuzzy_left_join(
  odba_data_3a1,
  env_data_3a1,
  by = c("UUID" = "UUID",
         "Transmitting_time" = "start_time",
         "Collecting_time" = "end_time"),
  match_fun = list(`==`, `>=`, `<=`)
)

