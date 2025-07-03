
benthos_data <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRaQpjiWfO2pMm_gv0BkQ_AqAQrVGgM6F35-iL4JwVu_yCdMy7XlW0ThNpmsJ7sCkBwmtjuU84PjPw_/pub?gid=1619420848&single=true&output=csv")

observation_data <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQl5pI4xRYpDKPVxhR_9XzzAtZetAqFbsSrpdtAzhqFHmvlBKZD0m9s-5jymajZGA/pub?gid=746495893&single=true&output=csv")

benthos_data <- benthos_data %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y")) |>
  group_by(Date, Species_ID) |>
  filter(!(Distance == "0")) |>
  filter(!(Transect_ID %in% c("4.1", "4.2", "4.3", "4.4", "4.5", "4.6")))

species_counts <- benthos_data %>%
  group_by(Date, Species_ID) %>%
  summarise(count = n(), .groups = "drop") |>
  filter(count > 7)


library(dplyr)
library(lubridate)

monthly_species_counts <- benthos_data %>%
  mutate(Month = floor_date(Date, unit = "month")) %>%  # Truncate to month
  group_by(Month, Species_ID) %>%
  summarise(count = n(), .groups = "drop")

ggplot(monthly_species_counts, aes(x = Month, y = count, color = Species_ID)) +
  geom_line() +
  labs(title = "Species Counts Over Time",
       x = "Date",
       y = "Count",
       color = "Species ID") +
  theme_minimal()

timeline_data <- observation_data |>
  dplyr::mutate(Strategy = case_when(
    bird_code %in% c("Of-AUC/R", "Of-CLC/R", "Of-CTC/R", "Of-ECC/R", "Of-JEL/R", "Of-JLT/R", "Of-JNL/R", "Of-JPU/R", "Of-JTN/R", "Of-JYL/R","Of-KJU/R", "Of-KKN/R", "Of-KNP/R", "Of-KPM/R", "Of-KTJ/R", "Of-KXM/R", "Of-LAV/R", "Of-LCP/R", "Of-LCT/R", "Of-LEH/R","Of-LHE/R", "Of-LLM/R", "Of-LLP/R", "Of-LMN/R", "Of-LNJ/R", "Of-LPC/R", "Of-LPE/R", "Of-LPL/R", "Of-PLT/R", "Of-LPT/R", "Of-LTA/R", "Of-LTV/R", "Of-LUC/R", "Of-LYK/R", "Of-MAU/R", "Of-MCH/R", "Of-MCU/R", "Of-MEY/R", "Of-MHN/R", "Of-MKE/R", "Of-MKY/R", "Of-MNN/R", "Of-MPC/R", "Of-MPV/R", "Of-MTJ/R", "Of-MYV/R", "Of-NJM/R", "Of-NJU/R", "Of-NLJ/R", "Of-NNC/R", "Of-NPE/R", "Of-NTV/R", "Of-NUM/R", "Of-NUN/R", "Of-NVK/R", "Of-PAE/R", "Of-PAJ/R", "Of-PAC/R", "Of-PAM/R", "Of-PHN/R", "Of-PKN/R", "Of-PMP/R", "Of-PNL/R", "Of-PPT/R", "Of-PTA/R", "Of-PVA/R", "Of-PXX/R", "Of-PYC/R")  ~ "overwinterer",
    bird_code %in% c("Of-CCU/R", "Of-HPL/R", "Of-HPV/R", "Of-JAE/R", "Of-JHL/R", "Of-JHM/R", "Of-JHY/R", "Of-JLU/R", "Of-JNN/R", "Of-JTC/R","Of-JXM/R", "Of-KCY/R", "Of-KET/R", "Of-KKH/R", "Of-KMC/R", "Of-KMY/R", "Of-KNX/R", "Of-KPH/R", "Of-KTP/R", "Of-KYM/R", "Of-MCV/R", "Of-MJA/R", "Of-NKA/R", "Of-NNP/R", "Of-NUK/R", "Of-PCU/R", "Of-PLA/R", "Of-PNV/R", "Of-PVK/R") ~ "early_northward_migration",
    bird_code %in% c("Of-AVM/R", "Of-HNW/R", "Of-JCH/R", "Of-JHN/R", "Of-JJJ/R", "Of-JJV/R", "Of-JKM/R", "Of-JLH/R", "Of-JMN/R", "Of-JMU/R", "Of-JNH/R", "Of-JNM/R", "Of-JPX/R", "Of-JVH/R", "Of-JVL/R", "Of-JXN/R", "Of-JXY/R", "Of-KAH/R", "Of-KAX/R", "Of-KCT/R", "Of-KEN/R", "Of-KHE/R", "Of-KHK/R", "Of-KHU/R", "Of-KKM/R", "Of-KKT/R", "Of-KLP/R", "Of-KMT/R", "Of-KMV/R", "Of-KPN/R", "Of-KPU/R", "Of-KTE/R", "Of-KTK/R", "Of-KTN/R", "Of-KTM/R", "Of-KTT/R", "Of-KTV/R", "Of-KUH/R", "Of-KUL/R", "Of-KUX/R", "Of-KVA/R", "Of-KVC/R", "Of-KVH/R", "Of-KVV/R", "Of-KXC/R", "Of-KXV/R", "Of-KYC/R", "Of-LAJ/R", "Of-LCV/R", "Of-LEA/R", "Of-MAC/R", "Of-MCP/R", "Of-MJM/R", "Of-MLC/R", "Of-MXL/R", "Of-NEL/R", "Of-NKE/R", "Of-NLL/R", "Of-NNN/R", "Of-NPA/R", "Of-NPP/R", "Of-NPY/R", "Of-NUU/R", "Of-NXP/R", "Of-NYA/R", "Of-PAA/R", "Of-PHT/R", "Of-PJC/R", "Of-PJU/R", "Of-PLK/R", 
                     "Of-PXA/R", "Of-PAN/R", "Of-MAT/R", "Of-MCN/R", "Of-PLH/R") ~ "late_northward_migration",
    TRUE ~ "not_seen_in_2025"))

timeline_data <- timeline_data |>
  filter(Strategy %in% c("overwinterer", "early_northward_migration", "late_northward_migration")) |>
  group_by(bird_code, Strategy) |>
  filter(observation_date >= as.Date("2024-12-01") & observation_date <= as.Date("2025-06-01")) |>
  filter(n_distinct(observation_date) > 1) |>
  ungroup()

bird_counts <- timeline_data %>%
  group_by(observation_date, Strategy) %>%
  summarise(count = n(), .groups = "drop")

monthly_bird_counts <- timeline_data %>%
  mutate(Month = floor_date(observation_date, unit = "month")) %>%  # Truncate to month
  group_by(Month, Strategy) %>%
  summarise(count = n(), .groups = "drop") |>
  filter(!(Month %in% c(as.Date("2024-12-01"), as.Date("2025-01-01"), as.Date("2025-02-01")))) # Exclude December 2024 and January 2025 and February 2025

birds_prey_data <- left_join(monthly_bird_counts, monthly_species_counts, by = c("Month"))

birds_prey_data <- birds_prey_data |>
  filter(count.y > 30) |>
  filter(Species_ID %in% c("cor vol", "oli", "per ulv"))

# I want to make a ggplot with on the x-axis the date, on the right y-axis the count of birds, and on the left y-axis the count of benthos species.
birds_prey <- ggplot(birds_prey_data, aes(x = Month)) +
  geom_line(aes(y = count.x, color = Strategy), size = 1) +  # Bird counts on left axis
  geom_bar(aes(y = count.y / 13.64, fill = Species_ID), stat = "identity", 
           position = "dodge", 
           alpha = 0.2) +  # Scaled benthos
  scale_y_continuous(
    name = "Count of Observed Birds",  # Left axis (0 to 110)
    limits = c(0, 110),
    sec.axis = sec_axis(~ . * 13.64, name = "Count of Benthos Species")  # Right axis (0 to ~1500)
  ) +
  labs(
    title = "Bird Counts and Benthos Species Over Time",
    x = "Date",
    color = "Strategy / Species ID"
  ) +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_color_manual(
    values = c(
      "overwinterer" = "#4DC8F9",
      "early_northward_migration" = "#E777F2",
      "late_northward_migration" = "#4DD2A4")) +
    scale_fill_manual(
      values = c(
      "cor vol" = "#FF5733",
      "oli" = "#FFC300",
      "per ulv" = "#C70039"
    ))
birds_prey

# Save the plot
ggsave("birds_prey_availability.png", plot = birds_prey, width = 10, height = 6)

ggplot(birds_prey_data, aes(x = count.x)) +
  geom_histogram(bins = 30, fill = "#69b3a2", color = "black") +
  theme_minimal()
ggplot(birds_prey_data, aes(x = count.y)) +
  geom_histogram(bins = 30, fill = "#69b3a2", color = "black") +
  theme_minimal()

#################################################################################3

# run script 07 Boris forage for dataframe of complete_dataset
prey_data <- complete_dataset |>
  dplyr::select(Week, Strategy, Habitat, Three_letter_code, Comment) |>
  group_by(Three_letter_code) |>
  distinct()

prey_data <- prey_data |>
mutate(Comment = str_to_lower(Comment),         # Make lowercase
       Comment = str_remove_all(Comment, "\\?")) |>
  filter(Comment %in% c( "worm", "wadkreeftje", "Wadkreeftje", "worm?", "wadkreeftje?", "nonnetje", "Worm", "Klein ding", "Wadkreeftje of ander klein ding geen worm")) #|>
  group_by(Strategy) |>
  summarise(count = n(), .groups = "drop")
# early n=37
# late  n=72
# overwinter n=64
  
  bird_counts <- c(
  "overwinterer" = 64,
  "early_northward_migration" = 37,
  "late_northward_migration" = 72)

prey_counts <- prey_data %>%
  group_by(Comment, Strategy) %>%
  summarise(count = n(), .groups = "drop") |>
  mutate(Count_per_bird = case_when(
    Strategy == "overwinterer" ~ count / bird_counts["overwinterer"],
    Strategy == "early_northward_migration" ~ count / bird_counts["early_northward_migration"],
    Strategy == "late_northward_migration" ~ count / bird_counts["late_northward_migration"],
    TRUE ~ NA_real_)) 

ggplot(prey_counts, aes(x=Strategy, y=count, fill = Comment)) +
  geom_bar(stat = "identity", width = 1)

p_preycount <- ggplot(prey_counts, aes(x=Strategy, y=Count_per_bird, 
                                       fill = Comment)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Prey Selection by Strategy",
       x = "Strategy",
       y = "Count of Prey",
       fill = "Prey Type") +
  theme_minimal() +
  scale_fill_manual(values = c("worm" = "#FFC300", "wadkreeftje" = "#FF5733", "nonnetje" = "#900C3F", "Klein ding" = "#900C3F"),
  labels = c("wadkreeftje" = "Corophium", "nonnetje" = "Macoma", "worm"="Polychaetes")) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, size = 25, face = "bold"),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 20),
    axis.title.x = element_text(size = 23),
    axis.title.y = element_text(size = 23),
    legend.text = element_text(size = 20),
  )
p_preycount  

# Save the plot
ggsave("prey_availability_by_strategy.png", plot = p_preycount, width = 12, height = 6)

