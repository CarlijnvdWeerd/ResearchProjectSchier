library(readxl)
library(tidyverse)
library(cowplot)
library(readxl)
library(car)
library(emmeans)
library(lme4)
library(lmerTest)
library(GGally)
library(tidyverse) 
library(cowplot) 
library(mgcv) 
library(ggplot2)
library(Rmisc)
library(faraway)
library(PMCMRplus)
library(performance)
library(readxl)

############################ Average behavior counts per week
library(readxl)
library(readxl)
library(dplyr)
library(ggplot2)
library(readxl)
ALL <- read_excel("C:/Users/conso/Downloads/Ringed Video Analyses (5).xlsx", 
                  sheet = "ALL!")
View(ALL)

# Count how often each behavior occurs per bird per week
behavior_frequency <- ALL %>%
  group_by(`Observation id`, Week, Behavior) %>%
  summarise(count = n()) %>%
  ungroup()

# Count number of distinct behaviors per bird per week
behavior_summary <- behavior_frequency %>%
  group_by(`Observation id`, `Week`) %>%
  summarise(num_behaviors = n()) %>%
  ungroup()

# Join them to get a combined table showing behavior counts and number of behaviors per bird-week
result <- behavior_frequency %>%
  left_join(behavior_summary, by = c("Week", "Observation id"))

print(result)

avg_behavior_per_week <- result %>%
  group_by(Week, Behavior) %>%
  summarise(avg_count = mean(count)) %>%
  ungroup()

ggplot(avg_behavior_per_week, aes(x = factor(Week), y = avg_count, color = Behavior, group = Behavior)) +
  geom_line(size = 1.2) +      # Line plot for trends over weeks
  geom_point(size = 3) + # Points on the lines
  labs(
    title = "Average Behavior Counts per Week (Across All Birds)",
    x = "Week",
    y = "Average Count",
    color = "Behavior"
  ) +
  theme_minimal()

#################################################### behavior counts
behavior_counts <- ALL %>%
  group_by(Week, `Behavior`) %>%
  summarise(count = n(), .groups = "drop")

avg_behavior <- behavior_counts %>%
  group_by(`Behavior`) %>%
  summarise(avg_per_week = mean(count), .groups = "drop")
library(ggplot2)

ggplot(avg_behavior_per_week, aes(x = Week, y = avg_count)) +
  geom_col(fill = "steelblue") +
  facet_wrap(~ `Behavior`, scales = "free_y") +
  labs(title = "Behavior Counts per Week",
       x = "Week",
       y = "Count") +
  theme_minimal()

###################################################
swallowing_data <- ALL %>%
  filter(Behavior == "Swallowing")

weekly_counts <- swallowing_data %>%
  group_by(Week, `Observation id`) %>%
  summarise(n_events = n(), .groups = "drop")

ggplot(weekly_counts, aes(x = factor(Week), y = n_events)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Swallowing Events per Week",
       x = "Week",
       y = "Average number of Swallowing Events") +
  coord_cartesian(ylim = c(0, 50)) +
  theme_minimal()

##################################################### 
library(dplyr)
library(tidyr)
library(tidyverse)

# If not already, convert to tibble
ALL <- as_tibble(ALL)

# Clean column names if needed (optional)
# library(janitor)
# df <- clean_names(df)

# Separate point and duration events
point_events <- ALL %>% 
  filter(`Behavior type` == "POINT") %>%
  mutate(duration = 0)

duration_events <- ALL %>%
  filter(`Behavior type` %in% c("START", "STOP")) %>%
  arrange(Behavior, Time)

# Pair START and STOP by behavior
duration_summary <- duration_events %>%
  group_by(Behavior) %>%
  filter(n() %% 2 == 0) %>%  # Keep only complete pairs
  mutate(event_id = rep(1:(n()/2), each = 2)) %>%
  pivot_wider(
    names_from = `Behavior type`,
    values_from = Time
  ) %>%
  mutate(duration = STOP - START) %>%
  ungroup()

# Combine both
all_events <- bind_rows(
  point_events %>% select(Behavior, Time, duration),
  duration_summary %>% select(Behavior, START, duration) %>% 
    rename(Time = START)
)

# Optional: sort by time
all_events <- all_events %>% arrange(Time)

# View the result
print(all_events)



################################## overview prey species selected
ALL$Comment <- tolower(ALL$Comment)  # make all lowercase
ALL$Comment[ALL$Comment %in% c("worm", "Worm")] <- "Worm"  # unify names
ALL$Comment[ALL$Comment %in% c("Wadkreeftje", "wadkreeftje")] <- "Wadkreeftje"  # unify names

selected_species <- c("worm", "Worm", "borstelworm", "wadkreeftje", "Wadkreeftje?", "Wadkreeftje","nonnetje" )

# Filter dataset to include only those species
filtered_species_summary <- ALL %>%
  filter(Comment %in% selected_species) %>%
  group_by(Comment) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

ggplot(filtered_species_summary, aes(x = "", y = count, fill = Comment)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Selected Prey Species Distribution", fill = "Species") +
  theme_void()

ggplot(filtered_species_summary, aes(x = reorder(Comment, count), y = count, fill = Comment)) +
  geom_col() +
  coord_flip() +
  labs(title = "Counts of Selected Prey Species", x = "Prey Species", y = "Count") +
  theme_minimal()





