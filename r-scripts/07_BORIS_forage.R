# script with the BORIS data
renv::restore()

boris_data <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRlB920gRUzQvTQz40p3ru1OEbB9f9y9lZjsRn7meJ_nhudpivp1UB536fHQRjVCcONiiuEqrnHuJVx/pub?gid=1901273527&single=true&output=csv")

video_data <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRTzpl8cLIfyyycDNx_vbnulWr64YsPvfg0a4R9-q3hWTFq87Znl-odcKJX6LHaKgpCODNia0WpgQwy/pub?gid=2044682262&single=true&output=csv")

complete_dataset <- boris_data |>
  dplyr::left_join(video_data, by = "Observation_id", relationship = "many-to-many") |>
  dplyr::filter(Analysed == "Yes") |>
  dplyr::mutate(Time = as.numeric(Time)) 

## All time columns to numberic 
complete_dataset$Observation.duration <- as.numeric(complete_dataset$Observation.duration)
complete_dataset$Media.duration..s. <- as.numeric(complete_dataset$Media.duration..s.)

# Check for outliers
ggplot(complete_dataset, aes(x = Observation.duration)) +
  geom_histogram(binwidth = 1, fill = "#69b3a2", color = "black") +
  labs(
    title = "Distribution of Observation Durations",
    x = "Observation Duration (seconds)",
    y = "Count of Observations"
  ) +
  scale_x_continuous(breaks = seq(0, max(complete_dataset$Observation.duration, na.rm = TRUE), by = 5)) +
  theme_minimal()

# Assign right strategy to the Three_letter_code
complete_dataset <- complete_dataset |>
  dplyr::mutate(Strategy = case_when(
    Three_letter_code %in% c("JTN", "JYL", "KJU", "KNP", "KPM", "KXM", 
                             "LCT", "LMN", "LPT", 
                             "LYK", "MAU", "MCH", "MTJ", "MYV", "NLJ", 
                             "PAE", "PAJ", "PKN", 
                             "PMP", "PNL") ~ "overwinterer",
        Three_letter_code %in% c("PVK", "PLA", "MJA", "MCV", "KTP", 
                                 "KNX", "KMY", "KMC", "KKH", 
                             "KET", "JXM", "JTC", "JLU", "JHY", "JHM", 
                             "JAE", "HPV", "HPL", 
                             "CCU") ~ "early_northward_migration",
                              TRUE ~ "late_northward_migration"))


complete_dataset |>
  count(Strategy)
# early_northward_migrant n=12588, late_northward_migration = 16840, overwinterer n=14393

#######################################################################
# Converting dataset to work with duration time, etc

start_stop <- complete_dataset |>
  filter(Behavior.type %in% c("START", "STOP"))

# Ensure it's sorted properly
start_stop <- start_stop |>
  arrange(Observation_id, Behavior, Time)

# Add a row number within each behavior to pair starts and stops
start_stop <- start_stop |>
  group_by(Observation_id, Behavior) |>
  mutate(pair_id = cumsum(Behavior.type == "START"))

# Separate starts and stops
starts <- start_stop |> filter(Behavior.type == "START")
stops  <- start_stop |> filter(Behavior.type == "STOP")

# Join them together
durations <- left_join(starts, stops,
                       by = c("Observation_id", "Behavior", "pair_id"),
                       suffix = c("_start", "_stop"))

# Calculate the point behaviours
behaviors <- complete_dataset |>
  filter(Behavior.type == "POINT") |>
  mutate(Duration = 0.1)

# Calculate the duration
durations <- durations |>
  mutate(Duration = Time_stop - Time_start)

#######################################################################
str(durations)

behaviors$Duration <- as.numeric(as.character(behaviors$Duration))
durations$Duration <- as.numeric(as.character(durations$Duration))


# Combine point and duration events
library(dplyr)

behaviors <- behaviors %>% 
  bind_rows(
    behaviors %>% 
      dplyr::select(Observation_id, Behavior, Time, Duration, Week,    
                    Three_letter_code, Tide, Habitat, Social_behavior, 
                    Aggressive.or.submissive, Strategy, Transect_ID, Date.x, 
                    Media.duration..s.),
    durations %>%
      dplyr::select(Observation_id, Behavior, Time_start, Week_start, 
                    Three_letter_code_start, Tide_start, Habitat_start, 
                    Aggressive.or.submissive_start, Strategy_start, 
                    Transect_ID_start, Date.x_start, Media.duration..s._start, 
                    Duration) %>%
      rename(Time = Time_start,
             Week = Week_start,
             Three_letter_code = Three_letter_code_start,
             Tide = Tide_start,
             Habitat = Habitat_start,
             Aggressive.or.submissive = Aggressive.or.submissive_start,
             Strategy = Strategy_start,
             Transect_ID = Transect_ID_start,
             Date.x = Date.x_start,
             Media.duration..s. = Media.duration..s._start)
  )


# Sort out
behaviors <- behaviors |>
    arrange(Three_letter_code) |>
    dplyr::select(Observation_id, Media.duration..s., Behavior,
                  Time,  Week, Date.x, Transect_ID, 
                  Three_letter_code, Tide, Habitat, Social_behavior,
                  Aggressive.or.submissive, Strategy, Duration)

# View the result
print(behaviors)

behaviors <- behaviors |>
  distinct()
#######################################################################
# Look at surface_pecking per week per strategy in behavior dataframe

# Calculate the point behaviours
point_behaviors <- complete_dataset |>
  filter(Behavior.type == "POINT") |>
  filter(!Observation_id %in% c("KET_20.03", "KET.20.03", "LEA_21.05")) |>
  group_by(Three_letter_code, Behavior, Strategy, Week, Tide, Habitat,   Observation_id, Transect_ID, Date.x, Media.duration..s.,   
           Social_behavior, Aggressive.or.submissive) |>
  summarise(Behavior_Count = n(), .groups = "drop")|>
  mutate(Behavior_Rate = Behavior_Count / Media.duration..s.) |>
  filter(Week %in% c(12, 13, 16, 17, 18 ,19, 20 ,21))
## Video of KET on 20 03 is out of proportion in comparison of the other video's of that day but also over the past time.

# Check for normality of probing
qqnorm(point_behaviors$Behavior_Count)

counts_point <- point_behaviors |>
  group_by(Week, Strategy, Behavior) |>
  summarise(n = n_distinct(Observation_id), .groups = "drop") |>
  filter(!Week %in% c("9", "11", "15"))

p5a <- ggplot(point_behaviors |> filter(Behavior == "Probing"),
             aes(x = as.factor(Week), y = Behavior_Rate, 
                 fill = Strategy)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#FF9999", "#1ED760", "#66B3FF")) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Probing by Strategy",
    x = "Week",
    y = "Duration Rate")
p5a

p6a <- ggplot(point_behaviors |> filter(Behavior == "Surface_pecking"),
             aes(x = as.factor(Week), y = Behavior_Rate, 
                 fill = Strategy)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#FF9999", "#1ED760", "#66B3FF")) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Surface pecking by Strategy",
    x = "Week",
    y = "Duration Rate")
p6a

p7a <- ggplot(point_behaviors |> filter(Behavior == "Turning_stuff"),
             aes(x = as.factor(Week), y = Behavior_Rate, 
                 fill = Strategy)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#FF9999", "#1ED760",  "#66B3FF")) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Turning by Strategy",
    x = "Week",
    y = "Duration Rate")
p7a

# Fill in the "gaps" 
behaviors <- behaviors |>
  group_by(Three_letter_code, Observation_id) |>
  mutate(total_duration = sum(Duration, na.rm = TRUE)) |>
  mutate(total_duration = ifelse(is.na(total_duration), 0, 
                                 total_duration), gap_time = Media.duration..s. - total_duration) |>
  rename(visually_foraging = gap_time) 

#################################################################################
# The same analysis but than for the stage event behaviors
stage_behavior <- behaviors |>
  filter(Behavior %in% c("Walking", "Alert", "Digging", "Routing", "Handling_prey")) |>
  mutate(Behavior_Rate = (total_duration / Media.duration..s.))

# Plotting the stage behaviors
p_stage <- ggplot(stage_behavior,
                  aes(x = as.factor(Week), y = Behavior_Rate, 
                      fill = Strategy)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#FF9999","#1ED760", "#66B3FF")) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Stage Behaviour by Strategy",
    x = "Week",
    y = "Duration Rate")  +
  facet_wrap(~Behavior)
  
p_stage

qqnorm(stage_behavior$Behavior_Rate)

counts_stage <- stage_behavior |>
  group_by(Week, Strategy, Behavior) |>
  summarise(n = n_distinct(Observation_id), .groups = "drop") |>
  filter(!Week %in% c("9", "11", "15"))

p10a <- ggplot(stage_behavior |> filter(Behavior == "Walking"),
              aes(x = as.factor(Week), y = Behavior_Rate, 
                  fill = Strategy)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#FF9999","#1ED760", "#66B3FF")) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Walking by Strategy",
    x = "Week",
    y = "Duration Rate")
p10a


p11a <- ggplot(stage_behavior |> filter(Behavior == "Alert"),
              aes(x = as.factor(Week), y = Behavior_Rate, 
                  fill = Strategy)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#FF9999", "#1ED760", "#66B3FF")) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Alert by Strategy",
    x = "Week",
    y = "Duration Rate")
p11a


p12a <- ggplot(stage_behavior |> filter(Behavior == "Digging"),
              aes(x = as.factor(Week), y = Behavior_Rate, 
                  fill = Strategy)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#FF9999", "#1ED760","#66B3FF")) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Digging by Strategy",
    x = "Week",
    y = "Duration Rate")
p12a

p13a <- ggplot(stage_behavior |> filter(Behavior == "Routing"),
              aes(x = as.factor(Week), y = Behavior_Rate, 
                  fill = Strategy)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#FF9999", "#1ED760", "#66B3FF")) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Routing by Strategy",
    x = "Week",
    y = "Duration Rate")
p13a

p14a <- ggplot(stage_behavior |> filter(Behavior == "Handling_prey"),
              aes(x = as.factor(Week), y = Behavior_Rate, 
                  fill = Strategy)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#FF9999", "#1ED760", "#66B3FF")) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Handling prey by Strategy",
    x = "Week",
    y = "Duration Rate")
p14a

######################################################################
transect_counts <- complete_dataset |>
  group_by(Transect_ID, Strategy, Week) |>
  distinct() |>
  summarise(n = n_distinct(Observation_id), .groups = "drop") 

complete_dataset$Transect_ID <- as.factor(complete_dataset$Transect_ID)
transect_dataset <- complete_dataset |> 
  group_by(Transect_ID, Strategy, Week) |> 
  summarise(Count = n_distinct(Observation_id), .groups = "drop")
transect_dataset$Transect_ID <- as.factor(transect_dataset$Transect_ID)

transect_counts$Transect_ID <- as.factor(transect_counts$Transect_ID)

p_early <- ggplot(complete_dataset |> filter(Strategy == "early_northward_migration"), aes(x = as.factor(Week), fill = Transect_ID)) +
  geom_bar(stat = "count", position = "dodge") +
  scale_fill_manual(values = c("#FF9999", "#E81C2D", "#DB6A39","#E8749C","#F6AAB1" )) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Early Northward Migration per Week",
    x = "Week",
    y = "Count"
  )
p_early

p_early <- p_early + geom_text(data = transect_counts |> filter(Strategy == "early_northward_migration"), aes(x = as.factor(Week), y = 2000,                                             label = paste0("n=", n)),
                         position = position_dodge(width = 0.8), size = 2)
p_early

p_late <- ggplot(complete_dataset |> filter(Strategy == "late_northward_migration"), aes(x = as.factor(Week), fill = Transect_ID)) +
  geom_bar(stat = "count", position = "dodge") +
  scale_fill_manual(values = c("#FF9999", "#E81C2D", "#DB6A39","#E8749C","#F6AAB1")) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Late Northward Migration per Week",
    x = "Week",
    y = "Count"
  )
p_late

p_late <- p_late + geom_text(data = transect_counts |> filter(Strategy == "late_northward_migration"), aes(x = as.factor(Week), y = 3500,                                             label = paste0("n=", n)),
                           position = position_dodge(width = 0.8), size = 2)
p_late

p_overwinterer <- ggplot(complete_dataset |> filter(Strategy == "overwinterer"), aes(x = as.factor(Week), fill = Transect_ID)) +
  geom_bar(stat = "count", position = "dodge") +
  scale_fill_manual(values = c("#FF9999", "#E81C2D", "#DB6A39","#E8749C","#F6AAB1")) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Overwinterer per Week",
    x = "Week",
    y = "Count"
  )
p_overwinterer

p_overwinterer <- p_overwinterer + geom_text(data = transect_counts |> filter(Strategy == "overwinterer"), aes(x = as.factor(Week), y = 3200,                                             label = paste0("n=", n)),
                             position = position_dodge(width = 0.8), size = 2)
p_overwinterer

p_all_strategies <- p_early / p_late / p_overwinterer
p_all_strategies

ggsave("transect_strategy_week.png", plot = p_all_strategies, width = 18, height = 10, dpi = 300)

#######################################################################
# Habitat usage 

habitat_counts <- complete_dataset |>
  group_by(Habitat, Strategy, Week) |>
  distinct() |>
  summarise(n = n_distinct(Observation_id), .groups = "drop") |>
  filter(!Week %in% c("9", "11", "12", "13", "15"))

habitat_counts$Habitat[habitat_counts$Habitat == "Combination "] <- "Combination"

complete_dataset$Habitat <- complete_dataset$Habitat |>
  str_trim()

habitat_counts <- complete_dataset |>
  filter(!Week %in% c("9", "11", "12", "13", "15")) |>
  group_by(Habitat, Strategy, Week) |>
  summarise(n = n_distinct(Observation_id), .groups = "drop")

# Now join back to complete_dataset
complete_dataset <- complete_dataset |>
  filter(!Week %in% c("9", "11", "12", "13", "15")) |>
  left_join(habitat_counts, by = c("Habitat", "Strategy", "Week"))

habitat_counts$Habitat <- habitat_counts$Habitat |>
  str_trim()

p_habitat <- ggplot(complete_dataset, aes(x = as.factor(Week), fill = Habitat)) +
  geom_bar(stat = "count", position = "dodge") +
  scale_fill_manual(values = c("#A2A8B4", "#D6C78A", "#66B3FF")) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Habitat Usage per Week",
    x = "Week",
    y = "Count"
  ) +
  facet_wrap(~Strategy)
p_habitat

p_early_hab <- ggplot(complete_dataset |> filter(Strategy == "early_northward_migration"), aes(x = as.factor(Week), fill = Habitat)) +
  geom_bar(stat = "count", position = "dodge") +
  scale_fill_manual(values = c("#A2A8B4", "#D6C78A", "#66B3FF")) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Habitat Usage Early Northward Migration",
    x = "Week",
    y = "Count"
  )
p_early_hab

p_early_hab <- p_early_hab + geom_text(data = habitat_counts |> filter(Strategy == "early_northward_migration"), aes(x = as.factor(Week), y = 2100,                                             label = paste0("n=", n)),
                                             position = position_dodge(width = 0.8), size = 3)
p_early_hab

p_late_hab <- ggplot(complete_dataset |> filter(Strategy == "late_northward_migration"), aes(x = as.factor(Week), fill = Habitat)) +
  geom_bar(stat = "count", position = "dodge") +
  scale_fill_manual(values = c("#A2A8B4", "#D6C78A", "#66B3FF")) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Habitat Usage Late Northward Migration **",
    x = "Week",
    y = "Count"
  )
p_late_hab

p_late_hab <- p_late_hab + geom_text(data = habitat_counts |> filter(Strategy == "late_northward_migration"), aes(x = as.factor(Week), y = 3100,                                             label = paste0("n=", n)),
                                       position = position_dodge(width = 0.8), size = 3)
p_late_hab

p_win_hab <- ggplot(complete_dataset |> filter(Strategy == "overwinterer"), aes(x = as.factor(Week), fill = Habitat)) +
  geom_bar(stat = "count", position = "dodge") +
  scale_fill_manual(values = c("#A2A8B4", "#D6C78A", "#66B3FF")) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Habitat Usage Overwinterers",
    x = "Week",
    y = "Count"
  )
p_win_hab

p_win_hab <- p_win_hab + geom_text(data = habitat_counts |> filter(Strategy == "overwinterer"), aes(x = as.factor(Week), y = 2600,                                             label = paste0("n=", n)),
                                     position = position_dodge(width = 0.8), size = 3)
p_win_hab

p_all_habitat <- p_early_hab / p_late_hab / p_win_hab
p_all_habitat


