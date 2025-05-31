# script with the BORIS data

boris_data <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRlB920gRUzQvTQz40p3ru1OEbB9f9y9lZjsRn7meJ_nhudpivp1UB536fHQRjVCcONiiuEqrnHuJVx/pub?gid=1901273527&single=true&output=csv")

video_data <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRTzpl8cLIfyyycDNx_vbnulWr64YsPvfg0a4R9-q3hWTFq87Znl-odcKJX6LHaKgpCODNia0WpgQwy/pub?gid=2044682262&single=true&output=csv")

complete_dataset <- boris_data |>
  dplyr::left_join(video_data, by = "Observation_id", relationship = "many-to-many") |>
  dplyr::filter(Analysed == "Yes") |>
  dplyr::mutate(Time = as.numeric(Time)) |>
  dplyr::filter(between(Time, 0, 124))

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

# I want the three_letter_code for JAE, JHY, JTN, JXM, JYL, KCY, KKH, KNX, KPM, KUL, MCH, MLC, NKE, PAE, PHT and PKN to be assigned in a new column called strategy to be assigned overwinterer and for the rest of the three_letter_code to be assigned migrant

complete_dataset <- complete_dataset |>
  dplyr::mutate(Strategy = ifelse(
    Three_letter_code %in% c("JAE", "JHY", "JTN", "JXM", "JYL", "KCY", "KKH", "KNX", "KPM", "KUL", "MCH", "MLC", "NKE", "PAE", "PHT", "PKN"),
    "overwinterer",
    "migrant"
  ))

complete_dataset |>
  count(Strategy)
# migrant n=30308, overwinterer n=13444

##############################################################################
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

# Calculate the duration
durations <- durations |>
  mutate(Duration = Time_stop - Time_start)

# Calculate the point behaviours
behaviors <- complete_dataset |>
  filter(Behavior.type == "POINT") |>
  mutate(Duration = 0.1)

# if the duration is NA, then set it to 0
# complete_dataset$Duration[is.na(complete_dataset$Duration)] <- 0

###############################################################################
str(behaviors)
str(durations)

behaviors$Duration <- as.numeric(as.character(behaviors$Duration))
durations$Duration <- as.numeric(as.character(durations$Duration))


# Combine point and duration events
behaviors <- behaviors |> bind_rows(
  behaviors %>% 
    select(Observation_id, Behavior, Time, Duration, Week,    
    Three_letter_code, Tide, Habitat, Social_behavior, Aggressive.or.submissive, 
    Strategy, Transect_ID, Date.x, Media.duration..s.),
  durations %>% 
    select(Observation_id, Behavior, Time_start, Week_start, 
    Three_letter_code_start, Tide_start, Habitat_start, 
    Aggressive.or.submissive_start, , Strategy_start, Transect_ID_start, 
    Date.x_start, Media.duration..s._start, Duration) |>
    rename(Time = Time_start) |>
    rename(Week = Week_start)|>
    rename(Three_letter_code = Three_letter_code_start) |>
    rename(Tide = Tide_start) |>
    rename(Habitat = Habitat_start) |>
    rename(Aggressive.or.submissive = Aggressive.or.submissive_start) |>
    rename(Strategy = Strategy_start) |>
    rename(Transect_ID = Transect_ID_start) |>
    rename(Date.x = Date.x_start) |>
    rename(Media.duration..s. = Media.duration..s._start))

# Sort out
behaviors <- behaviors |>
    arrange(Observation_id) |>
    dplyr::select(Observation_id, Media.duration..s., Behavior,
                  Time,  Week, Date.x, Transect_ID, 
                  Three_letter_code, Tide, Habitat, Social_behavior,
                  Aggressive.or.submissive, Strategy, Duration)

# View the result
print(behaviors)

behaviors <- behaviors |>
  distinct()


##############################################################################
# Fill in the "gaps" 

# Step 1: Calculate behavior time per video
behavior_time <- behaviors |>
  group_by(Observation_id, Three_letter_code) |>
  summarise(total_behavior_time = sum(Duration, na.rm = TRUE))

# Step 2: Get unique video durations
video_durations <- behaviors |>
  select(Observation_id, Three_letter_code, Media.duration..s.) |>
  distinct() |>
  rename(video_duration = Media.duration..s.)

# Step 3: Calculate missing behavior time
behavior_coverage <- video_durations |>
  left_join(behavior_time, by = "Observation_id") |>
  mutate(
    total_behavior_time = ifelse(is.na(total_behavior_time), 0, total_behavior_time),
    gap_time = video_duration - total_behavior_time)

# Step 4: Create gap behavior records
gap_behavior <- behavior_coverage |>
  filter(gap_time > 0) |>
  mutate(
    Behavior = "Foraging_walking",
    Duration = gap_time) |>
  select(Observation_id, Three_letter_code.x, Behavior, Duration)

for_walk <- gap_behavior |>
  left_join(video_data, by = "Observation_id")

for_walk <- for_walk |>
  dplyr::mutate(Strategy = ifelse(
    Three_letter_code %in% c("JAE", "JHY", "JTN", "JXM", "JYL", "KCY", "KKH", "KNX", "KPM", "KUL", "MCH", "MLC", "NKE", "PAE", "PHT", "PKN"),
    "overwinterer",
    "migrant")) 

### Add in the weeks again
library(dplyr)
library(lubridate)

for_walk <- for_walk |>
  mutate(Week = case_when(
      Date == ("27-02") ~ 9,
      Date == ("04-03") ~ 10,
      Date %in% (c("18-03", "19-03", "20-03")) ~ 12,
      Date %in% (c("26-03", "27-03", "28-03")) ~ 13,
      Date == ("03-04") ~ 14,
      Date %in% (c("09-04", "10-04", "11-04")) ~ 15,
      Date %in% (c("15-04", "16-04", "17-04")) ~ 16,
      Date %in% (c("24-04", "25-04")) ~ 17,
      Date %in% (c("29-04", "30-04", "01-05")) ~ 18,
      Date %in% (c("07-05", "08-05", "09-05")) ~ 19,
      Date %in% (c("13-05", "14-05", "15-05")) ~ 20,
      Date %in% (c("20-05", "21-05")) ~ 21,
      TRUE ~ NA_real_ ))


#Boxplot graph to look at the difference in duration of foraging walking between migrant and overwinterer 

# Calculate counts per Week and Strategy
counts <- for_walk %>%
  group_by(Week, Strategy) %>%
  summarise(n = n())

# Base boxplot
p1 <- ggplot(for_walk, aes(x = as.factor(Week), y = Duration, color = Strategy)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.5, size = 1) +  # add jittered points
  scale_color_manual(values = c("#FF9999", "#66B3FF")) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Foraging walking per Strategy",
    x = "Week",
    y = "Duration (seconds)")

# Add text labels with counts above boxes
p2 <- p1 + geom_text(data = counts, aes(x = as.factor(Week), 
                                        y = max(for_walk$Duration) + 5, 
                                  label = paste0("n=", n), color = Strategy),
               position = position_dodge(width = 0.75), size = 4)



# The graph shows some differences in duration of foraging walking per strategy.
# So it would be interesting to analyse this further 

m0 <- lm(Duration ~ Strategy, data = for_walk)
print(m0)
summary(m0)

m1 <- lm(Duration ~ Week + Strategy, data = for_walk)
print(m1)
summary(m1)

m2 <- lm(Duration ~ Week * Strategy, data = for_walk)
print(m2)
summary(m2)

anova(m1,m2)
# The interaction model (m2) is significantly better than the main effects model (m1), indicating that the effect of week on duration depends on the strategy.
# This suggests that the relationship between week and duration is different for migrants and overwinterers.

anova(m0, m2)
# So adding Week as an interaction to the strategy makes the model a quite good predictior of the duration of foraging walking. The p-value is ***


m3 <- lm(Duration ~ Week * Strategy + Three_letter_code.x, data = for_walk)
anova(m2, m3) 

# Adding the Three letter code to the model does not make it a better predictive model for the duration of foraging walking.

m4 <- lmer(Duration ~ Week * Strategy + ( 1 | Transect_ID), data = for_walk)
print(m4)
summary(m4)

m5 <- lm(Duration ~ Week * Strategy + Tide, data = for_walk)
anova(m2, m5)

m6 <- lm(Duration ~ Week * Strategy + Habitat, data = for_walk)
anova(m2, m6)

# Adding Transect_ID, Tide or Habitat does not make it a better predictive model for the duration of foraging walking.

new_data <- expand.grid(
  Week = unique(for_walk$Week),
  Strategy = unique(for_walk$Strategy)
)

new_data$predicted <- predict(m2, newdata = new_data)

p3 <- p2 +
  geom_point(data = new_data,
             aes(x = as.factor(Week), y = predicted, color = Strategy, shape = Strategy),
             size = 3,
             position = position_dodge(width = 0.75)) +
  scale_shape_manual(values = c("migrant" = 17, "overwinterer" = 18)) +
  scale_color_manual(values = c("migrant" = "#a15454", "overwinterer" = "#3f6f9e"))

p3

ggsave("foraging_walking_duration_strategy.png", plot = p2, width = 13, height = 6, dpi = 300)
ggsave("foraging_walking_duration_strategy_with_predictions.png", plot = p3, width = 10, height = 6, dpi = 300)

###############################################################################




