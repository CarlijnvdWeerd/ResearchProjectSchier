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
point_behaviors <- complete_dataset |>
  filter(Behavior.type == "POINT") |>
  group_by(Observation_id, Behavior) |>
  summarise(Count = n(), .groups = "drop")

# filter duration dataframe 
durations <- durations |>
  dplyr::select(c(Observation_id, Behavior, Duration))

# CAN NOT JOIN DURATION WITH COMPLETE DATASET BECAUSE DURATION OF BEHAVIOUR IS IN A PAIR WHILE IN COMPLETE DATASET IT IS SEPARATE

# Join durations with the complete_dataset
# complete_dataset <- complete_dataset |>
#  left_join(durations, by = c("Observation_id", "Behavior"))

# if the duration is NA, then set it to 0
# complete_dataset$Duration[is.na(complete_dataset$Duration)] <- 0

###############################################################################

complete_dataset$Duration <- as.numeric(complete_dataset$Duration)

#Summarize per individual per behaviour
summary_data <- complete_dataset |>
  group_by(Observation_id, Strategy, Behavior) |>
  summarise(
    count = n(),
    total_duration = sum(Duration, na.rm = TRUE)
  )

ggplot(summary_data, aes(x = Strategy, y = total_duration, fill = Behavior)) +
  geom_boxplot() +
  facet_wrap(~Behavior)

# point_behaviors needs to have the Strategy column of complete_dataset
point_behaviors <- point_behaviors |>
  left_join(complete_dataset |>
              dplyr::select(Observation_id, Strategy) |>
              distinct(), by = "Observation_id")

ggplot(point_behaviors, aes(x = Strategy, y = Count, fill = Behavior)) +
  geom_boxplot() +
  labs(
    title = "Point Behaviors by Strategy",
    x = "Strategy",
    y = "Count of Point Behaviors"
  ) +
  theme_minimal(
    base_size = 14
  ) +
  scale_fill_brewer(palette = "Set3") +
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
  )

#####################################################################
# Check for other differences
# I want to be for social_behavior only possibilities to be "Yes" and "No", "No " should become the same as "No" and if " " than it also should be "No"
complete_dataset$Social_behavior <- as.character(complete_dataset$Social_behavior)
complete_dataset$Social_behavior[complete_dataset$Social_behavior == "No "] <- "No"
complete_dataset$Social_behavior[complete_dataset$Social_behavior == " "] <- "No"

complete_dataset |>
 count(Social_behavior)


ggplot(complete_dataset, aes(x=Social_behavior, fill=Strategy)) +
  geom_bar(position="fill") +
  labs(title="Social Behavior by Strategy", x="Social Behavior", y="Proportion") +
  scale_fill_manual(values=c("#FF9999", "#66B3FF"))

Social <- complete_dataset |>
  filter(Social_behavior == "Yes") |>
  group_by(Strategy) 

ggplot(Social, aes(x = Aggressive.or.submissive, fill=Strategy)) +
  geom_bar(position="fill") +
  labs(title="Aggressive or Submissive Behavior by Strategy", x="Behavior Type", y="Proportion") +
  scale_fill_manual(values=c("#FF9999", "#66B3FF"))    

Social <- Social |>
  filter(Aggressive.or.submissive %in% c("Aggressive", "Both", "Submissive"))

Social |> count(Aggressive.or.submissive)
Social <- Social |>
  mutate(Aggressive = ifelse(Aggressive.or.submissive == "Aggressive", 1, 0)) |>
  group_by(Week, overwinterer = Strategy) |>
  summarise(Aggressive = sum(Aggressive), .groups = "drop")
ggplot(Social, aes(x=Week, y=Aggressive, fill=overwinterer)) +
  geom_bar(stat="identity", position="dodge") +
  labs(title="Aggressive Behavior by Week and Strategy", x="Week", y="Count of Aggressive Behavior") +
  scale_fill_manual(values=c("#FF9999", "#66B3FF")) +
  theme_minimal(base_size = 14)
