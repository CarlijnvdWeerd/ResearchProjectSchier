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

complete_dataset <- complete_dataset |>
  dplyr::mutate(Strategy = ifelse(
    Three_letter_code %in% c("JTN", "JYL", "KJU", "KNP", "KPM", "KXM", "LCT", "LMN", "LPT", "LYK", "MAU", "MCH", "MTJ", "MYV", "NLJ", "PAE", "PAJ", "PKN", "PMP", "PNL"),
    "overwinterer",
    "migrant"
  ))

# I want to be for social_behavior only possibilities to be "Yes" and "No", "No " should become the same as "No" and if " " than it also should be "No"
complete_dataset$Social_behavior <- as.character(complete_dataset$Social_behavior)
complete_dataset$Social_behavior[complete_dataset$Social_behavior == "No "] <- "No"
complete_dataset$Social_behavior[complete_dataset$Social_behavior == " "] <- "No"

complete_dataset |>
  count(Social_behavior)


ggplot(complete_dataset |> filter(Analysed == "Yes"), aes(x=Aggressive.or.submissive, fill=Strategy)) +
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
# Save the plot
ggsave("Aggressive_Behavior_by_Week_and_Strategy.png", width = 10, height = 6)

