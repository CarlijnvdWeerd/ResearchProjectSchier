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
  dplyr::mutate(Strategy = case_when(
    Three_letter_code %in% c("JTN", "JYL", "KJU", "KNP", "KPM", "KXM", "LCT", "LMN", "LPT", 
                             "LYK", "MAU", "MCH", "MTJ", "MYV", "NLJ", "PAE", "PAJ", "PKN", 
                             "PMP", "PNL") ~ "overwinterer",
    
    Three_letter_code %in% c("PVK", "PLA", "MJA", "MCV", "KTP", "KNX", "KMY", "KMC", "KKH", 
                             "KET", "JXM", "JTC", "JLU", "JHY", "JHM", "JAE", "HPV", "HPL", 
                             "CCU") ~ "early_northward_migration",
    
    TRUE ~ "late_northward_migration"  # default case for all others
  ))

# I want to be for social_behavior only possibilities to be "Yes" and "No", "No " should become the same as "No" and if " " than it also should be "No"
complete_dataset$Social_behavior <- as.character(complete_dataset$Social_behavior)
complete_dataset$Social_behavior[complete_dataset$Social_behavior == "No "] <- "No"
complete_dataset$Social_behavior[complete_dataset$Social_behavior == " "] <- "No"

complete_dataset |>
  count(Social_behavior)


Social <- complete_dataset |>
  filter(Social_behavior == "Yes") |>
  group_by(Strategy) 

ggplot(Social, aes(x = Aggressive.or.submissive, fill=Strategy)) +
  geom_bar(position="fill") +
  labs(title="Aggressive or Submissive Behavior by Strategy", x="Behavior Type", y="Proportion") +
  scale_fill_manual(values=c("#FF9999","#1ED760", "#66B3FF"))    

# Calculate counts per Week and Strategy
counts <- Social |>
  group_by(Week, Strategy) |>
  summarise(n = n_distinct(Observation_id), .groups = "drop") |>
  filter(!Week %in% c("9", "11", "15"))

Social <- Social |>
  group_by(Three_letter_code, Behavior, Strategy, Week, Tide, Habitat,   
           Observation_id, Transect_ID, Date.x, Media.duration..s.,    
           Social_behavior, Aggressive.or.submissive) |>
  summarise(Behavior_Count = n(), .groups = "drop")

  Averaged_Social <- Social |>
  group_by(Week, Behavior, Strategy, Tide, Habitat, Transect_ID, Date.x, Media.duration..s., Social_behavior, Aggressive.or.submissive) |>
  summarise(Social_Count = mean(Behavior_Count, na.rm = TRUE), .groups = "drop")
  
  Social_annotated <- Social |>
    group_by(Strategy, Aggressive.or.submissive) |>
    mutate(Count = n()) |>
    ungroup()
  



p1 <- ggplot(Social,
             aes(x = as.factor(Week), y = Aggressive.or.submissive, 
                 fill = Strategy)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#FF9999", "#1ED760", "#66B3FF")) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Social behaviour by Strategy",
    x = "Week",
    y = "Category")

p1
