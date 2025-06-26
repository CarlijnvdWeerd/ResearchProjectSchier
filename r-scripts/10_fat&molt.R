
video_data <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRTzpl8cLIfyyycDNx_vbnulWr64YsPvfg0a4R9-q3hWTFq87Znl-odcKJX6LHaKgpCODNia0WpgQwy/pub?gid=2044682262&single=true&output=csv")

## remove NA's
video_data <- na.omit(video_data)
video_data <- video_data |>
  mutate(Molting_score = (Molting_score_Carlijn + Molting_score_Lisanne)/2) 
video_data <- video_data |>
  mutate(Fat_score = (Fat_score_Carlijn + Fat_score_Lisanne)/2)

video_data <- video_data |>
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

winter_data <- video_data |>
  filter(Strategy == "overwinterer") |>
  group_by(Three_letter_code) |>
  filter(!Observation_id == "KPM.24.04(real)")

head(winter_data$Date)


winter_data$Date <- as.Date(winter_data$Date, format = "%m/%d/%Y")



ggplot(winter_data, aes(x = as.Date(Date), y = Molting_score, color = Three_letter_code, group = Three_letter_code)) +
  geom_point() +
  geom_line() +
  labs(title = "Molting Scores of Overwintering Birds",
       x = "Date",
       y = "Molting Score") +
  theme_minimal() +
  theme(legend.position = "bottom")


