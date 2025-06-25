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


csv_urls <- c(
  "https://docs.google.com/spreadsheets/d/e/2PACX-1vQbC8HZliMJ3n3fpKuI0aZCOiWq_WKZtbxV2rgLU_u0cUSBYNdTDptJ1r43gbr_TLHIuKZxvEvEo0W5/pub?gid=1949364362&single=true&output=csv",
  "https://docs.google.com/spreadsheets/d/e/2PACX-1vQbC8HZliMJ3n3fpKuI0aZCOiWq_WKZtbxV2rgLU_u0cUSBYNdTDptJ1r43gbr_TLHIuKZxvEvEo0W5/pub?gid=1613773180&single=true&output=csv",
  "https://docs.google.com/spreadsheets/d/e/2PACX-1vQbC8HZliMJ3n3fpKuI0aZCOiWq_WKZtbxV2rgLU_u0cUSBYNdTDptJ1r43gbr_TLHIuKZxvEvEo0W5/pub?gid=613070440&single=true&output=csv",
  "https://docs.google.com/spreadsheets/d/e/2PACX-1vQbC8HZliMJ3n3fpKuI0aZCOiWq_WKZtbxV2rgLU_u0cUSBYNdTDptJ1r43gbr_TLHIuKZxvEvEo0W5/pub?gid=440870806&single=true&output=csv",
  "https://docs.google.com/spreadsheets/d/e/2PACX-1vQbC8HZliMJ3n3fpKuI0aZCOiWq_WKZtbxV2rgLU_u0cUSBYNdTDptJ1r43gbr_TLHIuKZxvEvEo0W5/pub?gid=33799828&single=true&output=csv",
  "https://docs.google.com/spreadsheets/d/e/2PACX-1vQbC8HZliMJ3n3fpKuI0aZCOiWq_WKZtbxV2rgLU_u0cUSBYNdTDptJ1r43gbr_TLHIuKZxvEvEo0W5/pub?gid=790226670&single=true&output=csv",
  "https://docs.google.com/spreadsheets/d/e/2PACX-1vQbC8HZliMJ3n3fpKuI0aZCOiWq_WKZtbxV2rgLU_u0cUSBYNdTDptJ1r43gbr_TLHIuKZxvEvEo0W5/pub?gid=1398166804&single=true&output=csv",
  "https://docs.google.com/spreadsheets/d/e/2PACX-1vQbC8HZliMJ3n3fpKuI0aZCOiWq_WKZtbxV2rgLU_u0cUSBYNdTDptJ1r43gbr_TLHIuKZxvEvEo0W5/pub?gid=1050370309&single=true&output=csv",
  "https://docs.google.com/spreadsheets/d/e/2PACX-1vQbC8HZliMJ3n3fpKuI0aZCOiWq_WKZtbxV2rgLU_u0cUSBYNdTDptJ1r43gbr_TLHIuKZxvEvEo0W5/pub?gid=2060297964&single=true&output=csv",
  "https://docs.google.com/spreadsheets/d/e/2PACX-1vQbC8HZliMJ3n3fpKuI0aZCOiWq_WKZtbxV2rgLU_u0cUSBYNdTDptJ1r43gbr_TLHIuKZxvEvEo0W5/pub?gid=1483582770&single=true&output=csv",
  "https://docs.google.com/spreadsheets/d/e/2PACX-1vTCLs0L8lKZBMmTHKpN1B3DFWjgMlWHo3iKv8f9yr5jyGc7kt90OTjsICfNveWx4dvWObvAmQ-yNi5X/pub?gid=184556996&single=true&output=csv",
  "https://docs.google.com/spreadsheets/d/e/2PACX-1vQbC8HZliMJ3n3fpKuI0aZCOiWq_WKZtbxV2rgLU_u0cUSBYNdTDptJ1r43gbr_TLHIuKZxvEvEo0W5/pub?gid=1355663027&single=true&output=csv",
  "https://docs.google.com/spreadsheets/d/e/2PACX-1vQbC8HZliMJ3n3fpKuI0aZCOiWq_WKZtbxV2rgLU_u0cUSBYNdTDptJ1r43gbr_TLHIuKZxvEvEo0W5/pub?gid=33667183&single=true&output=csv"
) 

m1 <- read.csv("C:/Users/conso/Downloads/NVT_WATHTBRKD_SCHIERMNOG_extremen.csv")
library(readr)
library(dplyr)
library(lubridate)
library(tidyr)
library(dplyr)

raw <- readLines("C:/Users/conso/Downloads/NVT_WATHTBRKD_SCHIERMNOG_extremen.csv")
head(raw, 15)
# Basis R
m1 <- read.csv("C:/Users/conso/Downloads/NVT_WATHTBRKD_SCHIERMNOG_extremen.csv", skip = 8, sep = ";", header = TRUE)

# Of met readr (beter met rare karakters)
library(readr)
m2 <- read_delim("C:/Users/conso/Downloads/NVT_WATHTBRKD_SCHIERMNOG_extremen.csv", delim = ";", skip = 8)

# Eerst de datum en tijd samenvoegen tot één tijdstempel, zodat je goed kunt sorteren
m2$datetime <- as.POSIXct(
  paste(m2$Datum, m2$`Nederlandse tijd`),
  format = "%d-%m-%Y %H:%M",
  tz = "CET"  # Nederlandse tijd
)

# Sorteer op datum en tijd
m2 <- m2[order(m2$datetime), ]

# Voeg een kolom toe met opeenvolgende nummers
m2$nummer <- seq_len(nrow(m2))

m2_clean$nummer <- rep(1:ceiling(nrow(m2_clean)/2), each = 2)[1:nrow(m2_clean)]

library(lubridate)
library(readr)
library(dplyr)
library(purrr)

read_and_clean <- function(url) {
  df <- tryCatch(
    read_csv(url, show_col_types = FALSE),
    error = function(e) {
      message("❌ Error reading ", url)
      return(NULL)
    }
  )
  
  if (is.null(df)) return(NULL)
  
  # Standaardiseer kolomnamen: trim whitespace
  colnames(df) <- trimws(colnames(df))
  
  # Check en hernoem varianten van datum-kolom
  if ("Collecting time" %in% names(df)) {
    df <- dplyr::rename(df, Collecting_time = `Collecting time`)
  } else if ("Collecting_time" %in% names(df)) {
    # Al goed, niks doen
  } else if ("collecting_time" %in% names(df)) {
    df <- dplyr::rename(df, Collecting_time = collecting_time)
  }
  
  # Parse Collecting_time of maak NA als niet aanwezig
  if ("Collecting_time" %in% names(df)) {
    df$Collecting_time <- as.POSIXct(df$Collecting_time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
    # Probeer alternatieve ISO-formaat als eerste mislukt
    if (all(is.na(df$Collecting_time))) {
      df$Collecting_time <- as.POSIXct(df$Collecting_time, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    }
  } else {
    df$Collecting_time <- as.POSIXct(NA)
  }
  
  # ODBA naar numeriek indien aanwezig
  if ("ODBA" %in% names(df)) {
    df$ODBA <- as.numeric(df$ODBA)
  } else {
    df$ODBA <- NA_real_
  }
  
  # UUID naar character indien aanwezig
  if ("UUID" %in% names(df)) {
    df$UUID <- as.character(df$UUID)
  } else {
    df$UUID <- NA_character_
  }
  
  # Voeg bron-url toe
  df$source_url <- url
  
  return(df)
}

# Remove any URLs that do not end with output=csv
csv_urls_clean <- csv_urls[grepl("output=csv$", csv_urls)]

library(purrr)

combined_odba <- map_dfr(csv_urls_clean, read_and_clean)

combined_odba_clean <- combined_odba %>%
  filter(!is.na(Collecting_time), is.finite(Collecting_time))

ggplot(combined_odba_clean, aes(x = ODBA)) + 
  geom_histogram(bins = 200, fill = "steelblue", color = "black") + 
  theme_minimal() + 
  labs(title = "Distribution of ODBA-values")
ggsave("ODBA_histogram.png", dpi = 600, width = 8, height = 6, units = "in")

set.seed(123)
kmeans_res <- kmeans(df$ODBA, centers = 2)
cluster_centers <- sort(kmeans_res$centers)
drempel <- mean(cluster_centers)  # grens tussen clusters
print(drempel)

library(lubridate)
# Zet de kolom om naar POSIXct, specificeer dat het in UTC is
combined_odba_clean$Transmitting_time_utc <- ymd_hms(combined_odba_clean$Collecting_time, tz = "UTC")

# Zet om naar Amsterdam tijdzone (Nederland)
combined_odba_clean$Transmitting_time_nl <- with_tz(combined_odba_clean$Collecting_time, tzone = "Europe/Amsterdam")

# Nieuwe kolom toevoegen op basis van ODBA waarde
combined_odba_clean$foerageer <- ifelse(combined_odba_clean$ODBA > 2506.483, TRUE, FALSE)

# Bekijk resultaat
head(combined_odba_clean[, c("ODBA", "foerageer")])

# Zorg dat de datetime-kolommen in POSIXct staan:
m2$datetime <- as.POSIXct(m2$datetime, tz="Europe/Amsterdam")
combined_odba_clean$Transmitting_time_nl <- as.POSIXct(combined_odba_clean$Transmitting_time_nl, tz="Europe/Amsterdam")

any(is.na(tijd_vector))

# Maak een vector van getijtijden (inclusief laatste tijd + 1 interval om laatste moment te vangen)
tijd_vector <- c(m2$datetime, max(m2$datetime) + 3600*12) # voeg 12 uur toe als eindpunt

# Bepaal voor elke ODBA tijd het intervalnummer:
combined_odba_clean$interval_nummer <- findInterval(combined_odba_clean$Transmitting_time_nl, tijd_vector)

# Check:
head(combined_odba_clean[, c("Transmitting_time_nl", "interval_nummer")])



library(dplyr)

# Maak een kleine tabel met alleen de kolommen die je wil toevoegen
water_info <- m2_clean %>% 
  dplyr::select(nummer, nummer2, `Hoogwater/laagwater`)

# Voeg toe aan combined_odba_clean op basis van interval_nummer = nummer
combined_odba_clean <- combined_odba_clean %>%
  left_join(water_info, by = c("interval_nummer" = "nummer2"))


library(dplyr)

# Eerst sorteren op vogel en tijd:
combined_odba_clean <- combined_odba_clean %>%
  arrange(UUID, Transmitting_time_nl)

combined_odba_clean <- combined_odba_clean %>%
  group_by(UUID, interval_nummer) %>%
  mutate(
    tijd_verschil_sec = as.numeric(difftime(lead(Transmitting_time_nl), Transmitting_time_nl, units = "secs"))
  ) %>%
  ungroup()
summary(combined_odba_clean$tijd_verschil_sec)

combined_odba_clean <- combined_odba_clean %>%
  filter(tijd_verschil_sec %in% c(60, 600))


foerageertijd_per_getij <- combined_odba_clean %>%
  filter(foerageer == TRUE) %>%  # alleen records waar vogel foerageert
  group_by(UUID, interval_nummer) %>%
  dplyr::summarise(totaal_foerageertijd_sec = sum(tijd_verschil_sec, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(totaal_foerageertijd_uur = totaal_foerageertijd_sec / 3600)

gemiddelde_foerageertijd_per_getij <- foerageertijd_per_getij %>%
  group_by(interval_nummer) %>%
  dplyr::summarise(gemiddelde_uur = mean(totaal_foerageertijd_uur, na.rm = TRUE))

ggplot(gemiddelde_foerageertijd_per_getij, aes(x = interval_nummer, y = gemiddelde_uur)) +
  geom_line(color = "steelblue") +
  geom_point(color = "darkblue") +
  labs(
    title = "Gemiddelde foerageertijd per getij (alleen foerageren)",
    x = "Getij intervalnummer",
    y = "Gemiddelde foerageertijd (uur)"
  ) +
  theme_minimal()

library(ggplot2)
library(dplyr)

# Data met individuele vogels per getij
foerageertijd_per_getij <- combined_odba_clean %>%
  filter(foerageer == TRUE) %>%
  group_by(UUID, interval_nummer, nummer) %>%
  dplyr::summarise(totaal_foerageertijd_sec = sum(tijd_verschil_sec, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(totaal_foerageertijd_uur = totaal_foerageertijd_sec / 3600)

# Gemiddelde per getij
gemiddelde_per_getij <- foerageertijd_per_getij %>%
  group_by(interval_nummer) %>%
  dplyr::summarise(gemiddelde_uur = mean(totaal_foerageertijd_uur, na.rm = TRUE)) %>%
  ungroup()

# Tel het aantal observaties per interval_nummer
aantal_per_interval <- foerageertijd_per_getij %>%
  group_by(interval_nummer) %>%
  dplyr::summarise(aantal = n()) %>%
  filter(aantal >= 3)  # alleen intervalnummers met 3 of meer observaties behouden

# Filter de originele dataset op die interval_nummer met genoeg observaties
foerageertijd_per_getij_filtered <- foerageertijd_per_getij %>%
  filter(interval_nummer %in% aantal_per_interval$interval_nummer)


library(dplyr)

foerageertijd_met_getij1 <- left_join(
  foerageertijd_per_getij_filtered,
  m2_clean %>%
    dplyr::select(nummer2, getij = `Hoogwater/laagwater`),  # hernoem hier als dat lukt
  by = c("interval_nummer" = "nummer2")
)

# Aanname: je dataframe heet foerageertijd_met_getij
foerageertijd_per_vogel_en_nummer <- foerageertijd_met_getij1 %>%
  group_by(UUID, nummer) %>%
  dplyr::summarise(
    totale_foerageertijd_uur = sum(totaal_foerageertijd_uur, na.rm = TRUE),
    .groups = "drop"
  )

# Bekijk het resultaat
head(foerageertijd_per_vogel_en_nummer)


library(ggplot2)

p <- ggplot(foerageertijd_per_vogel_en_nummer, aes(x = nummer, y = totale_foerageertijd_uur)) +
  geom_point(aes(color = factor(UUID)), alpha = 0.6, size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "black", size = 1.2) +
  labs(
    title = "Foerageertijd per vogel en lineaire trend per getijcyclus",
    x = "Getijcyclus (nummer)",
    y = "Totale foerageertijd (uur)",
    color = "Vogel (UUID)"
  ) +
  theme_minimal()

p


p <- ggplot(foerageertijd_met_getij1, aes(x = nummer, y = totaal_foerageertijd_uur)) +
  geom_point(aes(color = UUID), alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "black", size = 1.2) +
  labs(
    title = "Foerageertijd per vogel en lineaire trend per getij",
    x = "Tide number",
    y = "Foraging time (hour)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
p
ggsave("foerageertijd_per_getij.png", plot = , width = 12, height = 8, dpi = 300)


###### STATISTICS
library(tidyr)
library(dplyr)

# Zet data in brede vorm: HW en LW in aparte kolommen per UUID en nummer
breed <- foerageertijd_met_getij1 %>%
  select(UUID, nummer, getij, totaal_foerageertijd_uur) %>%
  pivot_wider(
    names_from = getij,
    values_from = totaal_foerageertijd_uur
  )

breed <- breed %>%
  mutate(
    totaal = HW + LW,
    verschil = LW - HW,
    verhouding = LW / HW
  )

# Verwijder rijen waar 'totaal' NA is
breed <- breed[!is.na(breed$totaal), ]


# Shapiro-Wilk test voor normaliteit van het verschil tussen LW en HW
shapiro.test(breed$totaal)
shapiro.test(breed$verschil)
# Histogram
hist(breed$totaal, breaks = 10, main = "Histogram verschil LW - HW", xlab = "Verschil (uur)")
# QQ-plot
qqnorm(breed$totaal)
qqline(breed$totaal, col = "red")
t.test(breed$LW, breed$HW, paired = TRUE)
wilcox.test(breed$LW, breed$HW, paired = TRUE)

library(lme4)
library(MuMIn)  # Voor AICc en modelvergelijking
breed$UUID <- factor(breed$UUID)
breed$nummer <- as.numeric(breed$nummer)  # nummer is eigenlijk numeriek, dus juist geen factor

model_null <- lmer(totaal ~ 1 + (1 | UUID), data = breed, REML = FALSE)
model_lw <- lmer(totaal ~ 1, data = breed, REML = FALSE)
model_nummer <- lmer(totaal ~ nummer + (1 | UUID), data = breed, REML = FALSE)
model2 <- lmer(totaal ~ nummer + verschil + (1 | UUID), data = breed, REML = FALSE)
model5 <- lmer(totaal ~ nummer * verschil + (1 | UUID), data = breed, REML = FALSE)

AIC(model2, model5, model_null, model_nummer)
summary(model_nummer)

library(lmerTest)  # voor je model

# Voeg voorspellingen toe aan model_data
model_data$predicted <- predict(model_nummer, re.form = NA)

# Plot met model_data, die overeenkomt met model_nummer
library(ggplot2)

ggplot(model_data, aes(x = nummer, y = totaal)) +
  geom_point(aes(colour = factor(UUID)), alpha = 0.6) +
  geom_line(aes(y = predicted), colour = "black", size = 1) +
  labs(title = "Foerageertijd over getijcyclus nummer per vogel",
       x = "Getijcyclus nummer",
       y = "Totale foerageertijd (uur)",
       colour = "Vogel ID (UUID)") +
  theme_minimal()
ggsave("foerageertijd_per_getij_model.png", plot = last_plot(), width = 12, height = 8, dpi = 300)

mean_totaal <- mean(breed$totaal, na.rm = TRUE)
sd_totaal <- sd(breed$totaal, na.rm = TRUE)

cat("Gemiddelde foerageertijd per getij:", round(mean_totaal, 2), "uur\n")
cat("Standaardafwijking:", round(sd_totaal, 2), "uur\n")


