
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
  filter(!(Observation_id %in%  c("KPM.24.04(real)", "JTN.16.04", "PAE_25_04", "PAE.29.04", "PAE.30.04", "KNP.24.04", "PAE_15.05", "MYV_16.04", "	
PKN_24.04", "KJU.24.04")))
### removed double observation dates and birds with only one observation date

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


early_data <- video_data |>
  filter(Strategy == "early_northward_migration") |>
  group_by(Three_letter_code) |>
  filter(!(Observation_id %in%  c("CCU.10.04", "CCU_15.04", "JHM.16.04", "KET.20.03", "PVK_24.04", "HPL.29.04", "KKH_28.03", "KTP_24.04.High", "KTP.24.04", "JLU_10.04", "KMC_24.04", "KMC_14.05.ochtend", "MJA_08.05", "JAE_24.04", "JAE_14.05", "JXM_24.04", "HPL_29.04", "KMY.30.04")))
### removed double observation dates and birds with only one observation date

head(early_data$Date)
early_data$Date <- as.Date(early_data$Date, format = "%m/%d/%Y")

ggplot(early_data, aes(x = as.Date(Date), y = Molting_score, color = Three_letter_code, group = Three_letter_code)) +
  geom_point() +
  geom_line() +
  labs(title = "Molting Scores of Early Northward Migrating Birds",
       x = "Date",
       y = "Molting Score") +
  theme_minimal() +
  theme(legend.position = "bottom")

late_data <- video_data |>
  filter(Strategy == "late_northward_migration") |>
  group_by(Three_letter_code) |>
  filter(!(Observation_id %in%  c("JXN_17.04", "KLP.13.05", "KYC.16.04", "NVH.30.04", "HNW.17.04", "JXY.16.04", "KPN.07.05", "PHT.29.04", "PHT.15.05", "JCH.25.04", "KAH.16.04", "KTE.01.05", "MLC.16.04", "MLC_16.04", "	
PJC.21.05", "JHN.07.05", "KTM.16.04", "PJU_08.05", "PLH_20.05", "	
JJV.16.04", "KHK.01.05", "NPP.30.04", "JKL.29.04", "KKM_08.05", "NPY_08.05", "KKN.20.05", "NTV.10.04(2)", "NTV_24.04", "NTV.25.04", "JLO.28.03", "	
KKT_16.04", "KXV.01.05", "NUK_24.04")))
### removed double observation dates and birds with only one observation date

head(late_data$Date)
late_data$Date <- as.Date(late_data$Date, format = "%m/%d/%Y")

ggplot(late_data, aes(x = as.Date(Date), y = Molting_score, color = Three_letter_code, group = Three_letter_code)) +
  geom_point() +
  geom_line() +
  labs(title = "Molting Scores of Late Northward Migrating Birds",
       x = "Date",
       y = "Molting Score") +
  theme_minimal() +
  theme(legend.position = "bottom")

video_data <- video_data |>
  group_by(Three_letter_code, Strategy) |>
  filter(!(Observation_id %in%  c("KPM.24.04(real)", "JTN.16.04", "PAE_25_04", "PAE.29.04", "PAE.30.04", "KNP.24.04", "PAE_15.05", "MYV_16.04", "	
PKN_24.04", "KJU.24.04"))) |>
  filter(!(Observation_id %in%  c("CCU.10.04", "CCU_15.04", "JHM.16.04", "KET.20.03", "PVK_24.04", "HPL.29.04", "KKH_28.03", "KTP_24.04.High", "KTP.24.04", "JLU_10.04", "KMC_24.04", "KMC_14.05.ochtend", "MJA_08.05", "JAE_24.04", "JAE_14.05", "JXM_24.04", "HPL_29.04", "KMY.30.04"))) |>
  filter(!(Observation_id %in%  c("JXN_17.04", "KLP.13.05", "KYC.16.04", "NVH.30.04", "HNW.17.04", "JXY.16.04", "KPN.07.05", "PHT.29.04", "PHT.15.05", "JCH.25.04", "KAH.16.04", "KTE.01.05", "MLC.16.04", "MLC_16.04", "	
PJC.21.05", "JHN.07.05", "KTM.16.04", "PJU_08.05", "PLH_20.05", "	
JJV.16.04", "KHK.01.05", "NPP.30.04", "JKL.29.04", "KKM_08.05", "NPY_08.05", "KKN.20.05", "NTV.10.04(2)", "NTV_24.04", "NTV.25.04", "JLO.28.03", "	
KKT_16.04", "KXV.01.05", "NUK_24.04")))
video_data$Date <- as.Date(video_data$Date, format = "%m/%d/%Y")

ggplot(video_data, aes(x = as.Date(Date), y = Molting_score, 
                       color = Strategy, group = Three_letter_code)) +
  geom_point() +
  geom_line() +
  labs(title = "Molting Scores of Birds by Strategy",
       x = "Date",
       y = "Molting Score") +
  theme_minimal() +
  theme(legend.position = "bottom")

ggplot(video_data, aes(x = Date, y = Molting_score, color = Strategy)) +
  geom_point(alpha = 0.5) +  # Optional: lower point opacity
  geom_smooth(se = FALSE, method = "loess") +  # or method = "gam", "lm"
  labs(title = "Smoothed Molting Trends by Strategy",
       x = "Date",
       y = "Molting Score") +
  theme_minimal() +
  theme(legend.position = "bottom")

p_molt <- ggplot(video_data, aes(x = Date, y = Molting_score, color = Strategy, group = Three_letter_code)) +
  geom_line(alpha = 0.3) +  # individual bird lines, faint
  geom_point(alpha = 0.3) +
  geom_smooth(aes(group = Strategy), se = FALSE, method = "loess", size = 1.2) +
  scale_color_manual(values = c("#E777F2", "#4DD2A4", "#4DC8F9")) +
  labs(title = "Molting Scores of Birds with Smoothed Strategy Trends",
       x = "Date",
       y = "Molting Score") +
  theme_minimal() +
  theme(legend.position = "bottom")
p_molt

###########################################################################

p_fat<-ggplot(video_data, aes(x = Date, y = Fat_score, color = Strategy, group = Three_letter_code)) +
  geom_line(alpha = 0.3) +  # individual bird lines, faint
  geom_point(alpha = 0.3) +
  geom_smooth(aes(group = Strategy), se = FALSE, method = "loess", size = 1.2) +
  scale_color_manual(values = c("#E777F2", "#4DD2A4", "#4DC8F9")) +
  labs(title = "Fat Scores of Birds with Smoothed Strategy Trends",
       x = "Date",
       y = "Fat Score") +
  theme_minimal() +
  theme(legend.position = "bottom")
p_fat
