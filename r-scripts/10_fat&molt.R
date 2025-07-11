
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
molt_data <- video_data |>
  filter(!(Three_letter_code %in% c("MAU", "PAE", "PNL", "PAJ", "LPT", "MTJ", "PKN", "MJA", "JHY", "KET", "PLA", "KKH", "KTP", "PVK", "LEA", "NUK", "NEL", "JJV", "KKT", "PJC")))
# Removed birds that did not molt

ggplot(molt_data, aes(x = as.Date(Date), y = Molting_score, 
                       color = Strategy, group = Three_letter_code)) +
  geom_point() +
  geom_line() +
  labs(title = "Molting Scores of Birds by Strategy",
       x = "Date",
       y = "Molting Score") +
  theme_minimal() +
  theme(legend.position = "bottom")

ggplot(molt_data, aes(x = Date, y = Molting_score, color = Strategy)) +
  geom_point(alpha = 0.5) +  # Optional: lower point opacity
  geom_smooth(se = FALSE, method = "loess") +  # or method = "gam", "lm"
  labs(title = "Smoothed Molting Trends by Strategy",
       x = "Date",
       y = "Molting Score") +
  theme_minimal() +
  theme(legend.position = "bottom")

slopes_with_strategy <- slopes_with_strategy |>
  group_by(Strategy) |>
  mutate(Molting_score =  2)


p_molt <- ggplot(molt_data, aes(x = Date, y = Molting_score, color = Strategy, group = Three_letter_code)) +
  geom_line(alpha = 0.5) +  # individual bird lines, faint
  geom_point(alpha = 0.5) +
  geom_smooth(aes(group = Strategy), se = FALSE, method = "loess", size = 1.5) +
  scale_color_manual(values = c("#E777F2", "#4DD2A4", "#4DC8F9")) +
  labs(title = "Moulting Scores of Birds with Smoothed Strategy Trends",
       x = "Date",
       y = "Moulting Score") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 21),
    axis.text.y = element_text(size = 21),
    axis.title.x = element_text(size = 23),
    axis.title.y = element_text(size = 23),
    plot.title = element_text(size = 26, face = "bold", hjust = 0.5),
    legend.position = "none") +
  geom_boxplot(data = slopes_with_strategy,
               mapping = aes(x = First_Date, y = Molting_score, fill = Strategy),
               alpha = 0.2, outlier.shape = NA, width = 0.5)
p_molt

ggsave("moult_rate.png", plot = p_molt, width = 28, height = 15, dpi = 300)

###########################################################################
## Need to make a new dataframe and not use video_data, because removed the birds that did not moult
p_fat<-ggplot(video_data, aes(x = Date, y = Fat_score, color = Strategy, group = Three_letter_code)) +
  geom_line(alpha = 0.5) +  # individual bird lines, faint
  geom_point(alpha = 0.5) +
  geom_smooth(aes(group = Strategy), se = FALSE, method = "loess", size = 1.5) +
  scale_color_manual(values = c("#E777F2", "#4DD2A4", "#4DC8F9")) +
  labs(title = "Fat Scores of Birds with Smoothed Strategy Trends",
       x = "Date",
       y = "Fat Score") +
  theme_minimal()  +
  theme(
    axis.text.x = element_text(size = 21),
    axis.text.y = element_text(size = 21),
    axis.title.x = element_text(size = 23),
    axis.title.y = element_text(size = 23),
    plot.title = element_text(size = 26, face = "bold", hjust = 0.5),
    legend.position = "none")
p_fat

ggsave("fat_rate.png", plot = p_fat, width = 28, height = 15, dpi = 300)
p_fat

########################################################################
#### remove observations so last day with moult score 5, is the day they scored 5 for the first time.
real_last_day <- molt_data |>
  filter(!(Observation_id %in% c("KXM_14.05", "JYL_08.05", "MCH_21.05", "KPM.15.05", "KPM.20.05", "PMP.20.05", "JHM.21.05", "KMC_13.05", "KMC_14.05", "KMC.15.05", "KMC_20.05", "JAE_08.05", "JAE.14.05", "JAE_20.05", "	
JLU.25.04", "KCY.21.05", "NKE.01.05", "NKE.14.05", "PHT_20.05", "JLH_20.05", "KVV_20.05", "NTV_14.05" )))
real_last_day <- real_last_day |>
  filter(!Observation_id %in% c("CCU_27.03", "KNP.17.04", "KNP.19.03", "KPM.27.02", "LYK.26.03", "NTV.24.04", "NTV.10.04", "NTV.09.04"))

# Calculate per-bird slope (molting change per day)
molting_slopes <- real_last_day %>%
  group_by(Three_letter_code) %>%
  arrange(Date) %>%
  summarise(
    First_Date = first(Date),
    Last_Date = last(Date),
    First_Score = first(Molting_score),
    Last_Score = last(Molting_score),
    Slope = (Last_Score - First_Score) / as.numeric(Last_Date - First_Date),
    .groups = "drop"
  )

slopes_with_strategy <- molting_slopes %>%
  left_join(video_data %>% dplyr::select(Three_letter_code, Strategy) %>% distinct(), by = "Three_letter_code") |>
  filter(!(Three_letter_code %in% c("KAX", "KKM", "KTN", "PJC")))
## to remove birds that where only seen in breeding plumage 

p_moultslopes <- ggplot(slopes_with_strategy, aes(x = Strategy, y= Slope, fill = Strategy)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#E777F2", "#4DD2A4", "#4DC8F9")) +
  labs(title = "Distribution of Moulting Slopes by Strategy",
       x = "Strategy",
       y = "Slope (Moulting Change per Day") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 21),
    axis.text.y = element_text(size = 21),
    axis.title.x = element_text(size = 23),
    axis.title.y = element_text(size = 23),
    plot.title = element_text(size = 27, face = "bold", hjust = 0.5),
    legend.position = "none")
p_moultslopes

ggsave("moult_slope.png", plot = p_moultslopes, width = 28, height = 15, dpi = 300)

ggplot(molting_slopes, aes(x = Slope)) +
  geom_histogram(bins = 30, fill = "#69b3a2", color = "black") +
  theme_minimal()

moult <- slopes_with_strategy |> 
    mutate(
    Slope_log = log(Slope + 1),      
    Slope_sqrt = sqrt(Slope)
  )

hist(moult$Slope_log, main = "Log-transformed")
hist(moult$Slope_sqrt, main = "Square-root-transformed")

model_logref <- lm(Slope_log ~ Last_Date * Strategy, data = moult)

model_squared <- lm(Slope_sqrt ~ Last_Date * Strategy, data = moult)

AIC(model_logref, model_squared)
plot(model_logref)
plot(model_squared)
qqnorm(resid(model_logref)); qqline(resid(model_logref))
qqnorm(resid(model_squared)); qqline(resid(model_squared))

shapiro.test(resid(model_logref))
shapiro.test(resid(model_squared))

lm1 <- lm(Slope_sqrt ~ 1,
            data = moult)

lm2 <- lm(Slope_sqrt ~ Strategy,
            data = moult)
summary(lm2)

model.sel(lm1, lm2)
anova(lm1, lm2, test = "Chisq")

lm3 <- lm(Slope_sqrt ~ First_Date,
            data = moult)

lm4 <- lm(Slope_sqrt ~ First_Date + Strategy,
            data = moult)

model.sel(lm1, lm2, lm3, lm4)

lm5 <- lm(Slope_sqrt ~ First_Date * Strategy,
            data = moult)

lm6 <- lm(Slope_sqrt ~ Last_Date,
            data = moult)

lm7 <- lm(Slope_sqrt ~ First_Date + Last_Date + Strategy,
            data = moult)
summary(lm7)

lm8 <- lm(Slope_sqrt ~ Last_Date * Strategy ,
            data = moult)

moult.model <- model.sel(lm1, lm2, lm3, lm4, lm5, lm6, lm8)
anova(lm3, lm4, lm5, lm6, lm7, lm8, test = "Chisq")
moult.model

plot(lm8)

install.packages("car") 
library(car)

vif(lm7)
## nothing above 2
vif(lm8, type = "predictor")
# but this one is probably better

emm_molt <- emmeans(lm8, ~ Strategy, type = "response")
pairs(emm_molt)
library(multcomp)

cld(emm_molt, Letters = letters, adjust = "tukey")
# I want a First_Date and Last_Date summarized for each Strategy in the data
slopes_with_strategy |>
  group_by(Strategy) |>
  summarise(
  Avg_First_Date = mean(First_Date, na.rm = TRUE),
  Avg_Last_Date = mean(Last_Date, na.rm = TRUE),
  Count = n_distinct(Three_letter_code)) |>
  arrange(desc(Count))




############################################################################
video_data <- video_data |>
  filter(!Observation_id %in% c("	
JLU.10.04", "	KPM.27.02", "KPM_27.03"))

# Calculate per-bird slope (molting change per day)
fatting_slopes <- video_data %>%
  group_by(Three_letter_code) %>%
  arrange(Date) %>%
  summarise(
    First_Date = first(Date),
    Last_Date = last(Date),
    First_Score = first(Fat_score),
    Last_Score = last(Fat_score),
    Slope = (Last_Score - First_Score) / as.numeric(Last_Date - First_Date),
    .groups = "drop"
  )

fatslopes_with_strategy <- fatting_slopes %>%
  left_join(video_data %>% dplyr::select(Three_letter_code, Strategy) %>% distinct(), by = "Three_letter_code")  |>
  filter(!(Three_letter_code %in% c("JJJ", "JYL")))
## birds with a slope of 0 removed

p_fat_slopes <- ggplot(fatslopes_with_strategy, aes(x = Strategy, y= Slope, 
                                    fill = Strategy)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#E777F2", "#4DD2A4", "#4DC8F9")) +
  labs(title = "Distribution of Fatting Slopes by Strategy",
       x = "Slope (Fatting Up per Day)",
       y = "Density") +
  theme_minimal()  +
  theme(
    axis.text.x = element_text(size = 19),
    axis.text.y = element_text(size = 19),
    axis.title.x = element_text(size = 21),
    axis.title.y = element_text(size = 21),
    plot.title = element_text(size = 25, face = "bold", hjust = 0.5),
    legend.position = "none")
p_fat_slopes

ggsave("fat_slope.png", plot = p_fat_slopes, width = 28, height = 15, dpi = 300)

### removing outlier (in two days fatting up from 2.5 to 5, not possible)
fatslopes_with_strategy <- fatslopes_with_strategy |>
  filter(!Three_letter_code %in% c("KTN", "MTJ", "LPT", "NEL", "JHY", "JJV", "KKT", "LEA", "PJC", "PKN"))

ggplot(fatslopes_with_strategy, aes(x = Slope)) +
  geom_histogram(bins = 30, fill = "#69b3a2", color = "black") +
  theme_minimal()

fatslopes_with_strategy$slope_log_reflected <- log(max(fatslopes_with_strategy$Slope, na.rm = TRUE) + 1 - fatslopes_with_strategy$Slope)
fatslopes_with_strategy$slope_squared <- fatslopes_with_strategy$Slope^2

model_logref <- lm(slope_log_reflected ~ Last_Date * Strategy, data = fatslopes_with_strategy)

model_squared <- lm(slope_squared ~ Last_Date * Strategy , data = fatslopes_with_strategy)

ggplot(fatslopes_with_strategy, aes(x = slope_log_reflected)) +
  geom_histogram(bins = 30, fill = "#69b3a2", color = "black") +
  theme_minimal()

AIC(model_logref, model_squared)
plot(model_logref)
plot(model_squared)
qqnorm(resid(model_logref)); qqline(resid(model_logref))

shapiro.test(resid(model_logref))

lm1 <- lm(slope_log_reflected ~ 1,
              data = fatslopes_with_strategy)

lm2 <- lm(slope_log_reflected ~ Strategy,
              data = fatslopes_with_strategy)
summary(lm2)

lm3 <- lm(slope_log_reflected ~ First_Date * Strategy,
              data = fatslopes_with_strategy)
summary(lm3)

lm4 <- lm(slope_log_reflected ~ First_Date + Strategy,
              data = fatslopes_with_strategy)
summary(lm4)

lm5  <- lm(slope_log_reflected ~ Last_Date + Strategy,
              data = fatslopes_with_strategy)

lm6 <- lm(slope_log_reflected ~ First_Date + Last_Date + Strategy,
              data = fatslopes_with_strategy)
summary(lm6)

model.sel(lm1, lm2, lm3, lm4, lm5, lm6)
anova(lm1, lm2, lm3, lm4, lm5, lm6, test = "Chisq")

emm_fat <- emmeans(lm2, ~ Strategy, type = "response")
pairs(emm_fat)
library(multcomp)

cld(emm_fat, Letters = letters, adjust = "tukey")

video_data |>
  group_by(Strategy) 
# I want to know how many of every strategy are present in the df video_data
video_data |>
  group_by(Strategy) |>
  summarise(Count = n_distinct(Three_letter_code)) |>
  arrange(desc(Count))

filtered_data |>
  group_by(category) |>
  summarise(Count = n_distinct(bird_code)) |>
  arrange(desc(Count))
