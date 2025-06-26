### script to analyse biometric data

ring_data <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQAgtmW1VexFUfTdRqa3r15Qke29Kd9niyy0w0wtojaSNmlyR9kdLcC2XqV5XzVZnL9m0tTdBybK0RC/pub?gid=1616529556&single=true&output=csv")

#observation_data <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQl5pI4xRYpDKPVxhR_9XzzAtZetAqFbsSrpdtAzhqFHmvlBKZD0m9s-5jymajZGA/pub?gid=746495893&single=true&output=csv")

ring_data <- ring_data |>
  dplyr::mutate(Strategy = case_when(
    Flagcode %in% c("Of-AUC/R", "Of-CLC/R", "Of-CTC/R", "Of-ECC/R", "Of-JEL/R", "Of-JLT/R", "Of-JNL/R", "Of-JPU/R", "Of-JTN/R", "Of-JYL/R","Of-KJU/R", "Of-KKN/R", "Of-KNP/R", "Of-KPM/R", "Of-KTJ/R", "Of-KXM/R", "Of-LAV/R", "Of-LCP/R", "Of-LCT/R", "Of-LEH/R","Of-LHE/R", "Of-LLM/R", "Of-LLP/R", "Of-LMN/R", "Of-LNJ/R", "Of-LPC/R", "Of-LPE/R", "Of-LPL/R", "Of-PLT/R", "Of-LPT/R", "Of-LTA/R", "Of-LTV/R", "Of-LUC/R", "Of-LYK/R", "Of-MAU/R", "Of-MCH/R", "Of-MCU/R", "Of-MEY/R", "Of-MHN/R", "Of-MKE/R", "Of-MKY/R", "Of-MNN/R", "Of-MPC/R", "Of-MPV/R", "Of-MTJ/R", "Of-MYV/R", "Of-NJM/R", "Of-NJU/R", "Of-NLJ/R", "Of-NNC/R", "Of-NPE/R", "Of-NTV/R", "Of-NUM/R", "Of-NUN/R", "Of-NVK/R", "Of-PAE/R", "Of-PAJ/R", "Of-PAC/R", "Of-PAM/R", "Of-PHN/R", "Of-PKN/R", "Of-PMP/R", "Of-PNL/R", "Of-PPT/R", "Of-PTA/R", "Of-PVA/R", "Of-PXX/R", "Of-PYC/R")  ~ "overwinterer",
    Flagcode %in% c("Of-CCU/R", "Of-HPL/R", "Of-HPV/R", "Of-JAE/R", "Of-JHL/R", "Of-JHM/R", "Of-JHY/R", "Of-JLU/R", "Of-JNN/R", "Of-JTC/R","Of-JXM/R", "Of-KCY/R", "Of-KET/R", "Of-KKH/R", "Of-KMC/R", "Of-KMY/R", "Of-KNX/R", "Of-KPH/R", "Of-KTP/R", "Of-KYM/R", "Of-MCV/R", "Of-MJA/R", "Of-NKA/R", "Of-NNP/R", "Of-NUK/R", "Of-PCU/R", "Of-PLA/R", "Of-PNV/R", "Of-PVK/R") ~ "early_northward_migration",
    Flagcode %in% c("Of-AVM/R", "Of-HNW/R", "Of-JCH/R", "Of-JHN/R", "Of-JJJ/R", "Of-JJV/R", "Of-JKM/R", "Of-JLH/R", "Of-JMN/R", "Of-JMU/R", "Of-JNH/R", "Of-JNM/R", "Of-JPX/R", "Of-JVH/R", "Of-JVL/R", "Of-JXN/R", "Of-JXY/R", "Of-KAH/R", "Of-KAX/R", "Of-KCT/R", "Of-KEN/R", "Of-KHE/R", "Of-KHK/R", "Of-KHU/R", "Of-KKM/R", "Of-KKT/R", "Of-KLP/R", "Of-KMT/R", "Of-KMV/R", "Of-KPN/R", "Of-KPU/R", "Of-KTE/R", "Of-KTK/R", "Of-KTN/R", "Of-KTM/R", "Of-KTT/R", "Of-KTV/R", "Of-KUH/R", "Of-KUL/R", "Of-KUX/R", "Of-KVA/R", "Of-KVC/R", "Of-KVH/R", "Of-KVV/R", "Of-KXC/R", "Of-KXV/R", "Of-KYC/R", "Of-LAJ/R", "Of-LCV/R", "Of-LEA/R", "Of-MAC/R", "Of-MCP/R", "Of-MJM/R", "Of-MLC/R", "Of-MXL/R", "Of-NEL/R", "Of-NKE/R", "Of-NLL/R", "Of-NNN/R", "Of-NPA/R", "Of-NPP/R", "Of-NPY/R", "Of-NUU/R", "Of-NXP/R", "Of-NYA/R", "Of-PAA/R", "Of-PHT/R", "Of-PJC/R", "Of-PJU/R", "Of-PLK/R", 
"Of-PXA/R", "Of-PAN/R", "Of-MAT/R", "Of-MCN/R", "Of-PLH/R") ~ "late_northward_migration",
    TRUE ~ "not_seen_in_2025"))

ring_data$Wing <- as.numeric(ring_data$Wing)
ring_data <- ring_data |>
  filter(!Ringnumber %in% c("Z096328", "Z121557"))

qqnorm(ring_data$Weight)
qqnorm(ring_data$Wing)
qqnorm(ring_data$Beak)
qqnorm(ring_data$Tarsus)

ring_data <- ring_data %>%
  mutate(
    Catchnight_end = mdy(Catchnight_end),
    Year = as.factor(year(Catchnight_end))
  )

ring_data <- ring_data |>
  filter(Year %in% c("2023", "2024", "2025"))

ring_counts <- ring_data |>
  group_by(Year, Strategy) |>
  summarise(n = n_distinct(Ringnumber), .groups = "drop") |>
  filter(Year %in% c("2023", "2024", "2025"))

p1a <- ggplot(ring_data,
              aes(x = as.factor(Year), y = Wing, 
                  fill = Strategy)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#E777F2", "#4DD2A4","#BFC04D", "#4DC8F9")) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Wing lenght per Strategy",
    x = "Strategy",
    y = "Wing length in mm") +
  theme(
    axis.text.x = element_text(size = 19),
    axis.text.y = element_text(size = 19),
    axis.title.x = element_text(size = 21),
    axis.title.y = element_text(size = 21),
    plot.title = element_text(size = 25, face = "bold", hjust = 0.5), 
    legend.position = "none",)
p1a      

p1b <- p1a + geom_text(data = ring_counts, aes(x = as.factor(Year), 
                                                                                 y = 172, 
                                                                                 label = paste0("", n)),
                       position = position_dodge(width = 0.8), size = 8)
p1b

library(dplyr)
library(lubridate)
#year_data <- ring_data %>%
#  mutate(Catchnight_end = mdy(Catchnight_end),
#    Season = case_when(
#      Catchnight_end >= make_date(year(Catchnight_end), 1, 1)  & #Catchnight_end <= make_date(year(Catchnight_end), 3, 20) ~ paste0(year(Catchnight_end), ".winter"),
#      Catchnight_end >= make_date(year(Catchnight_end), 3, 21) & Catchnight_end <= make_date(year(Catchnight_end), 6, 20) ~ paste0(year(Catchnight_end), ".spring"),
#      Catchnight_end >= make_date(year(Catchnight_end), 6, 21) & Catchnight_end <= make_date(year(Catchnight_end), 9, 21) ~ paste0(year(Catchnight_end), ".summer"),
#      Catchnight_end >= make_date(year(Catchnight_end), 9, 22) & Catchnight_end <= make_date(year(Catchnight_end), 12, 31) ~ paste0(year(Catchnight_end), ".winter"),
#      TRUE ~ NA_character_  )) 

#year_data <- year_data|>
#  group_by(Season, Strategy) |>
#  select(Ringsites_ID, Catchnight_end,  Species_ID, Ringnumber, Ringer_ID, Age_form, Sex, Weight, Wing, Beak, Tarsus, Moult_index, Plumage, Flagcode, #Influenza_code, Strategy, Season) |>
#  filter(Season %in% c("2023.winter", "2023.spring", "2023.summer", "2023.autumn","2024.winter", "2024.spring", "2024.summer", "2024.autumn", "2025.winter", "2025.spring", "2025.summer", "2025.autumn"))

#year_counts <- year_data |>
#  group_by(Season, Strategy) |>
#  summarise(n = n_distinct(Ringnumber), .groups = "drop") |>
#  filter(Season %in% c("2023.winter", "2023.summer", "2024.winter", "2024.summer", "2025.winter", "2025.spring"))


p2a <- ggplot(ring_data,
              aes(x = as.factor(Year), y = Weight, 
                  fill = Strategy)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#E777F2", "#4DD2A4","#BFC04D", "#4DC8F9")) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Weight per Strategy",
    x = "Year",
    y = "Weight in grams") +
  theme(
    axis.text.x = element_text(size = 19),
    axis.text.y = element_text(size = 19),
    axis.title.x = element_text(size = 21),
    axis.title.y = element_text(size = 21),
    plot.title = element_text(size = 25, face = "bold", hjust = 0.5),
    legend.position = "none")
p2a      

p2b <- p2a + geom_text(data = ring_counts, aes(x = as.factor(Year), 
                                               y = 172, 
                                               label = paste0("", n)),
                       position = position_dodge(width = 0.8), size = 8)
p2b


      
p3a <- ggplot(ring_data,
              aes(x = as.factor(Year), y = Beak, 
                  fill = Strategy)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#E777F2", "#4DD2A4","#BFC04D", "#4DC8F9")) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Beak length per Strategy",
    x = "Year",
    y = "Beak length in mm") +
  theme(
    axis.text.x = element_text(size = 19),
    axis.text.y = element_text(size = 19),
    axis.title.x = element_text(size = 21),
    axis.title.y = element_text(size = 21),
    plot.title = element_text(size = 25, face = "bold", hjust = 0.5),
    legend.position = "none")
p3a  

p3b <- p3a + geom_text(data = ring_counts, aes(x = as.factor(Year), 
                                               y = 29, 
                                               label = paste0("", n)),
                       position = position_dodge(width = 0.8), size = 8)
p3b

p4a <- ggplot(ring_data,
              aes(x = as.factor(Year), y = Tarsus, 
                  fill = Strategy)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#E777F2", "#4DD2A4","#BFC04D", "#4DC8F9")) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Tarsus length per Strategy",
    x = "Year",
    y = "Tarsus length in cm") +
  theme(
    axis.text.x = element_text(size = 19),
    axis.text.y = element_text(size = 19),
    axis.title.x = element_text(size = 21),
    axis.title.y = element_text(size = 21),
    plot.title = element_text(size = 25, face = "bold", hjust = 0.5),
    legend.position = "none")
p4a  

p4b <- p4a + geom_text(data = ring_counts, aes(x = as.factor(Year), 
                                               y = 30, 
                                               label = paste0("", n)),
                       position = position_dodge(width = 0.8), size = 8)
p4b

##########################################################
# Check for normality of wing
ggplot(ring_data, aes(x = Wing)) +
  geom_histogram(bins = 30, fill = "#69b3a2", color = "black") +
  theme_minimal()


glm1 <- glm(Wing ~ 1, 
           family = gaussian(),
            data = ring_data)

glm2 <- glm(Wing ~ Year,
           family = gaussian(),
            data = ring_data)

glm3 <- glm(Wing ~ Strategy,
            family = gaussian(),
            data = ring_data)

glm4 <- glm(Wing ~ Year + Strategy,
            family = gaussian(),
            data = ring_data)

glm5 <- glm(Wing ~ Year * Strategy,
            family = gaussian(),
            data = ring_data)

anova(glm1, glm2, glm3, glm4, glm5)  
model.sel(glm1, glm2, glm3, glm4, glm5)

library(emmeans)
# Compute emmeans
#emm_wing <- emmeans(lm5, ~ Strategy | Year, type = "response")

library(multcomp)

# Use cld to assign group letters
#cld_wing <- cld(emm_wing, adjust = "tukey", Letters = letters, type = "response")

# View result
#print(cld_wing)

#cld_wing$Year <- as.factor(cld_wing$Year)  # Match plot's x-axis

# Example: pick a position slightly above max wing values
library(dplyr)

#wing_label_positions <- ring_data %>%
#  group_by(Year, Strategy) %>%
#  summarise(y_pos = max(Wing, na.rm = TRUE) + 2)  # Adjust +2 as needed

# Join with letters
#cld_plot_wing <- left_join(cld_wing, wing_label_positions, by = c("Year", "Strategy"))

p1c <- p1b +
  geom_text(data = cld_plot_wing,
            aes(x = Year, y = y_pos, label = .group, group = Strategy),
            position = position_dodge(width = 0.8), size = 8)

p1c

ggsave("wing_lenght.png", plot = p1b, width = 28, height = 15, dpi = 300)

################################################################
ggplot(ring_data, aes(x = Weight)) +
  geom_histogram(bins = 30, fill = "#69b3a2", color = "black") +
  theme_minimal()


glm1 <- glm(Weight ~ 1, 
           family = gaussian(),
          data = ring_data)

glm2 <- glm(Weight ~ Year,
            family = gaussian(),
          data = ring_data)

glm3 <- glm(Weight ~ Strategy,
            family = gaussian(),
          data = ring_data)

glm4 <- glm(Weight ~ Year + Strategy,
          family = gaussian(),
          data = ring_data)

glm5 <- glm(Weight ~ Year * Strategy,
            family = gaussian(),
          data = ring_data)
model.sel(glm1, glm2, glm3, glm4, glm5)


# Extract residuals
res <- residuals(lm4)

# Q-Q plot
qqnorm(res)
qqline(res, col = "red", lwd = 2)

# Histogram
hist(res, breaks = 30, main = "Residual Histogram", xlab = "Residuals")

# Shapiro-Wilk test (for small samples)
shapiro.test(res)

library(emmeans)

# Compute emmeans
emm_weight <- emmeans(glm4, ~ Strategy | Year, type = "response")

library(multcomp)

# Use cld to assign group letters
cld_weight <- cld(emm_weight, adjust = "tukey", Letters = letters, type = "response")

# View result
print(cld_weight)

cld_weight$Year <- as.factor(cld_weight$Year)  # Match plot's x-axis

# Example: pick a position slightly above max wing values
library(dplyr)

weight_label_positions <- ring_data %>%
  group_by(Year, Strategy) %>%
  summarise(y_pos = max(Weight, na.rm = TRUE) + 6)  # Adjust +6 as needed

# Join with letters
cld_plot_weight <- left_join(cld_weight, weight_label_positions, by = c("Year", "Strategy"))

cld_plot_weight <- cld_plot_weight |>
  filter(!(Strategy == "not_seen_in_2025" & Year == "2025"))

p2c <- p2b +
  geom_text(data = cld_plot_weight,
            aes(x = Year, y = y_pos, label = .group, group = Strategy),
            position = position_dodge(width = 0.8), size = 8)

p2c

ggsave("weight.png", plot = p2c, width = 28, height = 15, dpi = 300)



##########################################################################
ggplot(ring_data, aes(x = Beak)) +
  geom_histogram(bins = 30, fill = "#69b3a2", color = "black") +
  theme_minimal()

glm1 <- glm(Beak ~ 1, 
            family = gaussian(),
            data = ring_data)

glm2 <- glm(Beak ~ Year,
            family = gaussian(),
            data = ring_data)

glm3 <- glm(Beak ~ Strategy,
            family = gaussian(),
            data = ring_data)

glm4 <- glm(Beak ~ Year + Strategy,
            family = gaussian(),
            data = ring_data)

glm5 <- glm(Beak ~ Year * Strategy,
            family = gaussian(),
            data = ring_data)

model.sel(glm1, glm2, glm3, glm4, glm5)
# Extract residuals
res_glm5 <- residuals(glm5)
# Q-Q plot
qqnorm(res_glm5)
qqline(res_glm5, col = "red", lwd = 2)
# Histogram
hist(res_glm5, breaks = 30, main = "Residual Histogram", xlab = "Residuals")

# Shapiro-Wilk test (for small samples)
shapiro.test(res_glm5)

library(emmeans)

# Compute emmeans
emm_beak <- emmeans(glm5, ~ Strategy | Year, type = "response")

library(multcomp)

# Use cld to assign group letters
cld_beak <- cld(emm_beak, adjust = "tukey", Letters = letters, type = "response")

# View result
print(cld_beak)

cld_beak$Year <- as.factor(cld_beak$Year)  # Match plot's x-axis

beak_label_positions <- ring_data %>%
  group_by(Year, Strategy) %>%
  summarise(y_pos = max(Beak, na.rm = TRUE) + 0.5)  # Adjust +6 as needed

# Join with letters
cld_plot_beak <- left_join(cld_beak, beak_label_positions, by = c("Year", "Strategy"))

#cld_plot_beak <- cld_plot_beak |>
#  filter(!(Strategy == "not_seen_in_2025" & Year == "2025"))

p3c <- p3b +
  geom_text(data = cld_plot_beak,
            aes(x = Year, y = y_pos, label = .group, group = Strategy),
            position = position_dodge(width = 0.8), size = 8)

p3c

ggsave("beak.png", plot = p3c, width = 28, height = 15, dpi = 300)


##########################################################################
ggplot(ring_data, aes(x = Tarsus)) +
  geom_histogram(bins = 30, fill = "#69b3a2", color = "black") +
  theme_minimal()

qqnorm(ring_data$Tarsus)

glm1 <- glm(Tarsus 
            ~ 1, 
            family = gaussian(),
            data = ring_data)

glm2 <- glm(Tarsus ~ Year,
            family = gaussian(),
            data = ring_data)

glm3 <- glm(Tarsus ~ Strategy,
            family = gaussian(),
            data = ring_data)

glm4 <- glm(Tarsus ~ Year + Strategy,
            family = gaussian(),
            data = ring_data)

glm5 <- glm(Tarsus ~ Year * Strategy,
            family = gaussian(),
            data = ring_data)

model.sel(glm1, glm2, glm3, glm4, glm5)
# Extract residuals
res_glm1 <- residuals(glm1)
# Q-Q plot
qqnorm(res_glm1)
qqline(res_glm1, col = "red", lwd = 2)
# Histogram
hist(res_glm1, breaks = 30, main = "Residual Histogram", xlab = "Residuals")

# Shapiro-Wilk test (for small samples)
shapiro.test(res_glm1)

library(emmeans)

# Compute emmeans
#emm_tarsus <- emmeans(glm1, ~ Strategy | Year, type = "response")

#library(multcomp)

# Use cld to assign group letters
#cld_tarus <- cld(emm_tarsus, adjust = "tukey", Letters = letters, type = "response")

# View result
#print(cld_tarus)

ggsave("tarsus_lenght.png", plot = p4b, width = 28, height = 15, dpi = 300)

