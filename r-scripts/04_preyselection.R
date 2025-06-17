library(readxl)
install.packages('tidyverse')
library(tidyverse)
library(cowplot)
library(readxl)
library(car)
library(emmeans)
library(lme4)
library(lmerTest)
library(GGally)
library(tidyverse) 
library(cowplot) 
library(mgcv) 
library(ggplot2)
library(Rmisc)
library(faraway)
library(PMCMRplus)
library(performance)
library(readxl)
library(dplyr)
library(readxl)
library(readxl)
library(dplyr)
library(ggplot2)
library(readxl)


library(readxl)
ALL <- read_excel("C:/Users/conso/Downloads/Ringed Video Analyses (9).xlsx", 
                                       sheet = "ALL!")
View(ALL)

ALL$`Observation id` <- ALL$`Observation_id`

# Count how often each behavior occurs per bird per week
behavior_frequency <- ALL %>%
  group_by(`Observation id`, Week, Behavior) %>%
  summarise(count = n()) %>%
  ungroup()

# Count number of distinct behaviors per bird per week
behavior_summary <- behavior_frequency %>%
  group_by(`Observation id`, `Week`) %>%
  summarise(num_behaviors = n()) %>%
  ungroup()

# Join them to get a combined table showing behavior counts and number of behaviors per bird-week
result <- behavior_frequency %>%
  left_join(behavior_summary, by = c("Week", "Observation id"))

print(result)

avg_behavior_per_week <- result %>%
  group_by(Week, Behavior) %>%
  summarise(avg_count = mean(count)) %>%
  ungroup()

#################################################### behavior counts
behavior_counts <- ALL %>%
  group_by(Week, `Behavior`) %>%
  summarise(count = n(), .groups = "drop")

avg_behavior <- behavior_counts %>%
  group_by(`Behavior`) %>%
  summarise(avg_per_week = mean(count), .groups = "drop")
library(ggplot2)

ggplot(avg_behavior_per_week, aes(x = Week, y = avg_count)) +
  geom_col(fill = "steelblue") +
  facet_wrap(~ `Behavior`, scales = "free_y") +
  labs(title = "Behavior Counts per Week",
       x = "Week",
       y = "Count") +
  theme_minimal()

##############################################################
# Step 1: Create minute bins from the Time column
ALL <- ALL %>%
  mutate(Minute = floor(Time / 60))  # bin each behavior event into 1-minute intervals

# Step 2: Count the number of behavior events per Minute per Week
minute_behavior_counts <- ALL %>%
  group_by(Week, Minute, Behavior) %>%
  summarise(count = n(), .groups = "drop")

# Step 3: Calculate the average number of events per minute per behavior per week
avg_behavior_per_week <- minute_behavior_counts %>%
  group_by(Week, Behavior) %>%
  summarise(avg_count = mean(count), .groups = "drop")

# Step 4: Plot the results
ggplot(avg_behavior_per_week, aes(x = as.factor(Week), y = avg_count)) +
  geom_col(fill = "steelblue") +
  facet_wrap(~ Behavior, scales = "free_y") +
  labs(title = "Average Behavior Count per Minute per Week",
       x = "Week",
       y = "Average Count per Minute") +
  theme_minimal()

######## Swallowing with N numbers given in the boxplot
swallowing_data <- ALL %>%
  filter(Behavior == "Swallowing")

weekly_counts <- swallowing_data %>%
  group_by(Week, `Observation id`) %>%
  summarise(n_events = n(), .groups = "drop")

ALL$Media_duration_s <- as.numeric(as.character(ALL$`Media duration (s)`))
ALL$Observation_id <- as.character(ALL$`Observation id`)
ALL$Week <- as.numeric(as.character(ALL$Week))
ALL$Media_file <- as.character(ALL$`Media file name`)

video_sessions <- ALL %>%
  distinct(Week, Observation_id, Media_file, .keep_all = TRUE)

recording_time <- video_sessions %>%
  group_by(Week, Observation_id) %>%
  summarise(total_video_minutes = sum(Media_duration_s, na.rm = TRUE) / 60,
            .groups = "drop")

swallowing_counts <- swallowing_data %>%
  group_by(Week, Observation_id) %>%
  summarise(swallowing_events = n(), .groups = "drop")

swallowing_rate <- swallowing_counts %>%
  left_join(recording_time, by = c("Week", "Observation_id")) %>%
  mutate(swallowing_per_minute = swallowing_events / total_video_minutes)

n_per_week <- ALL %>%
  group_by(Week) %>%
  summarise(n = n_distinct(Observation_id))

# Filter out weeks 9, 11, and 12 from both data frames
weeks_to_remove <- c(9, 11, 12)

swallowing_rate_filtered <- swallowing_rate %>%
  filter(!Week %in% weeks_to_remove)

n_per_week_filtered <- n_per_week %>%
  filter(!Week %in% weeks_to_remove)

# Make sure Week is a factor with the right order after filtering
swallowing_rate_filtered$Week <- factor(swallowing_rate_filtered$Week, levels = sort(unique(swallowing_rate_filtered$Week)))
n_per_week_filtered$Week <- factor(n_per_week_filtered$Week, levels = sort(unique(n_per_week_filtered$Week)))

# Plot with filtered data
ggplot(swallowing_rate_filtered, aes(x = Week, y = swallowing_per_minute)) +
  geom_boxplot(fill = "skyblue") +
  geom_jitter(width = 0.2, alpha = 0.5, color = "black") +
  geom_text(data = n_per_week_filtered, 
            aes(x = Week, y = max(swallowing_rate_filtered$swallowing_per_minute) * 1.05, label = paste0("n=", n)),
            inherit.aes = FALSE,
            size = 4) +
  labs(
    title = "Swallowing Rate per Minute per Bird per Week",
    x = "Week",
    y = "Swallowing Events per Minute"
  ) +
  theme_minimal()

######Data analysis checking for normality 
ggsave("swallowing_rate_plot.png", dpi = 600, width = 8, height = 6, units = "in")
getwd()

swallowing_rate_filtered %>%
  filter(!is.na(Week)) %>%               # remove NA weeks
  group_by(Week) %>%
  filter(n() >= 3) %>%                   # keep only weeks with ≥ 3 data points
  summarise(
    p_value = shapiro.test(swallowing_per_minute)$p.value
  )

ggplot(swallowing_rate_filtered, aes(sample = swallowing_per_minute)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~Week) +
  theme_minimal()

library(car)

leveneTest(swallowing_per_minute ~ as.factor(Week), data = swallowing_rate_filtered)

anova_result <- aov(swallowing_per_minute ~ as.factor(Week), data = swallowing_rate_filtered)
summary(anova_result)

TukeyHSD(anova_result)

kruskal.test(swallowing_per_minute ~ as.factor(Week), data = swallowing_rate_filtered)

library(dunn.test)
dunn.test::dunn.test(swallowing_rate_filtered$swallowing_per_minute, 
                     swallowing_rate_filtered$Week, 
                     method = "bonferroni")

################# Number of probing events per minute
probing <- ALL %>%
  filter(Behavior == "Probing")
probing_counts <- probing %>%
  group_by(Week, Observation_id) %>%
  summarise(probing_events = n(), .groups = "drop")
probing_rate <- probing_counts %>%
  left_join(recording_time, by = c("Week", "Observation_id")) %>%
  mutate(probing_per_minute = probing_events / total_video_minutes)
probing_rate_filtered <- probing_rate %>%
  filter(!Week %in% weeks_to_remove)

n_per_week <- ALL %>%
  group_by(Week) %>%
  summarise(n = n_distinct(Observation_id))

n_per_week_filtered_probing <- ALL %>%
  filter(Behavior == "Probing") %>%
  group_by(Week) %>%
  summarise(n = n_distinct(Observation_id)) %>%
  filter(!Week %in% weeks_to_remove)

# Ensure factors are ordered correctly
probing_rate_filtered$Week <- factor(probing_rate_filtered$Week, levels = sort(unique(probing_rate_filtered$Week)))
n_per_week_filtered_probing$Week <- factor(n_per_week_filtered_probing$Week, levels = sort(unique(n_per_week_filtered_probing$Week)))
ggplot(probing_rate_filtered, aes(x = Week, y = probing_per_minute)) +
  geom_boxplot(fill = "lightgreen") +
  geom_jitter(width = 0.2, alpha = 0.5, color = "black") +
  geom_text(data = n_per_week_filtered_probing, 
            aes(x = Week, y = max(probing_rate_filtered$probing_per_minute) * 1.05, label = paste0("n=", n)),
            inherit.aes = FALSE,
            size = 4) +
  labs(
    title = "Probing Rate per Minute per Bird per Week",
    x = "Week",
    y = "Probing Events per Minute"
  ) +
  theme_minimal()
ggsave("Probing_Rate_plot.png", dpi = 600, width = 8, height = 6, units = "in")
getwd()

probing_rate_filtered %>%
  filter(!is.na(Week)) %>%
  group_by(Week) %>%
  filter(n() >= 3) %>%
  summarise(
    p_value = shapiro.test(probing_per_minute)$p.value
  )
ggplot(probing_rate_filtered, aes(sample = probing_per_minute)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~Week) +
  theme_minimal()
leveneTest(probing_per_minute ~ as.factor(Week), data = probing_rate_filtered)
anova_result_probing <- aov(probing_per_minute ~ as.factor(Week), data = probing_rate_filtered)
summary(anova_result_probing)
TukeyHSD(anova_result_probing)
kruskal.test(probing_per_minute ~ as.factor(Week), data = probing_rate_filtered)

# If not yet installed
# install.packages("dunn.test")
library(dunn.test)
dunn.test::dunn.test(probing_rate_filtered$probing_per_minute, 
                     probing_rate_filtered$Week, 
                     method = "bonferroni")


#################################### number of surface pecking events
surface_pecking <- ALL %>%
  filter(Behavior == "Surface_pecking")

surface_pecking_counts <- surface_pecking %>%
  group_by(Week, Observation_id) %>%
  summarise(surface_pecking_events = n(), .groups = "drop")

surface_pecking_rate <- surface_pecking_counts %>%
  left_join(recording_time, by = c("Week", "Observation_id")) %>%
  mutate(surface_pecking_per_minute = surface_pecking_events / total_video_minutes)

surface_pecking_rate_filtered <- surface_pecking_rate %>%
  filter(!Week %in% weeks_to_remove)

n_per_week <- ALL %>%
  group_by(Week) %>%
  summarise(n = n_distinct(Observation_id))

n_per_week_filtered_probing <- ALL %>%
  filter(Behavior == "Surface_pecking") %>%
  group_by(Week) %>%
  summarise(n = n_distinct(Observation_id)) %>%
  filter(!Week %in% weeks_to_remove)

# Set factor levels for correct plotting order
surface_pecking_rate_filtered$Week <- factor(surface_pecking_rate_filtered$Week, levels = sort(unique(surface_pecking_rate_filtered$Week)))
n_per_week_filtered_surface$Week <- factor(n_per_week_filtered_surface$Week, levels = sort(unique(n_per_week_filtered_surface$Week)))

ggplot(surface_pecking_rate_filtered, aes(x = Week, y = surface_pecking_per_minute)) +
  geom_boxplot(fill = "orange") +
  geom_jitter(width = 0.2, alpha = 0.5, color = "black") +
  geom_text(data = n_per_week_filtered_surface, 
            aes(x = Week, y = max(surface_pecking_rate_filtered$surface_pecking_per_minute) * 1.05, label = paste0("n=", n)),
            inherit.aes = FALSE,
            size = 4) +
  labs(
    title = "Surface Pecking Rate per Minute per Bird per Week",
    x = "Week",
    y = "Surface Pecking Events per Minute"
  ) +
  theme_minimal()
######Data analysis checking for normality 
ggsave("Pecking_rate_plot.png", dpi = 600, width = 8, height = 6, units = "in")
getwd()

surface_pecking_rate_filtered %>%
  filter(!is.na(Week)) %>%
  group_by(Week) %>%
  filter(n() >= 3) %>%
  summarise(
    p_value = shapiro.test(surface_pecking_per_minute)$p.value
  )

ggplot(surface_pecking_rate_filtered, aes(sample = surface_pecking_per_minute)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~Week) +
  theme_minimal()

leveneTest(surface_pecking_per_minute ~ as.factor(Week), data = surface_pecking_rate_filtered)

anova_result_surface <- aov(surface_pecking_per_minute ~ as.factor(Week), data = surface_pecking_rate_filtered)
summary(anova_result_surface)
TukeyHSD(anova_result_surface)


##################################### BAR AND PIE CHART SPECIES OBSERVED
library(readxl)
library(dplyr)
library(ggplot2)

ALL$`Observation id` <- ALL$`Observation_id`
# Filter only rows where Behavior is "Swallowing"
data_swallowing <- ALL %>%
  filter(Behavior == "Swallowing")

# Convert actual NA values to "NA" as a character
data_swallowing <- data_swallowing %>%
  mutate(Comment = ifelse(is.na(Comment), "NA", Comment))

# Group species/comments as desired
data_swallowing <- data_swallowing %>%
  mutate(Species_Group = case_when(
    Comment %in% c("worm", "Worm") ~ "Worm",
    Comment %in% c("Wadkreeftje", "wadkreeftje") ~ "Wadkreeftje",
    Comment %in% c("Wadkreeftje of ander klein ding geen worm") ~ "wadkreeftje?",
    Comment %in% c("Lange dunne worm", "Kleine worm", "Klein ding") ~ "worm?",
    Comment %in% c("LEA_20.06", "KUL_07.06", "NA", "17.0") ~"Unidentified",
    TRUE ~ Comment
  ))

# Count occurrences per group
species_counts_swallowing <- data_swallowing %>%
  group_by(Species_Group) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Reorder factor levels for Species_Group based on count
species_counts_swallowing <- species_counts_swallowing %>%
  mutate(Species_Group = factor(Species_Group, levels = Species_Group))

# Bereken percentages voor de labels
species_counts_swallowing <- species_counts_swallowing %>%
  mutate(percentage = count / sum(count) * 100)

species_counts_swallowing <- species_counts_swallowing %>%
  mutate(Species_Group = case_when(
    Species_Group == "Wadkreeftje" ~ "Corophium volutator", 
    Species_Group == "Worm" ~ "Worm",
    Species_Group == "wadkreeftje?" ~ "Corophium volutator?",
    Species_Group == "worm?" ~ "Polychaeta?",
    Species_Group == "borstelworm" ~ "Polychaeta",
    Species_Group == "nonnetje" ~ "Limecola balthica",
    TRUE ~ as.character(Species_Group)
  ))

ggplot(species_counts_swallowing, aes(x = reorder(Species_Group, count), y = count, fill = Species_Group)) +
  geom_col(width = 0.7) +
  geom_text(
    aes(label = paste0(count, " (", round(percentage, 1), "%)")),
    hjust = -0.3, 
    size = 5  # Hier pas je de tekstgrootte aan (bijv. 5)
  ) +
  coord_flip(clip = "off") +  # Zodat tekst niet wordt afgeknipt
  expand_limits(y = max(species_counts_swallowing$count) * 1.2) +  # Extra ruimte rechts
  labs(
    title = "Number of observations per species (Swallowing)",
    x = "Species",
    y = "Number of observations"
  ) +
  theme_minimal(base_size = 14) +  # Basistekstgrootte verhogen
  theme(
    legend.position = "none",
    axis.text = element_text(size = 14),   # Aslabels groter
    axis.title = element_text(size = 16),  # Astitels groter
    plot.title = element_text(size = 18, face = "bold"),  # Titel groter
    plot.margin = margin(5, 20, 5, 5)  # Extra ruimte rechts voor labels
  )
ggsave("Bar_Species_Camera.png", dpi = 600, width = 8, height = 6, units = "in")
getwd()

####################################
#################################### Combination SWALLOWING, PECKING, PROBING
# Filter behaviors
swallowing_data <- ALL %>% filter(Behavior == "Swallowing")
probing_data <- ALL %>% filter(Behavior == "Probing")
surface_pecking_data <- ALL %>% filter(Behavior == "Surface_pecking")

# Swallowing
swallowing_counts <- swallowing_data %>%
  group_by(Week, Observation_id) %>%
  summarise(n_events = n(), .groups = "drop") %>%
  mutate(Behavior = "Swallowing")

# Probing
probing_counts <- probing_data %>%
  group_by(Week, Observation_id) %>%
  summarise(n_events = n(), .groups = "drop") %>%
  mutate(Behavior = "Probing")

# Surface_pecking
surface_pecking_counts <- surface_pecking_data %>%
  group_by(Week, Observation_id) %>%
  summarise(n_events = n(), .groups = "drop") %>%
  mutate(Behavior = "Surface_pecking")

# Combine all
all_counts <- bind_rows(swallowing_counts, probing_counts, surface_pecking_counts)

# Calculate recording time
video_sessions <- ALL %>%
  distinct(Week, Observation_id, Media_file, .keep_all = TRUE)

recording_time <- video_sessions %>%
  group_by(Week, Observation_id) %>%
  summarise(total_video_minutes = sum(Media_duration_s, na.rm = TRUE) / 60,
            .groups = "drop")

# Merge with recording time
rate_data <- all_counts %>%
  left_join(recording_time, by = c("Week", "Observation_id")) %>%
  mutate(events_per_minute = n_events / total_video_minutes)

weeks_to_remove <- c(9, 11, 12)
rate_data_filtered <- rate_data %>%
  filter(!Week %in% weeks_to_remove) %>%
  mutate(
    Week = factor(Week, levels = sort(unique(Week))),
    Behavior = factor(Behavior, levels = c("Swallowing", "Probing", "Surface_pecking"))
  )

n_per_week_filtered <- rate_data_filtered %>%
  group_by(Week, Behavior) %>%
  summarise(n = n_distinct(Observation_id), .groups = "drop")

ggplot(rate_data_filtered, aes(x = Week, y = events_per_minute, fill = Behavior)) +
  geom_boxplot(position = position_dodge(width = 0.75)) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75), alpha = 0.5, color = "black") +
  geom_text(
    data = n_per_week_filtered,
    aes(x = Week, y = max(rate_data_filtered$events_per_minute, na.rm = TRUE) * 1.05,
        label = paste0("n=", n), group = Behavior),
    position = position_dodge(width = 0.75),
    size = 3.5
  ) +
  labs(
    title = "Behavior Events per Minute per Bird per Week",
    x = "Week",
    y = "Events per Minute"
  ) +
  scale_fill_manual(values = c("skyblue", "lightgreen", "orange")) +
  theme_minimal()

rate_data_filtered$Week_num <- as.numeric(rate_data_filtered$Week)

# For Swallowing
swallowing_model <- lm(events_per_minute ~ Week_num, 
                       data = rate_data_filtered %>% filter(Behavior == "Swallowing"))
summary(swallowing_model)

# Maak een residual plot:
par(mar = c(4, 4, 2, 1))  # c(bottom, left, top, right)
plot(swallowing_model$fitted.values, residuals(swallowing_model))
abline(h = 0, col = "red")

# QQ-plot:
qqnorm(residuals(swallowing_model))
qqline(residuals(swallowing_model), col = "red")

shapiro.test(residuals(swallowing_model))

library(car)
durbinWatsonTest(swallowing_model)

model_quad <- lm(events_per_minute ~ poly(Week_num, 2), 
                 data = rate_data_filtered %>% filter(Behavior == "Swallowing"))
summary(model_quad)

AIC(model_quad, swallowing_model)

# Maak een residual plot:
par(mar = c(4, 4, 2, 1))  # c(bottom, left, top, right)
plot(model_quad$fitted.values, residuals(model_quad))
abline(h = 0, col = "red")

# QQ-plot:
qqnorm(residuals(model_quad))
qqline(residuals(model_quad), col = "red")

shapiro.test(residuals(model_quad))

library(car)
durbinWatsonTest(model_quad)

library(ggplot2)
rate_data_filtered$Week_num <- as.numeric(as.character(rate_data_filtered$Week_num))

ggplot(rate_data_filtered %>% filter(Behavior == "Swallowing"), aes(x = Week_num, y = events_per_minute)) +
  geom_point(alpha = 0.5) +
  stat_smooth(method = "lm", formula = y ~ poly(x, 2), se = TRUE, color = "blue") +
  labs(title = "Quadratic fit for Swallowing Events per Minute over Weeks",
       x = "Week Number",
       y = "Events per Minute") +
  theme_minimal()

glm_poisson <- glm(events_per_minute ~ Week_num, 
                   data = rate_data_filtered %>% filter(Behavior == "Swallowing"), 
                   family = poisson)
summary(glm_poisson)
dispersion <- sum(residuals(glm_poisson, type = "pearson")^2) / df.residual(glm_poisson)
dispersion

glm_quasi <- glm(events_per_minute ~ Week_num, 
                 data = rate_data_filtered %>% filter(Behavior == "Swallowing"), 
                 family = quasipoisson)
summary(glm_quasi)

library(MASS)
glm_nb <- glm.nb(events_per_minute ~ Week_num, 
                 data = rate_data_filtered %>% filter(Behavior == "Swallowing"))
summary(glm_nb)

AIC(model_quad, swallowing_model, glm_nb, glm_quasi)

ggplot(rate_data_filtered, aes(x = Week, y = events_per_minute, fill = Behavior)) +
  geom_boxplot(position = position_dodge(width = 0.75)) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75),
              alpha = 0.5, color = "black") +
  geom_text(
    data = n_per_week_filtered,
    aes(x = Week, y = max(rate_data_filtered$events_per_minute, na.rm = TRUE) * 1.05,
        label = paste0("n=", n), group = Behavior),
    position = position_dodge(width = 0.75),
    size = 3.5
  ) +
  # Quadratisch model toevoegen voor Swallowing
  stat_smooth(
    data = rate_data_filtered %>% filter(Behavior == "Swallowing"),
    aes(x = as.numeric(Week), y = events_per_minute, group = 1),  # group = 1 zorgt dat er één lijn komt
    method = "lm",
    formula = y ~ poly(x, 2),
    se = TRUE,
    color = "blue",
    size = 1
  ) +
  labs(
    title = "Behavior Events per Minute per Bird per Week (met Swallowing trendlijn)",
    x = "Week",
    y = "Events per Minute"
  ) +
  scale_fill_manual(values = c("skyblue", "lightgreen", "orange")) +
  theme_minimal()
ggsave("Behavior_Events_per_Minute_with_Swallowing_Trend.png", dpi = 600, width = 8, height = 6, units = "in")


###########################
########################### For Probing
probing_model <- lm(events_per_minute ~ Week_num, 
                    data = rate_data_filtered %>% filter(Behavior == "Probing"))
summary(probing_model)

# Maak een residual plot:
par(mar = c(4, 4, 2, 1))  # c(bottom, left, top, right)
plot(probing_model$fitted.values, residuals(probing_model))
abline(h = 0, col = "red")

# QQ-plot:
qqnorm(residuals(probing_model))
qqline(residuals(probing_model), col = "red")

shapiro.test(residuals(probing_model))

library(car)
durbinWatsonTest(probing_model)

model_quad <- lm(events_per_minute ~ poly(Week_num, 2), 
                 data = rate_data_filtered %>% filter(Behavior == "Probing"))
summary(model_quad)

AIC(model_quad, probing_model)

# Maak een residual plot:
par(mar = c(4, 4, 2, 1))  # c(bottom, left, top, right)
plot(model_quad$fitted.values, residuals(model_quad))
abline(h = 0, col = "red")

# QQ-plot:
qqnorm(residuals(model_quad))
qqline(residuals(model_quad), col = "red")

shapiro.test(residuals(model_quad))

library(car)
durbinWatsonTest(model_quad)

glm_poisson <- glm(events_per_minute ~ Week_num, 
                   data = rate_data_filtered %>% filter(Behavior == "Probing"), 
                   family = poisson)
summary(glm_poisson)
dispersion <- sum(residuals(glm_poisson, type = "pearson")^2) / df.residual(glm_poisson)
dispersion

glm_quasi <- glm(events_per_minute ~ Week_num, 
                 data = rate_data_filtered %>% filter(Behavior == "Probing"), 
                 family = quasipoisson)
summary(glm_quasi)



##########################
########################## For Surface Pecking
pecking_model <- lm(events_per_minute ~ Week_num, 
                    data = rate_data_filtered %>% filter(Behavior == "Surface_pecking"))
summary(pecking_model)

# Maak een residual plot:
par(mar = c(4, 4, 2, 1))  # c(bottom, left, top, right)
plot(pecking_model$fitted.values, residuals(pecking_model))
abline(h = 0, col = "red")

# QQ-plot:
qqnorm(residuals(pecking_model))
qqline(residuals(pecking_model), col = "red")

shapiro.test(residuals(pecking_model))

library(car)
durbinWatsonTest(pecking_model)

model_quad <- lm(events_per_minute ~ poly(Week_num, 2), 
                 data = rate_data_filtered %>% filter(Behavior == "Surface_pecking"))
summary(model_quad)

AIC(model_quad, pecking_model)

# Maak een residual plot:
par(mar = c(4, 4, 2, 1))  # c(bottom, left, top, right)
plot(model_quad$fitted.values, residuals(model_quad))
abline(h = 0, col = "red")

# QQ-plot:
qqnorm(residuals(model_quad))
qqline(residuals(model_quad), col = "red")

shapiro.test(residuals(model_quad))

library(car)
durbinWatsonTest(model_quad)

ggplot(rate_data_filtered, aes(x = Week_num, y = events_per_minute, color = Behavior)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  scale_x_continuous(breaks = 1:length(levels(rate_data_filtered$Week)), 
                     labels = levels(rate_data_filtered$Week)) +
  labs(
    title = "Linear Trends in Events per Minute per Bird per Week",
    x = "Week",
    y = "Events per Minute"
  ) +
  theme_minimal()

library(car)
durbinWatsonTest(model_quad)

glm_poisson <- glm(events_per_minute ~ Week_num, 
                   data = rate_data_filtered %>% filter(Behavior == "Surface_pecking"), 
                   family = poisson)
summary(glm_poisson)
dispersion <- sum(residuals(glm_poisson, type = "pearson")^2) / df.residual(glm_poisson)
dispersion

glm_quasi <- glm(events_per_minute ~ Week_num, 
                 data = rate_data_filtered %>% filter(Behavior == "Surface_pecking"), 
                 family = quasipoisson)
summary(glm_quasi)

library(MASS)
glm_nb <- glm.nb(events_per_minute ~ Week_num, 
                 data = rate_data_filtered %>% filter(Behavior == "Surface_pecking"))
summary(glm_nb)

AIC(model_quad, pecking_model, glm_nb, glm_quasi)

glm_quasi <- glm(events_per_minute ~ Week_num, 
                 data = rate_data_filtered %>% filter(Behavior == "Probing"), 
                 family = quasipoisson)
summary(glm_quasi)

library(dplyr)

# 1. Maak predicte data aan:
prediction_data <- data.frame(
  Week_num = seq(min(rate_data_filtered$Week_num), max(rate_data_filtered$Week_num), length.out = 100)
)

# 2. Maak voorspellingen (linke schaal, dus we transformeren naar response schaal)
prediction_data$predicted_events <- predict(glm_nb, newdata = prediction_data, type = "response")

# 3. Voeg 'Behavior' kolom toe zodat je makkelijk kunt onderscheiden
prediction_data$Behavior <- "Surface_pecking"

# 4. Plot alles samen:
ggplot(rate_data_filtered, aes(x = Week, y = events_per_minute, fill = Behavior)) +
  geom_boxplot(position = position_dodge(width = 0.75)) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75),
              alpha = 0.5, color = "black") +
  geom_text(
    data = n_per_week_filtered,
    aes(x = Week, y = max(rate_data_filtered$events_per_minute, na.rm = TRUE) * 1.05,
        label = paste0("n=", n), group = Behavior),
    position = position_dodge(width = 0.75),
    size = 3.5
  ) +
  # Quadratische lijn voor Swallowing
  stat_smooth(
    data = rate_data_filtered %>% filter(Behavior == "Swallowing"),
    aes(x = as.numeric(Week), y = events_per_minute, group = 1),
    method = "lm",
    formula = y ~ poly(x, 2),
    se = TRUE,
    color = "blue",
    size = 1
  ) +
  # glm.nb lijn voor Surface_pecking
  geom_line(
    data = prediction_data,
    aes(x = Week_num, y = predicted_events),
    color = "red",
    size = 1
  ) +
  labs(
    title = "Behavior Events per Minute per Bird per Week (met Swallowing & Surface_pecking lijnen)",
    x = "Week",
    y = "Events per Minute"
  ) +
  scale_fill_manual(values = c("skyblue", "lightgreen", "orange")) +
  theme_minimal()





########################## ALGEMEEN PROBING
library(dplyr)

# Bereken gemiddelde en standaarddeviatie/standaardfout per week en gedragstype
summary_table <- rate_data_filtered %>%
  group_by(Week, Behavior) %>%
  summarise(
    mean_events_per_minute = mean(events_per_minute, na.rm = TRUE),
    sd_events_per_minute = sd(events_per_minute, na.rm = TRUE),
    n = n(),
    se_events_per_minute = sd_events_per_minute / sqrt(n)
  ) %>%
  ungroup()

# Print de tabel
print(summary_table)

# Desgewenst kun je de tabel ook als .csv exporteren:
# write.csv(summary_table, "summary_events_per_minute_per_week.csv", row.names = FALSE)




####################### (swallowing/(probinf+sruface_pecking))
event_counts <- ALL %>%
  filter(Behavior %in% c("Swallowing", "Probing", "Surface_pecking")) %>%
  group_by(Week, Observation_id, Behavior) %>%
  summarise(n_events = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = Behavior,
    values_from = n_events,
    values_fill = list(n_events = 0)
  )

library(tidyr)

video_sessions <- ALL %>%
  distinct(Week, Observation_id, Media_file, .keep_all = TRUE)

recording_time <- video_sessions %>%
  group_by(Week, Observation_id) %>%
  summarise(total_video_minutes = sum(Media_duration_s, na.rm = TRUE) / 60,
            .groups = "drop")

rate_data <- event_counts %>%
  left_join(recording_time, by = c("Week", "Observation_id"))

rate_data <- rate_data %>%
  mutate(
    swallowing_rate = Swallowing / total_video_minutes,
    other_rate = (Probing + Surface_pecking) / total_video_minutes,
    swallowing_ratio = ifelse(other_rate > 0, swallowing_rate / other_rate, NA)
  )

mean_ratio_data <- rate_data %>%
  group_by(Week, Observation_id) %>%
  summarise(mean_swallowing_ratio = mean(swallowing_ratio, na.rm = TRUE),
            .groups = "drop")
weeks_to_remove <- c(9, 11, 12)
mean_ratio_data <- mean_ratio_data %>%
  filter(!Week %in% weeks_to_remove) %>%
  mutate(
    Week = factor(Week, levels = sort(unique(Week)))
  )

sample_sizes <- mean_ratio_data %>%
  group_by(Week) %>%
  summarise(n = n(), .groups = "drop")

ggplot(mean_ratio_data, aes(x = Week, y = mean_swallowing_ratio)) +
  geom_boxplot(fill = "lightblue") +
  geom_jitter(width = 0.2, alpha = 0.5, color = "black") +
  geom_text(
    data = sample_sizes,
    aes(x = Week, y = 1.05, label = paste0("n=", n)),
    inherit.aes = FALSE,
    size = 3
  ) +
  labs(
    title = "Average ratio swallowing / (probing + surface_pecking) per minute per individual",
    x = "Week",
    y = "Average ratio per minute"
  ) +
  ylim(0, 1.1) +  # Iets hoger om ruimte te maken voor de labels
  theme_minimal()


ggsave("ratio_swallow.png", dpi = 600, width = 8, height = 6, units = "in")
getwd()

kruskal.test(mean_swallowing_ratio ~ Week, data = mean_ratio_data)

# Convert Week to numeric (if applicable)
mean_ratio_data <- mean_ratio_data %>%
  mutate(Week_numeric = as.numeric(as.character(Week)))

# Fit the model
trend_model <- lm(mean_swallowing_ratio ~ Week_numeric, data = mean_ratio_data)
summary(trend_model)

library(lme4)
library(lmerTest)  # for p-values

model <- lmer(mean_swallowing_ratio ~ Week_numeric + (1 | Observation_id), data = mean_ratio_data)
summary(model)

library(ggplot2)

ggplot(mean_ratio_data, aes(x = Week_numeric, y = mean_swallowing_ratio)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(
    title = "Trend in Swallowing Ratio over Time",
    x = "Week (numeric)",
    y = "Mean Swallowing Ratio"
  ) +
  theme_minimal()

mean_ratio_data <- mean_ratio_data %>%
  mutate(
    Week_numeric2 = Week_numeric^2
  )

model_quad <- lmer(mean_swallowing_ratio ~ Week_numeric + Week_numeric2 + (1 | Observation_id), data = mean_ratio_data)
summary(model_quad)
plot(model_quad)


mean_ratio_data$Week_num <- as.numeric(as.character(mean_ratio_data$Week))
model1 <- lm(mean_swallowing_ratio ~ Week_num, data = mean_ratio_data)
summary(model1)

mean_ratio_data$Week_num_sq <- mean_ratio_data$Week_num^2

model3 <- lm(mean_swallowing_ratio ~ Week_num + Week_num_sq, data = mean_ratio_data)
summary(model3)

AIC(model1, model2, model3)
anova(model1, model2)







###################### SWALLOWING RATE Per SURFACE_PECK AND PROBING
library(dplyr)
library(ggplot2)
library(tidyr)

# Bereken aantal events per minuut per individu
event_counts <- ALL %>%
  filter(Behavior %in% c("Swallowing", "Probing", "Surface_pecking")) %>%
  group_by(Week, Observation_id, Minute, Behavior) %>%
  summarise(n_events = n(), .groups = "drop")

# Maak breed formaat voor ratio’s
event_ratios <- event_counts %>%
  pivot_wider(names_from = Behavior, values_from = n_events, values_fill = 0) %>%
  mutate(
    swallow_per_probing = ifelse(Probing > 0, Swallowing / Probing, NA),
    swallow_per_surface = ifelse(Surface_pecking > 0, Swallowing / Surface_pecking, NA)
  )

# Neem het gemiddelde per week per individu
weekly_means <- event_ratios %>%
  group_by(Week, Observation_id) %>%
  summarise(
    mean_swallow_per_probing = mean(swallow_per_probing, na.rm = TRUE),
    mean_swallow_per_surface = mean(swallow_per_surface, na.rm = TRUE)
  ) %>%
  pivot_longer(
    cols = c(mean_swallow_per_probing, mean_swallow_per_surface),
    names_to = "RatioType",
    values_to = "Ratio"
  )

# Boxplot
ggplot(weekly_means, aes(x = as.factor(Week), y = Ratio, fill = RatioType)) +
  geom_boxplot(outlier.shape = NA, position = position_dodge(width = 0.8), alpha = 0.5) +
  geom_jitter(aes(color = RatioType), position = position_dodge(width = 0.8), size = 2, alpha = 0.7) +
  labs(
    title = "Mean Ratio of Swallowing to Probing and Surface Pecking per Individual per Week",
    x = "Week",
    y = "Mean Ratio",
    fill = "Ratio Type"
  ) +
  scale_fill_manual(values = c("mean_swallow_per_probing" = "orange", "mean_swallow_per_surface" = "blue")) +
  scale_color_manual(values = c("mean_swallow_per_probing" = "orange", "mean_swallow_per_surface" = "blue")) +
  theme_minimal()
