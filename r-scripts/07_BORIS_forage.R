# script with the BORIS data
renv::restore()

boris_data <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRlB920gRUzQvTQz40p3ru1OEbB9f9y9lZjsRn7meJ_nhudpivp1UB536fHQRjVCcONiiuEqrnHuJVx/pub?gid=1901273527&single=true&output=csv")

video_data <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRTzpl8cLIfyyycDNx_vbnulWr64YsPvfg0a4R9-q3hWTFq87Znl-odcKJX6LHaKgpCODNia0WpgQwy/pub?gid=2044682262&single=true&output=csv")

complete_dataset <- boris_data |>
  dplyr::left_join(video_data, by = "Observation_id", relationship = "many-to-many") |>
  dplyr::filter(Analysed == "Yes") |>
  dplyr::mutate(Time = as.numeric(Time)) 

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

# I want the three_letter_code for JTN, JYL, KJU, KNP, KPM, KXM, LCT, LMN, LPT, LYK, MAU, MCH, MTJ, MYV, NLJ, NTV, PAE, PAJ, PKN, PMP, PNL to be assigned in a new column called strategy to be assigned overwinterer and for the rest of the three_letter_code to be assigned migrant
            

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


complete_dataset |>
  count(Strategy)
# early_northward_migrant n=12568, late_northward_migration = 16817, overwinterer n=14367

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

# Calculate the point behaviours
behaviors <- complete_dataset |>
  filter(Behavior.type == "POINT") |>
  mutate(Duration = 0.1)


# Calculate the duration
durations <- durations |>
  mutate(Duration = Time_stop - Time_start)

###############################################################################
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
    arrange(Three_letter_code) |>
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
behaviors <- behaviors |>
  group_by(Three_letter_code, Observation_id) |>
  mutate(total_duration = sum(Duration, na.rm = TRUE)) |>
  mutate(total_duration = ifelse(is.na(total_duration), 0, 
  total_duration), gap_time = Media.duration..s. - total_duration) |>
  rename(visually_foraging = gap_time) 


### Add in the weeks again
library(dplyr)
library(lubridate)

#Boxplot graph to look at the difference in duration of foraging walking between migrant and overwinterer 

# Calculate counts per Week and Strategy
counts <- behaviors |>
  group_by(Week, Strategy) |>
  summarise(n = n_distinct(Observation_id), .groups = "drop") |>
  filter(!Week %in% c("9", "11", "15"))

behaviors <- behaviors |>
  filter(!Observation_id %in% c("KKH_15.04", "KUX_15.04")) |>
  filter(!Week %in% c("9", "11", "15"))

# Base boxplot
p1 <- ggplot(behaviors |> filter(visually_foraging < 200),
             aes(x = as.factor(Week), y = visually_foraging, 
             fill = Strategy)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#FF9999", "#1ED760", "#66B3FF")) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Visually foraging by Strategy",
    x = "Week",
    y = "Duration of visually foraging in s")

p1

# Add text labels with counts above boxes
p2 <- p1 + geom_text(data = counts, aes(x = as.factor(Week), 
                                        y = 180, 
                                  label = paste0("n=", n)),
               position = position_dodge(width = 1.00), size = 4)
p2

# The graph shows some differences in duration of foraging walking per strategy.
# So it would be interesting to analyse this further 


# Check for normality of visually_foraging
ggplot(behaviors, aes(x = visually_foraging)) +
  geom_histogram(bins = 30, fill = "#69b3a2", color = "black") +
  labs(
    title = "Distribution of Visually Foraging Durations",
    x = "Visually Foraging Duration (seconds)",
    y = "Count"
  ) +
  theme_minimal()

qqnorm(behaviors$visually_foraging)

# Check for residuals
ggplot(behaviors, aes(x = fitted(lm(visually_foraging ~ Strategy, data = behaviors)), 
                     y = resid(lm(visually_foraging ~ Strategy, data = behaviors)))) +
  geom_point() +
  geom_smooth(method = "lm", color = "red") +
  labs(
    title = "Residuals vs Fitted Values",
    x = "Fitted Values",
    y = "Residuals"
  ) +
  theme_minimal()

#### So data is not normally distributed, need to do choose another model than lm, use glm instead

glm1 <- glm(visually_foraging ~ Week + Strategy + Week * Strategy + Transect_ID + Habitat + Tide,  family = Gamma(link = "log"),
  data = behaviors)
summary(glm1)

glm2 <- glm(visually_foraging ~ Week + Strategy + Week * Strategy, family = Gamma(link = "log"),
  data = behaviors)
summary(glm2)
AIC(glm1, glm2)

glm3 <- glm(visually_foraging ~ Week + Strategy + Week * Strategy + Transect_ID + Habitat + Tide + Three_letter_code,
            family = Gamma(link = "log"),
            data = behaviors)
summary(glm3)
AIC(glm1, glm2, glm3)

glmer1 <- glmer(visually_foraging ~ Week + Strategy + (1 | Three_letter_code),
  family = Gamma(link = "log"),
  data = behaviors,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
summary(glmer1)

glmer2 <- glmer(visually_foraging ~ Week + Strategy + 
                  Week*Strategy + (1|Three_letter_code), 
  family = Gamma(link = "log"),
  data = behaviors,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
summary(glmer2)

AIC(glmer1, glmer2)

glmer3 <- glmer(visually_foraging ~Week * Strategy + (1 | Three_letter_code) ,
  family = Gamma(link = "log"),
  data = behaviors,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
summary(glmer3)

AIC(glmer1, glmer3)


#### GLM 3 shows best AIC with 324637.6!!!!!!!!

#m0 <- lm(visually_foraging ~ Strategy, data = behaviors)
#print(m0)
#summary(m0)

#m1 <- lm(visually_foraging ~ Week + Strategy, data = behaviors)
#print(m1)
#summary(m1)

#m2 <- lm(visually_foraging ~ Week * Strategy, data = behaviors)
#print(m2)
#summary(m2)

#anova(m1,m2)
# The interaction model (m2) is significantly better than the main effects model (m1), indicating that the effect of week on duration depends on the strategy.
# This suggests that the relationship between week and duration is different for migrants and overwinterers.

#anova(m0, m2)
# So adding Week as an interaction to the strategy makes the model a quite good predictior of the duration of foraging walking. The p-value is ***


#m3 <- lm(visually_foraging ~ Week * Strategy + Habitat, data = behaviors)
#anova(m2, m3) 


#m4 <- lm(visually_foraging ~ Week * Strategy + Transect_ID + Habitat, data = behaviors)
#print(m4)
#summary(m4)
#anova(m3,m4)

#m5 <- lm(visually_foraging ~ Week * Strategy + Transect_ID + Habitat + Tide, data = behaviors)
#summary(m5)
#anova(m2, m5)
#anova(m4, m5)

# Adding Transect_ID, Tide or Habitat does not make it a better predictive model for the duration of foraging walking.

new_data <- unique(behaviors[, c("Week", "Strategy", "Transect_ID", "Habitat", "Tide", "Three_letter_code")])
new_data$predicted <- predict(glm3, newdata = new_data, re.form = ~(1 | Three_letter_code), type = "response")

new_data$Strategy <- factor(new_data$Strategy, levels = c(
    "early_northward_migration", "late_northward_migration", "overwinterer"
))

p3 <- p2 +
  geom_point(
    data = new_data,
    aes(
      x = as.factor(Week),
      y = predicted,
      shape = Strategy,
      group = Strategy,
      color = Strategy
    ),
    size = 2,
    position = position_dodge(width = 0.6)
  ) +
  scale_shape_manual(values = c(
    "overwinterer" = 18,
    "late_northward_migration" = 19,
    "early_northward_migration" = 16  # add it here
  )) +
  scale_color_manual(values = c(
    "overwinterer" = "#5190cf",
    "late_northward_migration" = "#0FA85B",
    "early_northward_migration" = "#c77575"  # and here
  )) +
  theme_minimal()
p3


#wilcox.test(visually_foraging ~ Strategy, data = behaviors)
### W = 162771137, p-value < 2.2e-16

behaviors %>%
  group_by(Week) %>%
  summarise(p_value = kruskal.test(visually_foraging ~ Strategy)$p.value)


p4 <- p3 +
  geom_text(data = data.frame(
    Week = factor(c(13, 16, 17, 18, 19, 20, 21)),
    label = c("***", "***", "***", "***", "***", "***", "***"),
    y = c(115, 120, 118, 118, 130, 115, 120)
  ),
  aes(x = Week, y = y, label = label),
  vjust = -0.5,
  size = 4,
  inherit.aes = FALSE)  # <--- This is the key!
p4

ggsave("foraging_walking_duration_strategy.png", plot = p2, width = 13, height = 6, dpi = 300)
ggsave("foraging_walking_duration_strategy_with_predictions.png", plot = p4, width = 13, height = 6, dpi = 300)

###############################################################################
# Look at surface_pecking per week per strategy in behavior dataframe

# Calculate the point behaviours
point_behaviors <- complete_dataset |>
  filter(Behavior.type == "POINT") |>
  filter(!Observation_id %in% c("KET_20.03", "KET.20.03", "LEA_21.05")) |>
  group_by(Three_letter_code, Behavior, Strategy, Week, Tide, Habitat,   
           Observation_id, Transect_ID, Date.x, Media.duration..s., Social_behavior, Aggressive.or.submissive) |>
  summarise(Behavior_Count = n(), .groups = "drop")|>
  mutate(Behavior_Rate = Behavior_Count / Media.duration..s.) |>
  filter(Week %in% c(12, 13, 16, 17, 18 ,19, 20 ,21))
  
## Video of KET on 20 03 is out of proportion in comparison of the other video's of that day but also over the past time.

# Check for normality of probing
qqnorm(point_behaviors$Behavior_Count)

counts_point <- point_behaviors |>
  group_by(Week, Strategy, Behavior) |>
  summarise(n = n_distinct(Observation_id), .groups = "drop") |>
  filter(!Week %in% c("9", "11", "15"))

p5a <- ggplot(point_behaviors |> filter(Behavior == "Probing"),
             aes(x = as.factor(Week), y = Behavior_Rate, 
                 fill = Strategy)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#FF9999", "#1ED760", "#66B3FF")) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Probing by Strategy",
    x = "Week",
    y = "Duration Rate")
p5a

p5b <- p5a + geom_text(data = counts_point |> filter(Behavior == "Probing"), aes(x = as.factor(Week), 
                                               y = 1.6, 
                                               label = paste0("n=", n)),
                            position = position_dodge(width = 0.8), size = 3)
p5b

glm4 <- glm(Behavior_Rate ~ Week + Strategy + Week * Strategy, 
                 family = Gamma(link = "log"),
                 data = subset(point_behaviors, Behavior == "Probing"))
summary(glm4)

glm5 <- glm(Behavior_Rate ~  Week * Strategy + Transect_ID + Habitat + Tide, 
            family = Gamma(link = "log"),
            data = subset(point_behaviors, Behavior == "Probing"))
summary(glm5)

glm6 <- glm(Behavior_Rate ~ Week * Strategy, 
            family = Gamma(link = "log"),
            data = subset(point_behaviors, Behavior == "Probing"))
summary(glm6)

AIC(glm4, glm5, glm6)

point_behaviors %>%
  filter(Behavior == "Probing") %>%
  group_by(Week) %>%
  summarise(p_value = kruskal.test(Behavior_Rate ~ Strategy)$p.value)

dunn1 <- dunnTest(Behavior_Rate ~ Strategy, 
                 data = subset(point_behaviors, Behavior == "Probing"), 
                 method = "bonferroni")
dunn1



p6a <- ggplot(point_behaviors |> filter(Behavior == "Surface_pecking"),
             aes(x = as.factor(Week), y = Behavior_Rate, 
                 fill = Strategy)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#FF9999", "#1ED760", "#66B3FF")) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Surface pecking by Strategy",
    x = "Week",
    y = "Duration Rate")
p6a

p6b <- p6a + geom_text(data = counts_point |> filter(Behavior == "Surface_pecking"), aes(x = as.factor(Week), 
                                                y = 0.9, 
                                                label = paste0("n=", n)),
                       position = position_dodge(width = 0.8), size = 4)

p6b

glmer9 <- glmer(Behavior_Rate ~ Week * Strategy + (1 | Three_letter_code), 
                family = Gamma(link = "log"),
                data = subset(point_behaviors, Behavior == "Surface_pecking"),
                control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
summary(glmer9)

glmer10 <- glmer(Behavior_Rate ~ Strategy + (1 | Week) + (1 | Three_letter_code), 
                family = Gamma(link = "log"),
                data = subset(point_behaviors, Behavior == "Surface_pecking"),
                control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
summary(glmer10)

AIC(glmer9, glmer10)

glmer11 <- glmer(Behavior_Rate ~ Week * Strategy + (1 | Three_letter_code) 
                + (1| Transect_ID) + (1|Tide) + (1|Habitat), data = subset
                (point_behaviors, Behavior == "Surface_pecking"),
                family = Gamma(link = "log"),
                control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
summary(glmer11)
## Nothing explains the difference between surface_pecking ?

AIC(glmer9, glmer11)

glm5 <- glm(Behavior_Rate ~ Week + Strategy + Week * Strategy, 
            family = Gamma(link = "log"),
            data = subset(point_behaviors, Behavior == "Surface_pecking"))
summary(glm5)

point_behaviors %>%
  filter(Behavior == "Surface_pecking") %>%
  group_by(Week) %>%
  summarise(p_value = kruskal.test(Behavior_Rate ~ Strategy)$p.value)

p7a <- ggplot(point_behaviors |> filter(Behavior == "Turning_stuff"),
             aes(x = as.factor(Week), y = Behavior_Rate, 
                 fill = Strategy)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#FF9999", "#1ED760",  "#66B3FF")) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Turning by Strategy",
    x = "Week",
    y = "Duration Rate")
p7a

p7b <- p7a + geom_text(data = counts_point |> filter(Behavior == "Turning_stuff"), aes(x = as.factor(Week), y = 0.1,                                             label = paste0("n=", n)),
                       position = position_dodge(width = 0.8), size = 4)

p7b

glm6 <- glm(Behavior_Rate ~ Week + Strategy + Week * Strategy, 
            family = Gamma(link = "log"),
            data = subset(point_behaviors, Behavior == "Turning_stuff"))
summary(glm6)

p_point_behavior <- p5b + p6b + p7b
p_point_behavior

#emmeans_results <- emmeans(aov(Behavior_Rate ~ Behavior * Strategy, data = #point_behaviors), 
#                           pairwise ~ Strategy | Behavior)
#print(emmeans_results)

##wilcox.test(Behavior_Rate ~ Strategy, data = point_behaviors)
# W = 57959, p-value = 0.2195

point_behaviors %>%
  group_by(Week) %>%
  summarise(p_value = kruskal.test(Behavior_Rate ~ Strategy)$p.value)


ggsave("rate_forage_stratgy.png", plot = p_point_behavior, width = 28, height = 10, dpi = 300)

##### Looking at all the different behaviors
aov(Behavior_Rate ~ Behavior * Strategy, data = point_behaviors)
summary(aov(Behavior_Rate ~ Behavior * Strategy, data = point_behaviors))

# Plotting the results
# I want to do an emmeans test to compare the strategies for each behavior

# Plotting the results
p9 <- ggplot(point_behaviors |> filter(!Behavior == "Swallowing"), aes(x = Behavior, y = Behavior_Rate, fill = Strategy)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c("#FF9999","#1ED760", "#66B3FF")) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Behavior Rate per Strategy",
    x = "Behavior",
    y = "Behavior Rate"
  )
p9

# I want there to be a "***" above the boxplots of probing of both migrant and overwintering and  "**" above the boxplots of surface_pecking of both migrant and overwintering
p10 <- p9 +
  geom_text(data = data.frame(Behavior = "Probing", y = 0.14, label = "*"),
            aes(x = Behavior, y = y, label = label), vjust = -1, size = 5) 
p10
ggsave("point_behaviors_strategy.png", plot = p9, width = 18, height = 10, dpi = 300)

p11 <- ggplot(point_behaviors |> filter(!Behavior == "Swallowing"), aes(x = as.factor(Week), y = Behavior_Rate, fill = Behavior)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c("#FF9999", "#66B3FF","#26435F")) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Behavior Rate per Strategy",
    x = "Behavior",
    y = "Behavior Rate"
  )
p11

#################################################################################
# The same analysis but than for the stage event behaviors
# I want to add up the duration per week per behavior per strategy and i also want to add up the Media.duration..s per week. Then i want to divide the duration with the media.duration..s so i end up with duration per week per behavior per strategy that is normalized

# Sum duration per Week, Strategy, Behavior
#behavior_duration <- behaviors |>
#  group_by(Week, Strategy, Behavior) |>
#  summarise(Total_Duration = sum(Duration), .groups = "drop")

# Get total media duration per week per strategy
#media_duration_per_week <- behaviors |>
#  distinct(Strategy, Observation_id, Week, Media.duration..s.) |>
#  group_by(Strategy, Week) |>
#  summarise(Total_Media_Duration = sum(Media.duration..s.), .groups = "drop")

#  Join and normalize
#stage_behavior_summary <- behavior_duration |>
#  left_join(media_duration_per_week, by = c("Week", "Strategy")) |>
#  mutate(Behavior_Rat = (Total_Duration / Total_Media_Duration) * 100) 

#  Reorder Week factor (optional)
#stage_behavior_summary$Week <- factor(
#  stage_behavior_summary$Week,
#  levels = c(9, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21))

#stage_behavior_summary <- stage_behavior_summary |>
#  filter(Behavior %in% c("Walking", "Alert", "Digging", "Routing", #"Handling_prey"))

stage_behavior <- behaviors |>
  filter(Behavior %in% c("Walking", "Alert", "Digging", "Routing", "Handling_prey")) |>
  mutate(Behavior_Rate = (total_duration / Media.duration..s.))

# Plotting the stage behaviors
p_stage <- ggplot(stage_behavior,
                  aes(x = as.factor(Week), y = Behavior_Rate, 
                      fill = Strategy)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#FF9999","#1ED760", "#66B3FF")) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Stage Behaviour by Strategy",
    x = "Week",
    y = "Duration Rate")  +
  facet_wrap(~Behavior)
  
p_stage

qqnorm(stage_behavior$Behavior_Rate)

counts_stage <- stage_behavior |>
  group_by(Week, Strategy, Behavior) |>
  summarise(n = n_distinct(Observation_id), .groups = "drop") |>
  filter(!Week %in% c("9", "11", "15"))

p11a <- ggplot(stage_behavior |> filter(Behavior == "Walking"),
              aes(x = as.factor(Week), y = Behavior_Rate, 
                  fill = Strategy)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#FF9999","#1ED760", "#66B3FF")) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Walking by Strategy",
    x = "Week",
    y = "Duration Rate")
p11a

p11b <- p11a + geom_text(data = counts_stage |> filter(Behavior == "Walking"), aes(x = as.factor(Week), y = 1.2,                                             label = paste0("n=", n)),
                       position = position_dodge(width = 0.8), size = 4)

p11b

glmer12 <- glmer(Behavior_Rate ~ Week * Strategy + (1 | Three_letter_code), 
                family = Gamma(link = "log"),
                data = subset(stage_behavior, Behavior == "Walking"),
                control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
summary(glmer12)

p11c <- p11b +
  geom_text(data = data.frame(
    Week = factor(c(13, 18, 19, 21)),
    label = c("***", "***", "***", "***"),
    y = c(1.00, 1.00, 0.75, 0.7)
  ),
  aes(x = Week, y = y, label = label),
  vjust = -0.5,
  size = 6,
  inherit.aes = FALSE)  # <--- This is the key!
p11c

stage_behavior %>%
  filter(Behavior == "Walking") %>%
  group_by(Week) %>%
  summarise(p_value = kruskal.test(Behavior_Rate ~ Strategy)$p.value)

p12a <- ggplot(stage_behavior |> filter(Behavior == "Alert"),
              aes(x = as.factor(Week), y = Behavior_Rate, 
                  fill = Strategy)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#FF9999", "#1ED760", "#66B3FF")) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Alert by Strategy",
    x = "Week",
    y = "Duration Rate")
p12a

p12b <- p12a + geom_text(data = counts_stage |> filter(Behavior == "Alert"), aes(x = as.factor(Week), y = 1.2,                                             label = paste0("n=", n)),
                         position = position_dodge(width = 0.8), size = 4)

p12b

glmer13 <- glmer(Behavior_Rate ~ Week * Strategy + (1 | Three_letter_code), 
                family = Gamma(link = "log"),
                data = subset(stage_behavior, Behavior == "Alert"),
                control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
summary(glmer13)

p12c <- p12b +
  geom_text(data = data.frame(
    Week = factor(c(18, 19, 21)),
    label = c("***", "***", "**"),
    y = c(1.00, 0.7, 0.7)
  ),
  aes(x = Week, y = y, label = label),
  vjust = -0.5,
  size = 6,
  inherit.aes = FALSE)  # <--- This is the key!
p12c

stage_behavior %>%
  filter(Behavior == "Alert") %>%
  group_by(Week) %>%
  summarise(p_value = kruskal.test(Behavior_Rate ~ Strategy)$p.value)

p13a <- ggplot(stage_behavior |> filter(Behavior == "Digging"),
              aes(x = as.factor(Week), y = Behavior_Rate, 
                  fill = Strategy)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#FF9999", "#1ED760","#66B3FF")) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Digging by Strategy",
    x = "Week",
    y = "Duration Rate")
p13a

p13b <- p13a + geom_text(data = counts_stage |> filter(Behavior == "Digging"), aes(x = as.factor(Week), y = 1.2,                                             label = paste0("n=", n)),
                         position = position_dodge(width = 0.8), size = 2)

p13b
glmer14 <- glmer(Behavior_Rate ~ Week * Strategy + (1 | Three_letter_code), 
                family = Gamma(link = "log"),
                data = subset(stage_behavior, Behavior == "Digging"),
                control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
summary(glmer14)

p13c <- p13b +
  geom_text(data = data.frame(
    Week = factor(18),
    label = "**",
    y = 0.7
  ),
  aes(x = Week, y = y, label = label),
  vjust = -0.5,
  size = 6,
  inherit.aes = FALSE)  # <--- This is the key!
p13c

p14a <- ggplot(stage_behavior |> filter(Behavior == "Routing"),
              aes(x = as.factor(Week), y = Behavior_Rate, 
                  fill = Strategy)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#FF9999", "#1ED760", "#66B3FF")) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Routing by Strategy",
    x = "Week",
    y = "Duration Rate")
p14a

p14b <- p14a + geom_text(data = counts_stage |> filter(Behavior == "Routing"), aes(x = as.factor(Week), y = 1.2,                                             label = paste0("n=", n)),
                         position = position_dodge(width = 0.8), size = 2)

p14b

p15a <- ggplot(stage_behavior |> filter(Behavior == "Handling_prey"),
              aes(x = as.factor(Week), y = Behavior_Rate, 
                  fill = Strategy)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#FF9999", "#1ED760", "#66B3FF")) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Handling prey by Strategy",
    x = "Week",
    y = "Duration Rate")
p15a

p15b <- p15a + geom_text(data = counts_stage |> filter(Behavior == "Handling_prey"), aes(x = as.factor(Week), y = 1.2,                                             label = paste0("n=", n)),
                         position = position_dodge(width = 0.8), size = 4)

p15b

glmer15 <- glmer(Behavior_Rate ~ Week * Strategy + (1 | Three_letter_code), 
                family = Gamma(link = "log"),
                data = subset(stage_behavior, Behavior == "Handling_prey"),
                control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
summary(glmer15)

p15c <- p15b +
  geom_text(data = data.frame(
    Week = factor(c(13, 17, 18, 21)),
    label = c("***", "***", "***", "***"),
    y = c(1.00, 0.75, 1.0, 0.7)
  ),
  aes(x = Week, y = y, label = label),
  vjust = -0.5,
  size = 6,
  inherit.aes = FALSE)  # <--- This is the key!
p15c

p_stage_behavior <- p11b + p12b + p13b + p14b + p15b
p_stage_behavior
ggsave("stage_behaviors_strategy.png", plot = p_stage_behavior, width = 18, height = 10, dpi = 300)

p_stage_filtered <- p11c + p12c + p13c + p15c
p_stage_filtered
ggsave("stage_behaviors_strategy_filtered.png", plot = p_stage_filtered, width = 25, height = 10, dpi = 300)
##### Looking at whether strategy influences duration
#aov(Behavior_Rate ~ Behavior * Strategy, data = stage_behavior_summary)
#summary(aov(Behavior_Rate ~ Behavior * Strategy, data = #stage_behavior_summary))

#wilcox.test(Behavior_Rate ~ Strategy, data = stage_behavior)
# W = 2862355, p-value = 0.715

#####
#emmeans_results2 <- emmeans(aov(Behavior_Rate ~ Behavior * Strategy, data = #stage_behavior), 
#                           pairwise ~ Strategy | Behavior)
#print(emmeans_results2)
# Plotting the results

p16 <- ggplot(stage_behavior , aes(x = Behavior, y = Behavior_Rate, fill = Strategy)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c("#FF9999","#1ED760", "#66B3FF")) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Behavior Rate per Strategy",
    x = "Behavior",
    y = "Behavior Rate")
p16

# I want there to be a "***" above the boxplots of routing and alert, and "* above walking
#p17 <- p16 +
#  geom_text(data = data.frame(Behavior = "Routing", y = 0.9, label = "***"),
#            aes(x = Behavior, y = y, label = label),
#            vjust = -1, size = 5, inherit.aes = FALSE) +
#  geom_text(data = data.frame(Behavior = "Alert", y = 0.9, label = "***"),
#            aes(x = Behavior, y = y, label = label),
#            vjust = -1, size = 5, inherit.aes = FALSE) +
#  geom_text(data = data.frame(Behavior = "Walking", y = 0.9, label = "*"),
#            aes(x = Behavior, y = y, label = label),
#            vjust = -1, size = 5, inherit.aes = FALSE)

#p17

ggsave("stage_behaviors_strategy_boxplot.png", plot = p16, width = 18, height = 10, dpi = 300)


transect_counts <- complete_dataset |>
  group_by(Transect_ID, Strategy, Week) |>
  distinct() |>
  summarise(n = n_distinct(Observation_id), .groups = "drop") 

p_1.1 <- ggplot(complete_dataset |> filter(Transect_ID == "1.1"), aes(x = as.factor(Week), fill = Strategy)) +
  geom_bar(stat = "count", position = "dodge") +
  scale_fill_manual(values = c("#FF9999", "#1ED760", "#66B3FF")) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Strategy per Transect",
    x = "Week",
    y = "Counts"
  ) 
p_1.1

p_1.1b <- p_1.1 + geom_text(data = transect_counts |> filter(Transect_ID == "1.1"), aes(x = as.factor(Week), y = 1300,                                             label = paste0("n=", n)),
                         position = position_dodge(width = 0.8), size = 2)

p_1.1b

p_1.2 <- ggplot(complete_dataset |> filter(Transect_ID == "1.2"), aes(x = as.factor(Week), fill = Strategy)) +
  geom_bar(stat = "count", position = "dodge") +
  scale_fill_manual(values = c("#FF9999", "#1ED760", "#66B3FF")) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Strategy per Transect",
    x = "Transect ID",
    y = "Count"
  )
p_1.2

p_1.2b <- p_1.2 + geom_text(data = transect_counts |> filter(Transect_ID == "1.2"), aes(x = as.factor(Week), y = 1100,                                             label = paste0("n=", n)),
                            position = position_dodge(width = 0.8), size = 2)

p_1.2b

p_1.3 <- ggplot(complete_dataset |> filter(Transect_ID == "1.3"), aes(x = as.factor(Week), fill = Strategy)) +
  geom_bar(stat = "count", position = "dodge") +
  scale_fill_manual(values = c("#FF9999", "#1ED760", "#66B3FF")) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Strategy per Transect",
    x = "Transect ID",
    y = "Count"
  )
p_1.3

p_2.1 <- ggplot(complete_dataset |> filter(Transect_ID == "2.1"), aes(x = as.factor(Week), fill = Strategy)) +
  geom_bar(stat = "count", position = "dodge") +
  scale_fill_manual(values = c("#FF9999", "#1ED760", "#66B3FF")) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Strategy per Transect",
    x = "Transect ID",
    y = "Count"
  )
p_2.1

p_2.2 <- ggplot(complete_dataset |> filter(Transect_ID == "2.2"), aes(x = as.factor(Week), fill = Strategy)) +
  geom_bar(stat = "count", position = "dodge") +
  scale_fill_manual(values = c("#FF9999", "#1ED760", "#66B3FF")) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Strategy per Transect",
    x = "Transect ID",
    y = "Count"
  )
p_2.2

p_alltransect <- p_1.1 + p_1.2 + p_1.3 + p_2.1 
p_alltransect

complete_dataset$Transect_ID <- as.factor(complete_dataset$Transect_ID)
transect_dataset <- complete_dataset |> 
  group_by(Transect_ID, Strategy, Week) |> 
  summarise(Count = n_distinct(Observation_id), .groups = "drop")
transect_dataset$Transect_ID <- as.factor(transect_dataset$Transect_ID)

transect_counts$Transect_ID <- as.factor(transect_counts$Transect_ID)

p_early <- ggplot(complete_dataset |> filter(Strategy == "early_northward_migration"), aes(x = as.factor(Week), fill = Transect_ID)) +
  geom_bar(stat = "count", position = "dodge") +
  scale_fill_manual(values = c("#FF9999", "#E81C2D", "#DB6A39","#E8749C","#F6AAB1" )) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Early Northward Migration per Week",
    x = "Week",
    y = "Count"
  )
p_early

p_early <- p_early + geom_text(data = transect_counts |> filter(Strategy == "early_northward_migration"), aes(x = as.factor(Week), y = 2000,                                             label = paste0("n=", n)),
                         position = position_dodge(width = 0.8), size = 2)
p_early

p_late <- ggplot(complete_dataset |> filter(Strategy == "late_northward_migration"), aes(x = as.factor(Week), fill = Transect_ID)) +
  geom_bar(stat = "count", position = "dodge") +
  scale_fill_manual(values = c("#FF9999", "#E81C2D", "#DB6A39","#E8749C","#F6AAB1")) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Late Northward Migration per Week",
    x = "Week",
    y = "Count"
  )
p_late

p_late <- p_late + geom_text(data = transect_counts |> filter(Strategy == "late_northward_migration"), aes(x = as.factor(Week), y = 3500,                                             label = paste0("n=", n)),
                           position = position_dodge(width = 0.8), size = 2)
p_late

p_overwinterer <- ggplot(complete_dataset |> filter(Strategy == "overwinterer"), aes(x = as.factor(Week), fill = Transect_ID)) +
  geom_bar(stat = "count", position = "dodge") +
  scale_fill_manual(values = c("#FF9999", "#E81C2D", "#DB6A39","#E8749C","#F6AAB1")) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Overwinterer per Week",
    x = "Week",
    y = "Count"
  )
p_overwinterer

p_overwinterer <- p_overwinterer + geom_text(data = transect_counts |> filter(Strategy == "overwinterer"), aes(x = as.factor(Week), y = 3200,                                             label = paste0("n=", n)),
                             position = position_dodge(width = 0.8), size = 2)
p_overwinterer

p_all_strategies <- p_early / p_late / p_overwinterer
p_all_strategies
ggsave("transect_strategy_week.png", plot = p_all_strategies, width = 18, height = 10, dpi = 300)

qqnorm(transect_dataset$Count)

glm7 <- glm(Count ~ Week + Transect_ID + Strategy, 
            family = Gamma(link = "log"),
            data = transect_dataset)
summary(glm7)

glm8 <- glm(Count ~ Week * Transect_ID + Strategy, 
            family = Gamma(link = "log"),
            data = transect_dataset)
summary(glm8)

glm9 <- glm(Count ~ Week * Strategy + Transect_ID, 
            family = Gamma(link = "log"),
            data = transect_dataset)
summary(glm9)

glm10 <- glm(Count ~ Transect_ID * Strategy + Week, 
            family = Gamma(link = "log"),
            data = transect_dataset)
summary(glm10)

## glm8 is the best model according to AIC

lm8 <- lm(Count ~ Week * Transect_ID + Strategy, 
            data = transect_dataset)
summary(lm8)
# if would use linear instead of generalized linear there would be significant difference in use of transect points between early and late arrivals 

############################################################################
## Habitat usage 

habitat_counts <- complete_dataset |>
  group_by(Habitat, Strategy, Week) |>
  distinct() |>
  summarise(n = n_distinct(Observation_id), .groups = "drop") |>
  filter(!Week %in% c("9", "11", "12", "13", "15"))

habitat_counts$Habitat[habitat_counts$Habitat == "Combination "] <- "Combination"

complete_dataset$Habitat <- complete_dataset$Habitat |>
  str_trim()

habitat_counts <- complete_dataset |>
  filter(!Week %in% c("9", "11", "12", "13", "15")) |>
  group_by(Habitat, Strategy, Week) |>
  summarise(n = n_distinct(Observation_id), .groups = "drop")

# Now join back to complete_dataset
complete_dataset <- complete_dataset |>
  filter(!Week %in% c("9", "11", "12", "13", "15")) |>
  left_join(habitat_counts, by = c("Habitat", "Strategy", "Week"))


habitat_counts$Habitat <- habitat_counts$Habitat |>
  str_trim()

p_habitat <- ggplot(complete_dataset, aes(x = as.factor(Week), fill = Habitat)) +
  geom_bar(stat = "count", position = "dodge") +
  scale_fill_manual(values = c("#A2A8B4", "#D6C78A", "#66B3FF")) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Habitat Usage per Week",
    x = "Week",
    y = "Count"
  ) +
  facet_wrap(~Strategy)
p_habitat

p_early_hab <- ggplot(complete_dataset |> filter(Strategy == "early_northward_migration"), aes(x = as.factor(Week), fill = Habitat)) +
  geom_bar(stat = "count", position = "dodge") +
  scale_fill_manual(values = c("#A2A8B4", "#D6C78A", "#66B3FF")) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Habitat Usage Early Northward Migration",
    x = "Week",
    y = "Count"
  )
p_early_hab

p_early_hab <- p_early_hab + geom_text(data = habitat_counts |> filter(Strategy == "early_northward_migration"), aes(x = as.factor(Week), y = 2100,                                             label = paste0("n=", n)),
                                             position = position_dodge(width = 0.8), size = 3)
p_early_hab

p_late_hab <- ggplot(complete_dataset |> filter(Strategy == "late_northward_migration"), aes(x = as.factor(Week), fill = Habitat)) +
  geom_bar(stat = "count", position = "dodge") +
  scale_fill_manual(values = c("#A2A8B4", "#D6C78A", "#66B3FF")) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Habitat Usage Late Northward Migration **",
    x = "Week",
    y = "Count"
  )
p_late_hab

p_late_hab <- p_late_hab + geom_text(data = habitat_counts |> filter(Strategy == "late_northward_migration"), aes(x = as.factor(Week), y = 3100,                                             label = paste0("n=", n)),
                                       position = position_dodge(width = 0.8), size = 3)
p_late_hab

p_win_hab <- ggplot(complete_dataset |> filter(Strategy == "overwinterer"), aes(x = as.factor(Week), fill = Habitat)) +
  geom_bar(stat = "count", position = "dodge") +
  scale_fill_manual(values = c("#A2A8B4", "#D6C78A", "#66B3FF")) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Habitat Usage Overwinterers",
    x = "Week",
    y = "Count"
  )
p_win_hab

p_win_hab <- p_win_hab + geom_text(data = habitat_counts |> filter(Strategy == "overwinterer"), aes(x = as.factor(Week), y = 2600,                                             label = paste0("n=", n)),
                                     position = position_dodge(width = 0.8), size = 3)
p_win_hab

p_all_habitat <- p_early_hab / p_late_hab / p_win_hab
p_all_habitat

glm11 <- glm(n ~ Week + Habitat + Strategy, 
            family = Gamma(link = "log"),
            data = complete_dataset)
summary(glm11)

glm12 <- glm(n ~ Week * Habitat + Strategy, 
            family = Gamma(link = "log"),
            data = complete_dataset)
summary(glm12)

glm13 <- glm(n ~ Week * Strategy + Habitat, 
            family = Gamma(link = "log"),
            data = complete_dataset)
summary(glm13)

glm14 <- glm(n ~ Habitat * Strategy + Week, 
            family = Gamma(link = "log"),
            data = complete_dataset)
summary(glm14)

AIC(glm11, glm12, glm13, glm14)
## glm14 is the best model according to AIC

glm15 <- glm(n ~ Habitat * Strategy * Week, 
             family = Gamma(link = "log"),
             data = complete_dataset)
summary(glm15)

AIC(glm11,glm14, glm15)
## glm15 is best according to AIC

kruskal.test(n ~ Strategy, data = complete_dataset)
# Kruskal-Wallis chi-squared = 2321.7, df = 2, p-value < 2.2e-16

library(ggplot2)
library(dplyr)
library(FSA)
library(multcompView)

#Run Dunn Test
dunn <- dunnTest(n ~ Strategy, data = complete_dataset, 
                 method = "bonferroni")
dunn
dunn_df <- dunn$res


ggsave("habitat_usage_strategy.png", plot = p_all_habitat, width = 18, height = 10, dpi = 300)

