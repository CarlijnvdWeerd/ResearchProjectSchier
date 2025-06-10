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

# I want the three_letter_code for JTN, JYL, KJU, KNP, KPM, KXM, LCT, LMN, LPT, LYK, MAU, MCH, MTJ, MYV, NLJ, NTV, PAE, PAJ, PKN, PMP, PNL to be assigned in a new column called strategy to be assigned overwinterer and for the rest of the three_letter_code to be assigned migrant
            

complete_dataset <- complete_dataset |>
  dplyr::mutate(Strategy = ifelse(
    Three_letter_code %in% c("JTN", "JYL", "KJU", "KNP", "KPM", "KXM", "LCT", "LMN", "LPT", "LYK", "MAU", "MCH", "MTJ", "MYV", "NLJ", "PAE", "PAJ", "PKN", "PMP", "PNL"),
    "overwinterer",
    "migrant"
  ))

complete_dataset |>
  count(Strategy)
# migrant n=29385, overwinterer n=14367

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
  summarise(n = n_distinct(Observation_id), .groups = "drop")

# Base boxplot
p1 <- ggplot(behaviors |> filter(visually_foraging < 200),
             aes(x = as.factor(Week), y = visually_foraging, 
             fill = Strategy)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#FF9999", "#66B3FF")) +
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

m0 <- lm(visually_foraging ~ Strategy, data = behaviors)
print(m0)
summary(m0)

m1 <- lm(visually_foraging ~ Week + Strategy, data = behaviors)
print(m1)
summary(m1)

m2 <- lm(visually_foraging ~ Week * Strategy, data = behaviors)
print(m2)
summary(m2)

anova(m1,m2)
# The interaction model (m2) is significantly better than the main effects model (m1), indicating that the effect of week on duration depends on the strategy.
# This suggests that the relationship between week and duration is different for migrants and overwinterers.

anova(m0, m2)
# So adding Week as an interaction to the strategy makes the model a quite good predictior of the duration of foraging walking. The p-value is ***


m3 <- lm(visually_foraging ~ Week * Strategy + Habitat, data = behaviors)
anova(m2, m3) 


m4 <- lm(visually_foraging ~ Week * Strategy + Transect_ID + Habitat, data = behaviors)
print(m4)
summary(m4)
anova(m3,m4)

m5 <- lm(visually_foraging ~ Week * Strategy + Transect_ID + Habitat + Tide, data = behaviors)
summary(m5)
anova(m2, m5)
anova(m4, m5)

# Adding Transect_ID, Tide or Habitat does not make it a better predictive model for the duration of foraging walking.

new_data <- unique(behaviors[, c("Week", "Strategy", "Transect_ID", "Habitat", "Tide")])
new_data$predicted <- predict(m5, newdata = new_data)

p3 <- p2 +
  geom_point(data = new_data,
             aes(x = as.factor(Week), y = predicted, shape = Strategy, group = Strategy),
             color = ifelse(new_data$Strategy == "migrant", "#c77575", "#5190cf"),
             size = 3,
             position = position_dodge(width = 0.6)) +
  scale_shape_manual(values = c("migrant" = 17, "overwinterer" = 18))

p3

emmeans_results <- emmeans(aov(visually_foraging ~ Week * Strategy, data = behaviors), 
                           pairwise ~ Strategy | Week)
print(emmeans_results)

p4 <- p3 +
  geom_text(data = data.frame(
    Week = factor(c(12, 13, 16, 17, 18, 19, 20, 21)),
    label = c("*", "***", "***", "***", "***", "***", "***", "***"),
    y = c(130, 110, 130, 130, 130, 130, 120, 120)
  ),
  aes(x = Week, y = y, label = label),
  vjust = -0.5,
  size = 6,
  inherit.aes = FALSE)  # <--- This is the key!
p4

ggsave("foraging_walking_duration_strategy.png", plot = p2, width = 13, height = 6, dpi = 300)
ggsave("foraging_walking_duration_strategy_with_predictions.png", plot = p4, width = 13, height = 6, dpi = 300)

###############################################################################
# Look at surface_pecking per week per strategy in behavior dataframe

# Calculate the point behaviours
point_behaviors <- complete_dataset |>
  filter(Behavior.type == "POINT") |>
  filter(!Observation_id %in% c("KET_20.03", "KET.20.03")) |>
  group_by(Three_letter_code, Behavior, Strategy, Week, Tide, Habitat,   
           Observation_id, Transect_ID, Date.x, Media.duration..s.) |>
  summarise(Behavior_Count = n(), .groups = "drop")|>
  mutate(Behavior_Rate = Behavior_Count / Media.duration..s.) |>
  filter(Week %in% c(12, 13, 16, 17, 18 ,19, 20 ,21))
  
## Video of KET on 20 03 is out of proportion in comparison of the other video's of that day but also over the past time.


p5 <- ggplot(point_behaviors |> filter(Behavior == "Probing"),
             aes(x = as.factor(Week), y = Behavior_Rate, 
                 fill = Strategy)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#FF9999", "#66B3FF")) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Probing by Strategy",
    x = "Week",
    y = "Duration Rate")
p5

## making models to understand the trend of the probing 
m6 <- lm(Behavior_Rate ~ Week * Strategy, data = subset(point_behaviors, Behavior == "Probing"))
summary(m6)

m7 <- lmer(Behavior_Rate ~ Week * Strategy + (1 | Three_letter_code), 
                  data = subset(point_behaviors, Behavior == "Probing"))
summary(m7)

m8 <- lmer(Behavior_Rate ~ Week * Strategy + (1 | Three_letter_code) 
            + (1| Transect_ID) + (1|Tide) + (1|Habitat), data = subset
            (point_behaviors, Behavior == "Probing"))
summary(m8)
anova(m7, m8)

m9 <- lmer(Behavior_Rate ~ Week * Strategy + (1|Three_letter_code)
            + (1| Habitat), data = subset(point_behaviors, Behavior == 
                                            "Probing"))
summary(m9)
anova(m7,m9)
anova(m8,m9)

t.test(Behavior_Rate ~ Strategy, data = subset(point_behaviors, Behavior == "Probing"))

#Welch Two Sample t-test

#data:  Behavior_Rate by Strategy
#t = -3.7197, df = 145.35, p-value = 0.0002842
#alternative hypothesis: true difference in means between group migrant and group #overwinterer is not equal to 0
#95 percent confidence interval:
#  -0.024071895  -0.07367087
#sample estimates:
#  mean in group migrant mean in group overwinterer 
#0.4411215                 0.5983164 

p6 <- ggplot(point_behaviors |> filter(Behavior == "Surface_pecking"),
             aes(x = as.factor(Week), y = Behavior_Rate, 
                 fill = Strategy)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#FF9999", "#66B3FF")) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Surface pecking by Strategy",
    x = "Week",
    y = "Duration Rate")
p6

p7 <- ggplot(point_behaviors |> filter(Behavior == "Turning_stuff"),
             aes(x = as.factor(Week), y = Behavior_Rate, 
                 fill = Strategy)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#FF9999", "#66B3FF")) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Turning by Strategy",
    x = "Week",
    y = "Duration Rate")
p7

p_point_behavior <- p5 + p6 + p7
p_point_behavior

emmeans_results <- emmeans(aov(Behavior_Rate ~ Behavior * Strategy, data = point_behaviors), 
                           pairwise ~ Strategy | Behavior)
print(emmeans_results)


ggsave("rate_forage_stratgy.png", plot = p_point_behavior, width = 18, height = 10, dpi = 300)

##### Looking at all the different behaviors
aov(Behavior_Rate ~ Behavior * Strategy, data = point_behavior_id)
summary(aov(Behavior_Rate ~ Behavior * Strategy, data = point_behavior_id))

# Post-hoc test
library(emmeans)
emmeans_results <- emmeans(aov(Behavior_Rate ~ Behavior * Strategy, data = point_behavior_id), 
                           pairwise ~ Behavior | Strategy)
print(emmeans_results)
# Plotting the results
library(ggplot2)
behavior_summary$Behavior <- factor(behavior_summary$Behavior, 
                                     levels = c("Probing", "Surface_pecking", "Swallowing", "Turning_stuff"))
p8 <- ggplot(behavior_summary, aes(x = Behavior, y = Behavior_Rate, fill = Strategy)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#FF9999", "#66B3FF")) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Normalized Behavior Counts by Strategy",
    x = "Behavior",
    y = "Normalized Count"
  ) +
  facet_wrap(~Strategy)
p8

# I want to also do an emmeans test to compare the strategies for each behavior

# Plotting the results
p9 <- ggplot(point_behavior_id, aes(x = Behavior, y = Behavior_Rate, fill = Strategy)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c("#FF9999", "#66B3FF")) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Behavior Rate per Strategy",
    x = "Behavior",
    y = "Behavior Rate"
  )
p9

# I want there to be a "***" above the boxplots of probing of both migrant and overwintering and  "**" above the boxplots of surface_pecking of both migrant and overwintering
p10 <- p9 +
  geom_text(data = data.frame(Behavior = "Probing", Strategy = "migrant", y = 0.14, label = "***"),
            aes(x = Behavior, y = y, label = label), vjust = -1, size = 5) +
  geom_text(data = data.frame(Behavior = "Probing", Strategy = "overwinterer", y = 0.15, label = "***"),
            aes(x = Behavior, y = y, label = label), vjust = -1, size = 5) +
  geom_text(data = data.frame(Behavior = "Surface_pecking", Strategy = "migrant", y = 0.10, label = "**"),
            aes(x = Behavior, y = y, label = label), vjust = -1, size = 5) +
  geom_text(data = data.frame(Behavior = "Surface_pecking", Strategy = "overwinterer", y = 0.10, label = "**"),
            aes(x = Behavior, y = y, label = label), vjust = -1, size = 5)
p10
ggsave("point_behaviors_strategy.png", plot = p10, width = 18, height = 10, dpi = 300)

p11 <- ggplot(point_behavior_id, aes(x = as.factor(Week), y = Behavior_Rate, fill = Behavior)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c("#FF9999", "#5F3939", "#66B3FF", "#26435F")) +
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
behavior_duration <- behaviors |>
  group_by(Week, Strategy, Behavior) |>
  summarise(Total_Duration = sum(Duration), .groups = "drop")

# Get total media duration per week per strategy
media_duration_per_week <- behaviors |>
  distinct(Strategy, Observation_id, Week, Media.duration..s.) |>
  group_by(Strategy, Week) |>
  summarise(Total_Media_Duration = sum(Media.duration..s.), .groups = "drop")

#  Join and normalize
stage_behavior_summary <- behavior_duration |>
  left_join(media_duration_per_week, by = c("Week", "Strategy")) |>
  mutate(Behavior_Rat = (Total_Duration / Total_Media_Duration) * 100) 

#  Reorder Week factor (optional)
stage_behavior_summary$Week <- factor(
  stage_behavior_summary$Week,
  levels = c(9, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21)
)

stage_behavior_summary <- stage_behavior_summary |>
  filter(Behavior %in% c("Walking", "Alert", "Digging", "Routing", "Handling_prey"))

# Plotting the stage behaviors
p_stage <- ggplot(stage_behavior_summary, aes(x = as.factor(Week), y = Behavior_Rate, fill = Strategy)) +
  geom_col(position = "dodge", alpha = 0.7) +
  scale_fill_manual(values = c("#FF9999", "#66B3FF")) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Stage Behaviors Counts per Week by Strategy",
    x = "Week",
    y = "Normalized Stage Behavior Duration"
  ) +
  facet_wrap(~Behavior)
p_stage

p11 <- ggplot(stage_behavior_summary |> filter(Behavior == "Walking"), aes(x = as.factor(Week), y = Behavior_Rate , fill = Strategy)) +
  geom_col(position = position_dodge(), width = 0.9) +
  scale_fill_manual(values = c("#FF9999", "#66B3FF")) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Walking per Week by Strategy",
    x = "Week",
    y = "Walking Rate"
  )
p11

p12 <- ggplot(stage_behavior_summary |> filter(Behavior == "Alert"), aes(x = as.factor(Week), y = Behavior_Rate , fill = Strategy)) +
  geom_col(position = position_dodge(), width = 0.9) +
  scale_fill_manual(values = c("#FF9999", "#66B3FF")) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Alert per Week by Strategy",
    x = "Week",
    y = "Alert Rate"
  )
p12

p13 <- ggplot(stage_behavior_summary |> filter(Behavior == "Digging"), aes(x = as.factor(Week), y = Behavior_Rate , fill = Strategy)) +
  geom_col(position = position_dodge(), width = 0.9) +
  scale_fill_manual(values = c("#FF9999", "#66B3FF")) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Digging per Week by Strategy",
    x = "Week",
    y = "Digging Rate"
  )
p13

p14 <- ggplot(stage_behavior_summary |> filter(Behavior == "Routing"), aes(x = as.factor(Week), y = Behavior_Rate , fill = Strategy)) +
  geom_col(position = position_dodge(), width = 0.9) +
  scale_fill_manual(values = c("#FF9999", "#66B3FF")) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Routing per Week by Strategy",
    x = "Week",
    y = "Routing Rate"
  )
p14

p15 <- ggplot(stage_behavior_summary |> filter(Behavior == "Handling_prey"), aes(x = as.factor(Week), y = Behavior_Rate , fill = Strategy)) +
  geom_col(position = position_dodge(), width = 0.9) +
  scale_fill_manual(values = c("#FF9999", "#66B3FF")) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Handling Prey per Week by Strategy",
    x = "Week",
    y = "Handling Prey Rate"
  )
p15

p_stage_behavior <- p11 + p12 + p13 + p14 + p15
p_stage_behavior
ggsave("stage_behaviors_strategy.png", plot = p_stage_behavior, width = 18, height = 10, dpi = 300)

##### Looking at whether strategy influences duration
aov(Behavior_Rate ~ Behavior * Strategy, data = stage_behavior_summary)
summary(aov(Behavior_Rate ~ Behavior * Strategy, data = stage_behavior_summary))

#####
emmeans_results2 <- emmeans(aov(Behavior_Rate ~ Behavior * Strategy, data = stage_behavior_summary), 
                           pairwise ~ Strategy | Behavior)
print(emmeans_results2)
# Plotting the results
p16 <- ggplot(stage_behavior_summary, aes(x = Behavior, y = Behavior_Rate, fill = Strategy)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#FF9999", "#66B3FF")) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Rate Stage Behavior Duration by Strategy",
    x = "Behavior",
    y = "Behavior Rate"
  ) +
  facet_wrap(~Strategy)
p16

# I want there to be a "***" above the boxplots of walking of both migrant and overwintering
p17 <- p16 +
  geom_text(
    data = data.frame(Behavior = "Walking", Strategy = "migrant", y = 40, label = "***"),
    aes(x = Behavior, y = y, label = label, Strategy = Strategy),  
    inherit.aes = FALSE,
    vjust = -1,
    size = 5
  ) +
  geom_text(
    data = data.frame(Behavior = "Walking", Strategy = "overwinterer", y = 40, label = "***"),
    aes(x = Behavior, y = y, label = label, Strategy = Strategy),  
    color = "black", 
    inherit.aes = FALSE,
    vjust = -1,
    size = 5
  ) +
  expand_limits(y = 45)  

p17
ggsave("stage_behaviors_strategy_boxplot.png", plot = p17, width = 18, height = 10, dpi = 300)
