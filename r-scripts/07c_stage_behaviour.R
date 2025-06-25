## Run the script 07_BORIS_forage first, then this script will work
renv::restore()

stage_behavior <- stage_behavior |>
  filter(!Week %in% c("9", "11", "12", "13", "15"))

# plot Duration_Rate to check normality
ggplot(stage_behavior |> filter(Behavior == "Walking"), aes(x = Duration_Rate)) +
  geom_histogram(bins = 30, fill = "#69b3a2", color = "black") +
  labs(
    title = "Distribution of Walking Durations",
    x = "Walking Duration (seconds)",
    y = "Count"
  ) +
  theme_minimal()


p10a <- ggplot(stage_behavior |> filter(Behavior == "Walking"),
               aes(x = as.factor(Week), y = Duration_Rate, 
                   fill = Strategy)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#E777F2", "#4DD2A4", "#4DC8F9")) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Walking by Strategy",
    x = "Week",
    y = "Duration Rate") +
  theme(
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    legend.position = "none"
  )
p10a


p10b <- p10a + geom_text(data = counts_stage |> filter(Behavior == "Walking"), aes(x = as.factor(Week), y = 0.3, label = n, group = Strategy),
                         position = position_dodge(width = 0.9),
                         size = 3,
                         inherit.aes = FALSE
)

p10b

glm1 <- glm(Duration_Rate ~ 1, 
            family = Gamma(link = "log"),
            data = subset(stage_behavior, Behavior == "Walking"))

glm2 <- glm(Duration_Rate ~ Week, 
            family = Gamma(link = "log"),
            data = subset(stage_behavior, Behavior == "Walking"))

glm3 <- glm(Duration_Rate ~ Strategy,
            family = Gamma(link = "log"),
            data = subset(stage_behavior, Behavior == "Walking"))

glm4 <- glm(Duration_Rate ~ Habitat,
            family = Gamma(link = "log"),
            data = subset(stage_behavior, Behavior == "Walking"))

glm5 <- glm(Duration_Rate ~ Tide,
            family = Gamma(link = "log"),
            data = subset(stage_behavior, Behavior == "Walking"))

model.sel(glm1, glm2, glm3, glm4, glm5)

glmerfull <- glmer(Duration_Rate ~ Week + Strategy + (1|Three_letter_code) +
                     (1|Habitat) + (1|Tide),
                   family = Gamma(link = "log"),
                   data = subset(stage_behavior, Behavior == "Walking"),
                   control = glmerControl(optimizer = "bobyqa", 
                                          optCtrl = list(maxfun = 2e5)))
summary(glmerfull)

glmer_int1 <- glmer(Duration_Rate ~ Week * Strategy + (1 | Three_letter_code)
                    + (1 | Habitat) + (1 | Tide), 
                   family = Gamma(link = "log"),
                   data = subset(stage_behavior, Behavior == "Walking"),
                   control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
summary(glmer_int1)

glmer_intpol1 <- glmer(Duration_Rate ~ poly(Week, 2) * Strategy 
                      + (1 | Three_letter_code)
                      + (1 | Habitat) + (1 | Tide), 
                     family = Gamma(link = "log"),
                     data = subset(stage_behavior, Behavior == "Walking"),
                     control = glmerControl(optimizer = "bobyqa", 
                                            optCtrl = list(maxfun = 2e5)))
summary(glmer_intpol1)

glmer_intpol2 <- glmer(Duration_Rate ~ poly(Week, 2) * Strategy 
                      + (1 | Three_letter_code)
                      + (1 | Habitat), 
                     family = Gamma(link = "log"),
                     data = subset(stage_behavior, Behavior == "Walking"),
                     control = glmerControl(optimizer = "bobyqa", 
                                            optCtrl = list(maxfun = 2e5)))
summary(glmer_intpol2)

walking_model <- model.sel(glmerfull, glmer_int1, glmer_intpol1, glmer_intpol2, glm1, glm2, glm3, glm4, glm5)
walking_model

walking_model_df <- as.data.frame(walking_model)
walking_model_df$model <- rownames(walking_model_df)
walking_model_df <- walking_model_df[, c("model", setdiff(names(walking_model_df), "model"))]
head(walking_model_df)
# Save to CSV

write.csv(walking_model_df, "walking_model_selection_table.csv", row.names = FALSE)

stage_behavior$Strategy <- factor(stage_behavior$Strategy)
stage_behavior$Three_letter_code <- factor(stage_behavior$Three_letter_code)
stage_behavior$Habitat <- factor(stage_behavior$Habitat)

strategies <- levels(stage_behavior$Strategy)
three_letter <- levels(stage_behavior$Three_letter_code)
habitat <- levels(stage_behavior$Habitat)
weeks_seq <- seq(min(stage_behavior$Week), max(stage_behavior$Week), length.out = 500)

new_walking_smooth <- expand.grid(
  Week = 12:21,
  Strategy = strategies,
  Three_letter_code = three_letter,
  Habitat = habitat,
  stringsAsFactors = FALSE
)

# Convert them back to factors using the original levels
new_walking_smooth$Strategy <- factor(new_walking_smooth$Strategy, levels = strategies)
new_walking_smooth$Three_letter_code <- factor(new_walking_smooth$Three_letter_code, levels = three_letter)
new_walking_smooth$Habitat <- factor(new_walking_smooth$Habitat, levels = habitat)

new_walking_smooth$predicted <- predict(glmer_intpol2,
                                           newdata = new_walking_smooth,
                                           re.form = NA,
                                           type = "response")
new_walking_smooth <- new_walking_smooth |>
  filter(!Week %in% c("12", "13", "14", "15"))

p10c <- p10b +
  geom_line(
    data = new_walking_smooth,
    aes(x = as.factor(Week), y = predicted, color = Strategy, 
        group = Strategy),
    size = 1.0, inherit.aes = FALSE
  )  +
  
  # Manual colors
  scale_color_manual(values = c(
    "overwinterer" = "#3487a8",
    "late_northward_migration" = "#2d8062",
    "early_northward_migration" = "#904a96"
  )) +
  scale_fill_manual(values = c(
    "overwinterer" = "#4DC8F9",
    "late_northward_migration" = "#4DD2A4",
    "early_northward_migration" = "#E777F2"
  )) +
  labs(
    y = "Duration Rate",
    x = "Week",
    title = "Walking Rate per Strategy"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    legend.position = "none"
  )
p10c

# Pairwise comparison
emm_walking <- emm <- emmeans(glmer_intpol2, ~ Strategy | Week,
                              at = list(Week = c(16, 17, 18, 19, 20, 21)),
                              type = "response")
# Pairwise comparisons of Strategy within each Week × Habitat group
pairwise_results_walking <- pairs(emm_walking, adjust = "tukey", type = "response")
pairwise_summary_walking <- summary(pairwise_results_walking)
# Print summary of comparisons
print(pairwise_summary_walking)
# Extract p-values and contrast names
walking_pvals <- pairwise_summary_walking$p.value
names(walking_pvals) <- gsub(" / ", " - ", pairwise_summary_walking$contrast)
# Remove duplicates (if any)
walking_pvals_unique <- walking_pvals[!duplicated(names(walking_pvals))]
library(multcompView)
# Generate compact letter display for grouping
walking_group_letters <- multcompLetters(walking_pvals_unique)$Letters
library(stringr)  # for str_trim()
# Trim spaces from names
walking_clean_names <- str_trim(names(walking_group_letters))
# Remove duplicates: keep first occurrence only
walking_unique_indices <- !duplicated(walking_clean_names)
walking_clean_names_unique <- walking_clean_names[walking_unique_indices]
walking_group_letters_unique <- walking_group_letters[walking_unique_indices]
# Rename with cleaned names
names(walking_group_letters_unique) <- walking_clean_names_unique
# Define factor level order (optional)
levels_strat <- c("early_northward_migration", "late_northward_migration", "overwinterer")
# Match levels to cleaned names
walking_group_letters_ordered <- walking_group_letters_unique[match(levels_strat, walking_clean_names_unique)]
print(walking_group_letters_ordered)

ggsave("walking_rate_per_strategy.png", plot = p10c, width = 28, height = 10, dpi = 300)

alert_stage_behavior <- stage_behavior |>
  filter(!Habitat == "Water")
## filtering habitat water because the sample size is very small

p11a <- ggplot(alert_stage_behavior |> filter(Behavior == "Alert"),
               aes(x = as.factor(Week), y = Duration_Rate, 
                   fill = Strategy)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#E777F2", "#4DD2A4", "#4DC8F9")) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Alert by Strategy",
    x = "Week",
    y = "Duration Rate") +
  theme(
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    legend.position = "none")
p11a

alert_counts_stage <- stage_behavior |>
  group_by(Week, Strategy, Behavior, Habitat) |>
  summarise(n = n_distinct(Observation_id), .groups = "drop") |>
  filter(!Week %in% c("9", "11","12", "13", "15")) |>
  filter(!Habitat == "Water")

p11b <- p11a + geom_text(data = alert_counts_stage |> filter(Behavior == "Alert"), aes(x = as.factor(Week), y = 0.23,                                             label = paste0("", n)),
                         position = position_dodge(width = 0.8), size = 3)

p11b

glm1 <- glm(Duration_Rate ~ 1, 
            family = Gamma(link = "log"),
            data = subset(alert_stage_behavior, Behavior == "Alert"))

glm2 <- glm(Duration_Rate ~ Week,
            family = Gamma(link = "log"),
            data = subset(alert_stage_behavior, Behavior == "Alert"))

glm3 <- glm(Duration_Rate ~ Strategy,
            family = Gamma(link = "log"),
            data = subset(alert_stage_behavior, Behavior == "Alert"))

glm4 <- glm(Duration_Rate ~ Habitat,
            family = Gamma(link = "log"),
            data = subset(alert_stage_behavior, Behavior == "Alert"))

glm5 <- glm(Duration_Rate ~ Tide,
            family = Gamma(link = "log"),
            data = subset(alert_stage_behavior, Behavior == "Alert"))

model.sel(glm1, glm2, glm3, glm4, glm5)

glmerfull <- glmer(Duration_Rate ~ Week + Strategy + (1|Three_letter_code) +
                     (1|Habitat) + (1|Tide),
                   family = Gamma(link = "log"),
                   data = subset(alert_stage_behavior, Behavior == "Alert"),
                   control = glmerControl(optimizer = "bobyqa", 
                                          optCtrl = list(maxfun = 2e5)))

glmer_red1 <- glmer(Duration_Rate ~ Week + Strategy + (1|Habitat) + (1|Tide),
                    family = Gamma(link = "log"),
                    data = subset(alert_stage_behavior, Behavior == "Alert"),
                    control = glmerControl(optimizer = "bobyqa", 
                                           optCtrl = list(maxfun = 2e5)))

glmer_red2 <- glmer(Duration_Rate ~ Week + Strategy + (1|Habitat), 
                    family = Gamma(link = "log"),
                    data = subset(alert_stage_behavior, Behavior == "Alert"),
                    control = glmerControl(optimizer = "bobyqa", 
                                           optCtrl = list(maxfun = 2e5)))

glmer_red3 <- glmer(Duration_Rate ~ (1|Habitat), 
                    family = Gamma(link = "log"),
                    data = subset(alert_stage_behavior, Behavior == "Alert"),
                    control = glmerControl(optimizer = "bobyqa", 
                                           optCtrl = list(maxfun = 2e5)))

glmer_pol1 <- glmer(Duration_Rate ~ poly(Week, 2) + Strategy + (1 | Habitat), 
                    family = Gamma(link = "log"),
                    data = subset(alert_stage_behavior, Behavior == "Alert"),
                    control = glmerControl(optimizer = "bobyqa", 
                                           optCtrl = list(maxfun = 2e5)))

glmer_polint1 <- glmer(Duration_Rate ~ poly(Week, 2) * Strategy + 
                         (1 | Habitat), 
                    family = Gamma(link = "log"),
                    data = subset(alert_stage_behavior, Behavior == "Alert"),
                    control = glmerControl(optimizer = "bobyqa", 
                                           optCtrl = list(maxfun = 2e5)))

glmer_polint2 <- glmer(Duration_Rate ~ poly(Week,2) * Strategy + Habitat +
                       (1|Three_letter_code),
                    family = Gamma(link = "log"),
                    data = subset(alert_stage_behavior, Behavior == "Alert"),
                    control = glmerControl(optimizer = "bobyqa", 
                                           optCtrl = list(maxfun = 2e5)))

glmer_spline <- glmer(Duration_Rate ~ Strategy * ns(Week, df = 3) 
                      + (1 | Habitat),
                      family = Gamma(link = "log"),
                      data = subset(alert_stage_behavior, Behavior == "Alert"),
                      control = glmerControl(optimizer = "bobyqa", 
                                             optCtrl = list(maxfun = 2e5)))

alert_model <- model.sel(glmerfull, glmer_red1, glmer_red2, glmer_red3, glmer_pol1, glmer_polint1, glmer_polint2, glm1, glm2, glm3, glm4, glm5, glmer_spline)
alert_model

summary(glmer_polint2)

alert_model_df <- as.data.frame(alert_model)
alert_model_df$model <- rownames(alert_model_df)
alert_model_df <- alert_model_df[, c("model", setdiff(names(alert_model_df), "model"))]
head(alert_model_df)
# Save to CSV

write.csv(alert_model_df, "alert_model_selection_table.csv", row.names = FALSE)

alert_stage_behavior$Strategy <- factor(alert_stage_behavior$Strategy)
alert_stage_behavior$Three_letter_code <- factor(alert_stage_behavior$Three_letter_code)
alert_stage_behavior$Habitat <- factor(alert_stage_behavior$Habitat)

strategies <- levels(alert_stage_behavior$Strategy)
three_letter <- levels(alert_stage_behavior$Three_letter_code)
habitat <- levels(alert_stage_behavior$Habitat)
weeks_seq <- seq(min(alert_stage_behavior$Week), max(alert_stage_behavior$Week), length.out = 500)

new_alert_smooth <- expand.grid(
  Week = 12:21,
  Strategy = strategies,
  Three_letter_code = three_letter,
  Habitat = habitat,
  stringsAsFactors = FALSE
)

# Convert them back to factors using the original levels
new_alert_smooth$Strategy <- factor(new_alert_smooth$Strategy, levels = strategies)
new_alert_smooth$Three_letter_code <- factor(new_alert_smooth$Three_letter_code, levels = three_letter)
new_alert_smooth$Habitat <- factor(new_alert_smooth$Habitat, levels = habitat)

new_alert_smooth$predicted <- predict(glmer_polint2,
                                        newdata = new_alert_smooth,
                                        re.form = NA,
                                        type = "response")
new_alert_smooth <- new_alert_smooth |>
  filter(!Week %in% c("12", "13", "14", "15")) |>
  filter(!Habitat == "Water")

p11c <- p11b +
  geom_line(
    data = new_alert_smooth,
    aes(x = as.factor(Week), y = predicted, color = Strategy, 
        group = Strategy),
    size = 1.0, inherit.aes = FALSE
  )  +
  
  # Manual colors
  scale_color_manual(values = c(
    "overwinterer" = "#3487a8",
    "late_northward_migration" = "#2d8062",
    "early_northward_migration" = "#904a96"
  )) +
  scale_fill_manual(values = c(
    "overwinterer" = "#4DC8F9",
    "late_northward_migration" = "#4DD2A4",
    "early_northward_migration" = "#E777F2"
  )) +
  labs(
    y = "Duration Rate",
    x = "Week",
    title = "Alert Rate per Strategy"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    legend.position = "none"
  ) +
  facet_wrap(~ Habitat, ncol = 2)
p11c

# Pairwise comparison
emm_alert <- emm <- emmeans(glmer_polint2, ~ Strategy | Week,
                              at = list(Week = c(16, 17, 18, 19, 20, 21)),
                              type = "response")
# Pairwise comparisons of Strategy within each Week × Habitat group
pairwise_results_alert <- pairs(emm_alert, adjust = "tukey", type = "response")
pairwise_summary_alert <- summary(pairwise_results_alert)
# Print summary of comparisons
print(pairwise_summary_alert)
# Extract p-values and contrast names
alert_pvals <- pairwise_summary_alert$p.value
names(alert_pvals) <- gsub(" / ", " - ", pairwise_summary_alert$contrast)
# Remove duplicates (if any)
alert_pvals_unique <- alert_pvals[!duplicated(names(alert_pvals))]
library(multcompView)
# Generate compact letter display for grouping
alert_group_letters <- multcompLetters(alert_pvals_unique)$Letters
library(stringr)  # for str_trim()
# Trim spaces from names
alert_clean_names <- str_trim(names(alert_group_letters))
# Remove duplicates: keep first occurrence only
alert_unique_indices <- !duplicated(alert_clean_names)
alert_clean_names_unique <-alert_clean_names[alert_unique_indices]
alert_group_letters_unique <- alert_group_letters[alert_unique_indices]
# Rename with cleaned names
names(alert_group_letters_unique) <- alert_clean_names_unique
# Define factor level order (optional)
levels_strat <- c("early_northward_migration", "late_northward_migration", "overwinterer")
# Match levels to cleaned names
alert_group_letters_ordered <- alert_group_letters_unique[match(levels_strat, alert_clean_names_unique)]
print(alert_group_letters_ordered)

ggsave("alert_rate_per_strategy.png", plot = p11c, width = 28, height = 10, dpi = 300)

### sample size digging to small to analyse
p12b <- p12a + geom_text(data = counts_stage |> filter(Behavior == "Digging"), aes(x = as.factor(Week), y = 1.2,                                             label = paste0("n=", n)),
                         position = position_dodge(width = 0.8), size = 2)

p12b
glmer9 <- glmer(Behavior_Rate ~ Week * Strategy + (1 | Three_letter_code), 
                family = Gamma(link = "log"),
                data = subset(stage_behavior, Behavior == "Digging"),
                control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
summary(glmer9)

p12c <- p12b +
  geom_text(data = data.frame(
    Week = factor(18),
    label = "**",
    y = 0.7
  ),
  aes(x = Week, y = y, label = label),
  vjust = -0.5,
  size = 6,
  inherit.aes = FALSE)  # <--- This is the key!
p12c

p13b <- p13a + geom_text(data = counts_stage |> filter(Behavior == "Routing"), aes(x = as.factor(Week), y = 1.2,                                             label = paste0("n=", n)),
                         position = position_dodge(width = 0.8), size = 2)

p13b

p14a <- ggplot(stage_behavior |> filter(Behavior == "Handling_prey"),
               aes(x = as.factor(Week), y = Duration_Rate, 
                   fill = Strategy)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#E777F2", "#4DD2A4", "#4DC8F9")) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Handling prey by Strategy",
    x = "Week",
    y = "Duration Rate")
p14a

p14b <- p14a + geom_text(data = counts_stage |> filter(Behavior == "Handling_prey"), aes(x = as.factor(Week), y = 0.5,                                             label = paste0("", n)),
                         position = position_dodge(width = 0.8), size = 4)

p14b

glm1 <- glm(Duration_Rate ~ 1, 
            family = Gamma(link = "log"),
            data = subset(stage_behavior, Behavior == "Handling_prey"))

glm2 <- glm(Duration_Rate ~ Week,
            family = Gamma(link = "log"),
            data = subset(stage_behavior, Behavior == "Handling_prey"))

glm3 <- glm(Duration_Rate ~ Strategy,
            family = Gamma(link = "log"),
            data = subset(stage_behavior, Behavior == "Handling_prey"))

glm4 <- glm(Duration_Rate ~ Habitat,
            family = Gamma(link = "log"),
            data = subset(stage_behavior, Behavior == "Handling_prey"))

glm5 <- glm(Duration_Rate ~ Tide,
            family = Gamma(link = "log"),
            data = subset(stage_behavior, Behavior == "Handling_prey"))

model.sel(glm1, glm2, glm3, glm4, glm5)

glmerfull <- glmer(Duration_Rate ~ Week + Strategy + (1|Three_letter_code) +
                     (1|Habitat) + (1|Tide),
                   family = Gamma(link = "log"),
                   data = subset(stage_behavior, Behavior == "Handling_prey"),
                   control = glmerControl(optimizer = "bobyqa", 
                                          optCtrl = list(maxfun = 2e5)))

glmer_red1 <- glmer(Duration_Rate ~ Week + Strategy + (1|Habitat) + (1|Tide),
                    family = Gamma(link = "log"),
                  data = subset(stage_behavior, Behavior == "Handling_prey"),
                    control = glmerControl(optimizer = "bobyqa", 
                                           optCtrl = list(maxfun = 2e5)))

glmer_pol1 <- glmer(Duration_Rate ~ poly(Week, 2) + Strategy + (1 |Tide), 
                    family = Gamma(link = "log"),
                   data = subset(stage_behavior, Behavior == "Handling_prey"),
                    control = glmerControl(optimizer = "bobyqa", 
                                           optCtrl = list(maxfun = 2e5)))

glmer_polint1 <- glmer(Duration_Rate ~ poly(Week, 2) * Strategy +
                         (1 | Tide), 
                       family = Gamma(link = "log"),
                 data = subset(stage_behavior, Behavior == "Handling_prey"),
                       control = glmerControl(optimizer = "bobyqa", 
                                              optCtrl = list(maxfun = 2e5)))

handling_model <- model.sel(glmerfull, glmer_red1, glmer_pol1, glmer_polint1, glm1, glm2, glm3, glm4, glm5)
handling_model

handling_model_df <- as.data.frame(handling_model)
handling_model_df$model <- rownames(handling_model_df)
handling_model_df <- handling_model_df[, c("model", setdiff(names(handling_model_df), "model"))]
head(handling_model_df)
# Save to CSV

write.csv(handling_model_df, "handling_model_selection_table.csv", row.names = FALSE)

stage_behavior$Strategy <- factor(stage_behavior$Strategy)
stage_behavior$Three_letter_code <- factor(stage_behavior$Three_letter_code)
stage_behavior$Tide <- factor(stage_behavior$Tide)

strategies <- levels(stage_behavior$Strategy)
three_letter <- levels(stage_behavior$Three_letter_code)
tide <- levels(stage_behavior$Tide)
weeks_seq <- seq(min(stage_behavior$Week), max(stage_behavior$Week), length.out = 500)

new_handling_smooth <- expand.grid(
  Week = 12:21,
  Strategy = strategies,
  Three_letter_code = three_letter,
  Tide = tide,
  stringsAsFactors = FALSE
)

# Convert them back to factors using the original levels
new_handling_smooth$Strategy <- factor(new_handling_smooth$Strategy, levels = strategies)
new_handling_smooth$Three_letter_code <- factor(new_handling_smooth$Three_letter_code, levels = three_letter)
new_handling_smooth$Tide <- factor(new_handling_smooth$Tide, levels = tide)

new_handling_smooth$predicted <- predict(glmer_polint1,
                                      newdata = new_handling_smooth,
                                      re.form = NA,
                                      type = "response")
new_handling_smooth <- new_handling_smooth |>
  filter(!Week %in% c("12", "13", "14", "15"))

p14c <- p14b +
  geom_line(
    data = new_handling_smooth,
    aes(x = as.factor(Week), y = predicted, color = Strategy, 
        group = Strategy),
    size = 1.0, inherit.aes = FALSE
  )  +
  
  # Manual colors
  scale_color_manual(values = c(
    "overwinterer" = "#3487a8",
    "late_northward_migration" = "#2d8062",
    "early_northward_migration" = "#904a96"
  )) +
  scale_fill_manual(values = c(
    "overwinterer" = "#4DC8F9",
    "late_northward_migration" = "#4DD2A4",
    "early_northward_migration" = "#E777F2"
  )) +
  labs(
    y = "Duration Rate",
    x = "Week",
    title = "Handling Prey Rate per Strategy"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    legend.position = "none"
  ) 
p14c

# Pairwise comparison
emm_handling <- emm <- emmeans(glmer_polint1, ~ Strategy | Week,
                            at = list(Week = c(16, 17, 18, 19, 20, 21)),
                            type = "response")
# Pairwise comparisons of Strategy within each Week × Habitat group
pairwise_results_handling <- pairs(emm_handling, adjust = "tukey", type = "response")
pairwise_summary_handling <- summary(pairwise_results_handling)
# Print summary of comparisons
print(pairwise_summary_handling)
# Extract p-values and contrast names
handling_pvals <- pairwise_summary_handling$p.value
names(handling_pvals) <- gsub(" / ", " - ", pairwise_summary_handling$contrast)
# Remove duplicates (if any)
handling_pvals_unique <- handling_pvals[!duplicated(names(handling_pvals))]
library(multcompView)
# Generate compact letter display for grouping
handling_group_letters <- multcompLetters(handling_pvals_unique)$Letters
library(stringr)  # for str_trim()
# Trim spaces from names
handling_clean_names <- str_trim(names(handling_group_letters))
# Remove duplicates: keep first occurrence only
handling_unique_indices <- !duplicated(handling_clean_names)
handling_clean_names_unique <-handling_clean_names[handling_unique_indices]
handling_group_letters_unique <- handling_group_letters[handling_unique_indices]
# Rename with cleaned names
names(handling_group_letters_unique) <- handling_clean_names_unique
# Define factor level order (optional)
levels_strat <- c("early_northward_migration", "late_northward_migration", "overwinterer")
# Match levels to cleaned names
handling_group_letters_ordered <- handling_group_letters_unique[match(levels_strat, handling_clean_names_unique)]
print(handling_group_letters_ordered)

ggsave("handling_rate_per_strategy.png", plot = p14c, width = 28, height = 10, dpi = 300)


p_stage_behavior <- p10b + p11b + p12b + p13b + p14b
p_stage_behavior
ggsave("stage_behaviors_strategy.png", plot = p_stage_behavior, width = 18, height = 10, dpi = 300)

p_stage_filtered <- p10c + p11c + p12c + p14c
p_stage_filtered
ggsave("stage_behaviors_strategy_filtered.png", plot = p_stage_filtered, width = 25, height = 10, dpi = 300)

#emmeans_results2 <- emmeans(aov(Behavior_Rate ~ Behavior * Strategy, data = #stage_behavior), 
#                           pairwise ~ Strategy | Behavior)
#print(emmeans_results2)
# Plotting the results