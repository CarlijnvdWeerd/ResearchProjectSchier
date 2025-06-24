## Run the script 07_BORIS_forage first, then this script will work
renv::restore()

# Check for normality of point behavior
ggplot(point_behaviors, aes(x = Behavior_Rate)) +
  geom_histogram(bins = 30, fill = "#69b3a2", color = "black") +
  labs(
    title = "Distribution of Visually Foraging Durations",
    x = "Visually Foraging Duration (seconds)",
    y = "Count"
  ) +
  theme_minimal()

qqnorm(point_behaviors$Behavior_Rate)

point_behaviors <- point_behaviors |>
  filter(!Week %in% c("12", "13"))

p5a <- ggplot(point_behaviors |> filter(Behavior == "Probing"),
              aes(x = as.factor(Week), y = Behavior_Rate, 
                  fill = Strategy)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#E777F2", "#4DD2A4", "#4DC8F9")) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Probing by Strategy",
    x = "Week",
    y = "Duration Rate")
p5a

counts_point <- counts_point |>
  filter(!Week %in% c("12", "13"))

p5b <- p5a + geom_text(data = counts_point |> filter(Behavior == "Probing"), aes(x = as.factor(Week), 
                                                                                 y = 1.6, 
                                                                                 label = paste0("", n)),
                       position = position_dodge(width = 0.8), size = 3)
p5b

point_behaviors$Tide <- point_behaviors$Tide |>
  str_trim()
point_behaviors$Habitat <- point_behaviors$Habitat |>
  str_trim()


glm1 <- glm(Behavior_Rate ~ 1, 
            family = Gamma(link = "log"),
            data = subset(point_behaviors, Behavior == "Probing"))
summary(glm1)

glm2 <- glm(Behavior_Rate ~ Week, 
            family = Gamma(link = "log"),
            data = subset(point_behaviors, Behavior == "Probing"))
summary(glm2)

glm3 <- glm(Behavior_Rate ~ Strategy, 
            family = Gamma(link = "log"),
            data = subset(point_behaviors, Behavior == "Probing"))
summary(glm3)

glm4 <- glm(Behavior_Rate ~ Tide, 
            family = Gamma(link = "log"),
            data = subset(point_behaviors, Behavior == "Probing"))
summary(glm4)

glm5 <- glm(Behavior_Rate ~   Habitat, 
            family = Gamma(link = "log"),
            data = subset(point_behaviors, Behavior == "Probing"))
summary(glm5)

glm6 <- glm(Behavior_Rate ~ Transect_ID, 
            family = Gamma(link = "log"),
            data = subset(point_behaviors, Behavior == "Probing"))
summary(glm6)

model.sel(glm1, glm2, glm3, glm4, glm5, glm6)

glmfull <- glm(Behavior_Rate ~ Week + Strategy + Tide + Habitat 
               + Transect_ID, 
               family = Gamma(link = "log"),
               data = subset(point_behaviors, Behavior == "Probing"))
summary(glmfull)

glmreduced1 <- glm(Behavior_Rate ~ Week + Strategy + Tide + Habitat,
                   family = Gamma(link = "log"),
                   data = subset(point_behaviors, Behavior == "Probing"))
summary(glmreduced1)

glmreduced2 <- glm(Behavior_Rate ~ Week + Strategy + Tide,
                   family = Gamma(link = "log"),
                   data = subset(point_behaviors, Behavior == "Probing"))
summary(glmreduced2)

glmreduced3 <- glm(Behavior_Rate ~ Week + Tide, 
                   family = Gamma(link = "log"),
                   data = subset(point_behaviors, Behavior == "Probing"))
summary(glmreduced3)

glm_int1 <- glm(Behavior_Rate ~ Week * Strategy + Tide, 
               family = Gamma(link = "log"),
               data = subset(point_behaviors, Behavior == "Probing"))
summary(glm_int1)

glm_int2 <- glm(Behavior_Rate ~ Week * Strategy * Tide, 
               family = Gamma(link = "log"),
               data = subset(point_behaviors, Behavior == "Probing"))
summary(glm_int2)

glm_int3 <- glm(Behavior_Rate ~ Week * Tide + Strategy,
                family = Gamma(link = "log"),
                data = subset(point_behaviors, Behavior == "Probing"))
summary(glm_int3)

model.sel(glmfull, glmreduced1, glmreduced2, glmreduced3, glm_int1, glm_int2, glm_int3, glm1, glm2, glm3, glm4, glm5, glm6)

glmerfull <- glmer(Behavior_Rate ~ Week + Strategy + (1 | Three_letter_code)
                   + (1 | Transect_ID) + (1 | Tide) + (1 | Habitat), 
                   family = Gamma(link = "log"),
                   data = subset(point_behaviors, Behavior == "Probing"),
                   control = glmerControl(optimizer = "bobyqa", 
                                          optCtrl = list(maxfun = 2e5)))
summary(glmerfull)

glmerreduced1 <- glmer(Behavior_Rate ~ Week + Strategy 
                       + (1 | Three_letter_code) + (1 | Transect_ID)
                       + (1 | Tide), 
                       family = Gamma(link = "log"),
                       data = subset(point_behaviors, Behavior == "Probing"),
                       control = glmerControl(optimizer = "bobyqa", 
                                              optCtrl = list(maxfun = 2e5)))
summary(glmerreduced1)

glmerreduced2 <- glmer(Behavior_Rate ~ Week + Strategy 
                       + (1 | Three_letter_code) + (1 | Transect_ID), 
                       family = Gamma(link = "log"),
                       data = subset(point_behaviors, Behavior == "Probing"),
                       control = glmerControl(optimizer = "bobyqa", 
                                              optCtrl = list(maxfun = 2e5)))
summary(glmerreduced2)

glmer_int1 <- glmer(Behavior_Rate ~ Week * Strategy + (1|Three_letter_code) 
                    + (1|Tide) + (1|Transect_ID), 
                   family = Gamma(link = "log"),
                   data = subset(point_behaviors, Behavior == "Probing"),
                   control = glmerControl(optimizer = "bobyqa", 
                                          optCtrl = list(maxfun = 2e5)))
summary(glmer_int1)

glmer_int2 <- glmer(Behavior_Rate ~ Week * Strategy + (1|Three_letter_code)
                    + (1|Transect_ID),
                    family = Gamma(link="log"),
                    data = subset(point_behaviors, Behavior == "Probing"),
                    control = glmerControl(optimizer = "bobyqa", 
                                          optCtrl = list(maxfun = 2e5)))
summary(glmer_int2)

library(splines)
glmer_spline <- glmer(Behavior_Rate ~ ns(Week, df = 4) + Strategy + 
                        (1 | Three_letter_code) + (1 | Transect_ID) + (1 | Tide),
                      family = Gamma(link = "log"),
                      data = subset(point_behaviors, Behavior == "Probing"),
                      control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))

glmer_poly2 <- glmer(
  Behavior_Rate ~ poly(Week, 2) + Strategy +
    (1 | Three_letter_code) + (1 | Transect_ID) + (1 | Tide),
  family = Gamma(link = "log"),
  data = subset(point_behaviors, Behavior == "Probing"),
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))

glmer_polyint <- glmer(Behavior_Rate ~ poly(Week, 2) * Strategy + 
                       (1 | Three_letter_code) + (1 | Transect_ID) 
                       + (1 | Tide),
                       family = Gamma(link = "log"),
                       data = subset(point_behaviors, Behavior == "Probing"),
                       control = glmerControl(optimizer = "bobyqa", 
                                              optCtrl = list(maxfun = 2e5)))


probing_model <- model.sel(glmfull, glmreduced1, glmreduced2, glmreduced3, glm_int1, glm_int2, glm_int3, glm1, glm2, glm3, glm4, glm5, glm6, glmerfull, glmerreduced1, glmerreduced2, glmer_int1, glmer_int2, glmer_spline, glmer_poly2, glmer_polyint)
probing_model
## Choose to use the model glmer_poly because it accounts for the mean difference in week (there is a dip in week 18) and it accounts for the random effect of three letter code. And the delta is underneath 2 and it still has weight of 0.116 which is i think quite okay because the best model in terms of AIC has a weight of 0.233

plot(residuals(glmer_poly2))
## Which shows a not really a pattern which is good!


probing_model_df <- as.data.frame(probing_model)
probing_model_df$model <- rownames(probing_model_df)
probing_model_df <- probing_model_df[, c("model", setdiff(names(probing_model_df), "model"))]
head(probing_model_df)

# Save to CSV
write.csv(probing_model_df, "probing_model_selection_table.csv", row.names = FALSE)

point_behaviors$Strategy <- factor(point_behaviors$Strategy)
point_behaviors$Three_letter_code <- factor(point_behaviors$Three_letter_code)
point_behaviors$Transect_ID <- factor(point_behaviors$Transect_ID)
point_behaviors$Tide <- factor(point_behaviors$Tide)

strategies <- levels(point_behaviors$Strategy)
three_letter <- levels(point_behaviors$Three_letter_code)
transect_ids <- levels(point_behaviors$Transect_ID)
tide <- levels(point_behaviors$Tide)

weeks_seq <- seq(min(point_behaviors$Week), max(point_behaviors$Week), length.out = 500)

new_probing_smooth <- expand.grid(
  Week = 12:21,
  Strategy = strategies,
  Three_letter_code = three_letter,
  Transect_ID = transect_ids,
  Tide = tide,
  stringsAsFactors = FALSE
)

# Convert them back to factors using the original levels
new_probing_smooth$Strategy <- factor(new_probing_smooth$Strategy, levels = strategies)
new_probing_smooth$Three_letter_code <- factor(new_probing_smooth$Three_letter_code, levels = three_letter)
new_probing_smooth$Transect_ID <- factor(new_probing_smooth$Transect_ID, levels = transect_ids)
new_probing_smooth$Tide <- factor(new_probing_smooth$Tide, levels = tide)

new_probing_smooth$predicted <- predict(glmer_poly2,
                                  newdata = new_probing_smooth,
                                  re.form = NA,
                                  type = "response")
new_probing_smooth <- new_probing_smooth |>
   filter(!Week %in% c("12", "13", "14", "15"))

p5c <- p5b +
  geom_line(
    data = new_probing_smooth,
    aes(x = as.factor(Week), y = predicted, color = Strategy, 
        group = Strategy),
    size = 1.0, inherit.aes = FALSE
  ) +
  
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
    y = "Behaviour Rate",
    x = "Week",
    title = "Probing Rate per Strategy"
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

p5c

# Pairwise comparison
emm_probing <- emm <- emmeans(glmer_poly2, ~ Strategy | Week, type = "response")

# Pairwise comparisons of Strategy within each Week × Habitat group
pairwise_results_probing <- pairs(emm_probing, adjust = "tukey", type = "response")
pairwise_summary_probing <- summary(pairwise_results_probing)

# Print summary of comparisons
print(pairwise_summary_probing)

# Extract p-values and contrast names
pvals <- pairwise_summary_probing$p.value
names(pvals) <- gsub(" / ", " - ", pairwise_summary_probing$contrast)

# Remove duplicates (if any)
pvals_unique <- pvals[!duplicated(names(pvals))]

library(multcompView)

# Generate compact letter display for grouping
group_letters <- multcompLetters(pvals_unique)$Letters

library(stringr)  # for str_trim()

# Trim spaces from names
clean_names <- str_trim(names(group_letters))

# Remove duplicates: keep first occurrence only
unique_indices <- !duplicated(clean_names)
clean_names_unique <- clean_names[unique_indices]
group_letters_unique <- group_letters[unique_indices]

# Rename with cleaned names
names(group_letters_unique) <- clean_names_unique

# Define factor level order (optional)
levels_strat <- c("early_northward_migration", "late_northward_migration", "overwinterer")

# Match levels to cleaned names
group_letters_ordered <- group_letters_unique[match(levels_strat, clean_names_unique)]

print(group_letters_ordered)

ggsave("probing_rate_per_strategy.png", plot = p5c, width = 28, height = 10, dpi = 300)

p6a <- ggplot(point_behaviors |> filter(Behavior == "Surface_pecking"),
              aes(x = as.factor(Week), y = Behavior_Rate, 
                  fill = Strategy)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#E777F2", "#4DD2A4", "#4DC8F9")) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Surface pecking by Strategy",
    x = "Week",
    y = "Duration Rate") +
  theme(
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    legend.position = "none")
p6a

p6b <- p6a + geom_text(data = counts_point |> filter(Behavior == "Surface_pecking"), aes(x = as.factor(Week), 
                                                                                         y = 0.9, 
                                                                                         label = paste0("", n)),
                       position = position_dodge(width = 0.8), size = 3)

p6b

glm1 <- glm(Behavior_Rate ~ 1, 
            family = Gamma(link = "log"),
            data = subset(point_behaviors, Behavior == "Surface_pecking"))

glm2 <- glm(Behavior_Rate ~ Week, 
            family = Gamma(link = "log"),
            data = subset(point_behaviors, Behavior == "Surface_pecking"))

glm3 <- glm(Behavior_Rate ~ Strategy,
            family = Gamma(link = "log"),
            data = subset(point_behaviors, Behavior == "Surface_pecking"))

glm4 <- glm(Behavior_Rate ~ Tide,
            family = Gamma(link = "log"),
            data = subset(point_behaviors, Behavior == "Surface_pecking"))

glm5 <- glm(Behavior_Rate ~ Habitat,
            family = Gamma(link = "log"),
            data = subset(point_behaviors, Behavior == "Surface_pecking"))

glm6 <- glm(Behavior_Rate ~ Transect_ID,
            family = Gamma(link = "log"),
            data = subset(point_behaviors, Behavior == "Surface_pecking"))

model.sel(glm1, glm2, glm3, glm4, glm5, glm6)

glmfull <- glm(Behavior_Rate ~ Week + Strategy + Tide + Habitat 
               + Transect_ID, 
               family = Gamma(link = "log"),
               data = subset(point_behaviors, Behavior == "Surface_pecking"))
summary(glmfull)

glm_int1 <- glm(Behavior_Rate ~ Week * Strategy + Tide + Habitat 
                + Transect_ID, 
            family = Gamma(link = "log"),
            data = subset(point_behaviors, Behavior == "Surface_pecking"))
summary(glm_int1)

glmer_full <- glmer(Behavior_Rate ~ Week + Strategy + (1 | Three_letter_code) 
                + (1 | Transect_ID) + (1 | Tide) + (1 | Habitat), 
                family = Gamma(link = "log"),
                data = subset(point_behaviors, Behavior == "Surface_pecking"),
                control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
summary(glmer_full)

glmer_reduced1 <- glmer(Behavior_Rate ~ Week + Strategy + 
                  (1 | Three_letter_code) + (1 | Habitat), 
                family = Gamma(link = "log"),
                data = subset(point_behaviors, Behavior == "Surface_pecking"),
                control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
summary(glmer_reduced1)

glmer_reduced2 <- glmer(Behavior_Rate ~ Week + Strategy + Transect_ID 
                        + Habitat + Tide + (1|Three_letter_code), 
                family = Gamma(link = "log"), 
                data = subset(point_behaviors, Behavior == "Surface_pecking"),
                        control = glmerControl(optimizer = "bobyqa",
                                               optCtrl = list(maxfun = 2e5)))

glmer_reduced3 <- glmer(Behavior_Rate ~ Week + Strategy 
                    + Habitat + (1|Three_letter_code),
                    family = Gamma(link = "log"),
                data = subset(point_behaviors, Behavior == "Surface_pecking"),
                        control = glmerControl(optimizer = "bobyqa",
                                               optCtrl = list(maxfun = 2e5)))

glmer_reduced4 <- glmer(Behavior_Rate ~ Week + Strategy 
                        + (1|Three_letter_code), 
                family = Gamma(link = "log"), 
                data = subset(point_behaviors, Behavior == "Surface_pecking"),
                        control = glmerControl(optimizer = "bobyqa",
                                               optCtrl = list(maxfun = 2e5)))

glmer_reduced5 <- glmer(Behavior_Rate ~ Week + Strategy + Tide + Habitat
                        + (1 | Three_letter_code),
                        family = Gamma(link = "log"),
                data = subset(point_behaviors, Behavior == "Surface_pecking"),
                        control = glmerControl(optimizer = "bobyqa", 
                                               optCtrl = list(maxfun = 2e5)))

glmer_reduced6 <- glmer(Behavior_Rate ~ Week + Strategy + Tide + (1|Habitat)
                        + (1|Three_letter_code),
                        family = Gamma(link = "log"),
               data = subset(point_behaviors, Behavior == "Surface_pecking"),
                        control = glmerControl(optimizer = "bobyqa", 
                                               optCtrl = list(maxfun = 2e5)))

glmer_int1 <- glmer(Behavior_Rate ~ Week * Strategy + Transect_ID 
                        + Habitat + Tide + (1|Three_letter_code), 
                        family = Gamma(link = "log"), 
                data = subset(point_behaviors, Behavior == "Surface_pecking"),
                        control = glmerControl(optimizer = "bobyqa",
                                               optCtrl = list(maxfun = 2e5)))

glmer_poly1 <- glmer(Behavior_Rate ~ poly(Week, 2) + Strategy + Transect_ID 
                     + Tide + Habitat + (1|Three_letter_code),
                     family = Gamma(link = "log"),
                data = subset(point_behaviors, Behavior == "Surface_pecking"),
                control = glmerControl(optimizer = "bobyqa", 
                                       optCtrl = list(maxfun = 2e5)))
summary(glmer_poly1)

glmer_polyint <- glmer(
  Behavior_Rate ~ poly(Week, 2) * Strategy + Tide + Habitat + (1|Three_letter_code),
  family = Gamma(link = "log"),
  data = subset(point_behaviors, Behavior == "Surface_pecking"),
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)


glmer_spline <- glmer(Behavior_Rate ~ ns(Week, df = 4) + Strategy
                      + Transect_ID 
                     + Tide + Habitat + (1|Three_letter_code),
                     family = Gamma(link = "log"),
                data = subset(point_behaviors, Behavior == "Surface_pecking"),
                control = glmerControl(optimizer = "bobyqa", 
                                       optCtrl = list(maxfun = 2e5)))

glmer_int2 <- glmer(Behavior_Rate ~ Week + Strategy * Tide + Habitat + 
                      (1|Three_letter_code),
                    family = Gamma(link = "log"),
                data = subset(point_behaviors, Behavior == "Surface_pecking"),
                    control = glmerControl(optimizer = "bobyqa", 
                                               optCtrl = list(maxfun = 2e5)))

glmer_int3 <- glmer(Behavior_Rate ~ Week * Tide + Strategy + Habitat + 
                    (1|Three_letter_code),
                    family = Gamma(link = "log"),
                data = subset(point_behaviors, Behavior == "Surface_pecking"),
                    control = glmerControl(optimizer = "bobyqa", 
                                               optCtrl = list(maxfun = 2e5)))

glmer_week_factor <- glmer(
  Behavior_Rate ~ as.factor(Week) + Strategy + Tide + Habitat +
    (1 | Three_letter_code),
  family = Gamma(link = "log"),
  data = subset(point_behaviors, Behavior == "Surface_pecking"),
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

glmer_strategy_factor <- glmer(
  Behavior_Rate ~ as.factor(Strategy) + Week + Tide + Habitat +
    (1 | Three_letter_code),
  family = Gamma(link = "log"),
  data = subset(point_behaviors, Behavior == "Surface_pecking"),
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

glmer_week_factor_int <- glmer(
  Behavior_Rate ~ as.factor(Week) * Strategy + Tide + Habitat +
    (1 | Three_letter_code),
  family = Gamma(link = "log"),
  data = subset(point_behaviors, Behavior == "Surface_pecking"),
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

glmer_week_random <- glmer(
  Behavior_Rate ~ as.factor(Week) + Strategy + (Week | Strategy) 
  + Tide + Habitat +
    (1 | Three_letter_code),
  family = Gamma(link = "log"),
  data = subset(point_behaviors, Behavior == "Surface_pecking"),
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)


pecking_model <- model.sel(glmfull, glm_int1, glmer_full, glmer_reduced1, glm1, glm2, glm3, glm4, glm5, glm6, glmer_reduced2, glmer_int1, glmer_reduced3, glmer_reduced4, glmer_poly1, glmer_spline, glmer_reduced5, glmer_reduced6, glmer_int2, glmer_int3, glmer_week_factor, glmer_week_factor_int, glmer_strategy_factor, glmer_polyint, glmer_week_random)
pecking_model

plot(residuals(glmer_week_factor))
## Which shows a not really a pattern which is good!


pecking_model_df <- as.data.frame(pecking_model)
pecking_model_df$model <- rownames(pecking_model_df)
pecking_model_df <- pecking_model_df[, c("model", setdiff(names(pecking_model_df), "model"))]
head(pecking_model_df)

# Save to CSV
write.csv(pecking_model_df, "pecking_model_selection_table.csv", row.names = FALSE)

point_behaviors$Strategy <- factor(point_behaviors$Strategy)
point_behaviors$Three_letter_code <- factor(point_behaviors$Three_letter_code)
point_behaviors$Habitat <- factor(point_behaviors$Habitat)
point_behaviors$Tide <- factor(point_behaviors$Tide)

strategies <- levels(point_behaviors$Strategy)
three_letter <- levels(point_behaviors$Three_letter_code)
habitat <- levels(point_behaviors$Habitat)
tide <- unique(point_behaviors$Tide)

weeks_seq <- seq(min(point_behaviors$Week), max(point_behaviors$Week), length.out = 500)

new_pecking_smooth <- expand.grid(
  Week = 12:21,
  Strategy = strategies,
  Three_letter_code = three_letter,
  Habitat = habitat,
  Tide = tide,
  stringsAsFactors = FALSE
)

# Convert them back to factors using the original levels
new_pecking_smooth$Strategy <- factor(new_pecking_smooth$Strategy, levels = strategies)
new_pecking_smooth$Three_letter_code <- factor(new_pecking_smooth$Three_letter_code, levels = three_letter)
new_pecking_smooth$Habitat <- factor(new_pecking_smooth$Habitat, levels = habitat)
new_pecking_smooth$Tide <- factor(new_pecking_smooth$Tide, levels = tide)

new_pecking_smooth$predicted <- predict(glmer_week_factor,
                                        newdata = new_pecking_smooth,
                                        re.form = NA,
                                        type = "response")
new_pecking_smooth <- new_pecking_smooth |>
  filter(!Week %in% c("12", "13", "14", "15"))

new_pecking_smooth_avg <- new_pecking_smooth %>%
  group_by(Week, Strategy) %>%
  summarise(predicted = mean(predicted), .groups = "drop")

p6c <- p6b +
  geom_line(
    data = new_pecking_smooth_avg,
    aes(x = as.factor(Week), y = predicted, color = Strategy, 
        group = Strategy),
    size = 1.0, inherit.aes = FALSE
  ) +
  
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
    y = "Behaviour Rate",
    x = "Week",
    title = "Surface Pecking Rate per Strategy"
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
  facet_wrap( ~ Tide)

p6c

# Pairwise comparison
emm_pecking <- emm <- emmeans(glmer_week_factor, ~ Strategy | Week, type = "response")

# Pairwise comparisons of Strategy within each Week × Habitat group
pairwise_results_pecking <- pairs(emm_pecking, adjust = "tukey", type = "response")
pairwise_summary_pecking <- summary(pairwise_results_pecking)

# Print summary of comparisons
print(pairwise_summary_pecking)

# Extract p-values and contrast names
pecking_pvals <- pairwise_summary_pecking$p.value
names(pecking_pvals) <- gsub(" / ", " - ", pairwise_summary_pecking$contrast)

# Remove duplicates (if any)
pecking_pvals_unique <- pecking_pvals[!duplicated(names(pecking_pvals))]

library(multcompView)

# Generate compact letter display for grouping
pecking_group_letters <- multcompLetters(pecking_pvals_unique)$Letters

library(stringr)  # for str_trim()

# Trim spaces from names
pecking_clean_names <- str_trim(names(pecking_group_letters))

# Remove duplicates: keep first occurrence only
pecking_unique_indices <- !duplicated(pecking_clean_names)
pecking_clean_names_unique <- pecking_clean_names[unique_indices]
pecking_group_letters_unique <- pecking_group_letters[unique_indices]

# Rename with cleaned names
names(pecking_group_letters_unique) <- pecking_clean_names_unique

# Define factor level order (optional)
levels_strat <- c("early_northward_migration", "late_northward_migration", "overwinterer")

# Match levels to cleaned names
pecking_group_letters_ordered <- group_letters_unique[match(levels_strat, pecking_clean_names_unique)]

print(pecking_group_letters_ordered)

ggsave("pecking_rate_per_strategy.png", plot = p6c, width = 28, height = 10, dpi = 300)

p7a <- ggplot(point_behaviors |> filter(Behavior == "Turning_stuff"),
              aes(x = as.factor(Week), y = Behavior_Rate, 
                  fill = Strategy)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#E777F2", "#4DD2A4", "#4DC8F9")) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Turning by Strategy",
    x = "Week",
    y = "Duration Rate")
p7a

p7b <- p7a + geom_text(data = counts_point |> filter(Behavior == "Turning_stuff"), aes(x = as.factor(Week), y = 0.1,                                             label = paste0("", n)),
                       position = position_dodge(width = 0.8), size = 4)

p7b

### Sample size is very low, probably not worth it to do a model selection

p_point_behavior <- p5c + p6c + p7b
p_point_behavior

ggsave("rate_forage_stratgy.png", plot = p_point_behavior, width = 28, height = 10, dpi = 300)

###########################################

p8a <- ggplot(point_behaviors |> filter(Behavior == "Swallowing"),
              aes(x = as.factor(Week), y = Behavior_Rate, 
                  fill = Strategy)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#E777F2", "#4DD2A4", "#4DC8F9")) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Swallowing by Strategy",
    x = "Week",
    y = "Duration Rate")
p8a

p8b <- p8a + geom_text(data = counts_point |> filter(Behavior == "Swallowing"), aes(x = as.factor(Week), y = 0.6,                                             label = paste0("", n)),
                       position = position_dodge(width = 0.8), size = 4)

p8b

library(dplyr)

success_swallowing <- point_behaviors %>%
  filter(Behavior %in% c("Surface_pecking", "Probing", "Swallowing")) %>%
  group_by(Observation_id, Week, Strategy, Tide, Habitat, Transect_ID, Three_letter_code) %>%
  summarise(
    Surface_pecking = sum(Behavior_Count[Behavior == "Surface_pecking"], na.rm = TRUE),
    Probing = sum(Behavior_Count[Behavior == "Probing"], na.rm = TRUE),
    Swallowing = sum(Behavior_Count[Behavior == "Swallowing"], na.rm = TRUE),
    Media_duration = first(Media.duration..s.),
    .groups = "drop"
  ) %>%
  mutate(
    Total_attempts = Surface_pecking + Probing,
    Swallowing_success = ifelse(Total_attempts > 0, Swallowing / Total_attempts, NA_real_),
    Swallowing_success_duration = Swallowing_success / Media_duration) |>
  filter(!Observation_id %in% c("KMP.16.04", "NUK.24.04"))

p9a <- ggplot(success_swallowing,
              aes(x = as.factor(Week), y = Swallowing_success_duration, 
                  fill = Strategy)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#E777F2", "#4DD2A4", "#4DC8F9")) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Succesfull attempts by Strategy",
    x = "Week",
    y = "Duration Rate")
p9a

qqnorm(success_swallowing$Swallowing_success_duration)

shapiro.test(success_swallowing$Swallowing_success_duration[!is.na(success_swallowing$Swallowing_success_duration)])

## plot distribution of Swallowing success duration
ggplot(success_swallowing, aes(x = Swallowing_success_duration)) +
  geom_histogram(bins = 30, fill = "#69b3a2", color = "black") +
  labs(
    title = "Distribution of Swallowing Success Duration",
    x = "Swallowing Success Duration (seconds)",
    y = "Count"
  ) +
  theme_minimal()
         

glm1 <- glm(Swallowing_success_duration ~ 1, 
            family = Gamma(link = "log"),
            data = success_swallowing)

glm2 <- glm(Swallowing_success_duration ~ Week, 
            family = Gamma(link = "log"),
            data = success_swallowing)

glm3 <- glm(Swallowing_success_duration ~ Strategy,
            family = Gamma(link = "log"),
            data = success_swallowing)

glm4 <- glm(Swallowing_success_duration ~ Tide,
            family = Gamma(link = "log"),
            data = success_swallowing)

glm5 <- glm(Swallowing_success_duration ~ Habitat,
            family = Gamma(link = "log"),
            data = success_swallowing)

glm6 <- glm(Swallowing_success_duration ~ Transect_ID,
            family = Gamma(link = "log"),
            data = success_swallowing)

model.sel(glm1, glm2, glm3, glm4, glm5, glm6)

glmfull <- glm(Swallowing_success_duration ~ Week + Strategy + Tide + Habitat 
               + Transect_ID, 
               family = Gamma(link = "log"),
               data = success_swallowing)

summary(glmfull)

glm_int1 <- glm(Swallowing_success_duration ~ Week * Strategy + Tide + Habitat 
                + Transect_ID, 
                family = Gamma(link = "log"),
                data = success_swallowing)
summary(glm_int1)

glmer_full <- glmer(Swallowing_success_duration ~ Week + Strategy +
                      (1 | Three_letter_code) + (1 | Transect_ID) 
                    + (1 | Tide) + (1 | Habitat), 
                family = Gamma(link = "log"),
                data = success_swallowing,
                control = glmerControl(optimizer = "bobyqa", 
                                       optCtrl = list(maxfun = 2e5)))
summary(glmer_full)

glmer_reduced1 <- glmer(Swallowing_success_duration ~ Week + Strategy + 
                        (1 | Three_letter_code) + (1 | Transect_ID), 
                        family = Gamma(link = "log"),
                        data = success_swallowing,
                        control = glmerControl(optimizer = "bobyqa", 
                                               optCtrl = list(maxfun = 2e5)))
summary(glmer_reduced1)

glmer_int1 <- glmer(Swallowing_success_duration ~ Week * Strategy + 
                        (1 | Three_letter_code) + (1 | Transect_ID), 
                        family = Gamma(link = "log"),
                        data = success_swallowing,
                        control = glmerControl(optimizer = "bobyqa", 
                                               optCtrl = list(maxfun = 2e5)))
summary(glmer_int1)

glmer_poly1 <- glmer(Swallowing_success_duration ~ poly(Week, 2) + Strategy
                  + (1 | Three_letter_code) + (1 | Transect_ID) + (1 | Tide),
  family = Gamma(link = "log"),
  data = success_swallowing,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
summary(glmer_poly1)

glmer_polyint <- glmer(Swallowing_success_duration ~ poly(Week, 2) * Strategy 
                       + (1 | Three_letter_code) + (1 | Transect_ID),
                       family = Gamma(link = "log"),
                       data = success_swallowing,
                       control = glmerControl(optimizer = "bobyqa", 
                                              optCtrl = list(maxfun = 2e5)))

model.sel(glmfull, glm_int1, glmer_full, glmer_reduced1, glm1, glm2, glm3, glm4, glm5, glm6, glmer_int1, glmer_poly1, glmer_polyint)

swallowing_model <- model.sel(glmfull, glm_int1, glmer_full, glmer_reduced1, glm1, glm2, glm3, glm4, glm5, glm6, glmer_int1, glmer_poly1, glmer_polyint)
swallowing_model

swallowing_model_df <- as.data.frame(swallowing_model)
swallowing_model_df$model <- rownames(swallowing_model_df)
swallowing_model_df <- swallowing_model_df[, c("model", setdiff(names(swallowing_model_df), "model"))]
head(swallowing_model_df)
# Save to CSV

write.csv(swallowing_model_df, "swallowing_model_selection_table.csv", row.names = FALSE)

success_swallowing$Strategy <- factor(success_swallowing$Strategy)
success_swallowing$Three_letter_code <- factor(success_swallowing$Three_letter_code)
success_swallowing$Transect_ID <- factor(success_swallowing$Transect_ID)
success_swallowing$Tide <- factor(success_swallowing$Tide)

strategies <- levels(success_swallowing$Strategy)
three_letter <- levels(success_swallowing$Three_letter_code)
transect_ids <- levels(success_swallowing$Transect_ID)
tide <- levels(success_swallowing$Tide)
weeks_seq <- seq(min(success_swallowing$Week), max(success_swallowing$Week), length.out = 500)

new_swallowing_smooth <- expand.grid(
  Week = 12:21,
  Strategy = strategies,
  Three_letter_code = three_letter,
  Transect_ID = transect_ids,
  Tide = tide,
  stringsAsFactors = FALSE
)
# Convert them back to factors using the original levels
new_swallowing_smooth$Strategy <- factor(new_swallowing_smooth$Strategy, levels = strategies)
new_swallowing_smooth$Three_letter_code <- factor(new_swallowing_smooth$Three_letter_code, levels = three_letter)
new_swallowing_smooth$Transect_ID <- factor(new_swallowing_smooth$Transect_ID, levels = transect_ids)
new_swallowing_smooth$Tide <- factor(new_swallowing_smooth$Tide, levels = tide)
new_swallowing_smooth$predicted <- predict(glmer_poly1,
                                  newdata = new_swallowing_smooth,
                                  re.form = NA,
                                  type = "response")
new_swallowing_smooth <- new_swallowing_smooth |>
  filter(!Week %in% c("12", "13", "14", "15"))

p9b <- p9a +
  geom_line(
    data = new_swallowing_smooth,
    aes(x = as.factor(Week), y = predicted, color = Strategy, 
        group = Strategy),
    size = 1.0, inherit.aes = FALSE
  ) +
  geom_text(
    data = swallow_counts,
    aes(x = as.factor(Week), y = 0.005, label = n, group = Strategy),
    position = position_dodge2(width = 0.8, preserve = "single"),
   # position = position_dodge(width = 1.0),
    size =3,
    inherit.aes = FALSE) +
  
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
    y = "Behaviour Rate",
    x = "Week",
    title = "Swallowing Succes Rate per Strategy"
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
p9b



swallow_counts <- success_swallowing |>
  group_by(Week, Strategy) |>
  summarise(n = n_distinct(Observation_id), .groups = "drop") |>
  filter(!Week %in% c("9", "11","12", "13", "15"))

# Pairwise comparison
emm_swallowing <- emm <- emmeans(glmer_poly1, ~ Strategy | Week, type = "response")
# Pairwise comparisons of Strategy within each Week × Habitat group
pairwise_results_swallowing <- pairs(emm_swallowing, adjust = "tukey", type = "response")
pairwise_summary_swallowing <- summary(pairwise_results_swallowing)
# Print summary of comparisons
print(pairwise_summary_swallowing)
# Extract p-values and contrast names
swallowing_pvals <- pairwise_summary_swallowing$p.value
names(swallowing_pvals) <- gsub(" / ", " - ", pairwise_summary_swallowing$contrast)
# Remove duplicates (if any)
swallowing_pvals_unique <- swallowing_pvals[!duplicated(names(swallowing_pvals))]
library(multcompView)
# Generate compact letter display for grouping
swallowing_group_letters <- multcompLetters(swallowing_pvals_unique)$Letters
library(stringr)  # for str_trim()
# Trim spaces from names
swallowing_clean_names <- str_trim(names(swallowing_group_letters))
# Remove duplicates: keep first occurrence only
swallowing_unique_indices <- !duplicated(swallowing_clean_names)
swallowing_clean_names_unique <- swallowing_clean_names[swallowing_unique_indices]
swallowing_group_letters_unique <- swallowing_group_letters[swallowing_unique_indices]
# Rename with cleaned names
names(swallowing_group_letters_unique) <- swallowing_clean_names_unique
# Define factor level order (optional)
levels_strat <- c("early_northward_migration", "late_northward_migration", "overwinterer")
# Match levels to cleaned names
swallowing_group_letters_ordered <- swallowing_group_letters_unique[match(levels_strat, swallowing_clean_names_unique)]
print(swallowing_group_letters_ordered)

ggsave("swallowing_rate_per_strategy.png", plot = p9b, width = 28, height = 10, dpi = 300)
