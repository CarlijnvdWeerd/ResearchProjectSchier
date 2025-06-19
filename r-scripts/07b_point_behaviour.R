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

probing_model <- model.sel(glmfull, glmreduced1, glmreduced2, glmreduced3, glm_int1, glm_int2, glm_int3, glm1, glm2, glm3, glm4, glm5, glm6, glmerfull, glmerreduced1, glmerreduced2, glmer_int1, glmer_int2)
## Chosing glmer_int2 because the df is 1 higher than the model with the lowest AIC and it takes into consideration the interaction between Strategy and Week which is what i want to look at. 

plot(residuals(glmer_int2))
## Which shows a not really a pattern which is good!


probing_model_df <- as.data.frame(probing_model)
probing_model_df$model <- rownames(probing_model_df)
probing_model_df <- model_df[, c("model", setdiff(names(model_df), "model"))]
head(probing_model_df)

# Save to CSV
write.csv(model_df, "probing_model_selection_table.csv", row.names = FALSE)

point_behaviors$Strategy <- factor(point_behaviors$Strategy)
point_behaviors$Three_letter_code <- factor(point_behaviors$Three_letter_code)
point_behaviors$Transect_ID <- factor(point_behaviors$Transect_ID)

strategies <- levels(point_behaviors$Strategy)
three_letter <- levels(point_behaviors$Three_letter_code)
transect_ids <- levels(point_behaviors$Transect_ID)

weeks_seq <- seq(min(point_behaviors$Week), max(point_behaviors$Week), length.out = 100)

new_probing_smooth <- expand.grid(
  Week = 12:21,
  Strategy = strategies,
  Three_letter_code = three_letter,
  Transect_ID = transect_ids,
  stringsAsFactors = FALSE
)

# Convert them back to factors using the original levels
new_probing_smooth$Strategy <- factor(new_probing_smooth$Strategy, levels = strategies)
new_probing_smooth$Three_letter_code <- factor(new_probing_smooth$Three_letter_code, levels = three_letter)
new_probing_smooth$Transect_ID <- factor(new_probing_smooth$Transect_ID, levels = transect_ids)

new_probing_smooth$predicted <- predict(glmer_int2,
                                  newdata = new_probing_smooth,
                                  re.form = NA,
                                  type = "response")
new_probing_smooth <- new_probing_smooth |>
   filter(!Week %in% c("14", "15"))

p5c <- p5b +
  geom_line(
    data = new_probing_smooth,
    aes(x = as.factor(Week), y = predicted, color = Strategy, 
        group = Strategy),
    size = 1.0, inherit.aes = FALSE) +
  
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
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    legend.position = "none"
  )
p5c

# Pairwise comparison
emm_probing <- emm <- emmeans(glmer_int2, ~ Strategy | Week, type = "response")

# Pairwise comparisons of Strategy within each Week × Habitat group
pairwise_results_probing <- pairs(emm_probing, adjust = "tukey", type = "response")
pairwise_summary_probing <- summary(pairwise_results_probing)

# Print summary of comparisons
print(pairwise_summary_probing)

# Extract p-values and contrast names
pvals <- pairwise_summary$p.value
names(pvals) <- gsub(" / ", " - ", pairwise_summary$contrast)

# Remove duplicates (if any)
pvals_unique <- pvals[!duplicated(names(pvals))]

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


p6b <- p6a + geom_text(data = counts_point |> filter(Behavior == "Surface_pecking"), aes(x = as.factor(Week), 
                                                                                         y = 0.9, 
                                                                                         label = paste0("n=", n)),
                       position = position_dodge(width = 0.8), size = 4)

p6b

glmer4 <- glmer(Behavior_Rate ~ Week * Strategy + (1 | Three_letter_code), 
                family = Gamma(link = "log"),
                data = subset(point_behaviors, Behavior == "Surface_pecking"),
                control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
summary(glmer4)

glmer5 <- glmer(Behavior_Rate ~ Strategy + (1 | Week) + (1 | Three_letter_code), 
                family = Gamma(link = "log"),
                data = subset(point_behaviors, Behavior == "Surface_pecking"),
                control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
summary(glmer5)

AIC(glmer4, glmer5)

glmer6 <- glmer(Behavior_Rate ~ Week * Strategy + (1 | Three_letter_code) 
                + (1| Transect_ID) + (1|Tide) + (1|Habitat), data = subset
                (point_behaviors, Behavior == "Surface_pecking"),
                family = Gamma(link = "log"),
                control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
summary(glmer6)

AIC(glmer4, glmer6)
### AIC glmer6 -214.9565, shows as only model significance, also habitat explains a big part of the variance (?)

#glm6 <- glm(Behavior_Rate ~ Week + Strategy + Week * Strategy, 
#            family = Gamma(link = "log"),
#            data = subset(point_behaviors, Behavior == "Surface_pecking"))
#summary(glm6)

point_behaviors %>%
  filter(Behavior == "Surface_pecking") %>%
  group_by(Week) %>%
  summarise(p_value = kruskal.test(Behavior_Rate ~ Strategy)$p.value)

## Only week 21 is then significant? 

p7b <- p7a + geom_text(data = counts_point |> filter(Behavior == "Turning_stuff"), aes(x = as.factor(Week), y = 0.1,                                             label = paste0("n=", n)),
                       position = position_dodge(width = 0.8), size = 4)

p7b

glm7 <- glm(Behavior_Rate ~ Week + Strategy + Week * Strategy, 
            family = Gamma(link = "log"),
            data = subset(point_behaviors, Behavior == "Turning_stuff"))
summary(glm7)

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
## Or not necessary maybe

ggsave("rate_forage_stratgy.png", plot = p_point_behavior, width = 28, height = 10, dpi = 300)

# I want to do an emmeans test to compare the strategies for each behavior