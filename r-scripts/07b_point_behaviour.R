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

p5b <- p5a + geom_text(data = counts_point |> filter(Behavior == "Probing"),
                       aes(x = as.factor(Week), y = 1.6, 
                       label = paste0("", n)),
                       position = position_dodge(width = 0.8), size = 8) 
p5b

point_behaviors$Tide <- point_behaviors$Tide |>
  str_trim()
point_behaviors$Habitat <- point_behaviors$Habitat |>
  str_trim()

ggplot(point_behaviors |> filter(Behavior == "Probing"), aes(x = Behavior_Rate)) +
  geom_histogram(bins = 30, fill = "#69b3a2", color = "black") +
  labs(
    title = "Distribution of probing Durations",
    x = "Probing Duration (seconds)",
    y = "Count"
  ) +
  theme_minimal()

shapiro.test(point_behaviors$Behavior_Rate[point_behaviors$Behavior == "Probing"])


lm1 <- lm(Behavior_Rate ~ 1, 
            data = subset(point_behaviors, Behavior == "Probing"))

lm2 <- lm(Behavior_Rate ~ Week, 
            data = subset(point_behaviors, Behavior == "Probing"))

lm3 <- lm(Behavior_Rate ~ Strategy, 
            data = subset(point_behaviors, Behavior == "Probing"))

lm4 <- lm(Behavior_Rate ~ Tide, 
            data = subset(point_behaviors, Behavior == "Probing"))

lm5 <- lm(Behavior_Rate ~   Habitat,
            data = subset(point_behaviors, Behavior == "Probing"))

model.sel(lm1, lm2, lm3, lm4, lm5)

lmfull <- lm(Behavior_Rate ~ Week + Strategy + Tide + Habitat , 
               data = subset(point_behaviors, Behavior == "Probing"))

lmreduced1 <- lm(Behavior_Rate ~ Week + Strategy + Tide + Habitat,
                   data = subset(point_behaviors, Behavior == "Probing"))

lmreduced2 <- lm(Behavior_Rate ~ Week + Strategy + Tide,
                   data = subset(point_behaviors, Behavior == "Probing"))

lmreduced3 <- lm(Behavior_Rate ~ Week + Tide, 
                   data = subset(point_behaviors, Behavior == "Probing"))

lm_int1 <- lm(Behavior_Rate ~ Week * Strategy,
               data = subset(point_behaviors, Behavior == "Probing"))

lm_pol1 <- lm(Behavior_Rate ~ poly(Week,2) + Strategy 
              + Tide + Habitat, 
               data = subset(point_behaviors, Behavior == "Probing"))

lm_pol2 <- lm(Behavior_Rate ~ poly(Week,5) + Strategy + Tide + Habitat, 
               data = subset(point_behaviors, Behavior == "Probing"))

lm_int2 <- lm(Behavior_Rate ~ Week * Strategy * Tide, 
               data = subset(point_behaviors, Behavior == "Probing"))

lm_int3 <- lm(Behavior_Rate ~ Week * Tide + Strategy,
                data = subset(point_behaviors, Behavior == "Probing"))

model.sel(lmfull, lmreduced1, lmreduced2, lmreduced3, lm_int1, lm_int2, lm_int3, lm1, lm2, lm3, lm4, lm5)

lmerfull <- lmer(Behavior_Rate ~ Week + Strategy + (1 | Three_letter_code)
                    + (1 | Tide) + (1 | Habitat), 
                   data = subset(point_behaviors, Behavior == "Probing"))

lmerreduced1 <- lmer(Behavior_Rate ~ Week + Strategy 
                       + (1 | Three_letter_code) 
                       + (1 | Tide), 
                       data = subset(point_behaviors, Behavior == "Probing"))

lmerreduced2 <- lmer(Behavior_Rate ~ Week + Strategy 
                       + (1 | Three_letter_code) ,
                     data = subset(point_behaviors, Behavior == "Probing"))

lmer_int1 <- lmer(Behavior_Rate ~ Week * Strategy + (1|Three_letter_code) 
                    + (1|Tide) , 
                   data = subset(point_behaviors, Behavior == "Probing"))


lmer_int2 <- lmer(Behavior_Rate ~ Week * Strategy + (1|Three_letter_code),
                  data = subset(point_behaviors, Behavior == "Probing"))


library(splines)
lmer_spline <- lmer(Behavior_Rate ~ ns(Week, df = 4) + Strategy + 
                        (1 | Three_letter_code) + (1 | Tide),
                     data = subset(point_behaviors, Behavior == "Probing"))

lmer_poly2 <- lmer(Behavior_Rate ~ poly(Week, 2) + Strategy +
    (1 | Three_letter_code)  + (1 | Tide),
  data = subset(point_behaviors, Behavior == "Probing"))

lmer_polyint <- lmer(Behavior_Rate ~ poly(Week, 2) * Strategy + 
                       (1 | Three_letter_code) 
                       + (1 | Tide),
                     data = subset(point_behaviors, Behavior == "Probing"))


probing_model <- model.sel(lmfull, lmreduced1, lmreduced2, lmreduced3, lm_int1, lm_int2, lm_int3, lm1, lm2, lm3, lm4, lm5, lmerfull, lmerreduced1, lmerreduced2, lmer_int1, lmer_int2, lmer_spline, lmer_poly2, lmer_polyint, lm_pol2, lm_pol1)
probing_model
## Choose lm_pol2


plot(residuals(lm_pol2))
## Which shows a not really a pattern which is good!


probing_model_df <- as.data.frame(probing_model)
probing_model_df$model <- rownames(probing_model_df)
probing_model_df <- probing_model_df[, c("model", setdiff(names(probing_model_df), "model"))]
head(probing_model_df)

# Save to CSV
write.csv(probing_model_df, "probing_model_selection_table.csv", row.names = FALSE)

# Create new data for prediction (adjust as needed)
newdata_prob <- expand.grid(
  Week = seq(min(point_behaviors$Week), max(point_behaviors$Week), 
             length.out = 100),
  Strategy = unique(point_behaviors$Strategy),
  Habitat = unique(point_behaviors$Habitat),
  Transect_ID = unique(point_behaviors$Transect_ID),
  Tide = unique(point_behaviors$Tide)
)

# Get predictions on  with confidence intervals
pred_prob <- predict(lm_pol2, newdata_prob, interval = "confidence")

newdata_prob$fit <- pred_prob[,"fit"]
newdata_prob$lwr <- pred_prob[,"lwr"]
newdata_prob$upr <- pred_prob[,"upr"]

pred_summary_prob <- newdata_prob |>
  group_by(Week, Strategy) |>
  summarise(
    fit = mean(fit),
    lwr = mean(lwr),
    upr = mean(upr),
    .groups = "drop")

p_prob <- ggplot(point_behaviors |> filter(Behavior == "Probing"), aes(x = Week, y = Behavior_Rate)) +
  geom_boxplot(aes(group = interaction(Week, Strategy), fill = Strategy), 
               position = position_dodge(width = 0.8)) +
  geom_line(data = pred_summary_prob, aes(x = Week, y = fit, color = Strategy, 
                                     group = Strategy), size = 1.0, inherit.aes = FALSE) +
  #geom_ribbon(data = pred_summary_prob, aes(x = Week, ymin = lwr, ymax = upr, 
   #                                    fill = Strategy), alpha = 0.2, 
   #           inherit.aes = FALSE) +
  geom_text(data = counts_point |> filter(Behavior == "Probing"), aes(x = Week, y = 1.6, label = n, group = Strategy, 
                               color = "black"),
            position = position_dodge(width = 0.8), size = 6, inherit.aes = FALSE) +
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
    y = "Behaviour Rate of Probing",
    x = "Week",
    title = "Behaviour Rate per Strategy") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    axis.title.x = element_text(size = 21),
    axis.title.y = element_text(size = 21),
    plot.title = element_text(size = 25, face = "bold", hjust = 0.5),
    legend.position = "none")
p_prob

# Loadpackages
library(emmeans)
library(multcompView)

# Pairwise comparison
emm_prob <- emm <- emmeans(lm_pol2, ~ Strategy | Week , type = "response")

# Use cld to assign group letters
cld_prob <- cld(emm_prob, adjust = "tukey", Letters = letters, 
                  type = "response")

# View result
print(cld_prob)


ggsave("probing_rate_per_strategy.png", plot = p_prob, width = 28, height = 10, dpi = 300)

##############################################################################
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
    axis.text.x = element_text(size = 23),
    axis.text.y = element_text(size = 23),
    axis.title.x = element_text(size = 24),
    axis.title.y = element_text(size = 24),
    plot.title = element_text(size = 26, face = "bold", hjust = 0.5),
    legend.position = "none")
p6a

p6b <- p6a + geom_text(data = counts_point |> filter(Behavior == "Surface_pecking"), aes(x = as.factor(Week), 
                                                                                         y = 0.9, 
                                                                                         label = paste0("", n)),
                       position = position_dodge(width = 0.8), size = 8)

p6b

ggplot(point_behaviors |> filter(Behavior == "Surface_pecking"), aes(x = Behavior_Rate)) +
  geom_histogram(bins = 30, fill = "#69b3a2", color = "black") +
  labs(
    title = "Distribution of Surface pecks duration",
    x = "surface pecks Duration (seconds)",
    y = "Count"
  ) +
  theme_minimal()

shapiro.test(point_behaviors$Behavior_Rate[point_behaviors$Behavior == "Surface_pecking"])
# so not normal data

library(dplyr)

surface_pecking <- point_behaviors |> 
  filter(Behavior == "Surface_pecking") |> 
  mutate(
    Behavior_Rate_log = log(Behavior_Rate + 1),       # Replace 'Count' with your numeric variable
    Behavior_Rate_sqrt = sqrt(Behavior_Rate)
  )

hist(surface_pecking$Behavior_Rate_log, main = "Log-transformed")
hist(surface_pecking$Behavior_Rate_sqrt, main = "Square-root-transformed")

model_logref <- lmer(Behavior_Rate_log~ Week * Strategy + (1 | Three_letter_code) + 
                       (1 | Habitat) + (1 | Tide) + (1 | Transect_ID), data = surface_pecking)

model_squared <- lmer(Behavior_Rate_sqrt ~ Week * Strategy + (1 | Three_letter_code) + 
                        (1 | Habitat) + (1 | Tide) + (1 | Transect_ID), data = surface_pecking)

AIC(model_logref, model_squared)
plot(model_logref)
plot(model_squared)
qqnorm(resid(model_logref)); qqline(resid(model_logref))
qqnorm(resid(model_squared)); qqline(resid(model_squared))
# So the log-transformed model is better, so we will use that one

lm1 <- lm(Behavior_Rate_log ~ 1, 
            data = surface_pecking)

lm2 <- lm(Behavior_Rate_log ~ Week, 
            data = surface_pecking)

lm3 <- lm(Behavior_Rate_log ~ Strategy,
         data = surface_pecking)

lm4 <- lm(Behavior_Rate_log ~ Tide,
            data = surface_pecking)

lm5 <- lm(Behavior_Rate_log ~ Habitat,
            data = surface_pecking)

model.sel(lm1, lm2, lm3, lm4, lm5)
anova(lm1, lm2, lm3, lm4, lm5)

lmfull <- lm(Behavior_Rate_log ~ Week + Strategy + Tide + Habitat, 
               data = surface_pecking)

lm_int1 <- lm(Behavior_Rate_log ~ Week * Strategy + Tide + Habitat, 
            data = surface_pecking)

lm_polint <- lm(Behavior_Rate_log ~ poly(Week, 2) * Strategy + Tide 
                        + Habitat, 
                    data = surface_pecking)

lm_pol <- lm(Behavior_Rate_log ~ poly(Week, 2) + Strategy + Tide 
                + Habitat, 
            data = surface_pecking)

lm_pol2 <- lm(Behavior_Rate_log ~ poly(Week,2) + Strategy + Habitat,
              data = surface_pecking)

lm_pol3 <- lm(Behavior_Rate_log ~ poly(Week,2) * Strategy + Habitat,
              data = surface_pecking)

lm_pol4 <- lm(Behavior_Rate_log ~ poly(Week,3) + Strategy + Habitat 
              + Tide,
              data = surface_pecking)

lm_pol5 <- lm(Behavior_Rate_log ~ poly(Week,3) * Strategy + Habitat 
              + Tide,
              data = surface_pecking)

lm_pol6 <- lm(Behavior_Rate_log ~ poly(Week,5) + Strategy + Habitat 
              +  Tide,
              data = surface_pecking)

lm_pol7 <- lm(Behavior_Rate_log ~ poly(Week,5) * Strategy + Habitat +
                Tide,
              data = surface_pecking)

lm_pollies <- lm(Behavior_Rate_log ~ poly(Week,2) + poly(Strategy,2) + Habitat
                 + Tide ,
                 data = surface_pecking)

lm_pollies2 <- lm(Behavior_Rate_log ~ poly(Week,2) * poly(Strategy,2) + Habitat
                 + Tide ,
                 data = surface_pecking)

lm_pol8 <- lm(Behavior_Rate_log ~ Week + poly(Strategy, 2) + Habitat 
              + Tide,
              data = surface_pecking)

lm_pol9 <- lm(Behavior_Rate_log ~ Week * poly(Strategy, 2) + Habitat 
              +  Tide,
              data = surface_pecking)

lmer_full <- lmer(Behavior_Rate_log ~ Week + Strategy + (1 | Three_letter_code) 
                 + (1 | Tide) + (1 | Habitat), 
                data = surface_pecking)

lmer_reduced1 <- lmer(Behavior_Rate_log ~ Week + Strategy + 
                  (1 | Three_letter_code) + (1 | Habitat),
                data = surface_pecking)

lmer_reduced2 <- lmer(Behavior_Rate_log ~ Week + Strategy  
                        + Habitat + Tide + (1|Three_letter_code), 
                data = surface_pecking)

lmer_reduced3 <- lmer(Behavior_Rate_log ~ Week + Strategy 
                    + Habitat + (1|Three_letter_code),
                data = surface_pecking)

lmer_reduced4 <- lmer(Behavior_Rate_log ~ Week + Strategy 
                        + (1|Three_letter_code), 
                data = surface_pecking)

lmer_reduced5 <- lmer(Behavior_Rate_log ~ Week + Strategy + Tide + Habitat
                        + (1 | Three_letter_code),
                data = surface_pecking)

lmer_reduced6 <- lmer(Behavior_Rate_log ~ Week + Strategy + Tide + (1|Habitat)
                        + (1|Three_letter_code),
               data = surface_pecking)

lmer_int1 <- lmer(Behavior_Rate_log ~ Week * Strategy
                        + Habitat + Tide + (1|Three_letter_code), 
                data = surface_pecking)

lmer_poly1 <- lmer(Behavior_Rate_log ~ poly(Week, 3) + Strategy  
                     + Tide + Habitat + (1|Three_letter_code),
                data = surface_pecking)
summary(lmer_poly1)

lmer_poly2 <- lmer(Behavior_Rate_log ~ poly(Week, 3) + Strategy  
                     + Tide + Habitat + (1|Three_letter_code),
                     data = surface_pecking)

lmer_polyint <- lmer(Behavior_Rate_log ~ poly(Week, 3) * Strategy + Tide 
                     + Habitat + (1|Three_letter_code),
                           data = surface_pecking)

lmer_spline <- lmer(Behavior_Rate_log ~ ns(Week, df = 4) + Strategy
                     + Tide + Habitat + (1|Three_letter_code),
                data = surface_pecking)

lmer_int2 <- lmer(Behavior_Rate_log ~ Week + Strategy * Tide + Habitat + 
                      (1|Three_letter_code),
                data = surface_pecking)

lmer_int3 <- lmer(Behavior_Rate_log ~ Week * Tide + Strategy + Habitat + 
                    (1|Three_letter_code),
                data = surface_pecking)

lmer_int4 <- lmer(Behavior_Rate_log ~ poly(Week,3) + Strategy * Habitat 
                    + (1|Three_letter_code), 
                data = surface_pecking)


pecking_model <- model.sel(lmfull, lm_int1, lmer_full, lmer_reduced1, lm1, lm2, lm3, lm4, lm5, lmer_reduced2, lmer_int1, lmer_reduced3, lmer_reduced4, lmer_poly1, lmer_spline, lmer_reduced5, lmer_reduced6, lmer_int2, lmer_int3,  lmer_polyint, lmer_int4, lmer_poly2, lm_pol, lm_polint, lm_pol3, lm_pol2, lm_pol4, lm_pol5, lm_pol6, lm_pol7, lm_pollies, lm_pollies2, lm_pol8, lm_pol9)
pecking_model

plot(residuals(lm_pol6))
## Which shows a not really a pattern which is good!


pecking_model_df <- as.data.frame(pecking_model)
pecking_model_df$model <- rownames(pecking_model_df)
pecking_model_df <- pecking_model_df[, c("model", setdiff(names(pecking_model_df), "model"))]
head(pecking_model_df)

# Save to CSV
write.csv(pecking_model_df, "pecking_model_selection_table.csv", row.names = FALSE)

# Create new data for prediction (adjust as needed)
newdata_peck <- expand.grid(
  Week = seq(min(surface_pecking$Week), max(surface_pecking$Week), 
             length.out = 100),
  Strategy = unique(surface_pecking$Strategy),
  Habitat = unique(surface_pecking$Habitat),
  Transect_ID = unique(surface_pecking$Transect_ID),
  Tide = unique(surface_pecking$Tide)
)

# Get predictions on  with confidence intervals
pred_peck <- predict(lm_pol6, newdata_peck, interval = "confidence")

newdata_peck$fit <- exp(pred_peck[,"fit"]) - 1
newdata_peck$lwr <- exp(pred_peck[,"lwr"]) - 1
newdata_peck$upr <- exp(pred_peck[,"upr"]) - 1

pred_summary_peck <- newdata_peck |>
  group_by(Week, Strategy) |>
  summarise(
    fit = mean(fit),
    lwr = mean(lwr),
    upr = mean(upr),
    .groups = "drop")

p_peck <- ggplot(surface_pecking, aes(x = Week, y = Behavior_Rate)) +
  geom_boxplot(aes(group = interaction(Week, Strategy), fill = Strategy), 
               position = position_dodge(width = 0.8)) +
  geom_line(data = pred_summary_peck, aes(x = Week, y = fit, color = Strategy, 
                                          group = Strategy), size = 1.0, inherit.aes = FALSE) +
  #geom_ribbon(data = pred_summary_peck, aes(x = Week, ymin = lwr, ymax = upr, 
  #                                    fill = Strategy), alpha = 0.2, 
  #           inherit.aes = FALSE) +
  geom_text(data = counts_point |> filter(Behavior == "Surface_pecking"), 
            aes(x = Week, y = 0.9, label = n, group = Strategy, 
             color = "black"),
            position = position_dodge(width = 0.8), size = 6, 
            inherit.aes = FALSE) +
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
    y = "Behaviour Rate of Surface Pecking",
    x = "Week",
    title = "Behaviour Rate per Strategy") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    axis.title.x = element_text(size = 21),
    axis.title.y = element_text(size = 21),
    plot.title = element_text(size = 25, face = "bold", hjust = 0.5),
    legend.position = "none")
p_peck

# Loadpackages
library(emmeans)
library(multcompView)

# Pairwise comparison
emm_peck <- emm <- emmeans(lm_pol6, ~ Strategy | Week , type = "response")

# Use cld to assign group letters
cld_peck <- cld(emm_peck, adjust = "tukey", Letters = letters, 
                type = "response")

# View result
print(cld_peck)


ggsave("peck_rate_per_strategy.png", plot = p_peck, width = 28, height = 10, dpi = 300)

#############################################################################

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

###############################################################################

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

success_swallowing <- success_swallowing |> 
  mutate(
    Success_Rate_log = log(Swallowing_success_duration + 1),       
    Success_Rate_sqrt = sqrt(Swallowing_success_duration)
  )

hist(success_swallowing$Success_Rate_log, main = "Log-transformed")
hist(success_swallowing$Success_Rate_sqrt, main = "Square-root-transformed")

model_logref <- lmer(Success_Rate_log~ Week * Strategy + (1 | Three_letter_code) + 
                       (1 | Habitat) + (1 | Tide) + (1 | Transect_ID), 
                     data = success_swallowing)

model_squared <- lmer(Success_Rate_sqrt ~ Week * Strategy + (1 | Three_letter_code) 
                      +  (1 | Habitat) + (1 | Tide) +
                                     (1 | Transect_ID), data = success_swallowing)

AIC(model_logref, model_squared)
plot(model_logref)
plot(model_squared)
qqnorm(resid(model_logref)); qqline(resid(model_logref))
qqnorm(resid(model_squared)); qqline(resid(model_squared))
         

lm1 <- lm(Success_Rate_sqrt ~ 1, 
            data = success_swallowing)

lm2 <- lm(Success_Rate_sqrt ~ Week, 
            data = success_swallowing)

lm3 <- lm(Success_Rate_sqrt ~ Strategy,
            data = success_swallowing)

lm4 <- lm(Success_Rate_sqrt ~ Tide,
            data = success_swallowing)

lm5 <- lm(Success_Rate_sqrt ~ Habitat,
            data = success_swallowing)


model.sel(lm1, lm2, lm3, lm4, lm5)

lmfull <- lm(Success_Rate_sqrt ~ Week + Strategy + Tide + Habitat,
               data = success_swallowing)

lm_int1 <- lm(Success_Rate_sqrt ~ Week * Strategy + Tide + Habitat, 
                data = success_swallowing)

lm_polint <- lm(Success_Rate_sqrt ~ poly(Week, 2) * Strategy, 
                    data = success_swallowing)


lmer_full <- lmer(Success_Rate_sqrt ~ Week + Strategy +
                      (1 | Three_letter_code)  
                    + (1 | Tide) + (1 | Habitat),
                data = success_swallowing)

lmer_reduced1 <- lmer(Success_Rate_sqrt ~ Week + Strategy + 
                        (1 | Three_letter_code), 
                        data = success_swallowing)

lmer_int1 <- lmer(Success_Rate_sqrt ~ Week * Strategy + 
                        (1 | Three_letter_code),
                        data = success_swallowing)

lmer_poly1 <- lmer(Success_Rate_sqrt ~ poly(Week, 2) + Strategy
                  + (1 | Three_letter_code) + (1 | Tide),
              data = success_swallowing)

lmer_polyint <- lmer(Success_Rate_sqrt ~ poly(Week, 2) * Strategy 
                       + (1 | Three_letter_code) ,
                       data = success_swallowing)


swallowing_model <- model.sel(lmfull, lm_int1, lmer_full, lmer_reduced1, lm1, lm2, lm3, lm4, lm5, lmer_int1, lmer_poly1, lmer_polyint, lm_polint)
swallowing_model

plot(residuals(lm_polint))
## Which shows a not really a pattern which is good!


swallowing_model_df <- as.data.frame(swallowing_model)
swallowing_model_df$model <- rownames(swallowing_model_df)
swallowing_model_df <- swallowing_model_df[, c("model", setdiff(names(swallowing_model_df), "model"))]
head(swallowing_model_df)
# Save to CSV

write.csv(swallowing_model_df, "swallowing_model_selection_table.csv", row.names = FALSE)

# Create new data for prediction (adjust as needed)
newdata_success <- expand.grid(
  Week = seq(min(success_swallowing$Week), max(success_swallowing$Week), 
             length.out = 100),
  Strategy = unique(success_swallowing$Strategy))

# Get predictions on  with confidence intervals
pred_success <- predict(lm_polint, newdata_success, interval = "confidence")

newdata_success$fit <- (pred_success[, "fit"])^2
newdata_success$lwr <- (pred_success[, "lwr"])^2
newdata_success$upr <- (pred_success[, "upr"])^2


pred_summary_success <- newdata_success |>
  group_by(Week, Strategy) |>
  summarise(
    fit = mean(fit),
    lwr = mean(lwr),
    upr = mean(upr),
    .groups = "drop")

counts_swallow <- success_swallowing |>
  group_by(Week, Strategy) |>
  summarise(n = n_distinct(Observation_id), .groups = "drop") |>
  filter(!Week %in% c("9", "11","12", "13", "15"))

p_success <- ggplot(success_swallowing, aes(x = Week, y = Swallowing_success_duration)) +
  geom_boxplot(aes(group = interaction(Week, Strategy), fill = Strategy), 
               position = position_dodge(width = 0.8)) +
  geom_line(data = pred_summary_success, aes(x = Week, y = fit, color = Strategy, 
                                          group = Strategy), size = 1.0, inherit.aes = FALSE) +
  geom_ribbon(data = pred_summary_success, aes(x = Week, ymin = lwr, ymax = upr, 
                                     fill = Strategy), alpha = 0.2, 
            inherit.aes = FALSE) +
  geom_text(data = counts_swallow, 
            aes(x = Week, y = 0.0052, label = n, group = Strategy, 
                color = "black"),
            position = position_dodge(width = 0.8), size = 6, 
            inherit.aes = FALSE) +
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
    y = "Success Rate of Swallowing",
    x = "Week",
    title = "Success Rate per Strategy") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    axis.title.x = element_text(size = 21),
    axis.title.y = element_text(size = 21),
    plot.title = element_text(size = 25, face = "bold", hjust = 0.5),
    legend.position = "none")
p_success

# Loadpackages
library(emmeans)
library(multcompView)

# Pairwise comparison
emm_success <- emm <- emmeans(lm_polint, ~ Strategy | Week , type = "response")

# Use cld to assign group letters
cld_success <- cld(emm_success, adjust = "tukey", Letters = letters, 
                type = "response")

# View result
print(cld_success)


ggsave("success_rate_per_strategy.png", plot = p_success, width = 28, height = 10, dpi = 300)


