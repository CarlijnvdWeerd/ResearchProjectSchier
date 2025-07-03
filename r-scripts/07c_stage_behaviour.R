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

walking <- stage_behavior |> 
  filter(Behavior == "Walking") |> 
  mutate(
    Duration_Rate_log = log(Duration_Rate + 1),       
    Duration_Rate_sqrt = sqrt(Duration_Rate))

hist(walking$Duration_Rate_log, main = "Log-transformed")
hist(walking$Duration_Rate_sqrt, main = "Square-root-transformed")

model_logref <- lmer(Duration_Rate_log~ Week * Strategy + (1 | Three_letter_code) + 
                       (1 | Habitat) + (1 | Tide), 
                     data = walking)

model_squared <- lmer(Duration_Rate_sqrt ~ Week * Strategy 
                      + (1 | Three_letter_code) + (1 | Habitat) + (1 | Tide) 
                      , data = walking)

AIC(model_logref, model_squared)
plot(model_logref)
plot(model_squared)
qqnorm(resid(model_logref)); qqline(resid(model_logref))
qqnorm(resid(model_squared)); qqline(resid(model_squared))

shapiro.test(resid(model_squared))
hist(resid(model_squared), breaks = 30)


glm1 <- glm(Duration_Rate ~ 1, 
            family = Gamma(link = "log"),
            data = walking)

glm2 <- glm(Duration_Rate ~ Week, 
            family = Gamma(link = "log"),
            data = walking)

glm3 <- glm(Duration_Rate ~ Strategy,
            family = Gamma(link = "log"),
            data = walking)

glm4 <- glm(Duration_Rate ~ Habitat,
            family = Gamma(link = "log"),
            data = walking)

glm5 <- glm(Duration_Rate ~ Tide,
            family = Gamma(link = "log"),
            data = walking)

model.sel(glm1, glm2, glm3, glm4, glm5)

glmerfull <- glmer(Duration_Rate ~ Week + Strategy + (1|Three_letter_code) +
                     (1|Habitat) + (1|Tide),
                   family = Gamma(link = "log"),
                   data = walking,
                   control = glmerControl(optimizer = "bobyqa", 
                                          optCtrl = list(maxfun = 2e5)))
summary(glmerfull)

glmer_int1 <- glmer(Duration_Rate ~ Week * Strategy + (1 | Three_letter_code)
                    + (1 | Habitat) + (1 | Tide), 
                   family = Gamma(link = "log"),
                   data = walking,
                   control = glmerControl(optimizer = "bobyqa", 
                                          optCtrl = list(maxfun = 2e5)))
summary(glmer_int1)

glmer_intpol1 <- glmer(Duration_Rate ~ poly(Week, 2) * Strategy 
                      + (1 | Three_letter_code)
                      + (1 | Habitat) + (1 | Tide), 
                     family = Gamma(link = "log"),
                     data = walking,
                     control = glmerControl(optimizer = "bobyqa", 
                                            optCtrl = list(maxfun = 2e5)))
summary(glmer_intpol1)

glmer_intpol2 <- glmer(Duration_Rate ~ poly(Week, 2) * Strategy 
                      + (1 | Three_letter_code)
                      + (1 | Habitat), 
                     family = Gamma(link = "log"),
                     data = walking,
                     control = glmerControl(optimizer = "bobyqa", 
                                            optCtrl = list(maxfun = 2e5)))
summary(glmer_intpol2)

glmer_intpol3 <- glmer(Duration_Rate ~ poly(Week, 5) * Strategy 
                     + (1 | Three_letter_code) + (1|Habitat),
                     family = Gamma(link = "log"),
                     data = walking,
                     control = glmerControl(optimizer = "bobyqa", 
                                            optCtrl = list(maxfun = 2e5)))

walking_model <- model.sel(glmerfull, glmer_int1, glmer_intpol1, glmer_intpol2, glm1, glm2, glm3, glm4, glm5, glmer_intpol3)
walking_model

plot(residuals(glmer_intpol2))

walking_model_df <- as.data.frame(walking_model)
walking_model_df$model <- rownames(walking_model_df)
walking_model_df <- walking_model_df[, c("model", setdiff(names(walking_model_df), "model"))]
head(walking_model_df)
# Save to CSV

write.csv(walking_model_df, "walking_model_selection_table.csv", row.names = FALSE)

newdata_walk <- expand.grid(
  Week = seq(min(walking$Week), max(walking$Week), 
             length.out = 100),
  Strategy = unique(walking$Strategy),
  Habitat = unique(walking$Habitat),
  Three_letter_code = unique(walking$Three_letter_code))

# Get predictions on  with confidence intervals
pred_walk <- predict(glmer_intpol2, newdata_walk, type = "response", re.form = NA)

newdata_walk$fit <- pred_walk

pred_summary_walk <- newdata_walk |>
  group_by(Week, Strategy) |>
  summarise(
    fit = mean(fit),
    .groups = "drop")

p_walk<- ggplot(walking, aes(x = Week, y = Duration_Rate)) +
  geom_boxplot(aes(group = interaction(Week, Strategy), fill = Strategy), 
               position = position_dodge(width = 0.8)) +
  geom_line(data = pred_summary_walk, aes(x = Week, y = fit, color = Strategy, 
                                          group = Strategy), size = 1.0, inherit.aes = FALSE) +
 # geom_ribbon(data = pred_summary_walk, aes(x = Week, ymin = lwr, ymax = upr, 
  #                                    fill = Strategy), alpha = 0.2, 
  #           inherit.aes = FALSE) +
  geom_text(data = counts_stage |> filter(Behavior == "Walking"), 
            aes(x = Week, y = 0.3, label = n, group = Strategy, 
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
    y = "Relative time spend walking (s)",
    x = "Week",
    title = "Walking behaviour per Strategy") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    axis.title.x = element_text(size = 21),
    axis.title.y = element_text(size = 21),
    plot.title = element_text(size = 25, face = "bold", hjust = 0.5),
    legend.position = "none")
p_walk

# Loadpackages
library(emmeans)
library(multcompView)

# Pairwise comparison
emm_walk <- emm <- emmeans(glmer_intpol2, ~ Strategy | Week , type = "response")

# Use cld to assign group letters
cld_walk <- cld(emm_walk, adjust = "tukey", Letters = letters, 
                type = "response")

# View result
print(cld_walk)


ggsave("walking_rate_per_strategy.png", plot = p_walk, width = 28, height = 10, dpi = 300)

##################################################################################

alert_stage_behavior <- stage_behavior |>
  filter(!Habitat == "Water")
## filtering habitat water because the sample size is very small

p11a <- ggplot(stage_behavior |> filter(Behavior == "Alert"),
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

counts_stage <- stage_behavior |>
  group_by(Week, Strategy, Behavior) |>
  summarise(n = n_distinct(Observation_id), .groups = "drop") |>
  filter(!Week %in% c("9", "11","12", "13", "15"))

p11b <- p11a + geom_text(data = counts_stage |> filter(Behavior == "Alert"), 
                         aes(x = as.factor(Week), y = 0.23,                                             label = paste0("", n)),
                         position = position_dodge(width = 0.8), size = 3)

p11b

# plot Duration_Rate to check normality
ggplot(stage_behavior |> filter(Behavior == "Alert"), aes(x = Duration_Rate)) +
  geom_histogram(bins = 30, fill = "#69b3a2", color = "black") +
  labs(
    title = "Distribution of Walking Durations",
    x = "Walking Duration (seconds)",
    y = "Count"
  ) +
  theme_minimal()

alert <- stage_behavior |> 
  filter(Behavior == "Alert") |> 
  mutate(
    Duration_Rate_log = log(Duration_Rate + 1),       
    Duration_Rate_sqrt = sqrt(Duration_Rate))

hist(alert$Duration_Rate_log, main = "Log-transformed")
hist(alert$Duration_Rate_sqrt, main = "Square-root-transformed")

model_logref <- lmer(Duration_Rate_log~ Week * Strategy + (1 | Three_letter_code) + 
                       (1 | Habitat) + (1 | Tide), 
                     data = alert)

model_squared <- lmer(Duration_Rate_sqrt ~ Week * Strategy 
                      + (1 | Three_letter_code) + (1 | Habitat) + (1 | Tide) 
                      , data = alert)

AIC(model_logref, model_squared)
plot(model_logref)
plot(model_squared)
qqnorm(resid(model_logref)); qqline(resid(model_logref))
qqnorm(resid(model_squared)); qqline(resid(model_squared))

shapiro.test(resid(model_squared))
hist(resid(model_squared), breaks = 30)

install.packages("MASS")
library(MASS)
boxcox(lm(Duration_Rate ~ 1, data = alert))

alert$Duration_inv <- 1 / alert$Duration_Rate
model_inv <- lmer(Duration_inv ~ Week * Strategy 
                     + (1 | Three_letter_code) + (1 | Habitat) + (1 | Tide) 
                     , data = alert)
plot(model_inv)
hist(resid(model_inv), breaks = 30)
qqnorm(resid(model_inv)); qqline(resid(model_inv))


glm1 <- glm(Duration_Rate ~ 1, 
            family = Gamma(link = "log"),
            data = alert)

glm2 <- glm(Duration_Rate ~ Week,
            family = Gamma(link = "log"),
            data = alert)

glm3 <- glm(Duration_Rate ~ Strategy,
            family = Gamma(link = "log"),
            data = alert)

glm4 <-  glm(Duration_Rate ~ Habitat,
            family = Gamma(link = "log"),
            data = alert)

glm5 <- glm(Duration_Rate ~ Tide,
            family = Gamma(link = "log"),
            data = alert)

glm_polint <- glm(Duration_Rate ~ poly(Week, 2) * Strategy + 
                        Habitat, 
                      family = Gamma(link = "log"),
                      data = alert)

glm_pol <- glm(Duration_Rate ~ poly(Week, 2) + Strategy + 
                    Habitat, 
                  family = Gamma(link = "log"),
                  data = alert)

glm_pol2 <- glm(Duration_Rate ~ poly(Week, 3) + Strategy + 
                 Habitat, 
               family = Gamma(link = "log"),
               data = alert)

model.sel(glm1, glm2, glm3, glm4, glm5)

glmerfull <- glmer(Duration_Rate ~ Week + Strategy + (1|Three_letter_code) +
                     (1|Habitat) + (1|Tide),
                   family = Gamma(link = "log"),
                   data = alert,
                   control = glmerControl(optimizer = "bobyqa", 
                                          optCtrl = list(maxfun = 2e5)))

glmer_red1 <- glmer(Duration_Rate ~ Week + Strategy + (1|Habitat) + (1|Tide),
                    family = Gamma(link = "log"),
                    data = alert,
                    control = glmerControl(optimizer = "bobyqa", 
                                           optCtrl = list(maxfun = 2e5)))

glmer_red2 <- glmer(Duration_Rate ~ Week + Strategy + (1|Habitat), 
                    family = Gamma(link = "log"),
                    data = alert,
                    control = glmerControl(optimizer = "bobyqa", 
                                           optCtrl = list(maxfun = 2e5)))

glmer_red3 <- glmer(Duration_Rate ~ (1|Habitat), 
                    family = Gamma(link = "log"),
                    data = alert,
                    control = glmerControl(optimizer = "bobyqa", 
                                           optCtrl = list(maxfun = 2e5)))

glmer_pol1 <- glmer(Duration_Rate ~ poly(Week, 2) + Strategy + (1 | Habitat), 
                    family = Gamma(link = "log"),
                    data = alert,
                    control = glmerControl(optimizer = "bobyqa", 
                                           optCtrl = list(maxfun = 2e5)))

glmer_polint1 <- glmer(Duration_Rate ~ poly(Week, 2) * Strategy + 
                         (1 | Habitat), 
                    family = Gamma(link = "log"),
                    data = alert,
                    control = glmerControl(optimizer = "bobyqa", 
                                           optCtrl = list(maxfun = 2e5)))

glmer_polint2 <- glmer(Duration_Rate ~ poly(Week,2) * Strategy + Habitat +
                       (1|Three_letter_code),
                    family = Gamma(link = "log"),
                    data = alert,
                    control = glmerControl(optimizer = "bobyqa", 
                                           optCtrl = list(maxfun = 2e5)))

glmer_spline <- glmer(Duration_Rate ~ Strategy * ns(Week, df = 3) 
                      + (1 | Habitat),
                      family = Gamma(link = "log"),
                      data = alert,
                      control = glmerControl(optimizer = "bobyqa", 
                                             optCtrl = list(maxfun = 2e5)))

glmer_polint3 <- glmer(Duration_Rate ~ poly(Week, 5) * Strategy + 
                         (1 | Habitat), 
                       family = Gamma(link = "log"),
                       data = alert,
                       control = glmerControl(optimizer = "bobyqa", 
                                              optCtrl = list(maxfun = 2e5)))

alert_model <- model.sel(glmerfull, glmer_red1, glmer_red2, glmer_red3, glmer_pol1, glmer_polint1, glmer_polint2, glm1, glm2, glm3, glm4, glm5, glmer_spline, glmer_polint3, glm_polint, glm_pol, glm_pol2)
alert_model

summary(glm_pol)

alert_model_df <- as.data.frame(alert_model)
alert_model_df$model <- rownames(alert_model_df)
alert_model_df <- alert_model_df[, c("model", setdiff(names(alert_model_df), "model"))]
head(alert_model_df)
# Save to CSV

write.csv(alert_model_df, "alert_model_selection_table.csv", row.names = FALSE)

newdata_alert <- expand.grid(
  Week = seq(min(alert$Week), max(alert$Week), 
             length.out = 100),
  Strategy = unique(alert$Strategy),
  Habitat = unique(alert$Habitat))

# Get predictions on  with confidence intervals
pred_alert <- predict(glm_pol, newdata_alert, type = "response", re.form = NA)

newdata_alert$fit <- pred_alert

pred_summary_alert <- newdata_alert |>
  group_by(Week, Strategy) |>
  summarise(
    fit = mean(fit),
    .groups = "drop")

p_alert <- ggplot(alert, aes(x = Week, y = Duration_Rate)) +
  geom_boxplot(aes(group = interaction(Week, Strategy), fill = Strategy), 
               position = position_dodge(width = 0.8)) +
  geom_line(data = pred_summary_alert, aes(x = Week, y = fit, color = Strategy, 
                                          group = Strategy), size = 1.0, inherit.aes = FALSE) +
  # geom_ribbon(data = pred_summary_walk, aes(x = Week, ymin = lwr, ymax = upr, 
  #                                    fill = Strategy), alpha = 0.2, 
  #           inherit.aes = FALSE) +
  geom_text(data = counts_stage |> filter(Behavior == "Alert"), 
            aes(x = Week, y = 0.23, label = n, group = Strategy, 
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
    y = "Relative time spend being alert (s)",
    x = "Week",
    title = "Alertness per Strategy") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    axis.title.x = element_text(size = 21),
    axis.title.y = element_text(size = 21),
    plot.title = element_text(size = 25, face = "bold", hjust = 0.5),
    legend.position = "none")
p_alert

# Loadpackages
library(emmeans)
library(multcompView)

# Pairwise comparison
emm_alert <- emm <- emmeans(glm_pol, ~ Strategy | Week , type = "response")

# Use cld to assign group letters
cld_alert <- cld(emm_alert, adjust = "tukey", Letters = letters, 
                type = "response")

# View result
print(cld_alert)


ggsave("alert_rate_per_strategy.png", plot = p_alert, width = 28, height = 10, dpi = 300)

################################################################################
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
#############################################################################
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

ggplot(stage_behavior |> filter(Behavior == "Handling_prey"), 
       aes(x = Duration_Rate)) +
  geom_histogram(bins = 30, fill = "#69b3a2", color = "black") +
  labs(
    title = "Distribution of Walking Durations",
    x = "Walking Duration (seconds)",
    y = "Count"
  ) +
  theme_minimal()

handling <- stage_behavior |> 
  filter(Behavior == "Handling_prey") |> 
  mutate(
    Duration_Rate_log = log(Duration_Rate + 1),       
    Duration_Rate_sqrt = sqrt(Duration_Rate))

hist(handling$Duration_Rate_log, main = "Log-transformed")
hist(handling$Duration_Rate_sqrt, main = "Square-root-transformed")

model_logref <- lmer(Duration_Rate_log~ Week * Strategy + (1 | Three_letter_code) + 
                       (1 | Habitat) + (1 | Tide), 
                     data = handling)

model_squared <- lmer(Duration_Rate_sqrt ~ Week * Strategy 
                      + (1 | Three_letter_code) + (1 | Habitat) + (1 | Tide) 
                      , data = handling)

AIC(model_logref, model_squared)
plot(model_logref)
plot(model_squared)
qqnorm(resid(model_logref)); qqline(resid(model_logref))
qqnorm(resid(model_squared)); qqline(resid(model_squared))

shapiro.test(resid(model_logref))
hist(resid(model_logref), breaks = 30)

lm1 <- lm(Duration_Rate_log ~ 1, 
            data = handling)

lm2 <- lm(Duration_Rate_log ~ Week,
            data = handling)

lm3 <- lm(Duration_Rate_log ~ Strategy,
            data = handling)

lm4 <- lm(Duration_Rate_log ~ Habitat,
            data = handling)

lm5 <- lm(Duration_Rate_log ~ Tide,
            data = handling)

model.sel(lm1, lm2, lm3, lm4, lm5)

lm_pol1 <- lm(Duration_Rate_log ~ poly(Week, 2) + Strategy + Tide, 
                   data = handling)

lm_polint <- lm(Duration_Rate_log ~ poly(Week, 3) * Strategy + Tide, 
                   data = handling)

lm_pol2 <- lm(Duration_Rate_log ~ poly(Week, 3) + Strategy + Tide, 
                   data = handling)

lm_int <- lm(Duration_Rate_log ~ Week * Strategy + Tide, 
                   data = handling)

lm_full <- lm(Duration_Rate_log ~ Week + Strategy + Habitat + Tide, 
                   data = handling)

lmerfull <- lmer(Duration_Rate_log ~ Week + Strategy + (1|Three_letter_code) +
                     (1|Habitat) + (1|Tide),
                   data = handling)

lmer_red1 <- lmer(Duration_Rate_log ~ Week + Strategy + (1|Habitat) + (1|Tide),
                  data = handling)

lmer_pol1 <- lmer(Duration_Rate_log ~ poly(Week, 2) + Strategy + (1 |Tide), 
                   data = handling)

lmer_polint1 <- lmer(Duration_Rate_log ~ poly(Week, 2) * Strategy +
                         (1 | Tide), 
                 data = handling)

handling_model <- model.sel(lmerfull, lmer_red1, lmer_pol1, lmer_polint1, lm1, lm2, lm3, lm4, lm5, lm_pol1, lm_polint, lm_pol2, lm_int, lm_full)
handling_model
anova(lmerfull, lmer_red1, lmer_pol1, lmer_polint1)
anova(lm_pol1, lm_polint, lm_pol2, lm_int, lm_full, test = "Chisq")

plot(residuals(lm_pol1))

handling_model_df <- as.data.frame(handling_model)
handling_model_df$model <- rownames(handling_model_df)
handling_model_df <- handling_model_df[, c("model", setdiff(names(handling_model_df), "model"))]
head(handling_model_df)
# Save to CSV

write.csv(handling_model_df, "handling_model_selection_table.csv", row.names = FALSE)

# Create new data for prediction (adjust as needed)
newdata_handling <- expand.grid(
  Week = seq(min(handling$Week), max(handling$Week), 
             length.out = 100),
  Strategy = unique(handling$Strategy),
  Tide = unique(handling$Tide))

# Get predictions on  with confidence intervals
pred_handling <- predict(lm_pol1, newdata_handling, interval = "confidence")

newdata_handling$fit <- exp(pred_handling[,"fit"]) - 1
newdata_handling$lwr <- exp(pred_handling[,"lwr"]) - 1
newdata_handling$upr <- exp(pred_handling[,"upr"]) - 1

pred_summary_handling <- newdata_handling |>
  group_by(Week, Strategy) |>
  summarise(
    fit = mean(fit),
    lwr = mean(lwr),
    upr = mean(upr),
    .groups = "drop")

p_handling <- ggplot(handling, aes(x = Week, y = Duration_Rate)) +
  geom_boxplot(aes(group = interaction(Week, Strategy), fill = Strategy), 
               position = position_dodge(width = 0.8)) +
  geom_line(data = pred_summary_handling, aes(x = Week, y = fit, color = Strategy, 
                                          group = Strategy), size = 1.0, inherit.aes = FALSE) +
 # geom_ribbon(data = pred_summary_handling, aes(x = Week, ymin = lwr, ymax = upr, 
  #                                    fill = Strategy), alpha = 0.2, 
   #          inherit.aes = FALSE) +
  geom_text(data = counts_stage |> filter(Behavior == "Handling_prey"), 
            aes(x = Week, y = 0.5, label = n, group = Strategy, 
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
    y = "Relative time spend handling prey (s)",
    x = "Week",
    title = "Handling prey behaviour per Strategy") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    axis.title.x = element_text(size = 21),
    axis.title.y = element_text(size = 21),
    plot.title = element_text(size = 25, face = "bold", hjust = 0.5),
    legend.position = "none")
p_handling

# Loadpackages
library(emmeans)
library(multcompView)

# Pairwise comparison
emm_handling <- emm <- emmeans(lm_pol1, ~ Strategy | Week , type = "response")

# Use cld to assign group letters
cld_handling <- cld(emm_handling, adjust = "tukey", Letters = letters, 
                type = "response")

# View result
print(cld_handling)


ggsave("handling_rate_per_strategy.png", plot = p_handling, width = 28, height = 10, dpi = 300)

