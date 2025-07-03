## Run the script 07_BORIS_forage first, then this script will work
renv::restore()

# Calculate counts per Week and Strategy
counts <- behaviors |>
  group_by(Week, Strategy) |>
  summarise(n = n_distinct(Observation_id), .groups = "drop") |>
  filter(!Week %in% c("9", "11","12", "13", "15"))


## filtered out because there was no visually foraging so it became negeative. But instead of filtering out need to make it to 0!
behaviors <- behaviors |>
  filter(!Observation_id %in% c("KKH_15.04", "KUX_15.04")) |>
  filter(!Week %in% c("9", "11", "15"))
visual <- behaviors |>
 dplyr::select(Observation_id, Week, Date.x, Transect_ID, Three_letter_code, Tide, Habitat, Social_behavior, Strategy, visually_foraging) 


behaviors$Tide <- behaviors$Tide |>
  str_trim()
behaviors$Habitat <- behaviors$Habitat |> str_trim()
 visual <- visual |>
   distinct()
 visual <- visual |> 
   distinct(Three_letter_code, Week, .keep_all = TRUE) |>
   filter(!Week %in% c("12", "13"))


 
# Check for normality of visually_foraging
ggplot(visual, aes(x = visually_foraging)) +
  geom_histogram(bins = 30, fill = "#69b3a2", color = "black") +
  labs(
    title = "Distribution of Visually Foraging Durations",
    x = "Visually Foraging Duration (seconds)",
    y = "Count"
  ) +
  theme_minimal()

qqnorm(visual$visually_foraging)

# Check for residuals
ggplot(visual, aes(x = fitted(lm(visually_foraging ~ Strategy, data = visual)), 
                      y = resid(lm(visually_foraging ~ Strategy, data = visual)))) +
  geom_point() +
  geom_smooth(method = "lm", color = "red") +
  labs(
    title = "Residuals vs Fitted Values",
    x = "Fitted Values",
    y = "Residuals"
  ) +
  theme_minimal()

visual$vf_log_reflected <- log(max(visual$visually_foraging, na.rm = TRUE) + 1 - visual$visually_foraging)
visual$vf_squared <- visual$visually_foraging^2

model_logref <- lmer(vf_log_reflected ~ Week * Strategy + (1 | Three_letter_code) + 
                       (1 | Habitat) + (1 | Tide) + (1 | Transect_ID), data = visual)

model_squared <- lmer(vf_squared ~ Week * Strategy + (1 | Three_letter_code) + 
                        (1 | Habitat) + (1 | Tide) + (1 | Transect_ID), data = visual)

AIC(model_logref, model_squared)
plot(model_logref)
plot(model_squared)
qqnorm(resid(model_logref)); qqline(resid(model_logref))
## so use log reflected transformation

lm1 <- lm(vf_log_reflected ~ 1 , 
            data = visual)

lm2 <- lm(vf_log_reflected ~ Week, 
            data = visual)

lm3 <- lm(vf_log_reflected ~ Strategy, 
            data = visual)

lm4 <- glm(vf_log_reflected ~ Transect_ID, 
           data = visual)

lm5 <- lm(vf_log_reflected ~ Habitat,
            data = visual)

lm6 <- lm(vf_log_reflected ~ Tide, 
            data = visual)

lmer1 <- lmer(vf_log_reflected ~ Week * Strategy + 
                  (1 | Three_letter_code) + (1 | Habitat) + 
                  (1 | Tide) + (1 | Transect_ID),
                data = visual)

lmer2 <- lmer(vf_log_reflected ~ Week * Strategy + 
          (1 | Habitat) + (1|Tide) + (1 | Transect_ID),
                data = visual)

lmer3 <- lmer(vf_log_reflected ~ Week * Strategy + 
             (1 | Tide) + (1 | Transect_ID), 
                data = visual)

lmer4 <- lmer(vf_log_reflected ~ Week * Strategy + 
                (1 | Transect_ID) + (1 | Habitat),
                data = visual)

lmer5 <- lmer(vf_log_reflected ~ Week * Strategy + (1 | Tide), 
                data = visual)

lmer6 <- lmer(vf_log_reflected ~ Week * Strategy + (1 | Transect_ID),
                data = visual)

lmer7 <- lmer(vf_log_reflected ~ Week * Strategy + 
                  (1 | Habitat), 
                data = visual)


model.sel(lmer1, lmer2, lmer3, lmer4, lmer5, lmer6, lmer7, lm1, lm2, lm3, lm4, lm5, lm6)

lm7 <- lm(vf_log_reflected ~ Week * Strategy + Habitat,
            data = visual)

lm8 <- lm(vf_log_reflected ~ Week + Habitat,
            data = visual)

model.sel(lm5, lm7, lm8)

lm9 <- lm(vf_log_reflected ~ Week + Habitat + Strategy,
            data = visual)

lm10 <- lm(vf_log_reflected ~ Week * Strategy,
             data = visual)

model.sel(lm8, lm9, lm10)

plot(residuals(lm9))

lm11<- lm(vf_log_reflected ~ Week + Habitat + Strategy, 
             data = visual)

lm12 <- lm(vf_log_reflected ~ Week * Strategy * Habitat, 
             data = visual)
model.sel(lm10, lm11, lm12, lm9)


lm13 <- lm(vf_log_reflected ~ poly(Week, 2) * Strategy + Habitat,
                data = visual)
summary(lm13)

lm14 <- lm(vf_log_reflected ~ splines::ns(Week, df = 4) * Strategy + Habitat,
                  data = visual)

model.sel(lm9, lm13, lm14)

lm15 <- lm(vf_log_reflected ~ Habitat * Strategy + Week, 
              data = visual)

model.sel(lm9, lm13, lm15)
model_selection <-model.sel(lmer1, lmer2, lmer3, lmer4, lmer5, lmer6, lmer7, lm1, lm2, lm3, lm4, lm5, lm6, lm7, lm8, lm9, lm10, lm11, lm12, lm13, lm14, lm15)
model_selection

# Load necessary package
library(MuMIn)

# Convert to data frame for easier manipulation
model_df <- as.data.frame(model_selection)

# Add model names as a column if they aren't already
model_df$model <- rownames(model_df)

# Reorder columns to have model name first
model_df <- model_df[, c("model", setdiff(names(model_df), "model"))]

# View the first few rows
head(model_df)

# Optional: save to CSV
write.csv(model_df, "model_selection_table.csv", row.names = FALSE)

# Create new data for prediction (adjust as needed)
newdata <- expand.grid(
  Week = seq(min(visual$Week), max(visual$Week), length.out = 100),
  Strategy = unique(visual$Strategy),
  Habitat = unique(visual$Habitat)
)

# Get predictions on transformed scale with confidence intervals
pred <- predict(lm13, newdata, interval = "confidence")

# Extract max value from original data
max_val <- max(visual$visually_foraging, na.rm = TRUE)

# Back-transform predicted fit and intervals
newdata$fit <- max_val + 1 - exp(pred[,"fit"])
newdata$lwr <- max_val + 1 - exp(pred[,"lwr"])
newdata$upr <- max_val + 1 - exp(pred[,"upr"])

pred_summary <- newdata |>
  group_by(Week, Strategy) |>
  summarise(
    fit = mean(fit),
    lwr = mean(lwr),
    upr = mean(upr),
    .groups = "drop")


p_visual_forage <- ggplot(visual, aes(x = Week, y = visually_foraging)) +
  geom_boxplot(aes(group = interaction(Week, Strategy), fill = Strategy), 
               position = position_dodge(width = 0.8)) +
  geom_line(data = pred_summary, aes(x = Week, y = fit, color = Strategy, 
                            group = Strategy), size = 1.0, inherit.aes = FALSE) +
  geom_ribbon(data = pred_summary, aes(x = Week, ymin = lwr, ymax = upr, 
                                       fill = Strategy), alpha = 0.2, 
              inherit.aes = FALSE) +
  geom_text(data = counts, aes(x = Week, y = 140, label = n, group = Strategy, 
                               color = "black"),
    position = position_dodge(width = 0.8), size = 4.2, inherit.aes = FALSE) +
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
    y = "Relative time spend visually foraging (s)",
    x = "Week",
    title = "Visually foraging behaviour per Strategy") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    axis.title.x = element_text(size = 21),
    axis.title.y = element_text(size = 21),
    plot.title = element_text(size = 25, face = "bold", hjust = 0.5),
    legend.position = "none")
p_visual_forage

# Loadpackages
library(emmeans)
library(multcompView)

# Pairwise comparison
emm_visual <- emm <- emmeans(lm13, ~ Strategy | Week * Habitat, type = "response")

# Use cld to assign group letters
cld_visual <- cld(emm_visual, adjust = "tukey", Letters = letters, 
                  type = "response")

# View result
print(cld_visual)

ggsave("foraging_walking_duration_strategy_with_predictions.png", plot = p_visual_forage, width = 13, height = 6, dpi = 300)

