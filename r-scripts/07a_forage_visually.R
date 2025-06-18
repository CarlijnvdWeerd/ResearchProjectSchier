## Run the script 07_BORIS_forage first, then this script will work
renv::restore()

# Fill in the "gaps" 
behaviors <- behaviors |>
  group_by(Three_letter_code, Observation_id) |>
  mutate(total_duration = sum(Duration, na.rm = TRUE)) |>
  mutate(total_duration = ifelse(is.na(total_duration), 0, 
                                 total_duration), gap_time = Media.duration..s. - total_duration) |>
  rename(visually_foraging = gap_time) 

# Calculate counts per Week and Strategy
counts <- behaviors |>
  group_by(Week, Strategy) |>
  summarise(n = n_distinct(Observation_id), .groups = "drop") |>
  filter(!Week %in% c("9", "11", "15"))


## explain why filtered out! 
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

glmer1 <- glmer(visually_foraging ~ Week + Strategy
                + (1 | Three_letter_code),
                family = Gamma(link = "log"),
                data = behaviors,
                control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
summary(glmer1)
# max|grad| = 0.00243058 (tol = 0.002, component 1)

glmer2 <- glmer(visually_foraging ~ Week + Strategy + 
                  Week*Strategy + (1|Three_letter_code), 
                family = Gamma(link = "log"),
                data = behaviors,
                control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
summary(glmer2)
# max|grad| = 0.00461877 (tol = 0.002, component 1)

AIC(glmer1, glmer2)

glmer3 <- glmer(visually_foraging ~Week * Strategy + (1 | Three_letter_code) ,
                family = Gamma(link = "log"),
                data = behaviors,
                control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
summary(glmer3)
### max|grad| = 0.00461877 (tol = 0.002, component 1)

AIC(glmer1, glmer3)


#### GLM 3 shows best AIC with 324637.6!!!!!!!!Only one with Three_letter_code as fixed effect (?) NO use as random effect!

new_data <- unique(behaviors[, c("Week", "Strategy", "Transect_ID", "Habitat", "Tide", "Three_letter_code")])
new_data$predicted <- predict(glmer3, newdata = new_data, re.form = ~ 0, type = "response")

new_data$Strategy <- factor(new_data$Strategy, levels = c(
  "early_northward_migration", "late_northward_migration", "overwinterer"
))

p3 <- p2 +
  geom_line(
    data = new_data,
    aes(
      x = as.factor(Week),
      y = predicted,
      #    shape = Strategy,
      group = Strategy,
      color = Strategy
    ),
    size = 2,
    #   position = position_dodge(width = 0.6)
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
## Can not find back *** !!??

ggsave("foraging_walking_duration_strategy.png", plot = p2, width = 13, height = 6, dpi = 300)
ggsave("foraging_walking_duration_strategy_with_predictions.png", plot = p4, width = 13, height = 6, dpi = 300)

