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
  filter(!Week %in% c("9", "11","12", "13", "15"))


## filtered out because there was no visually foraging so it became negeative. But instead of filtering out need to make it to 0!
behaviors <- behaviors |>
  filter(!Observation_id %in% c("KKH_15.04", "KUX_15.04")) |>
  filter(!Week %in% c("9", "11", "15"))
visual <- behaviors |>
  select(Observation_id, Week, Date.x, Transect_ID, Three_letter_code, Tide, Habitat, Social_behavior, Strategy, visually_foraging) 

behaviors$Tide <- behaviors$Tide |>
  str_trim()
behaviors$Habitat <- behaviors$Habitat |> str_trim()
 visual <- visual |>
   distinct()
 visual <- visual |> 
   distinct(Three_letter_code, Week, .keep_all = TRUE) |>
   filter(!Week %in% c("12", "13"))

# Base boxplot
p1 <- ggplot(visual |> filter(visually_foraging < 200),
             aes(x = as.factor(Week), y = visually_foraging, 
                 fill = Strategy)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#FF9999", "#1ED760", "#66B3FF")) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Visually foraging by Strategy",
    x = "Week",
    y = "Duration of visually foraging in s") +
  theme(
      axis.text.x = element_text(size = 12),       # X-axis numbers
      axis.text.y = element_text(size = 12),       # Y-axis labels
      axis.title.x = element_text(size = 14),      # X-axis title
      axis.title.y = element_text(size = 14),      # Y-axis title
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # Centered & bold title
      legend.position = "none"
    )

p1

# Add text labels with counts above boxes
p2 <- p1 + geom_text(data = counts, aes(x = as.factor(Week), 
                                        y = 140, 
                                        label = paste0(n)),
                     position = position_dodge(width = 1.00), size = 4)
p2

# The graph shows some differences in duration of foraging walking per strategy.
# So it would be interesting to analyse this further 


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

#### So data is not normally distributed, need to do choose another model than lm, use glm instead
glm1 <- glm(visually_foraging ~ 1 , family = Gamma(link = "log"),
            data = visual)
summary(glm1)

glm2 <- glm(visually_foraging ~ Week, family = Gamma(link = "log"),
            data = visual)
summary(glm2)

glm3 <- glm(visually_foraging ~ Strategy, 
              family = Gamma(link = "log"),
            data = visual)
summary(glm3)

glm4 <- glm(visually_foraging ~ Transect_ID, 
              family = Gamma(link = "log"),
            data = visual)
summary(glm4)

glm5 <- glm(visually_foraging ~ Habitat, 
              family = Gamma(link = "log"),
            data = visual)
summary(glm5)

glm6 <- glm(visually_foraging ~ Tide, 
              family = Gamma(link = "log"),
            data = visual)
summary(glm6)

model.sel(glm1, glm2, glm3, glm4, glm5, glm6)

glmer1 <- glmer(visually_foraging ~ Week * Strategy + 
                  (1 | Three_letter_code) + (1 | Habitat) + 
                  (1 | Tide) + (1 | Transect_ID),
                family = Gamma(link = "log"),
                data = visual,
                control = glmerControl(optimizer = "bobyqa", 
                                       optCtrl = list(maxfun = 1e5)))
summary(glmer1)

glmer2 <- glmer(visually_foraging ~ Week * Strategy + 
          (1 | Habitat) + (1|Tide) + (1 | Transect_ID), 
                family = Gamma(link = "log"),
                data = visual,
                control = glmerControl(optimizer = "bobyqa", 
                                       optCtrl = list(maxfun = 1e5)))
summary(glmer2)

glmer3 <- glmer(visually_foraging ~ Week * Strategy + 
             (1 | Tide) + (1 | Transect_ID), 
                family = Gamma(link = "log"),
                data = visual,
                control = glmerControl(optimizer = "bobyqa", 
                                       optCtrl = list(maxfun = 1e5)))
summary(glmer3)

glmer4 <- glmer(visually_foraging ~ Week * Strategy + 
                (1 | Transect_ID) + (1 | Habitat),
                family = Gamma(link = "log"),
                data = visual,
                control = glmerControl(optimizer = "bobyqa", 
                                       optCtrl = list(maxfun = 1e5)))
summary(glmer4)

glmer5 <- glmer(visually_foraging ~ Week * Strategy + (1 | Tide), 
                family = Gamma(link = "log"),
                data = visual,
                control = glmerControl(optimizer = "bobyqa", 
                                       optCtrl = list(maxfun = 1e5)))
summary(glmer5)

glmer6 <- glmer(visually_foraging ~Week * Strategy + (1 | Transect_ID),
                family = Gamma(link = "log"),
                data = visual,
                control = glmerControl(optimizer = "bobyqa", 
                                       optCtrl = list(maxfun = 1e5)))
summary(glmer6)

glmer7 <- glmer(visually_foraging ~ Week * Strategy + 
                  (1 | Habitat), 
                family = Gamma(link = "log"),
                data = visual,
                control = glmerControl(optimizer = "bobyqa", 
                                       optCtrl = list(maxfun = 1e5)))
summary(glmer7)

AIC(glmer1, glmer2, glmer3, glmer4, glmer5, glmer6, glmer7)
model.sel(glmer1, glmer2, glmer3, glmer4, glmer5, glmer6, glmer7, glm1, glm2, glm3, glm4, glm5, glm6)

glm7 <- glm(visually_foraging ~ Week * Strategy + Habitat,
            family = Gamma(link = "log"),
            data = visual)
summary(glm7)

glm8 <- glm(visually_foraging ~ Week + Habitat,
            family = Gamma(link = "log"),
            data = visual)
summary(glm8)

model.sel(glm5, glm7, glm8)

glm9 <- glm(visually_foraging ~ Week + Habitat + Strategy,
            family = Gamma(link = "log"),
            data = visual)
summary(glm9)

glm10 <- glm(visually_foraging ~ Week * Strategy,
             family = Gamma(link = "log"),
             data = visual)
summary(glm10)

model.sel(glm8, glm9, glm10)

plot(residuals(glm8))
glm8_log <- glm(log(visually_foraging) ~ Week + Habitat, data = visual)
plot(residuals(glm8_log))

glm11<- glm(log(visually_foraging) ~ Week + Habitat, data = visual, family = gaussian())
summary(glm11)

glm12<- glm(log(visually_foraging) ~ Week + Habitat + Strategy, 
             data = visual, family = gaussian())
summary(glm12)

glm13 <- glm(log(visually_foraging) ~ Week * Strategy * Habitat, 
             data = visual, family = gaussian())
summary(glm13)

model.sel(glm11, glm12, glm13, glm8)
plot(residuals(glm13))

glm14 <- glm(visually_foraging ~ poly(Week, 2) * Strategy + Habitat,
                family = Gamma(link = "log"), data = visual)

glm15 <- glm(visually_foraging ~ splines::ns(Week, df = 4) * Strategy + Habitat,
                  family = Gamma(link = "log"), data = visual)

model.sel(glm8, glm14, glm15)

glm16 <- glm(visually_foraging ~ Habitat * Strategy + Week, 
             family = Gamma(link = "log"), data = visual)

model.sel(glm8, glm9, glm16)

install.packages("ggeffects")
library(ggeffects)
pred <- ggpredict(glm16, terms = c("Week", "Strategy"))

ggplot(visual, aes(x = as.factor(Week), y = visually_foraging)) +
  geom_boxplot(aes(fill = Strategy), alpha = 0.5) +
  
  # Prediction lines and confidence ribbons
  geom_ribbon(data = pred, 
              aes(x = as.factor(x), ymin = conf.low, ymax = conf.high, fill = group), 
              alpha = 0.2, inherit.aes = FALSE) +
  
  geom_line(data = pred, 
            aes(x = as.factor(x), y = predicted, color = group, group = group), 
            size = 1.2, inherit.aes = FALSE) +
  
  labs(y = "Duration of visually foraging (s)", 
       x = "Week", 
       fill = "Strategy", 
       color = "Strategy") +
  
  theme_minimal() +
  theme(legend.position = "right")



new_data <- unique(behaviors[, c("Week", "Strategy", "Transect_ID", "Habitat", "Tide", "Three_letter_code")])
new_data$predicted <- predict(glm13, newdata = new_data, re.form = ~ 0, type = "response")
new_data <- new_data |>
  filter(!Week %in% c("9", "11", "12", "13", "15"))

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
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 12),       # X-axis numbers
    axis.text.y = element_text(size = 12),       # Y-axis labels
    axis.title.x = element_text(size = 14),      # X-axis title
    axis.title.y = element_text(size = 14),      # Y-axis title
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # Centered & bold title
    legend.position = "none"
  )
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


ggsave("foraging_walking_duration_strategy.png", plot = p2, width = 13, height = 6, dpi = 300)
ggsave("foraging_walking_duration_strategy_with_predictions.png", plot = p4, width = 13, height = 6, dpi = 300)

