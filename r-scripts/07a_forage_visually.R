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

# Base boxplot
 # Create a new variable for combined Strategy and Habitat
 visual$Strat_Hab <- interaction(visual$Strategy, visual$Habitat, sep = " - ")
print(visual)

 # Define custom colors (expand as needed based on your combinations)
 custom_colors <- c(
   "early_northward_migration - Land" = "#d73027",
   "early_northward_migration - Water" = "#fc8d59",
   "early_northward_migration - Combination" = "#fee090",
   "late_northward_migration - Land" = "#91bfdb",
   "late_northward_migration - Water" = "#4575b4",
   "late_northward_migration - Combination" = "#74add1",
   "overwinterer - Land" = "#1b7837",
   "overwinterer - Water" = "#a6dba0",
   "overwinterer - Combination" = "#d9f0d3"
 )
 
 p1 <- ggplot(visual |> filter(visually_foraging < 200),
              aes(x = as.factor(Week), y = visually_foraging,
                  fill = Strat_Hab)) +
   geom_boxplot(position = position_dodge(width = 0.9)) +
   scale_fill_manual(values = custom_colors) +
   theme_minimal(base_size = 14) +
   labs(
     title = "Visually foraging by Strategy and Habitat (within Week)",
     x = "Week",
     y = "Duration of visually foraging in s",
     fill = "Strategy - Habitat"
   ) +
   theme(
     axis.text.x = element_text(size = 12),
     axis.text.y = element_text(size = 12),
     axis.title.x = element_text(size = 14),
     axis.title.y = element_text(size = 14),
     plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
     legend.position = "right"
   )
 
 
p1

p1 <- ggplot(visual |> filter(visually_foraging < 200),
             aes(x = as.factor(Week), y = visually_foraging, 
                 fill = Strategy)) +
  geom_boxplot(position = position_dodge(width = 0.75)) +
  scale_fill_manual(values = c("#FF9999", "#1ED760", "#66B3FF")) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Visually foraging by Strategy and Habitat",
    x = "Week",
    y = "Duration of visually foraging in s") +
  theme(
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    legend.position = "top"
  ) +
  facet_wrap(~ Habitat)

# If you want the counts on top:
p2 <- p1 + geom_text(data = counts,
                     aes(x = as.factor(Week), y = 140, label = n),
                     position = position_dodge(width = 0.9), size = 4)
p2


# Add text labels with counts above boxes
#p2 <- p1 + geom_text(data = counts, aes(x = as.factor(Week), 
#                                        y = 140, 
#                                        label = paste0(n)),
#                     position = position_dodge(width = 1.00), 
#                                          size = 4)
#p2

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

model.sel(glm8, glm9, glm13, glm16)
model_selection <-model.sel(glmer1, glmer2, glmer3, glmer4, glmer5, glmer6, glmer7, glm1, glm2, glm3, glm4, glm5, glm6, glm7, glm8, glm9, glm10, glm11, glm12, glm13, glm14, glm15, glm16)
model_selection

# Load necessary package
library(MuMIn)

# Assuming 'model.sel' object is named 'model_selection'
# Example: model_selection <- model.sel(glm13, glm11, glm12, ...)

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


#install.packages("ggeffects")
#library(ggeffects)
#pred <- ggpredict(glm13, terms = c("Week", "Strategy", "Habitat"))

#visual$Strategy <- factor(visual$Strategy, levels = c(
#  "early_northward_migration", "late_northward_migration", #"overwinterer"
#))

#pred$group <- factor(pred$group, levels = c(
#  "early_northward_migration", "late_northward_migration", #"overwinterer"
#))

#ggplot(visual, aes(x = as.factor(Week), y = visually_foraging)) +
#  geom_boxplot(aes(fill = Strategy), alpha = 0.5) +
  
  # Prediction lines and confidence ribbons
#  geom_ribbon(data = pred, 
#              aes(x = as.factor(x), ymin = conf.low, ymax = conf.high, #fill = group), 
#              alpha = 0.2, inherit.aes = FALSE) +
  
##  geom_line(data = pred, 
#            aes(x = as.factor(x), y = predicted, color = group, group #= group), 
#            size = 1.2, inherit.aes = FALSE) +
  
#  labs(y = "Duration of visually foraging (s)", 
#       x = "Week", 
#       fill = "Strategy", 
#       color = "Strategy") +
  
#  theme_minimal() +
#  theme(legend.position = "right")



#new_data <- unique(behaviors[, c("Week", "Strategy", "Transect_ID", #"Habitat", "Tide", "Three_letter_code")])
#new_data$predicted <- exp(predict(glm13, newdata = new_data, re.form = #~ 0, type = "response"))
#new_data <- new_data |>
#  filter(!Week %in% c("9", "11", "12", "13", "15"))

#new_data$Strategy <- factor(new_data$Strategy, levels = c(
#  "early_northward_migration", "late_northward_migration", #"overwinterer"
#))

library(dplyr)
library(stringr)

# Clean Strategy and Habitat in `visual`
visual <- visual %>%
  mutate(
    Strategy = str_trim(as.character(Strategy)),
    Strategy = na_if(Strategy, ""),
    Habitat = str_trim(as.character(Habitat)),
    Habitat = na_if(Habitat, "")
  ) %>%
  filter(!is.na(Strategy), !is.na(Habitat))

# Set factor levels explicitly
visual$Strategy <- factor(visual$Strategy, levels = c(
  "early_northward_migration", "late_northward_migration", "overwinterer"
))
visual$Habitat <- factor(visual$Habitat)


# Step 1: Create the data
new_data_smooth <- expand.grid(
  Week = seq(min(visual$Week), max(visual$Week), length.out = 100),
  Strategy = levels(visual$Strategy),
  Habitat = unique(visual$Habitat)
)

# Step 2: Apply factor levels *after* creation
new_data_smooth$Strategy <- factor(new_data_smooth$Strategy, levels = levels(visual$Strategy))
new_data_smooth$Habitat <- factor(new_data_smooth$Habitat, levels = levels(visual$Habitat))

# Step 3: Predict
pred_smooth <- predict(glm13, newdata = new_data_smooth, type = "link", se.fit = TRUE)


# Back-transform (exponentiate because you log-transformed the response)
new_data_smooth$predicted <- exp(pred_smooth$fit)
new_data_smooth$lower <- exp(pred_smooth$fit - 1.96 * pred_smooth$se.fit)
new_data_smooth$upper <- exp(pred_smooth$fit + 1.96 * pred_smooth$se.fit)

library(stringr)
library(dplyr)

# Clean up Strategy values
visual <- visual %>%
  mutate(
    Strategy = str_trim(as.character(Strategy)),
    Strategy = factor(Strategy, levels = c(
      "early_northward_migration", "late_northward_migration", "overwinterer"
    ))
  )

new_data_smooth <- new_data_smooth %>%
  mutate(
    Strategy = str_trim(as.character(Strategy)),
    Strategy = factor(Strategy, levels = levels(visual$Strategy))
  )

p3 <- ggplot(visual, aes(x = Week, y = visually_foraging)) +
  # Boxplot with discrete x-axis and dodge by Strategy
  geom_boxplot(aes(group = interaction(Week, Strategy), fill = Strategy), 
               position = position_dodge(width = 0.8)) +
  
   # Prediction lines
  geom_line(
    data = new_data_smooth,
    aes(x = Week, y = predicted, 
        color = Strategy, group = interaction(Strategy, Habitat)),
    size = 1.0,
    inherit.aes = FALSE
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
  
  # Facet by Habitat
  facet_wrap(~ Habitat) +
  
  # Labels and theme
  labs(
    y = "Duration of visually foraging (s)",
    x = "Week",
    title = "Visually foraging by Strategy across Habitats"
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
  
#p3 <- p2 +
#  geom_line(
#    data = new_data,
#    aes(
#      x = as.factor(Week),
#      y = predicted,
#    shape = Strategy,
#      group = Strategy,
#      color = Strategy
#    ),
#    size = 1,
#    position = position_dodge(width = 0.6)
#  )  +
#  scale_color_manual(values = c(
#    "overwinterer" = "#5190cf",
#    "late_northward_migration" = "#0FA85B",
#    "early_northward_migration" = "#c77575"  # and here
#  )) +
#  theme_minimal() +
#  theme(
#    axis.text.x = element_text(size = 12),       # X-axis numbers
#    axis.text.y = element_text(size = 12),       # Y-axis labels
#    axis.title.x = element_text(size = 14),      # X-axis title
#    axis.title.y = element_text(size = 14),      # Y-axis title
#    plot.title = element_text(size = 16, face = "bold", hjust = 0.5), #Centered & bold title
#    legend.position = "none")
#  
#p3

# Load required packages
library(emmeans)
library(multcompView)

# Compute estimated marginal means by Strategy, within each Week × Habitat combination
emm <- emmeans(glm13, ~ Strategy | Week * Habitat, type = "response")

# Pairwise comparisons of Strategy within each Week × Habitat group
pairwise_results <- pairs(emm, adjust = "tukey", type = "response")
pairwise_summary <- summary(pairwise_results)

# Print summary of comparisons
print(pairwise_summary)

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


#p4 <- p3 +
#  geom_text(data = data.frame(
#    Week = factor(c(13, 16, 17, 18, 19, 20, 21)),
#    label = c("***", "***", "***", "***", "***", "***", "***"),
#    y = c(115, 120, 118, 118, 130, 115, 120)
#  ),
#  aes(x = Week, y = y, label = label),
#  vjust = -0.5,
#  size = 4,
#  inherit.aes = FALSE)  # <--- This is the key!
#p4


#ggsave("foraging_walking_duration_strategy.png", plot = p2, width = 13, height = 6, dpi = 300)
ggsave("foraging_walking_duration_strategy_with_predictions.png", plot = p3, width = 13, height = 6, dpi = 300)

