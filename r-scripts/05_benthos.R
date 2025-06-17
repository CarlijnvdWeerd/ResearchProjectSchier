library(readxl)
library(tidyverse)
library(cowplot)
library(readxl)
library(car)
library(emmeans)
library(lme4)
library(lmerTest)
library(GGally)
library(tidyverse) 
library(cowplot) 
library(mgcv) 
library(ggplot2)
library(Rmisc)
library(faraway)
library(PMCMRplus)
library(performance)
library(readxl)

################################################# Average length species
B <- read_excel("C:/Users/conso/Downloads/Benthos Determinatie  (1).xlsx", 
                sheet = "FactDeterm")
View(B)

length_stats <- B %>%
  group_by(Species_ID, Date) %>%
  summarise(
    mean_length = mean(Length, na.rm = TRUE),
    sd_length = sd(Length, na.rm = TRUE),
    n = n(),  # optional: count of individuals
    .groups = "drop"
  )

B2 <- ggplot(data = B, mapping=aes(x=factor(Date), y= `Length`, fill = Species_ID)) +
  geom_boxplot() +
  labs(x = "Species_ID", y = "Length")  + 
  scale_colour_discrete(name="") +
  labs(fill = "Treatment")
B2
############################################## Number of each species
custom_order <- c("20 March", "1 April", "4 April", "22 April", "29 April", "13 May")

B$Date <- factor(B$Date, levels = custom_order)

library(dplyr)

species_counts <- B %>%
  group_by(Species_ID) %>%
  summarise(count = n())

species_counts <- species_counts %>%
  arrange(desc(count))

library(dplyr)
species_week_counts <- B %>%
  group_by(Date, Species_ID) %>%
  summarise(count = n(), .groups = "drop")

ggplot(species_week_counts, aes(x = Date, y = count)) +
  geom_col(fill = "steelblue") +
  facet_wrap(~ Species_ID, scales = "free_y") +
  labs(title = "Species Counts per Week (Faceted)",
       x = "Week",
       y = "Count") +
  theme_minimal()

######################################### Number of COR VOL over time 
library(dplyr)
library(ggplot2)

# Filter for 'cor vol' and remove 1 April and 22 April
cor_vol_counts <- B %>%
  filter(Species_ID == "cor vol", !Date %in% c("1 April", "22 April")) %>%
  group_by(Date) %>%
  summarise(count = n())

# Reorder factor levels for plotting
custom_order <- c("20 March", "4 April", "29 April", "13 May")
cor_vol_counts$Date <- factor(cor_vol_counts$Date, levels = custom_order)

# Plot with bars, line, points, and text labels
ggplot(cor_vol_counts, aes(x = Date, y = count, group = 1)) +
  geom_col(fill = "#56B4E9") +                             # Bar
  geom_line(color = "#D55E00", size = 1.2) +               # Line
  geom_point(color = "#D55E00", size = 3) +                # Points
  geom_text(aes(label = count),                           # Add count labels
            vjust = -0.5, size = 4, color = "black") +
  labs(
    title = "Abundance of *Corophium volutator* Over Time",
    x = "Sampling Date",
    y = "Count of Individuals"
  ) +
  theme_minimal()
############################################### Length over time 
length_summary <- B %>%
  group_by(Date, Species_ID) %>%
  summarise(mean_length = mean(Length, na.rm = TRUE), .groups = "drop")

ggplot(B, aes(x = Date, y = Length)) +
  geom_boxplot(fill = "lightblue") +
  facet_wrap(~ Species_ID, scales = "free_y") +
  labs(title = "Length Distribution Over Time by Species",
       x = "Date",
       y = "Length (mm)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("Cor.Vol_Number_plot.png", dpi = 600, width = 8, height = 6, units = "in")
getwd()

library(dplyr)
library(ggplot2)
library(readxl)

# Load your data
B <- read_excel("C:/Users/conso/Downloads/Benthos Determinatie  (1).xlsx", sheet = "FactDeterm")

# Set custom date order and factor
custom_order <- c("20 March", "4 April", "29 April", "13 May")
B$Date <- factor(B$Date, levels = custom_order)

# Filter for 'cor vol' and exclude 1 April and 22 April (not in custom_order)
cor_vol_summary <- B %>%
  filter(Species_ID == "cor vol") %>%
  filter(Date %in% custom_order) %>%
  group_by(Date) %>%
  summarise(
    mean_count = mean(Transect_ID, na.rm = TRUE),
    se = sd(Transect_ID, na.rm = TRUE) / sqrt(n()),
    n = n()
  )

###################### BOXPLOT ALLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL COUNT
library(dplyr)
library(ggplot2)

# Zet Date als factor met levels in chronologische volgorde
B_counts$Date <- factor(B_counts$Date, levels = c("20 March", "4 April", "29 April", "13 May"))
sample_sizes$Date <- factor(sample_sizes$Date, levels = c("20 March", "4 April", "29 April", "13 May"))


species_to_include <- c("cor vol", "ner", "per ulv", "oli", "mac bal")

# Filter de data
B_filtered <- B %>%
  filter(Species_ID %in% species_to_include,
         Date %in% c("20 March", "4 April", "29 April", "13 May"))

# Tel aantal waarnemingen per Date, Transect_ID en Species_ID
B_counts <- B_filtered %>%
  count(Date, Transect_ID, Species_ID, name = "count")

# Zorg dat Date een factor is in de juiste volgorde voor de plot
B_counts$Date <- factor(B_counts$Date, levels = c("20 March", "4 April", "29 April", "13 May"))

# Bereken n = aantal transecten per Date en Species_ID (aantal boxplot-items)
sample_sizes <- B_counts %>%
  group_by(Date, Species_ID) %>%
  summarise(n_transects = n(), .groups = "drop")

sample_sizes$Date <- factor(sample_sizes$Date, levels = c("20 March", "4 April", "29 April", "13 May"))

library(dplyr)
library(ggplot2)

# label_positions met max count per groep
label_positions <- B_counts %>%
  group_by(Species_ID, Date) %>%
  summarise(max_count = max(count, na.rm = TRUE) * 1.1, .groups = "drop")

# label_data met sample sizes en max count voor posities
label_data <- sample_sizes %>%
  left_join(label_positions, by = c("Species_ID", "Date"))

ggplot(B_counts, aes(x = Date, y = count, fill = Species_ID)) +
  geom_boxplot(outlier.shape = NA, position = position_dodge(width = 0.8), alpha = 0.3) +
  geom_jitter(aes(color = Species_ID),
              position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
              alpha = 0.8, size = 2) +
  geom_text(data = label_data,
            aes(x = Date, y = max_count, label = paste0("n=", n_transects)),
            position = position_dodge(width = 0.8),
            inherit.aes = FALSE,
            size = 3) +
  facet_wrap(~ Species_ID, scales = "free_y") +
  labs(
    title = "Count per Transect, Date and Species",
    x = "Date",
    y = "Count per Transect",
    fill = "Species",
    color = "Species"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("SPECIES_BOX_COUNT.png", dpi = 600, width = 8, height = 6, units = "in")

###################### COR VOL
#####################

library(dplyr)

corvol_data <- B_counts %>%
  filter(Species_ID == "cor vol") %>%
  mutate(
    Date = factor(Date, levels = c("20 March", "4 April", "29 April", "13 May")),
    TimeNumeric = as.numeric(Date)
  )

library(ggplot2)

ggplot(corvol_data, aes(x = Date, y = count)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.3, fill = "skyblue") +
  geom_jitter(width = 0.2, alpha = 0.7, size = 2, color = "darkblue") +
  labs(
    title = "Aantal waarnemingen per Transect voor cor vol over tijd",
    x = "Datum",
    y = "Aantal waarnemingen per Transect"
  ) +
  theme_minimal()

corvol_means <- corvol_data %>%
  group_by(Date) %>%
  summarise(
    mean_count = mean(count),
    se = sd(count) / sqrt(n())
  )

ggplot(corvol_means, aes(x = Date, y = mean_count, group = 1)) +
  geom_line(color = "darkblue") +
  geom_point(size = 3, color = "darkblue") +
  geom_errorbar(aes(ymin = mean_count - se, ymax = mean_count + se), width = 0.2, color = "darkblue") +
  labs(
    title = "Gemiddelde waarnemingen per Transect voor cor vol over tijd",
    x = "Datum",
    y = "Gemiddeld aantal waarnemingen per Transect"
  ) +
  theme_minimal()

library(lme4)

corvol_model <- lmer(count ~ TimeNumeric + (1 | Transect_ID), data = corvol_data)
summary(corvol_model)

# QQ-plot van de residualen
qqnorm(residuals(corvol_model))
qqline(residuals(corvol_model))

# Residuen vs fitted plot
plot(fitted(corvol_model), residuals(corvol_model))
abline(h = 0, lty = 2)
library(DHARMa)
simulationOutput <- simulateResiduals(corvol_model)
plot(simulationOutput)

VarCorr(corvol_model)

library(lme4)

# Mixed model (met random effect)
corvol_model_mixed <- lmer(count ~ TimeNumeric + (1 | Transect_ID), data = corvol_data, REML = FALSE)

# Fixed effect model (zonder random effect)
corvol_model_fixed <- lm(count ~ TimeNumeric, data = corvol_data)

# Vergelijk modellen
anova(corvol_model_mixed, corvol_model_fixed)

library(DHARMa)
installed.packages(DHARMa)
simulationOutput <- simulateResiduals(fittedModel = corvol_model)
plot(simulationOutput)

library(ggplot2)
ggplot(corvol_data, aes(x = TimeNumeric, y = count)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, col = "blue") +
  labs(title = "Aantal cor vol per Transect over de tijd",
       x = "Tijd (numeriek)",
       y = "Aantal per Transect")

#######################
###################### PER ULV
library(dplyr)
library(lme4)

# Filter op per ulv en relevante datums
perulv_counts <- B %>%
  filter(Species_ID == "per ulv",
         Date %in% c("20 March", "4 April", "29 April", "13 May")) %>%
  group_by(Date, Transect_ID) %>%
  summarise(count = n(), .groups = "drop")

# Zet Date om naar factor met juiste volgorde
perulv_counts$Date <- factor(perulv_counts$Date, levels = c("20 March", "4 April", "29 April", "13 May"))

# Numerieke tijdvariabele maken
perulv_counts$TimeNumeric <- as.numeric(perulv_counts$Date)

# Fit een Poisson mixed model: count ~ time + (random intercept per transect)
model_count <- glmer(count ~ TimeNumeric + (1 | Transect_ID), 
                     data = perulv_counts, 
                     family = poisson)

summary(model_count)

# Eenvoudige overdispersie-check
overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model, type = "pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq / rdf
  pval <- pchisq(Pearson.chisq, df = rdf, lower.tail = FALSE)
  c(chisq = Pearson.chisq, ratio = prat, rdf = rdf, p = pval)
}

overdisp_fun(model_count)

library(lme4)

model_nb <- glmer.nb(count ~ TimeNumeric + (1 | Transect_ID), data = perulv_counts)
summary(model_nb)

model_nb_fixed <- glm.nb(count ~ TimeNumeric, data = perulv_counts)
summary(model_nb_fixed)

anova(model_nb_fixed, model_nb, test = "Chisq")

# Model met random effect
AIC(model_nb)

# Model zonder random effect
AIC(model_nb_fixed)

library(dplyr)

# Filter voor per ulv
perulv_data <- B %>% filter(Species_ID == "per ulv")

# Tel het aantal per Date en Transect_ID (dus aantal individuen per transect op een datum)
perulv_counts <- perulv_data %>%
  group_by(Date, Transect_ID) %>%
  summarise(count = n(), .groups = "drop")

# Bereken gemiddelde, standaardfout en aantal transecten per Date
summary_perulv <- perulv_counts %>%
  group_by(Date) %>%
  summarise(
    mean_count = mean(count),
    se_count = sd(count) / sqrt(n()),
    n_transects = n(),
    .groups = "drop"
  )

print(summary_perulv)

###############################
##############################



############################################
########################################## BOXPLOT ALLLLLLLLLLLLLLLLLLL Lengte
library(dplyr)
library(ggplot2)

species_to_include <- c("cor vol", "ner", "per ulv", "oli", "mac bal")

# Filter de data
B_filtered <- B %>%
  filter(Species_ID %in% species_to_include,
         Date %in% c("20 March", "4 April", "29 April", "13 May")) %>%
  filter(!is.na(Length))  # filter eventueel NA's eruit

# Zorg dat Date factor is in juiste volgorde
B_filtered$Date <- factor(B_filtered$Date, levels = c("20 March", "4 April", "29 April", "13 May"))

# Bereken n = aantal transecten per Date en Species_ID
sample_sizes <- B_filtered %>%
  group_by(Date, Species_ID) %>%
  summarise(n_transects = n_distinct(Transect_ID), .groups = "drop")

library(dplyr)
library(ggplot2)

# Bereken max y per Species_ID en Date voor labels
label_positions <- B_filtered %>%
  group_by(Species_ID, Date) %>%
  summarise(max_length = max(Length, na.rm = TRUE) * 1.05, .groups = "drop")

# Voeg max_length toe aan sample_sizes
label_data <- sample_sizes %>%
  left_join(label_positions, by = c("Species_ID", "Date"))

ggplot(B_filtered, aes(x = Date, y = Length, fill = Species_ID)) +
  geom_boxplot(outlier.shape = NA, 
               position = position_dodge(width = 0.8),
               alpha = 0.3) +
  geom_jitter(aes(color = Species_ID),
              position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
              alpha = 0.8, size = 2) +
  geom_text(data = label_data,
            aes(x = Date, y = max_length,
                label = paste0("n=", n_transects)),
            position = position_dodge(width = 0.8),
            inherit.aes = FALSE,
            size = 3) +
  facet_wrap(~ Species_ID, scales = "free_y") +
  labs(
    title = "Length per Transect and Date per Species",
    x = "Date",
    y = "Length (mm)",
    fill = "Species",
    color = "Species"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("SPECIES_BOX_LENGTH_facet.png", dpi = 600, width = 10, height = 8, units = "in")

###### ANALYSE ALL SPECIES LENGTH
B_filtered <- B_filtered %>%
  mutate(Date_numeric = as.numeric(Date))  # 1, 2, 3, 4 op basis van levels

library(tidyr)
library(purrr)
library(broom)

model_results <- B_filtered %>%
  group_by(Species_ID) %>%
  nest() %>%
  mutate(
    linear_model = map(data, ~lm(Length ~ Date_numeric, data = .x)),
    quad_model = map(data, ~lm(Length ~ Date_numeric + I(Date_numeric^2), data = .x)),
    linear_glance = map(linear_model, glance),
    quad_glance = map(quad_model, glance),
    model_comparison = map2(linear_model, quad_model, ~anova(.x, .y))
  )

model_results_summary <- model_results %>%
  mutate(
    linear_AIC = map_dbl(linear_model, AIC),
    quad_AIC = map_dbl(quad_model, AIC),
    better_model = ifelse(quad_AIC < linear_AIC, "Quadratic", "Linear"),
    p_value_anova = map_dbl(model_comparison, ~.x$`Pr(>F)`[2])
  ) %>%
  dplyr::select(Species_ID, linear_AIC, quad_AIC, better_model, p_value_anova)
print(model_results_summary)

ggplot(B_filtered %>% filter(Species_ID == "cor vol"), aes(x = Date_numeric, y = Length)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), color = "red", se = FALSE) +
  stat_smooth(method = "lm", formula = y ~ x, color = "blue", linetype = "dashed", se = FALSE) +
  labs(title = "mac bal: Linear vs Quadratic", x = "Date (numeric)", y = "Length")

model_results_details <- model_results %>%
  mutate(
    linear_tidy = map(linear_model, broom::tidy),
    quad_tidy = map(quad_model, broom::tidy),
    linear_glance = map(linear_model, broom::glance),
    quad_glance = map(quad_model, broom::glance),
    linear_resid = map(linear_model, resid),
    quad_resid = map(quad_model, resid)
  )

model_results_details <- model_results_details %>%
  mutate(
    shapiro_linear = map(linear_resid, ~shapiro.test(.x)$p.value),
    shapiro_quad = map(quad_resid, ~shapiro.test(.x)$p.value)
  )
summary_table <- model_results_details %>%
  transmute(
    Species_ID,
    linear_R2 = map_dbl(linear_glance, ~.x$r.squared),
    quad_R2 = map_dbl(quad_glance, ~.x$r.squared),
    linear_p_shapiro = unlist(shapiro_linear),
    quad_p_shapiro = unlist(shapiro_quad)
  )
print(summary_table)


### Ner





library(DHARMa)

# Voor soort "ner"
sim_res <- simulateResiduals(glm_models[["ner"]])
plot(sim_res)

library(ggplot2)

ggplot(B, aes(x = Transect_ID, y = Length, fill = Date)) +
  geom_boxplot() +
  facet_wrap(~ Species_ID, scales = "free_y") +
  theme_minimal()





library(lme4)
ner_data <- B_filtered %>% filter(Species_ID == "per ulv")

# Lineair mixed model
m1_linear <- lmer(Length ~ Date_numeric + (1 | Transect_ID), data = ner_data)
shapiro.test(residuals(m1_linear))  # Shapiro-Wilk test voor normaliteit van residuals

# Kwadratisch mixed model
m2_quad <- lmer(Length ~ Date_numeric + I(Date_numeric^2) + (1 | Transect_ID), data = ner_data)
anova(m1_linear, m2_quad)  # vergelijk lineair vs kwadratisch met random effect
shapiro.test(residuals(m2_quad))

model <-lmer(Length ~ Date + (1 | Transect_ID), data = ner_data)
library(emmeans)
emm <- emmeans(model, ~ Date)
pairs(emm)



library(nlme)

corvol_data <- B_filtered %>% filter(Species_ID == "cor vol")

# Variantie neemt toe met Date_numeric
mod_varIdent <- lme(
  Length ~ Date_numeric + I(Date_numeric^2),
  random = ~1 | Transect_ID,
  weights = varIdent(form = ~1 | Date),  # verschillende variantie per datum
  data = corvol_data
)
res <- residuals(mod_varIdent, type = "normalized")

shapiro.test(res)  # Test op normaalverdeling

dev.off()  # sluit huidige grafische device (bijv. als je RStudio plots venster vastzit)

qqnorm(res)
library(ggplot2)

ggplot(data.frame(residuals = res), aes(sample = residuals)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  theme_minimal() +
  labs(title = "QQ-plot van genormaliseerde residuen")

summary(mod_varIdent)
mod_constVar <- lme(
  Length ~ Date_numeric + I(Date_numeric^2),
  random = ~1 | Transect_ID,
  data = corvol_data
)

anova(mod_constVar, mod_varIdent)
ggplot(corvol_data, aes(x = Date_numeric, y = Length)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), color = "blue", se = TRUE) +
  labs(title = "Lengte van cor vol over tijd", x = "Datum (genummerd)", y = "Lengte (mm)") +
  theme_minimal()


m1 <- Length ~ Date_numeric + I(Date_numeric^2) + (1 | Transect_ID) # Shapiro-Wilk test voor normaliteit van residuals
m2 <- Length ~ Date_numeric * I(Date_numeric^2) + (1 | Transect_ID)
m3 <- Length ~ Date_numeric + I(Date_numeric^2) + (Date_numeric | Transect_ID)


ploplotResiduals(simres, form = corvol_data$Transect_ID)

### cor vol
cor_data <- B %>% filter(Species_ID == "cor vol")
cor_data %>% summarise(
  unique_transects = n_distinct(Transect_ID),
  unique_dates = n_distinct(Date)
)
cor_data <- cor_data %>%
  mutate(
    Transect_ID = as.factor(Transect_ID),
    Date = as.factor(Date)
  )
# Model zonder interactie
model1 <- glm(Length ~ Transect_ID + Date,
              family = Gamma(link = "log"),
              data = cor_data)

# Model met interactie
model2 <- glm(Length ~ Transect_ID * Date,
              family = Gamma(link = "log"),
              data = cor_data)
AIC(model1)
AIC(model2)
anova(model1, model2, test = "Chisq")
summary(model1)





###################### BOXPLOT COR VOL ABUNDANCE per transect
# Filter out the dates you don't want and focus on cor vol
B_filtered <- B %>%
  filter(Date %in% c("20 March", "4 April", "29 April", "13 May")) %>%
  filter(Species_ID == "cor vol")

# Count individuals per Transect_ID (core) per Date
counts_per_core <- B_filtered %>%
  group_by(Date, Transect_ID) %>%
  summarise(count = n(), .groups = "drop")

# Calculate sample sizes per date
sample_sizes <- counts_per_core %>%
  group_by(Date) %>%
  summarise(n_cores = n(), .groups = "drop")

# Plot boxplot with counts per core
ggplot(counts_per_core, aes(x = Date, y = count)) +
  geom_boxplot(fill = "skyblue", outlier.shape = NA) +  # No outlier points drawn
  geom_jitter(width = 0.2, alpha = 0.6, size = 2) +  # Adds individual points
  geom_text(data = sample_sizes, 
            aes(x = Date, y = max(counts_per_core$count) * 1.05, 
                label = paste0("n=", n_cores)), 
            inherit.aes = FALSE, 
            size = 3) +
  labs(
    title = "Count of cor vol per Transect (Core) by Date",
    x = "Date",
    y = "Count per Transect"
  ) +
  theme_minimal()

ggsave("CORVOL_BOX_COUNT.png", dpi = 600, width = 8, height = 6, units = "in")

# Convert Date to an ordered factor
counts_per_core$Date <- factor(
  counts_per_core$Date,
  levels = c("20 March", "4 April", "29 April", "13 May"),
  ordered = TRUE
)

counts_per_core$Transect_ID <- as.factor(counts_per_core$Transect_ID)

counts_per_core$Date_num <- as.numeric(counts_per_core$Date)
model1 <- lm(count ~ Date_num * Transect_ID, data = counts_per_core)
summary(model1)

model2 <- lm(count ~ Date_num + Transect_ID, data = counts_per_core)
summary(model2)

library(lme4)
model3 <- lmer(count ~ Date_num + (1|Transect_ID), data = counts_per_core)
summary(model3)


AIC(model1, model2, model3)

ggplot(counts_per_core, aes(x = Date_num, y = count)) +
  geom_point(aes(color = Date), size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  scale_x_continuous(breaks = 1:4, labels = c("20 March", "4 April", "29 April", "13 May")) +
  labs(
    title = "Linear Trend in cor vol Count over Time",
    x = "Date",
    y = "Count per Transect"
  ) +
  theme_minimal()
ggsave("Lineartrend_CORVOL.png", dpi = 600, width = 8, height = 6, units = "in")

summary_stats <- counts_per_core %>%
  group_by(Date) %>%
  summarise(
    mean_count = mean(count),
    se = sd(count) / sqrt(n()),
    n_cores = n()
  )

library(dplyr)
# Filter for cor vol
corvol_data <- B %>%
  filter(Species_ID == "cor vol") %>%
  group_by(Date, Transect_ID) %>%
  summarise(count = n(), .groups = "drop")

# Convert factors if needed
corvol_data$Date <- factor(corvol_data$Date)
corvol_data$Transect_ID <- factor(corvol_data$Transect_ID)
corvol_data$count <- as.numeric(corvol_data$count)

# Filter out unwanted dates (1 April and 22 April)
corvol_filtered <- corvol_data %>%
  filter(!Date %in% c("1 April", "22 April"))

# Re-fit the ANOVA model on the filtered data
anova_filtered <- aov(count ~ Date + Transect_ID, data = corvol_filtered)
summary(anova_filtered)

# Run Tukey HSD on the filtered model for Date
tukey_date_filtered <- TukeyHSD(anova_filtered, "Date")
print(tukey_date_filtered)

# (Optional) Run Tukey HSD on Transect_ID again, if needed
tukey_transect_filtered <- TukeyHSD(anova_filtered, "Transect_ID")
print(tukey_transect_filtered)

# Levene’s test for Date (AFTER filtering)
leveneTest(count ~ Date, data = corvol_filtered)

# Levene’s test for Transect_ID
leveneTest(count ~ Transect_ID, data = corvol_filtered)

# Fit new ANOVA model
anova_filtered <- aov(count ~ Date + Transect_ID, data = corvol_filtered)

# Shapiro-Wilk test for residual normality
shapiro.test(residuals(anova_filtered))

############################# Length Distribution of Cor Vol
# Filter for Corophium volutator and non-missing lengths
cor_vol_data <- B %>%
  filter(Species_ID == "cor vol", !is.na(Length))

# Convert Date to a factor for correct order in the plot (optional)
cor_vol_data$Date <- factor(cor_vol_data$Date, levels = unique(cor_vol_data$Date))

cor_vol_data$Date <- factor(cor_vol_data$Date, levels = c("20 March", "1 April", "4 April", "22 April", "29 April", "13 May"))

# Count observations per date
date_counts <- cor_vol_data %>%
  group_by(Date) %>%
  summarise(n = n()) %>%
  filter(n >= 3)

# Filter main dataset to keep only dates with n ≥ 3
cor_vol_data_filtered <- cor_vol_data %>%
  filter(Date %in% date_counts$Date)

# Ensure Date order is preserved after filtering
cor_vol_data_filtered$Date <- factor(
  cor_vol_data_filtered$Date,
  levels = unique(cor_vol_data_filtered$Date)
)
cor_vol_data_filtered$Date <- factor(cor_vol_data_filtered$Date, levels = c("20 March", "1 April", "4 April", "22 April", "29 April", "13 May"))


# Replot boxplot (without facets or transects)
ggplot(cor_vol_data_filtered, aes(x = Date, y = Length)) +
  geom_boxplot(fill = "lightblue") +
  geom_jitter(width = 0.2, alpha = 0.5) +
  geom_text(
    data = date_counts,
    aes(x = Date, y = max(cor_vol_data_filtered$Length) * 1.05, label = paste0("n=", n)),
    inherit.aes = FALSE,
    size = 4
  ) +
  labs(
    title = "Length of *Corophium volutator* per Date (n ≥ 3)",
    x = "Date",
    y = "Length (mm)"
  ) +
  theme_minimal() +
theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("Cor.Vol_Length_plot.png", dpi = 600, width = 8, height = 6, units = "in")
getwd()

table(cor_vol_data$Date)
date_counts <- cor_vol_data %>%
  group_by(Date) %>%
  summarise(n = n()) %>%
  filter(n >= 3)

cor_vol_data_filtered <- cor_vol_data %>%
  filter(Date %in% date_counts$Date)

kruskal.test(Length ~ Date, data = cor_vol_data_filtered)

by(cor_vol_data_filtered$Length, cor_vol_data_filtered$Date, 
   function(x) {
     if (length(x) >= 3) {
       shapiro.test(x)
     } else {
       NA
     }
   })

pairwise.wilcox.test(cor_vol_data_filtered$Length, cor_vol_data_filtered$Date, 
                     p.adjust.method = "bonferroni")


########################### Differentiate between the transects
library(ggplot2)
library(dplyr)

# Filter for Corophium volutator
cor_vol_data <- B %>%
  filter(Species_ID == "cor vol", !is.na(Length))

# Ensure Date is a factor in desired order
cor_vol_data$Date <- factor(cor_vol_data$Date, levels = unique(cor_vol_data$Date))

cor_vol_data$Date <- factor(cor_vol_data$Date, levels = c("20 March", "4 April", "22 April", "29 April", "13 May"))


ggplot(cor_vol_data, aes(x = Date, y = Length)) +
  geom_boxplot(fill = "lightblue") +
  geom_jitter(width = 0.2, alpha = 0.5, size = 1.2) +
  facet_wrap(~ as.factor(Transect_ID), nrow = 1) +
  labs(
    title = "Length of Corophium volutator per Date by Transect",
    x = "Date",
    y = "Length (mm)"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


################################################## Cor Vol energy content 
library(dplyr)

cor_vol_data_filtered <- B %>%
  filter(Species_ID == "cor vol")

cor_vol_data_filtered <- cor_vol_data_filtered %>%
  mutate(DryWeight_mg = 0.0034 * (Length ^ 2.57))

cor_vol_data_filtered <- cor_vol_data_filtered %>%
  mutate(Energy_J = DryWeight_mg * 12.69)

summary_stats <- cor_vol_data_filtered %>%
  summarise(
    mean_dryweight = mean(DryWeight_mg, na.rm = TRUE),
    mean_energy = mean(Energy_J, na.rm = TRUE),
    sd_energy = sd(Energy_J, na.rm = TRUE),
    n = n()
  )

library(lubridate)
cor_vol_data <- cor_vol_data_filtered %>%
  filter(Species_ID == "cor vol")

# Add year if missing (let’s assume 2025)
cor_vol_data <- cor_vol_data %>%
  mutate(Date = paste(Date, "2025"),
         Date = dmy(Date),             # Parse the date as day-month-year
         Week = week(Date))            # Extract week number

cor_vol_data <- cor_vol_data %>%
  mutate(Week = week(Date))

weekly_energy <- cor_vol_data %>%
  group_by(Week) %>%
  summarise(Mean_Energy_J = mean(Energy_J, na.rm = TRUE),
            SD_Energy_J = sd(Energy_J, na.rm = TRUE),
            n = n())

library(dplyr)
library(ggplot2)

# Filter out weeks 13 and 16
cor_vol_data_filtered <- cor_vol_data %>%
  filter(!(Week %in% c(13, 16)))

# Calculate sample sizes per week
sample_sizes <- cor_vol_data_filtered %>%
  group_by(Week) %>%
  summarise(n = n())

# Calculate a single y-position for the labels
max_energy <- max(cor_vol_data_filtered$Energy_J, na.rm = TRUE)
label_y <- max_energy * 1.1  # Place a bit above the highest boxplot

# Plot
ggplot(cor_vol_data_filtered, aes(x = factor(Week), y = Energy_J)) +
  geom_boxplot(fill = "lightblue", color = "darkblue") +
  geom_text(
    data = sample_sizes,
    aes(x = factor(Week), y = label_y, label = paste("n =", n)),
    size = 3,
    vjust = 0
  ) +
  labs(
    title = "Weekly Variation in Energy Content of *Corophium volutator*",
    x = "Week Number",
    y = "Energy Content (Joules)"
  ) +
  theme_minimal()
ggsave("CorVol.Energycontent.png", dpi = 600, width = 8, height = 6, units = "in")
getwd()

library(dplyr)

# Calculate summary statistics per week
summary_stats <- cor_vol_data_filtered %>%
  group_by(Week) %>%
  summarise(
    mean_energy = mean(Energy_J, na.rm = TRUE),
    sd_energy = sd(Energy_J, na.rm = TRUE),
    n = n()
  )
print(summary_stats)

