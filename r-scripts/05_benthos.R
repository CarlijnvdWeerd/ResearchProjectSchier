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
B <- read_excel("C:/Users/conso/Downloads/Benthos Determinatie  (1).xlsx", 
                sheet = "FactDeterm")
View(B)

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

species_week_counts <- species_week_counts %>%
  arrange(Date, desc(count))
ggplot(species_week_counts, aes(x = Date, y = count, fill = Species_ID)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Species Counts per Week",
       x = "Week",
       y = "Count",
       fill = "Species") +
  theme_minimal()

ggplot(species_week_counts, aes(x = Date, y = count)) +
  geom_col(fill = "steelblue") +
  facet_wrap(~ Species_ID, scales = "free_y") +
  labs(title = "Species Counts per Week (Faceted)",
       x = "Week",
       y = "Count") +
  theme_minimal()

############################################### Length over time 
library(ggplot2)

library(dplyr)

ggplot(B, aes(x = Date, y = Length, fill = Species_ID)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  labs(title = "Length Distribution Over Time by Species",
       x = "Date",
       y = "Length (mm)",
       fill = "Species") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

length_summary <- B %>%
  group_by(Date, Species_ID) %>%
  summarise(mean_length = mean(Length, na.rm = TRUE), .groups = "drop")

ggplot(length_summary, aes(x = Date, y = mean_length, color = Species_ID, group = Species_ID)) +
  geom_line() +
  geom_point() +
  labs(title = "Mean Length Over Time by Species",
       x = "Date",
       y = "Mean Length (mm)",
       color = "Species") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(B, aes(x = Date, y = Length)) +
  geom_boxplot(fill = "lightblue") +
  facet_wrap(~ Species_ID, scales = "free_y") +
  labs(title = "Length Distribution Over Time by Species",
       x = "Date",
       y = "Length (mm)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

################################################## Number per transect





