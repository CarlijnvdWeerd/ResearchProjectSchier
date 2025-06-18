## Run the script 07_BORIS_forage first, then this script will work
renv::restore()

p5a <- ggplot(point_behaviors |> filter(Behavior == "Probing"),
              aes(x = as.factor(Week), y = Behavior_Rate, 
                  fill = Strategy)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#FF9999", "#1ED760", "#66B3FF")) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Probing by Strategy",
    x = "Week",
    y = "Duration Rate")
p5a

p5b <- p5a + geom_text(data = counts_point |> filter(Behavior == "Probing"), aes(x = as.factor(Week), 
                                                                                 y = 1.6, 
                                                                                 label = paste0("n=", n)),
                       position = position_dodge(width = 0.8), size = 3)
p5b

glm4 <- glm(Behavior_Rate ~ Week + Strategy + Week * Strategy, 
            family = Gamma(link = "log"),
            data = subset(point_behaviors, Behavior == "Probing"))
summary(glm4)

glm5 <- glm(Behavior_Rate ~  Week * Strategy + Transect_ID + Habitat + Tide, 
            family = Gamma(link = "log"),
            data = subset(point_behaviors, Behavior == "Probing"))
summary(glm5)

glm6 <- glm(Behavior_Rate ~ Week * Strategy, 
            family = Gamma(link = "log"),
            data = subset(point_behaviors, Behavior == "Probing"))
summary(glm6)

AIC(glm4, glm5, glm6)

point_behaviors %>%
  filter(Behavior == "Probing") %>%
  group_by(Week) %>%
  summarise(p_value = kruskal.test(Behavior_Rate ~ Strategy)$p.value)

install.packages("FSA")
library(FSA)

dunn1 <- dunnTest(Behavior_Rate ~ Strategy, 
                  data = subset(point_behaviors, Behavior == "Probing"), 
                  method = "bonferroni")
dunn1
## Welke test geeft meer duidelijkheid? Want glm geeft aan significant maar kruskal en dunntest niet ?

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