## Run the script 07_BORIS_forage first, then this script will work
renv::restore()

p10b <- p10a + geom_text(data = counts_stage |> filter(Behavior == "Walking"), aes(x = as.factor(Week), y = 1.2,                                             label = paste0("n=", n)),
                         position = position_dodge(width = 0.8), size = 4)

p10b

glmer7 <- glmer(Behavior_Rate ~ Week * Strategy + (1 | Three_letter_code), 
                family = Gamma(link = "log"),
                data = subset(stage_behavior, Behavior == "Walking"),
                control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
summary(glmer7)

p10c <- p10b +
  geom_text(data = data.frame(
    Week = factor(c(13, 18, 19, 21)),
    label = c("***", "***", "***", "***"),
    y = c(1.00, 1.00, 0.75, 0.7)
  ),
  aes(x = Week, y = y, label = label),
  vjust = -0.5,
  size = 6,
  inherit.aes = FALSE)  # <--- This is the key!
p10c

stage_behavior %>%
  filter(Behavior == "Walking") %>%
  group_by(Week) %>%
  summarise(p_value = kruskal.test(Behavior_Rate ~ Strategy)$p.value)

p11b <- p11a + geom_text(data = counts_stage |> filter(Behavior == "Alert"), aes(x = as.factor(Week), y = 1.2,                                             label = paste0("n=", n)),
                         position = position_dodge(width = 0.8), size = 4)

p11b

glmer8 <- glmer(Behavior_Rate ~ Week * Strategy + (1 | Three_letter_code), 
                family = Gamma(link = "log"),
                data = subset(stage_behavior, Behavior == "Alert"),
                control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
summary(glmer8)

p11c <- p11b +
  geom_text(data = data.frame(
    Week = factor(c(18, 19, 21)),
    label = c("***", "***", "**"),
    y = c(1.00, 0.7, 0.7)
  ),
  aes(x = Week, y = y, label = label),
  vjust = -0.5,
  size = 6,
  inherit.aes = FALSE)  # <--- This is the key!
p11c

stage_behavior %>%
  filter(Behavior == "Alert") %>%
  group_by(Week) %>%
  summarise(p_value = kruskal.test(Behavior_Rate ~ Strategy)$p.value)

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

p14b <- p14a + geom_text(data = counts_stage |> filter(Behavior == "Handling_prey"), aes(x = as.factor(Week), y = 1.2,                                             label = paste0("n=", n)),
                         position = position_dodge(width = 0.8), size = 4)

p14b

glmer10 <- glmer(Behavior_Rate ~ Week * Strategy + (1 | Three_letter_code), 
                 family = Gamma(link = "log"),
                 data = subset(stage_behavior, Behavior == "Handling_prey"),
                 control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
summary(glmer10)

p14c <- p14b +
  geom_text(data = data.frame(
    Week = factor(c(13, 17, 18, 21)),
    label = c("***", "***", "***", "***"),
    y = c(1.00, 0.75, 1.0, 0.7)
  ),
  aes(x = Week, y = y, label = label),
  vjust = -0.5,
  size = 6,
  inherit.aes = FALSE)  # <--- This is the key!
p14c

p_stage_behavior <- p10b + p11b + p12b + p13b + p14b
p_stage_behavior
ggsave("stage_behaviors_strategy.png", plot = p_stage_behavior, width = 18, height = 10, dpi = 300)

p_stage_filtered <- p10c + p11c + p12c + p14c
p_stage_filtered
ggsave("stage_behaviors_strategy_filtered.png", plot = p_stage_filtered, width = 25, height = 10, dpi = 300)

#emmeans_results2 <- emmeans(aov(Behavior_Rate ~ Behavior * Strategy, data = #stage_behavior), 
#                           pairwise ~ Strategy | Behavior)
#print(emmeans_results2)
# Plotting the results