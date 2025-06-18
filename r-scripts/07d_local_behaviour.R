## Run the script 07_BORIS_forage first, then this script will work
renv::restore()

p_all_strategies

qqnorm(transect_dataset$Count)

glm8 <- glm(Count ~ Week + Transect_ID + Strategy, 
            family = Gamma(link = "log"),
            data = transect_dataset)
summary(glm8)

glm9 <- glm(Count ~ Week * Transect_ID + Strategy, 
            family = Gamma(link = "log"),
            data = transect_dataset)
summary(glm9)

glm10 <- glm(Count ~ Week * Strategy + Transect_ID, 
             family = Gamma(link = "log"),
             data = transect_dataset)
summary(glm10)

glm11 <- glm(Count ~ Transect_ID * Strategy + Week, 
             family = Gamma(link = "log"),
             data = transect_dataset)
summary(glm11)

AIC(glm8, glm9, glm10, glm11)
## glm9 is the best model according to AIC

lm8 <- lm(Count ~ Week * Transect_ID + Strategy, 
          data = transect_dataset)
summary(lm8)
# if would use linear instead of generalized linear there would be significant difference in use of transect points between early and late arrivals 

## The weeks are not the same, but that is ofcourse the case becaused late arrivals will not be there in week 9. Take out or keep in?

p_all_habitat

glm12 <- glm(n ~ Week + Habitat + Strategy, 
             family = Gamma(link = "log"),
             data = complete_dataset)
summary(glm12)

glm13 <- glm(n ~ Week * Habitat + Strategy, 
             family = Gamma(link = "log"),
             data = complete_dataset)
summary(glm13)

glm14 <- glm(n ~ Week * Strategy + Habitat, 
             family = Gamma(link = "log"),
             data = complete_dataset)
summary(glm14)

glm15 <- glm(n ~ Habitat * Strategy + Week, 
             family = Gamma(link = "log"),
             data = complete_dataset)
summary(glm15)

AIC(glm12, glm13, glm14, glm15)
## glm15 is the best model according to AIC

glm16 <- glm(n ~ Habitat * Strategy * Week, 
             family = Gamma(link = "log"),
             data = complete_dataset)
summary(glm16)
### Is this maybe a good idea to also do for the other models?

AIC(glm15, glm16)
## glm16 is best according to AIC

kruskal.test(n ~ Strategy, data = complete_dataset)
# Kruskal-Wallis chi-squared = 2321.7, df = 2, p-value < 2.2e-16

library(ggplot2)
library(dplyr)
library(FSA)
library(multcompView)

#Run Dunn Test
dunn2 <- dunnTest(n ~ Strategy, data = complete_dataset, 
                  method = "bonferroni")
dunn2


ggsave("habitat_usage_strategy.png", plot = p_all_habitat, width = 18, height = 10, dpi = 300)
