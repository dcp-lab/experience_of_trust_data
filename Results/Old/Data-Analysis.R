library(ggpmisc)
library(readxl)
library(tibble)
library(tinytex)
library(tidyverse)
library(afex)
library(emmeans)
library(ggbeeswarm)
library(cowplot)
library(ggplot2)
library(psych)

theme_set(theme_bw(base_size = 15) + theme(legend.position = "bottom"))
setwd("/Users/maximelebourgeois/Desktop/experience_of_trust_final/Results")
data <- read_excel("Cleaned Data - Value for R.xlsx")
glimpse(data)

## I - Data Cleaning

# 1) Factorisation of the categorical variable
data$Prolific_ID <- factor(data$Prolific_ID)
data$Condition <- factor(data$Condition, levels = c("DG", "TG"), labels = c("Dictator Game", "Trust Game"))
data$Sex <- factor(data$Sex)
data$Ethnicity_simplified <- factor(data$Ethnicity_simplified)
data$Nationality <- factor(data$Nationality)

# 2) Ensure DV variables are numeric
data$PANAS_Pre <- as.numeric(data$PANAS_Pre)
data$PANAS_Post <- as.numeric(data$PANAS_Post)
data$STAIS_Pre <- as.numeric(data$STAIS_Pre)
data$STAIS_Post <- as.numeric(data$STAIS_Post)
data$DV_PANAS <- as.numeric(data$DV_PANAS)
data$DV_STAIS <- as.numeric(data$DV_STAIS)
data$IV_Dif._Expected <- as.numeric(data$IV_Dif._Expected)
data$Age <- as.numeric(data$Age)

# 3) Remove NA values from the Condition column for plotting
# plot_data <- data[!is.na(data$Condition), ]

## II - Data Analysis

# 1) Demographics Data Analysis

data_sex <- data %>%
  group_by(Sex) %>%
  summarise(n = n()) %>%
  mutate(proportion = n / sum(n))
data_sex

data_ethnicity <- data %>%
  group_by(Ethnicity_simplified) %>%
  summarise(n = n()) %>%
  mutate(proportion = n / sum(n))
data_ethnicity

data_age <- data %>%
  summarise(sd = sd(Age),
            mean = mean(Age),
            min = min(Age),
            max = max(Age),
            )
data_age

data_nationality <- data %>%
  group_by(Nationality) %>%
  summarise(n = n()) %>%
  mutate(proportion = n / sum(n))
data_nationality

# 2) Plot for DV_-_PANAS
h_PANAS <- ggplot(data, aes(x = DV_PANAS, fill = Condition)) +
  geom_histogram(position = "dodge", binwidth = 1) +
  labs(title = "Distribution of DV_-_PANAS", x = "DV_-_Panas Values", y = "Count") +
  scale_fill_manual(values = c("Dictator Game" = "blue", "Trust Game" = "orange"))
h_PANAS

summary_stats_PANAS <- data %>%
  group_by(Condition) %>%
  summarise(
    Mean_PANAS_Pre = mean(PANAS_Pre, na.rm = TRUE),
    Mean_PANAS_Post = mean(PANAS_Post, na.rm = TRUE),
    Mean_Dif_PANAS = Mean_PANAS_Post - Mean_PANAS_Pre,
    Median_PANAS_Pre = median(PANAS_Pre, na.rm = TRUE),
    Median_PANAS_Post = mean(PANAS_Post, na.rm = TRUE),
    Median_Dif_PANAS = Median_PANAS_Post - Median_PANAS_Pre,
    SD_Dif_PANAS = sd(DV_PANAS, na.rm = TRUE),
    .groups = "drop"
  )
glimpse(summary_stats_PANAS)

# 3) Plot for DV_-_STAIS
h_STAIS <- ggplot(data, aes(x = DV_STAIS, fill = Condition)) +
  geom_histogram(position = "dodge", binwidth = 1) +
  labs(title = "Distribution of DV_-_STAIS", x = "DV_-_STAIS Values", y = "Count") +
  scale_fill_manual(values = c("Dictator Game" = "blue", "Trust Game" = "orange"))
h_STAIS

summary_stats_STAIS <- data %>%
  group_by(Condition) %>%
  summarise(
    Mean_STAIS_Pre = mean(STAIS_Pre, na.rm = TRUE),
    Mean_STAIS_Post = mean(STAIS_Post, na.rm = TRUE),
    Mean_Dif_STAIS = Mean_STAIS_Post - Mean_STAIS_Pre,
    Median_STAIS_Pre = median(STAIS_Pre, na.rm = TRUE),
    Median_STAIS_Post = mean(STAIS_Post, na.rm = TRUE),
    Median_Dif_STAIS = Median_STAIS_Post - Median_STAIS_Pre,
    SD_Dif_STAIS = sd(DV_STAIS, na.rm = TRUE),
    .groups = "drop"
  )
glimpse(summary_stats_STAIS)

### III - Statistical Analysis

#  1°) ANOVA Condition Analysis

res_ANOVA_PANAS <- aov_car(DV_PANAS ~ Condition + Error (Prolific_ID), data)
res_ANOVA_PANAS

Diff_ANOVA_PANAS = emmeans(res_ANOVA_PANAS,"Condition")
pairs(Diff_ANOVA_PANAS)

plot_ANOVA_PANAS <- afex_plot(res_ANOVA_PANAS,"Condition", data_geom = ggbeeswarm::geom_quasirandom)+ labs(x = "Condition",y = "PANAS Score")
plot_ANOVA_PANAS


res_ANOVA_STAIS <- aov_car(DV_STAIS ~ Condition + Error (Prolific_ID), data)
res_ANOVA_STAIS
Diff_ANOVA_STAIS = emmeans(res_ANOVA_STAIS,"Condition")
pairs(Diff_ANOVA_STAIS)

plot_ANOVA_STAIS <- afex_plot(res_ANOVA_STAIS,"Condition", data_geom = ggbeeswarm::geom_quasirandom)+ labs(x = "Condition",y = "STAIS Score")
plot_ANOVA_STAIS

# 2°) LR Diff Expectation

res_LR_PANAS <- lm(DV_PANAS ~ Condition * IV_Dif._Expected, data = data)
summary(res_LR_PANAS)
plot_LR_PANAS <- ggplot(data, aes(x = IV_Dif._Expected, y = DV_PANAS, color = Condition)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  theme_minimal() +
  labs(title = "Interaction: Condition × Expected Difference on PANAS",
       x = "Difference from Expected", y = "PANAS Score")
plot_LR_PANAS

res_LR_STAIS <- lm(DV_STAIS ~ Condition * IV_Dif._Expected, data = data)
summary(res_LR_STAIS)
plot_LR_STAIS <- ggplot(data, aes(x = IV_Dif._Expected, y = DV_STAIS, color = Condition)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  theme_minimal() +
  labs(title = "Interaction: Condition × Expected Difference on PANAS",
       x = "Difference from Expected", y = "STAIS Score")
plot_LR_STAIS



