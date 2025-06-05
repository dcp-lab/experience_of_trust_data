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
data <- read_excel("Cleaned_Data_R.xlsx")
glimpse(data)


## I - Data Cleaning

# 1) Factorisation of the categorical variable
data$Prolific_ID <- factor(data$Prolific_ID)
data$List <- factor(data$List)
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


## II - Data Analysis

# 1) Demographics Data Analysis

data_condition <- data %>%
  group_by(Condition) %>%
  summarise(n = n()) %>%
  mutate(proportion = n / sum(n))
data_condition

data_list <- data %>%
  group_by(List) %>%
  summarise(n = n()) %>%
  mutate(proportion = n / sum(n))
data_list

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
  summarise(mean = mean(Age),
            sd = sd(Age),
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
    n = n(),
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

# Correcting extreme values > or < than 2 * sd_diff_panas

# Compute mean and standard deviation of DV_PANAS
mean_diff_panas <- mean(data$DV_PANAS, na.rm = TRUE)
sd_diff_panas <- sd(data$DV_PANAS, na.rm = TRUE)

# Define the lower and upper bounds
lower_bound <- mean_diff_panas - 2 * sd_diff_panas
upper_bound <- mean_diff_panas + 2 * sd_diff_panas

# Filter the data
data_corrected_panas <- data %>%
  filter(DV_PANAS >= lower_bound & DV_PANAS <= upper_bound)

h_PANAS_corrected <- ggplot(data_corrected_panas, aes(x = DV_PANAS, fill = Condition)) +
  geom_histogram(position = "dodge", binwidth = 1) +
  labs(title = "Distribution of DV_-_PANAS", x = "DV_-_Panas Values", y = "Count") +
  scale_fill_manual(values = c("Dictator Game" = "blue", "Trust Game" = "orange"))
h_PANAS_corrected

summary_stats_PANAS_corrected <- data_corrected_panas %>%
  group_by(Condition) %>%
  summarise(
    n = n(),
    Mean_PANAS_Pre = mean(PANAS_Pre, na.rm = TRUE),
    Mean_PANAS_Post = mean(PANAS_Post, na.rm = TRUE),
    Mean_Dif_PANAS = Mean_PANAS_Post - Mean_PANAS_Pre,
    Median_PANAS_Pre = median(PANAS_Pre, na.rm = TRUE),
    Median_PANAS_Post = mean(PANAS_Post, na.rm = TRUE),
    Median_Dif_PANAS = Median_PANAS_Post - Median_PANAS_Pre,
    SD_Dif_PANAS = sd(DV_PANAS, na.rm = TRUE),
    .groups = "drop"
  )
glimpse(summary_stats_PANAS_corrected)

# 3) Plot for DV_-_STAIS
h_STAIS <- ggplot(data, aes(x = DV_STAIS, fill = Condition)) +
  geom_histogram(position = "dodge", binwidth = 1) +
  labs(title = "Distribution of DV_-_STAIS", x = "DV_-_STAIS Values", y = "Count") +
  scale_fill_manual(values = c("Dictator Game" = "blue", "Trust Game" = "orange"))
h_STAIS

summary_stats_STAIS <- data %>%
  group_by(Condition) %>%
  summarise(
    n = n(),
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

# Correcting extreme values > or < than 2 * sd_diff_stais

# Compute mean and standard deviation of DV_PANAS
mean_diff_stais <- mean(data$DV_STAIS, na.rm = TRUE)
sd_diff_stais <- sd(data$DV_STAIS, na.rm = TRUE)

# Define the lower and upper bounds
lower_bound <- mean_diff_stais - 2 * sd_diff_stais
upper_bound <- mean_diff_stais + 2 * sd_diff_stais

# Filter the data
data_corrected_stais <- data %>%
  filter(DV_STAIS >= lower_bound & DV_STAIS <= upper_bound)

h_STAIS_corrected <- ggplot(data_corrected_stais, aes(x = DV_STAIS, fill = Condition)) +
  geom_histogram(position = "dodge", binwidth = 1) +
  labs(title = "Distribution of DV_-_STAIS", x = "DV_-_STAIS Values", y = "Count") +
  scale_fill_manual(values = c("Dictator Game" = "blue", "Trust Game" = "orange"))
h_STAIS_corrected

summary_stats_STAIS_corrected <- data_corrected_stais %>%
  group_by(Condition) %>%
  summarise(
    n = n(),
    Mean_PANAS_Pre = mean(PANAS_Pre, na.rm = TRUE),
    Mean_PANAS_Post = mean(PANAS_Post, na.rm = TRUE),
    Mean_Dif_PANAS = Mean_PANAS_Post - Mean_PANAS_Pre,
    Median_PANAS_Pre = median(PANAS_Pre, na.rm = TRUE),
    Median_PANAS_Post = mean(PANAS_Post, na.rm = TRUE),
    Median_Dif_PANAS = Median_PANAS_Post - Median_PANAS_Pre,
    SD_Dif_PANAS = sd(DV_PANAS, na.rm = TRUE),
    .groups = "drop"
  )
glimpse(summary_stats_STAIS_corrected)


### III - Statistical Analysis

#  1°) ANOVA Condition * Time Analysis

# ANOVA PANAS

data_long_PANAS <- data_corrected_panas %>%
  select(Prolific_ID, Condition, PANAS_Pre, PANAS_Post) %>%
  pivot_longer(cols = c(PANAS_Pre, PANAS_Post),
               names_to = "Time",
               values_to = "PANAS") %>%
  mutate(Time = factor(Time, levels = c("PANAS_Pre", "PANAS_Post"),
                       labels = c("Pre", "Post")))

res_ANOVA_PANAS <- aov_car(PANAS ~ Condition * Time + Error(Prolific_ID/Time), data = data_long_PANAS)
summary(res_ANOVA_PANAS)

Diff_ANOVA_PANAS = emmeans(res_ANOVA_PANAS,"Condition")
pairs(Diff_ANOVA_PANAS,adjust="holm")

plot_ANOVA_PANAS <- afex_plot(res_ANOVA_PANAS, x = "Time", trace = "Condition", error = "within") +
  labs(
    title = "PANAS: Condition × Time Interaction",
    x = "Time", y = "PANAS Score"
  )
plot_ANOVA_PANAS

# ANOVA STAIS

data_long_STAIS <- data_corrected_stais %>%
  select(Prolific_ID, Condition, STAIS_Pre, STAIS_Post) %>%
  pivot_longer(cols = c(STAIS_Pre, STAIS_Post),
               names_to = "Time",
               values_to = "STAIS") %>%
  mutate(Time = factor(Time, levels = c("STAIS_Pre", "STAIS_Post"),
                       labels = c("Pre", "Post")))

res_ANOVA_STAIS <- aov_car(STAIS ~ Condition * Time + Error(Prolific_ID/Time),
                           data = data_long_STAIS)
summary(res_ANOVA_STAIS)

Diff_ANOVA_STAIS <- emmeans(res_ANOVA_STAIS, ~ Condition * Time)
pairs(Diff_ANOVA_STAIS, adjust = "holm")

plot_ANOVA_STAIS <- afex_plot(res_ANOVA_STAIS,
                              x = "Time",          
                              trace = "Condition", 
                              error = "within") + 
  labs(x = "Time", y = "STAIS Score",
       title = "STAIS: Condition × Time Interaction")
plot_ANOVA_STAIS

# 2°) LR Diff Expectation

# LR PANAS

res_LR_PANAS <- lm(DV_PANAS ~ IV_Dif._Expected, data = data_corrected_panas)
summary(res_LR_PANAS)
plot_LR_PANAS <- ggplot(data, aes(x = IV_Dif._Expected, y = DV_PANAS)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  theme_minimal() +
  labs(title = "Interaction: Condition × Expected Difference on PANAS",
       x = "Difference from Expected", y = "PANAS Score")
plot_LR_PANAS

# LR STAIS

res_LR_STAIS <- lm(DV_STAIS ~ IV_Dif._Expected, data = data_corrected_stais)
summary(res_LR_STAIS)

plot_LR_STAIS <- ggplot(data, aes(x = IV_Dif._Expected, y = DV_STAIS)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  theme_minimal() +
  labs(title = "Interaction: Condition × Expected Difference on PANAS",
       x = "Difference from Expected", y = "STAIS Score")
plot_LR_STAIS

# 3°) ANCOVA Amount Sent Back * Time Analysis

# ANCOVA PANAS

data_TG_PANAS <- data_corrected_panas %>%
  filter(Condition == "Trust Game")

summary(data_TG_PANAS$Average_Amounts_Sent_Back)

data_long_TG_PANAS <- data_TG_PANAS %>%
  select(Prolific_ID, PANAS_Pre, PANAS_Post, Average_Amounts_Sent_Back) %>%
  pivot_longer(cols = c(PANAS_Pre, PANAS_Post),
               names_to = "Time",
               values_to = "PANAS") %>%
  mutate(
    Time = factor(Time, levels = c("PANAS_Pre", "PANAS_Post"),
                  labels = c("Pre", "Post"))
  )

res_ANCOVA_TG_PANAS <- aov_car(
  PANAS ~ Time + Average_Amounts_Sent_Back + Error(Prolific_ID/Time),
  data = data_long_TG_PANAS
)
summary(res_ANCOVA_TG_PANAS)

plot_ANCOVA_TG_PANAS <- afex_plot(res_ANCOVA_TG_PANAS, x = "Time", error = "within") +
  labs(title = "PANAS Change Over Time (TG) Controlling for Amount Sent Back",
       y = "PANAS", x = "Time")
plot_ANCOVA_TG_PANAS

# ANCOVA STAIS

data_TG_STAIS <- data_corrected_stais %>%
  filter(Condition == "Trust Game")

summary(data_TG_STAIS$Average_Amounts_Sent_Back)

data_long_TG_STAIS <- data_TG_STAIS %>%
  select(Prolific_ID, STAIS_Pre, STAIS_Post, Average_Amounts_Sent_Back) %>%
  pivot_longer(cols = c(STAIS_Pre, STAIS_Post),
               names_to = "Time",
               values_to = "STAIS") %>%
  mutate(
    Time = factor(Time, levels = c("STAIS_Pre", "STAIS_Post"),
                  labels = c("Pre", "Post"))
  )

res_ANCOVA_TG_STAIS <- aov_car(
  STAIS ~ Time + Average_Amounts_Sent_Back + Error(Prolific_ID/Time),
  data = data_long_TG_STAIS
)
summary(res_ANCOVA_TG_STAIS)

plot_ANCOVA_TG_STAIS <- afex_plot(res_ANCOVA_TG_STAIS, x = "Time", error = "within") +
  labs(title = "PANAS Change Over Time (TG) Controlling for Amount Sent Back",
       y = "PANAS", x = "Time")
plot_ANCOVA_TG_STAIS


