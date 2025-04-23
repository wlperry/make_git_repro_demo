# Load packages -----------------------------------------------------------

library(readr)
library(ggplot2)
library(dplyr)
library(ggforestplot)
library(forestmodel)
library(ggcorrplot)


# Read data ---------------------------------------------------------------

hypoxia <- read_csv("data/hypoxia.csv")
View(hypoxia)


# Useful functions --------------------------------------------------------

# An AHI of 5-14 is mild; 15-29 is moderate and
# 30 or more events per hour characterizes severe sleep apnea.
# 1 = (AHI < 5); 2 = (5 ≤ AHI < 15);
# 3 = (15 ≤ AHI < 30); 4 = (AHI ≥ 30)

# Convert binary `Female` column to characters for Male and Female
convert_to_factors <- function(data = hypoxia) {
  output <- data |>
    mutate(
      Sex = if_else(.data$Female == 1, "Female", "Male"),
      .after = .data$Female
    ) |>
    mutate(AHI = factor(.data$AHI))
  return(output)
}
hypoxia <- convert_to_factors()


# Exploratory plots -------------------------------------------------------

# Histograms and bar charts
ggplot(hypoxia) +
  geom_histogram(aes(Age))
ggplot(hypoxia) +
  geom_histogram(aes(AHI))
ggplot(hypoxia) +
  geom_bar(aes(AHI))

# Correlation
hypoxia |>
  select(where(is.numeric)) |>
  cor(use = "complete.obs") |>
  ggcorrplot()
ggsave("corr_plot.png", width = 5, height = 5)

# Multiple factors
ggplot(hypoxia) +
  geom_bar(aes(AHI)) +
  facet_wrap(~Sex)

ggplot(hypoxia) +
  geom_bar(aes(AHI)) +
  facet_wrap(Race ~ Sex)

ggplot(hypoxia) +
  geom_bar(aes(AHI)) +
  facet_grid(Race ~ Sex)

hypoxia <- hypoxia |>
  mutate(Diabetesfct = factor(Diabetes))
ggplot(hypoxia) +
  geom_bar(aes(AHI, fill = Diabetesfct), position = "fill") +
  scale_fill_brewer(palette = "Dark2") +
  theme_minimal() # no colours


# Summary statistics ------------------------------------------------------

# Mean
mean(hypoxia$Age)
mean(hypoxia$BMI)
mean(hypoxia$`Duration of Surg`)

# Standard Deviation
sd(hypoxia$Age)

# Counts
table(hypoxia$AHI)
table(hypoxia$`Duration of Surg`)


# Statistical tests -------------------------------------------------------

# Chi-squared tests of AHI factors
chisq.test(table(hypoxia$AHI, hypoxia$Sex))
chisq.test(table(hypoxia$AHI, hypoxia$Race))
chisq.test(table(hypoxia$AHI, hypoxia$Smoking))
chisq.test(table(hypoxia$AHI, hypoxia$Diabetes))

# T-tests for BMI and Sleep time
# Assume variance equal
BMI1 <- hypoxia |> filter(BMI <= median(BMI)) # nolint
BMI2 <- hypoxia |> filter(BMI > median(BMI)) # nolint
t.test(BMI1$Sleeptime, BMI2$Sleeptime, var.equal = TRUE)
t.test(BMI1$Sleeptime, BMI2$Sleeptime, var.equal = TRUE)$p.value


# Modelling ---------------------------------------------------------------

mod_data <- hypoxia |>
  mutate(severeAHI = if_else(AHI == 4, 1, 0)) |>
  select(-c(Female, AHI))

mod1 <- glm(severe_AHI ~ ., data = mod_data, family = "binomial")
summary(mod1)

mod2 <- glm(severe_AHI ~ Age + Sex + BMI, data = mod_data, family = "binomial")
summary(mod2)

mod3 <- glm(severe_AHI ~ Age + Sex, data = mod_data, family = "binomial")
summary(mod3)

mod4 <- glm(severe_AHI ~ Age + BMI, data = mod_data, family = "binomial")
summary(mod4)

mod5 <- glm(severe_AHI ~ Sex + BMI, data = mod_data, family = "binomial")
summary(mod5)

mod6 <- glm(severe_AHI ~ Age, data = mod_data, family = "binomial")
summary(mod6)

mod7 <- glm(severe_AHI ~ Sex, data = mod_data, family = "binomial")
summary(mod7)

mod8 <- glm(severe_AHI ~ BMI, data = mod_data, family = "binomial")
summary(mod8)

# Results -----------------------------------------------------------------

# AIC
aic_results <- c(
  mod2$aic, mod3$aic, mod4$aic,
  mod5$aic, mod6$aic, mod7$aic, mod8$aic
)

# Forest plot of best model
forest_model(mod2)
ggsave("forestplot.png")
