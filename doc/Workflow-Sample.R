## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

data_file <- system.file("extdata", "IQ_Study_Data.csv", package = "nplearn")

## ----setup, echo = FALSE-------------------------------------------------
library(nplearn)
library(tidyverse)

## ------------------------------------------------------------------------
iq_data <- read_csv(data_file)

## ------------------------------------------------------------------------
iq_data_long <- gather(iq_data,
                       'Foster',
                       'Biological',
                       key = "Upbringing",
                       value = "IQ")

iq_data$IQ_Diff <- iq_data$Foster - iq_data$Biological

iq_data$Social <- as.factor(iq_data$Social)
iq_data_long$Social <- as.factor(iq_data_long$Social)

iq_data$Social <- fct_relevel(iq_data$Social,
                              c("low", "middle", "high"))

iq_data_long$Social <- fct_relevel(iq_data_long$Social,
                                   c("low", "middle", "high"))

## ------------------------------------------------------------------------
ggplot(iq_data_long, aes(x = Upbringing, y = IQ)) +
  geom_boxplot()

ggplot(iq_data, aes(x = Social, y = IQ_Diff)) +
  geom_boxplot()

ggplot(iq_data, aes(x = Biological, y = Foster)) +
  geom_point(aes(color = Social, shape = Social)) +
  geom_smooth(method = lm, se = FALSE)

## ------------------------------------------------------------------------
iq_data_long %>%
  group_by(Upbringing) %>%
  summarize(mean(IQ),
            sd(IQ),
            min(IQ),
            quantile(IQ, 0.25),
            median(IQ),
            quantile(IQ, 0.75),
            max(IQ))

iq_data %>%
  group_by(Social) %>%
  summarize(mean(IQ_Diff),
            sd(IQ_Diff),
            min(IQ_Diff),
            quantile(IQ_Diff, 0.25),
            median(IQ_Diff),
            quantile(IQ_Diff, 0.75),
            max(IQ_Diff))

lm(iq_data$Foster ~ iq_data$Biological)
cor(iq_data$Foster, iq_data$Biological)

## ------------------------------------------------------------------------
t.test(iq_data$Foster, iq_data$Biological)

anova(lm(iq_data$IQ_Diff ~ iq_data$Social))

TukeyHSD(aov(iq_data$IQ_Diff ~ iq_data$Social))

anova(lm(iq_data$Foster ~ iq_data$Biological))

cor.test(iq_data$Foster, iq_data$Biological)

