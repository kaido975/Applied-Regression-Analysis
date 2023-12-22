library(tidyverse)
library(kableExtra)
library(stringr)
library(ggplot2)
library(readxl)
library(ggcorrplot)
library(stargazer)
library(jtools)
library(ggstance)

# read the data
crime_data = read_tsv("Statewide_crime.tsv", col_types = "ciiddd")

population_data <- read_excel("population.xlsx")

crime_data <- cbind(crime_data, population_data[2])

crime_data <- crime_data %>%
  mutate(CrimeRate  = Violent/Pop)

corr <- round(cor(select(crime_data, c("CrimeRate", "Metro", "HighSchool"
                                       , "Poverty"))), 1)
cov <- cov(select(crime_data, c("CrimeRate", "Metro", "HighSchool"
                                      , "Poverty")))
ggcorrplot(corr)
ggsave(
  filename = "corr.png",
  device = "png", width = 6, height = 4)

mean(crime_data$CrimeRate)

#Part 3



crime_data.lm <- lm(CrimeRate ~ Metro + HighSchool + Poverty, data = crime_data)
summary(crime_data.lm)

confint(crime_data.lm)

star = stargazer(crime_data.lm,
          title = "Results of regressing CrimeRate on  Metro, HighSchool and 
          Poverty",
          label = "tab:regression-output",
          out = "regression-output.tex"
          , digits=6, digits.extra=1)

star = gsub("& ([0-9]+) ", "& \\1\\.0 ", star)

cat(star, sep = "\n")


plot_summs(crime_data.lm)
ggsave(
  filename = "summ.png",
  device = "png", width = 6, height = 4)


  kable(cov,
        format = "latex",
        row.names = NA,
        digits = 3,
        booktabs = TRUE,
        caption = "The covariance matrix") %>%
  kable_styling(latex_options = "hold_position") %>%
  save_kable("cov.tex")
