library(kableExtra)
library(cowplot)
library(MASS)
library(lmtest)
library(sandwich)
library(broom)
library(ggfortify)
library(reshape2)
library(tidyverse)
library(stats)
library(stargazer)

# read and plot data
blocks_data <- read_tsv("blocks.tsv")
blocks_data %>%
  ggplot(aes(y=Number, x=Age)) +
  geom_count(alpha=0.5) +
  labs(title = "Number vs Age",
       x = "Age",
       y = "Number",
       size = "")
  ggsave(
    filename = "3_a.png",
    device = "png", width = 6, height = 3.5)

#Part b: GLM fit
glm_fit = glm(Number~Age,
              family = "poisson",
              data = blocks_data)

#Summary of the GLM (Wald test statistic is taken from the summary)
summary(glm_fit)

#Wald based confidence interval
confint.default(glm_fit)

#Part C
#Intercept-Only Model
glm_fit_partial = glm(Number~1,
                      family = "poisson",
                      data = blocks_data)
#LRT based significant test
anova_fit = anova(glm_fit_partial, glm_fit, test = "LRT")
print(anova_fit)

#Part D
#Fitted values (log (mu))
ci_logs = predict(glm_fit,
                    newdata = blocks_data, 
                      se.fit = TRUE) %>%
  as.data.frame() %>%
  as_tibble() %>%
  select(fit, se.fit)

#Adding Age column to the dataset
ci_logs["Age"] <- blocks_data$Age

#Computing means by taking exponential of fitted values
means = function(x) exp(x)
ci_means = ci_logs %>%
  mutate(fits = means(fit),
         lower = means(fit-2*se.fit),
         upper = means(fit + 2*se.fit)) %>%
  select(Age, fits, lower, upper)

blocks_data <- data.frame(blocks_data, ci_means$fits,
                          ci_means$lower,ci_means$upper)

#Final Plot of fitted values along with confidence interval
blocks_data %>%
  ggplot(aes(x = Age)) + 
  geom_count(aes(y = Number), alpha=0.5) +
  geom_point(aes(y = ci_means.fits, color="Predicted")) +
  geom_line(aes(y = ci_means.lower, color="CI")) + 
  geom_line(aes(y = ci_means.upper, color="CI"))


  ggsave(
    filename = "3_d.png",
    device = "png", width = 6, height = 3.5)

