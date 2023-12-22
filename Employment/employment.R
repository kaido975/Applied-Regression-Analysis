library(tidyverse)
library(kableExtra)
library(stringr)
library(ggplot2)
library(stargazer)
library(sandwich)
library(matlib)
library(broom)
library(lmtest)

# read in the data
employment_data <- readRDS("employment-data.rds")
length(unique(employment_data$firm))
################################################################
# part (a)
firms_by_sector <- employment_data %>%
  group_by(sector) %>%
  summarise(distinct_firms = n_distinct(firm) ) %>%
  rename(Sector = sector, `Distinct Firms by Sector` = distinct_firms)

# format and save table
t(firms_by_sector) %>%
  kable(format = "latex",
        col.names = NA,
        booktabs = TRUE,
        digits = 2,
        caption = "Distinct Firms",
        label = "Distinct Firms") %>%
  kable_styling(latex_options = "hold_position") %>%
  save_kable("firms_by_sector.tex")

#Plot of employment data
ggplot(data=employment_data, aes(x=sector, y=emp)) +
  facet_grid(year ~. ) +
  geom_bar(colour="blue",stat="identity",fill="blue") +
  theme(axis.text = element_text(size = 8))

################################################################
# part (b)
#Model with sector dummies
lm_with_sector <- lm(emp ~ year+wage+capital+output+sector
                     , data = employment_data)


#Model without sector dummies
lm_without_sector <- lm(emp ~  year+wage+capital+output
                        , data = employment_data)

#ANOVA F-test
f_test <-anova(lm_without_sector, lm_with_sector)
print(f_test)

#ANOVA summary to table
f_test %>%
  kable(format = "latex",
        row.names = NA,
        booktabs = TRUE,
        digits = 2,
        caption = "F-test",
        label = "F-test") %>%
  kable_styling(latex_options = "hold_position") %>%
  save_kable("anova.tex")

# Plotting residuals of the regression against fitted values
fitvsres <- lm_with_sector %>%
  ggplot(aes(.fitted,.resid)) +
  geom_point() +
  geom_hline(yintercept=0, col="red", linetype="dashed") +
  xlab("Fitted values") +
  ylab("Residuals") +
  ggtitle("Residual vs Fitted Plot") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
# Saving the residual vs fitted values
ggsave(plot = fitvsres, filename = "residuals.png",
       device = "png", width = 4, height = 4)


#By sector Residual plot
sector.res <- data.frame(employment_data$sector, rstandard(lm_with_sector))
colnames(sector.res) <- c("Sector", "Residuals")
state.boxplot <- sector.res %>%
  ggplot(aes(x = Sector, y = Residuals, color = Sector)) +
  geom_boxplot() +
  coord_flip()
ggsave(state.boxplot,
       filename = "sector_box.png",
       device = "png", width = 6, height = 7.5
)

################################################################
# part (e)
################################################################
robust_anova <- function(lm_fit, lm_fit_partial, cluster) {
  #F-test using anova
  f_test <-anova(lm_without_sector, lm_with_sector)
  p_val_anova <- f_test[2, 6]
  cat("P value of F-test is",p_val_anova )
  print(" ")
  f_stat_anova <- f_test[2, 5]
  cat("F-statistic is: ", f_stat_anova)
  print(" ")
  # get the variables omitted in the partial model
  omitted_vars <- which(!(names(coef(lm_fit)) %in% names(coef(lm_fit_partial))))
  sandwich_est <- vcovCL(lm_fit, cluster)
  sandwich_est <- sandwich_est[omitted_vars, omitted_vars]
  
  #Plots and Tables are generated before the Wald test.
  #Comparison with OLS estimate with S.E inflation
  feature.var.LZ <- sqrt(diag(sandwich_est))
  feature.var.lm <- sqrt(diag(vcov(lm_with_sector)))[omitted_vars]
  LZ.lm <- feature.var.LZ / feature.var.lm
  
  n <- length(LZ.lm)
  ylab <- rep("Liang Zeger", n)
  df <- data.frame(
    value = LZ.lm
  )
  ggplot(df, aes(x=value)) + 
    geom_histogram(color="black", fill="red") +
    ggtitle("Histogram Plot of Standard Error inflation")
  
  ggsave(
    filename = "var_hist.png",
    device = "png", width = 4, height = 4
  )
  
  # Significant Coefficients from LZ robust estimate
  cov_LZ <- vcovCL(lm_fit, cluster = cluster)    
  covid_LZ.test <- tidy(coeftest(lm_fit,
                                 vcov = cov_LZ
  ))
  colnames(covid_LZ.test) <- c(
    "Predictor", "Estimate",
    "Standard Error",
    "Statistic", "p value"
  )
  
  covid_LZ.test %>%
    filter(`p value` <= 0.05) %>%
    select(-Statistic) %>%
    kable(
      format = "latex",
      row.names = NA, booktabs = T,
      digits = 8, align = rep("c", 4),
      linesep = ""
    ) %>%
    column_spec(1, border_right = T) %>%
    save_kable("LZ_test.tex")  
  
  # Significant Coefficients from OLS estimate
  cov_LS <- vcov(lm_fit)    
  covid_LS.test <- tidy(coeftest(lm_fit,
                                 vcov = cov_LS
  ))
  colnames(covid_LS.test) <- c(
    "Predictor", "Estimate",
    "Standard Error",
    "Statistic", "p value"
  )
  
  covid_LS.test %>%
    filter(`p value` <= 0.05) %>%
    select(-Statistic) %>%
    kable(
      format = "latex",
      row.names = NA, booktabs = T,
      digits = 8, align = rep("c", 4),
      linesep = ""
    ) %>%
    column_spec(1, border_right = T) %>%
    save_kable("LS_test.tex") 
  
  
  #Hypothesis test using Wald test statistic
  #Select coefficients to be tested
  beta_s <- lm_fit$coef[omitted_vars]
  
  #Wald Test statistic as derived in 2 (d)
  T_stat = t(beta_s) %*% inv(sandwich_est) %*% beta_s
  T_stat <- T_stat[1]
  cat("Wald Statistic is: ",T_stat)
  print(" ")
  # Using the approximation of normal distribution for the T_stat
  p_val_wald <- pchisq(q=T_stat, df=length(omitted_vars), lower.tail=FALSE)
  
   
    
}

p_val_wald <- robust_anova(lm_with_sector, lm_without_sector, 
                           cbind(employment_data$firm, employment_data$year))
cat("P-value from wald test is:", p_val_wald[1])