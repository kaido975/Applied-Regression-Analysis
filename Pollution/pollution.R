library(kableExtra)
library(cowplot)
library(MASS)
library(ggfortify)
library(reshape2)
library(tidyverse)
library(rsample)
library(stargazer)
library(sandwich)
library(matlib)
library(broom)
library(lmtest)


# read the data
pollution_data <- read_tsv("pollution_data.tsv", col_types = "idd")

# pollution_data$date<-as.Date(pollution_data$date,"2017-06-14")

pollution_data %>%
  ggplot(aes(x = wind)) +
  geom_point(aes(y = log_nox)) +
  labs(x = "Wind Speed",
       y = "NOX") 

ggsave(
  filename = "3_a.png",
  device = "png", width = 6, height = 3)

daily_avg <- pollution_data %>%
  group_by(date) %>%
  summarise(mean_nox = mean(log_nox),
            mean_wind = mean(wind) ) 

daily_avg %>%
  ggplot(aes(x = mean_wind )) +
  geom_point(aes(y = mean_nox)) +
  labs(x = "Average Wind Speed",
       y = "Average NOX") 

ggsave(
  filename = "3_a2.png",
  device = "png", width = 6, height = 3)

pollution_data[1:120, ] %>% 
  ggplot(aes(x=wind, y=log_nox)) +
  facet_grid(date ~.) +
  geom_bar(colour="blue",stat="identity",fill="blue") +
  theme(axis.text = element_text(size = 8))

ggsave(
  filename = "3_a3.png",
  device = "png", width = 6, height = 3)

#Part 2
model <- lm(log_nox ~ wind, data=pollution_data)

# Residual vs fitted values
diag.resfit <- autoplot(model,
                        which = 1,
                        label.size = 3,
                        label.vjust = 1.7,
                        label.colour = "red"
) +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(
  filename = "resfit.png",
  device = "png", width = 4, height = 4
)


# residual vs leverage plot (Cook's distance)
diag.plot.lev <- autoplot(model,
                          which = 5,
                          label.size = 3, label.hjust = 1.5,
                          label.colour = "red"
) +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(
  filename = "reslev.png",
  device = "png", width = 4, height = 4
)

#Function to Compute variance estimate using Pairs bootstrap
pairs_bootstrap <- function(bs, B){
  beta <- data.frame(ncol=0, nrow=0)
  for(i in 1:B) {
    sample_data<-as.tibble(bs$splits[[i]]) %>% unnest(cols = c(data))
    model <- lm(log_nox ~ wind, data=sample_data)
    new <- c(as.numeric(model$coefficients))     
    # print(new)# Create new column
    beta[nrow(beta) + 1 , ] <- new                  # Append new column
    # colnames(data)[ncol(data)] <- paste0("new", i)  # Rename column name
  }
  beta <- beta[-c(1), ]
  names(beta)[1] <- "(Intercept)"
  names(beta)[2] <- "wind"
  return(cov(beta))
}

#Standard error calculation borrowed from problem 2
se_calculation <- function(x, cov_mat){
  linear_combination <- sqrt(x[ ,1]^2*cov_mat[1,1] +
                               x[ ,2]^2*cov_mat[2,2]+
                               2*apply(x, 1, prod)*cov_mat[1,2])
}

n<-8092
p<-2
alpha<-0.05

#Comparison function similar to problem 2
comparison <- function(model){
  x<-pollution_data$wind
  B = 500
  D <- pollution_data %>% nest(data=-date)
  set.seed(154234)
  #Generating data for bootstrapping 
  bs <- bootstraps(D, times = B)
  #Getting Covariance matrix using bootstrap
  cov_boot<-pairs_bootstrap(bs, B)
  
  sandwich_est <- vcovCL(model, pollution_data$date)  
  print(cov_boot)
  # Significant Coefficients from LZ robust estimate
  pol_LZ.test <- tidy(coeftest(model,
                                 vcov = sandwich_est
  ))
  
  colnames(pol_LZ.test) <- c(
    "Predictor", "Estimate",
    "Standard Error",
    "Statistic", "p value"
  )
  
  pol_LZ.test %>%

    kable(
      format = "latex",
      row.names = NA, booktabs = T,
      digits = 8, align = rep("c", 4),
      linesep = ""
    ) %>%
    column_spec(1, border_right = T) %>%
    save_kable("LZ_test.tex")  
  
  # Significant Coefficients from pairs bootstrap
  pol_boot.test <- tidy(coeftest(model,
                                 vcov = cov_boot
  ))
  colnames(pol_boot.test) <- c(
    "Predictor", "Estimate",
    "Standard Error",
    "Statistic", "p value"
  )
  print(pol_boot.test)
  pol_boot.test %>%

    kable(
      format = "latex",
      row.names = NA, booktabs = T,
      digits = 8, align = rep("c", 4),
      linesep = ""
    ) %>%
    column_spec(1, border_right = T) %>%
    save_kable("boot_test.tex")  
    
  # Significant Coefficients from OLS estimate
  pol_LS <- vcov(model)    
  pol_LS.test <- tidy(coeftest(model,
                                 vcov = pol_LS
  ))
  colnames(pol_LS.test) <- c(
    "Predictor", "Estimate",
    "Standard Error",
    "Statistic", "p value"
  )
  
  pol_LS.test %>%

    kable(
      format = "latex",
      row.names = NA, booktabs = T,
      digits = 8, align = rep("c", 4),
      linesep = ""
    ) %>%
    column_spec(1, border_right = T) %>%
    save_kable("LS_test.tex")   
  
  X_data <- as.matrix(data.frame(rep(1, length(x)), x))
  
  se_boot<-se_calculation(X_data, cov_boot)
  se_liang<-se_calculation(X_data, sandwich_est)
  
  
  predictions <- predict(model, se.fit = TRUE)

  t_quantile <- qt(1-alpha/2, df = n-p)
  
  beta0 = model$coefficients[1]
  beta1 = model$coefficients[2]
  se1 = t_quantile*sqrt(cov_boot[1,1])
  se2 = t_quantile*sqrt(sandwich_est[1,1])
  se3 = t_quantile*sqrt(pol_LS[1,1])
  lower <- c(beta0 - se1,beta0 - se2,beta0 - se3)
  upper <- c(beta0 + se1,beta0 + se2,beta0 + se3)
  conf_int <- data.frame(lower,upper)
  conf_int <- t(conf_int)

  colnames(conf_int) <- c(
    "Bootstrap", "Liang Zeger",
    "OLS"
  )
    
  conf_int %>%
    
    kable(
      format = "latex",
      row.names = NA, booktabs = T,
      digits = 8, align = rep("c", 4),
      linesep = ""
    ) %>%
    column_spec(1, border_right = T) %>%
    save_kable("beta0.tex")   
  
  se1 = t_quantile*sqrt(cov_boot[2,2])
  se2 = t_quantile*sqrt(sandwich_est[2,2])
  se3 = t_quantile*sqrt(pol_LS[2,2])
  lower <- c(beta1 - se1,beta1 - se2,beta1 - se3)
  upper <- c(beta1 + se1,beta1 + se2,beta1 + se3)
  conf_int <- data.frame(lower,upper)
  conf_int <- t(conf_int)

  colnames(conf_int) <- c(
    "Bootstrap", "Liang Zeger",
    "OLS"
  )  
    
  conf_int %>%
    
    kable(
      format = "latex",
      row.names = NA, booktabs = T,
      digits = 8, align = rep("c", 4),
      linesep = ""
    ) %>%
    column_spec(1, border_right = T) %>%
    save_kable("beta1.tex")   
  
    print(1)
    pollution_data %>%
      mutate(fit = predictions$fit,
             se = predictions$se.fit,
             ptwise_width = t_quantile*se,
             ptwise_width_boot = t_quantile*se_boot,
             ptwise_width_huber = t_quantile*se_liang) %>%
      ggplot(aes(x = x)) +
      geom_point(aes(y = log_nox)) +
      geom_line(aes(y = fit), color = "blue") +
      geom_line(aes(y = fit + ptwise_width, color = "Pointwise")) +
      geom_line(aes(y = fit - ptwise_width, color = "Pointwise")) +
      geom_line(aes(y = fit + ptwise_width_boot, 
                    color = "Clustered Pairs Bootstrap")) +
      geom_line(aes(y = fit - ptwise_width_boot, 
                    color = "Clustered Pairs Bootstrap")) +
      geom_line(aes(y = fit + ptwise_width_huber, color = "Liang-Zeger")) +
      geom_line(aes(y = fit - ptwise_width_huber, color = "Liang-Zeger"))     
    
    ggsave(
      filename = "3_c.png",
      device = "png", width = 6, height = 3.5) 

  return(conf_int)
}

width<-comparison(model)
