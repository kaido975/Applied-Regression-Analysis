library(tidyverse)
library(kableExtra)
library(matlib)
library(formattable)
source("gamma_regression.R")

################################################################
# Part (a): Data preparation
################################################################

# import the data
bmi_data_raw <- read_csv("bmi_data.csv")
#Threshold for keeping a column
f = floor(0.4 * 11212)
bmi_data_raw = bmi_data_raw[,colSums(bmi_data_raw<=0)<=f]

#Removing all rows with negative values
bmi_data_raw <- bmi_data_raw %>%
  filter_all( all_vars(. > 0))

#Removing columns with single value
bmi_data_raw <- bmi_data_raw %>% select(where(~n_distinct(.) > 1))

col_names <- names(bmi_data_raw)
cols <- col_names[-c(2, 4, 6)]
bmi_data_raw[,cols] <- lapply(bmi_data_raw[,cols] , as.factor)

################################################################
# Part (b): Data exploration
################################################################

p <- ggplot(bmi_data_raw, aes(x=erbmi)) +
  geom_histogram(binwidth=1, position="identity") +
  geom_vline(xintercept = c(18.5, 25, 30), linetype="dashed", 
             color = "blue", size=1) +
  annotate("text", label = "(UW)", x = 14, y = 20, color = "RED")+
  annotate("text", label = "(NW)", x = 21, y = 20, color = "RED")+
  annotate("text", label = "(OV)", x = 27, y = 20, color = "RED")+
  annotate("text", label = "(OB)", x = 35, y = 20, color = "RED")+
  labs(x ="BMI", y = "Count")
p
ggsave(
  filename = "3_b.png",
  device = "png", width = 6, height = 4)
################################################################
# Part (c): Wald inference
################################################################
#Fit Gamma GLM
gamma_glm_fit_full <- fit_gamma_glm(erbmi ~ .,data=bmi_data_raw) 

#Wald Inference
wald <- wald_inference(gamma_glm_fit_full)

#Sorting Wald Statistics based on P -value
wald <- wald[order(wald$p_value),]

#Saving Wald statistics in scientific Notation
wald %>% mutate(p_value = format(p_value, digits=3, scientific = TRUE),
                Estimates = format(Estimates, digits=3, scientific = TRUE),
                SE = format(SE, digits=3, scientific = TRUE)) %>%
kable(
      format = "latex",
      row.names = NA,
      digits = 5,
      booktabs = TRUE,
      caption = "Wald Statistics for BMI fit \\label{wald}") %>%
  kable_styling(latex_options = "hold_position") %>%
  save_kable("wald.tex")

################################################################
# Part (d): Multiplicity adjustment
################################################################
#Significance Level
alpha <- 0.05

#Total Parameters
m <- dim(wald)[1]

#Pvalues
pj <- wald$p_value

#Get indices according to FWER
S <- which(pj < alpha/m)

#Renaming Significant Values
rownames(wald)[1] <- "Intercept"
rownames(wald)[2] <- "Exercise Frequency" 
rownames(wald)[3] <- "Household Income: Level 2"
rownames(wald)[4] <- "Beverage Consumption: Category 2"

#Saving Significant P values
wald[S, ] %>% mutate(p_value = format(p_value, digits=3, scientific = TRUE),
                Estimates = format(Estimates, digits=3, scientific = TRUE),
                SE = format(SE, digits=3, scientific = TRUE)) %>%
  kable(
    format = "latex",
    row.names = NA,
    digits = 5,
    booktabs = TRUE,
    caption = "Wald Statistics based on  Bonferroni-significant predictor
    \\label{wald2}") %>%
  kable_styling(latex_options = "hold_position") %>%
  save_kable("wald2.tex")

  
################################################################
# Part (e): Interpretation
################################################################

#Mean Prediction from GLM fit
# BMI as a function of exercise frequency with top 4 significant parameters

#Estimated of Significant Parameters
betas <- wald$Estimates[1:4]

#Frequency of Exercise (1-35)
freq <- seq(1, 35, length.out=50)

#BMI Prediction as a function of exercise frequency for different groups
bmi1 <- exp(betas[1] + betas[2] * freq)
bmi2 <- exp(betas[1] + betas[2] * freq + betas[3])
bmi3 <- exp(betas[1] + betas[2] * freq + betas[4])
bmi_pred <- data.frame(bmi1, bmi2, bmi3, freq)
bmi_base <- exp(betas[1])

#BMI prediction Plot
bmi_pred %>% ggplot(aes(x = freq)) +
  geom_line(aes(y = bmi1, color = "Group 1")) +
  geom_line(aes(y = bmi2 , color = "Group 2")) +
  geom_line(aes(y = bmi3 , color = "Group 3")) +
  theme(legend.title = element_blank(), legend.position = "right")+
  geom_hline(yintercept = bmi_base, linetype="dashed", 
             color = "black", size=1)+
  geom_hline(yintercept = c(18.5,25,30), linetype="dashed", 
             color = "purple", size=1)+
  annotate("text", label = "(NM)", x = 4, y = 22, color = "RED")+
  annotate("text", label = "(OV)", x = 4, y = 26, color = "RED")+
  annotate("text", x=17, y=bmi_base + 0.3, label="Baseline BMI",
           color = "Black")+
  xlab("Exercise Frequency") +
  ylab("Predicted BMI")

ggsave(
  filename = "3_e.png",
  device = "png", width = 6, height = 4)