library(tidyverse)
library(kableExtra)
library(cowplot)
library(MASS)
library(lmtest)
library(sandwich)
library(broom)
library(ggfortify)
library(reshape2)
library(stats)
library(stargazer)
library(modelr)
library(pracma)
library(speedglm)
# read the data
job_satisfaction <- read_tsv("job_satisfaction.tsv")


df1<-filter(job_satisfaction, Gender == "Female")
df2<-filter(job_satisfaction, Gender == "Male")

ggplot(data=df1, aes(x=Job.Satisfaction, y=Count,
                     fill= factor(Income, 
                                  levels=c("<5000", 
                                           "5000-15000",
                                           "15000-25000",
                                           ">25000"))  ))+
  geom_bar(stat="identity", position = position_dodge(width = .8), width = 0.7)+
  labs(fill = "Income")+
  geom_text(aes(label = Count), position = position_dodge(width = .8), vjust = -0.5)

ggsave(
  filename = "2_1.png",
  device = "png", width = 7, height = 4)

ggplot(data=df2, aes(x=Job.Satisfaction, y=Count,
                     fill= factor(Income, 
                                  levels=c("<5000", 
                                           "5000-15000",
                                           "15000-25000",
                                           ">25000"))  ))+
  geom_bar(stat="identity", position = position_dodge(width = .8), width = 0.7)+
  labs(fill = "Income")+
  geom_text(aes(label = Count), position = position_dodge(width = .8), vjust = -0.5)

ggsave(
  filename = "2_2.png",
  device = "png", width = 7, height = 4)

#Part2
job_satisfaction$Job.Satisfaction <- as.factor(job_satisfaction$Job.Satisfaction)
job_satisfaction$Income <- as.factor(job_satisfaction$Income)
job_satisfaction$Gender <- as.factor(job_satisfaction$Gender)


#Part B
#Below mutations compute each of y's required to calculate the score

#Group by Income and Gender to calculate y_jl
job_satisfaction <- job_satisfaction %>%
  group_by(Income, Gender) %>%
  mutate(yjl = sum(Count))

#Group by job satisfaction and Gender to calculate y_kl
job_satisfaction <- job_satisfaction %>%
  group_by(Job.Satisfaction, Gender) %>%
  mutate(ykl = sum(Count)) 

#Group by gender to calculate yl
job_satisfaction <- job_satisfaction %>%
  group_by(Gender) %>%
  mutate(yl = sum(Count)) 

#Expected means
job_satisfaction <- job_satisfaction %>%
  mutate(ujkl = yjl*ykl/yl)

#Individual Score
job_satisfaction <- job_satisfaction %>%
  mutate(score = (Count-ujkl)^2/ujkl)

#Total Score is simply sum of score column
X_sq<-sum(job_satisfaction$score)
print(X_sq)
dof<-2*(4-1)*(4-1)

# P values based on null distribution
p_val <- pchisq(q=X_sq, df=dof, lower.tail=FALSE)
print(p_val)

#Part 4
job_satisfaction <- read_tsv("job_satisfaction.tsv")

#Un-grouped format
ungrouped_data <- job_satisfaction[rep(1:nrow(job_satisfaction),
                                       job_satisfaction$Count), 1:3]

# Ranking the ordinal data in natural order
ungrouped_data <- ungrouped_data %>%
  mutate(Income=replace(Income, Income=="<5000", 1)) %>%
  mutate(Income=replace(Income, Income=="5000-15000", 2)) %>%
  mutate(Income=replace(Income, Income=="15000-25000", 3)) %>%
  mutate(Income=replace(Income, Income==">25000", 4)) %>%
  
  mutate(Job.Satisfaction=replace(Job.Satisfaction,
                                  Job.Satisfaction=="Very Dissatisfied", 1))%>%
  mutate(Job.Satisfaction=replace(Job.Satisfaction,
                                  Job.Satisfaction=="A Little Satisfied", 2))%>%
  mutate(Job.Satisfaction=replace(Job.Satisfaction,
                                  Job.Satisfaction=="Moderately Satisfied", 3))%>%
  mutate(Job.Satisfaction=replace(Job.Satisfaction,
                                  Job.Satisfaction=="Very Satisfied", 4))%>%
  as.data.frame()

#Performing the two suggested tests
B = 5000
test_stat1 <- rep(0, B)

test_stat1[1]<-cor.test(as.numeric(ungrouped_data$Income),
                                   as.numeric(ungrouped_data$Job.Satisfaction),
                                   method="kendall")$estimate

for (i in 2:B){
  #Sampling by gender
  ungrouped_sample <- ungrouped_data %>%
    group_by(Gender) %>%
    mutate(Income=sample(Income))

  test_stat1[i]<-cor.test(as.numeric(ungrouped_sample$Income),
                         as.numeric(ungrouped_sample$Job.Satisfaction),
                         method="kendall")$estimate

}

frac1 <- sum(test_stat1 >= test_stat1[1])

p_val1 <- frac1/B
print(p_val1)

