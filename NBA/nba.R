library(tidyverse)
library(tidyverse)
library(dplyr)
# read the data
nba_data <- read_tsv("nba_data.tsv")

matches <- 1230

r1<- rep(0, 31)
r1[1] <- -1
r1[2] <- 1
r1[31] <- 1
dataset_nba <- data.frame(t(r1)) 
teams<- unique(nba_data$team)
outcome <- unique(nba_data$result)
for (i in 2:1230){
  i1 <- 2*i - 1
  i2 <- 2*i
  t1 <- which(teams == nba_data$team[i1])
  t2 <- which(teams == nba_data$team[i2])
  
  r1<- rep(0, 31)
  r1[t1] <- -1
  r1[t2] <- 1
  res <- nba_data$result[i1]
  if (res=="Loss") r1[31] <- 1 
  dataset_nba[nrow(dataset_nba) + 1,] <- t(r1)
}
dataset_nba <- rename(dataset_nba, y=X31)

#Dropping column for phoenix suns
idx<-23

dataset_nba <- dataset_nba[-c(idx)]
#GLM does not contain X23
glm_fit <- glm(y ~ X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11+
                 X12+X13+X14+X15+X16+X17+X18+X19+X20+X21+X22+
                 X24+X25+X26+X27+X28+X29+X30, family = "binomial",
               data = dataset_nba)
summary(glm_fit)

confint.default(glm_fit)

#Wins plot
home_wins <- rep(0, 30)
away_wins <- rep(0, 30)
total_wins <- rep(0, 30)
for (i in 1:2460){
  t <- which(teams == nba_data$team[i])
  if (nba_data$result[i]=="Win"){
    if (nba_data$location[i]=="Away") away_wins[t] <- away_wins[t]+1
    else home_wins[t] <- home_wins[t]+1
  }
}

betas <- c(glm_fit$coefficients[2:30])
betas<-append(betas, 0.0, idx-1)

win_data <- data.frame(teams, 100*home_wins/41, 100*away_wins/41, 100*(away_wins+home_wins)/82, betas)

win_data<-win_data[order(win_data$betas, decreasing = TRUE), ]


colnames(win_data)[2] = "home_wins"
colnames(win_data)[3] = "away_wins"
colnames(win_data)[4] = "Total Wins"
rownames(win_data) <- 1:30

ggplot(data = win_data, aes(x=away_wins, y=home_wins))+
  geom_point()+
  geom_abline(slope=1, intercept=0)

ggsave(
  filename = "home_vs_away.png",
  device = "png", width = 6, height = 4)

print(win_data)

#Part 4
r1<- rep(0, 30)
r1[2] <- -1
r1[4] <- 1
r2<- rep(0, 30)
r2[2] <- 1
r2[4] <- -1
test_data<-data.frame(t(r1))
test_data[nrow(test_data)+1,]<-t(r1)
test_data[nrow(test_data)+1,]<-t(r2)
test_data[nrow(test_data)+1,]<-t(r2)
test_data <- test_data[-c(idx)]
predictions <- predict(glm_fit, test_data, se.fit = TRUE)

predictions <- c(predictions$fit, predictions$fit + qnorm(0.025)*predictions$se.fit,
  predictions$fit + qnorm(0.975)*predictions$se.fit)

predictions <- exp(predictions)

prob <- predictions/(1+predictions)
prob[c(3,4,7,8,11,12)] <- 1-prob[c(3,4,7,8,11,12)]