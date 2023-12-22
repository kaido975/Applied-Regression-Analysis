library(tidyverse)
library(kableExtra)
library(stringr)
library(ggplot2)
# read the data
anorexia_data = read_tsv("Anorexia.dat", col_types = "ifdd")

#Part A
anorexia_data <- select(anorexia_data, -subj)
anorexia_data <- anorexia_data %>% 
  mutate(across('therapy', str_replace, 'f', 'family'))
anorexia_data <- anorexia_data %>% 
  mutate(across('therapy', str_replace, 'c', 'control'))
anorexia_data <- anorexia_data %>% 
  mutate(across('therapy', str_replace, 'b', 'behavioral'))

anorexia_data <- anorexia_data %>%
  rename(weight_before = before)

anorexia_data <- anorexia_data %>%
  rename(weight_after = after)

anorexia_data <- anorexia_data %>%
  mutate(weight_gain = weight_after - weight_before)



print(as_tibble(anorexia_data))

#Part B:
#Creating the box plot
p1<-ggplot(anorexia_data, aes(x=therapy, y=weight_gain)) + 
  geom_boxplot()

ggsave(plot = p1,
       filename = "q3p1.pdf",
       device = "pdf",
       width = 4.5,
       height = 3.0)

#Scatter Plot
p2<-ggplot(anorexia_data, aes(x=weight_before , y=weight_gain, color=therapy)) +
  geom_point(alpha=0.7, size=2)

ggsave(plot = p2,
       filename = "q3p2.pdf",
       device = "pdf",
       width = 5.5,
       height = 3.0)

#Mean Weight gain
# compute the mean diamond price by cut
weight_therapy <- anorexia_data %>%
  group_by(therapy) %>%
  summarise(mean_weight_gain = mean(weight_gain),
            max_weight_gain = max(weight_gain) ,
            fraction = sum(weight_gain>0)/length(weight_gain) ) %>%
  rename(Therapy = therapy, `Mean Weight Gain` = mean_weight_gain,
         `Max Weight Gain`=max_weight_gain)



# format and save table
weight_therapy %>%
  kable(format = "latex",
        row.names = NA,
        booktabs = TRUE,
        digits = 2,
        caption = "Mean weight gain for therapy.",
        label = "Summary of Weight Gain") %>%
  kable_styling(latex_options = "hold_position") %>%
  save_kable("weight_gain.tex")

#Linear regression between weight gain and therapy
model1 <- lm(weight_gain ~ therapy, data=anorexia_data)
print(summary(model1))
#Change the baseline category
anorexia_data$therapy <- relevel(factor(anorexia_data$therapy), ref="control")
model2 <- lm(weight_gain ~ therapy, data=anorexia_data)
print(summary(model2))


#PART D
anorexia_data <- anorexia_data %>%
  group_by(therapy) %>%
  mutate(mean_weight = mean(weight_gain))

mean_weight_gain_sam = mean(anorexia_data$weight_gain)

ssr <- anorexia_data %>%
  group_by(therapy) %>%
  summarise(Freq = sum((mean_weight_gain_sam - mean_weight)^2))

ssr <- sum(ssr$Freq)  
cat("Between-groups sums of squares: ", ssr)

sse <- anorexia_data %>%
  group_by(therapy) %>%
  summarise(Freq = sum((weight_gain - mean_weight)^2))

sse <- sum(sse$Freq)  
cat("Within-groups sums of squares: ", sse)
cat("SSR + SSE: ", sse+ssr)

sst = sum((anorexia_data$weight_gain - mean_weight_gain_sam)^2)
cat("Corrected total sums of squares: ", sst)
cat("Ratio of between-group and corrected sum of squares: ", ssr/sst)

anorexia_data <- anorexia_data %>% 
  arrange(desc(weight_gain))
