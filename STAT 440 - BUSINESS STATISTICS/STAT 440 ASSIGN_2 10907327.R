##### STAT 440 #####
## Author: 10907327
## date: 8 / 11/ 2023 - dd/mm/yyy

library(tidyverse)
library(dplyr)
library(ggplot2)
library(readxl)
library(naniar)
library(fastDummies)
library(knitr)
library(broom)



getwd()
setwd("H:/UG/Academics/L400/SEM 1/Assignments/STAT 440 - BUSINESS STATISTICS/assignment_2")
list.files()
stock_dataset <- read_excel("stat440_classWork.xlsx" )


view(stock_dataset)
names(stock_dataset)
attach(stock_dataset)
dim(stock_dataset)
glimpse(stock_dataset)
unique(stock_dataset)

class(`How stock holders are informed of dividends`)
class(`where company stock is traded`)
class(Ratings)

##### Data Processing and Processing #####
## data cleaning

stock_dataset %>% 
  select(everything()) %>% 
  filter(!complete.cases(.))

vis_miss(stock_dataset) # visualize the missing data

##### Data Transformation #####
names(stock_dataset) <- tolower(gsub(" ", "_", names(stock_dataset)))

#renaming some variables.
stock_dataset <- stock_dataset %>% 
  rename(
    divInfoMethod = how_stock_holders_are_informed_of_dividends,
    stockExchange = where_company_stock_is_traded
  )

stock_dataset <- stock_dataset %>% 
  mutate_if(is.character, as.factor)

##### EDA #####
## Descriptive statistics

# Create a frequency table
cat_summary <- function(data, field) {
  data %>% 
    group_by({{field}}) %>% 
    summarise(
      counts = n(),
      percentage = n() / nrow(data) * 100,
      .groups = 'drop'
    )
}

freq_SE <- cat_summary(stock_dataset, stockExchange)
freq_DIM <- cat_summary(stock_dataset, divInfoMethod)

summary(ratings)

stock_dataset %>% 
  select(everything()) %>% 
  group_by(stockExchange) %>% 
  summarise(
    mean_ratings = mean(ratings),
    median_ratings = median(ratings),
    variance_ratings = var(ratings)
  ) %>% 
  arrange(mean_ratings)

stock_dataset %>% 
  select(everything()) %>% 
  group_by(divInfoMethod) %>% 
  summarise(
    mean_ratings = mean(ratings),
    median_ratings = median(ratings),
    variance_ratings = var(ratings)
  ) %>% 
  arrange(mean_ratings)

stock_dataset %>% 
  group_by(divInfoMethod, stockExchange) %>% 
  summarise(
    Median_rate = median(ratings)
  ) %>% 
  pivot_wider(names_from = divInfoMethod, values_from = Median_rate)

stock_dataset %>% 
  group_by(divInfoMethod, stockExchange) %>% 
  summarise(
    Median_rate = median(ratings)
  ) %>% 
  pivot_wider(names_from = stockExchange, values_from = Median_rate)

#contingency_table <- as.data.frame(table(stockExchange, divInfoMethod))
contingency_table <- xtabs(~ stockExchange + divInfoMethod, data = stock_dataset)
kable(contingency_table, format = "markdown")

fisher.test(contingency_table)

## Data visualization
stock_dataset %>% 
  ggplot(aes(x = reorder(divInfoMethod, ratings, FUN = mean), ratings)) +
  geom_boxplot(aes(col = divInfoMethod)) +
  geom_jitter() +
  
  ylab("Stock Ratings") +
  xlab("How Stock holders are informed of their Dividends") +
  ggtitle("Distribution of stock ratings against info methods
          in the U. S") +
  
  theme_economist() +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5),
    
    panel.grid.major.y = element_line(colour = "white", size = 1)
  )


stock_dataset %>% 
  ggplot(aes(x = reorder(divInfoMethod, ratings, FUN = mean), ratings)) +
  geom_violin(aes(col = divInfoMethod)) +
  geom_jitter() +
  
  ylab("Stock Ratings") +
  xlab("How Stock holders are informed of their Dividends") +
  ggtitle("Distribution of stock ratings against info methods
          in the U. S") +
  
  theme_economist() +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5),
    
    panel.grid.major.y = element_line(colour = "white", size = 1)
  )



stock_dataset %>% 
  ggplot(aes(x = stockExchange, y = ratings, fill = stockExchange)) + 
  geom_bar(stat = "identity") +
  
  scale_fill_brewer(palette = "Pastel1") +
  
  labs(x = "where company stock is traded", 
       y = "Stock Ratings", 
       title = "Distribution of trading location against
       stock ratings in the U. S") +
  
  theme_economist() +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5),
    
    panel.grid.major.y = element_line(colour = "white", size = 1)
  )




stock_dataset %>% 
  select(divInfoMethod, ratings) %>% 
  t.test(ratings ~ divInfoMethod, data = .,
         alternative = "two.sided")

stock_dataset %>% 
  select(stockExchange, ratings) %>% 
  aov(ratings ~ stockExchange, data = .) %>% 
  summary()


anova_result <- stock_dataset %>% 
  select(stockExchange, ratings) %>% 
  aov(ratings ~ stockExchange, data = .)



tukey_result <- stock_dataset %>% 
  select(stockExchange, ratings) %>% 
  aov(ratings ~ stockExchange, data = .) %>%
  TukeyHSD() # plot()

levels(divInfoMethod)
stock_dataset$divInfoMethod <- factor(stock_dataset$divInfoMethod,
                                      levels = c("Annual quarterly reports", 
                                                 "Presentation to analysts"),
                                      labels = c("0", "1"))


stock_dataset$stockExchange <- factor(stock_dataset$stockExchange,
                        levels = c("American Stock Exchange", "New York Stock Exchange", 
                                   "Over the counter"),
                        labels = c("1", "2", "3"))

attach(stock_dataset)
tukey_result <- stock_dataset %>% 
  select(stockExchange, ratings) %>% 
  aov(ratings ~ stockExchange, data = .) %>%
  TukeyHSD() %>%  plot()

tukey_df <- broom::tidy(tukey_result)

tukey_df %>% 
  ggplot(aes(contrast, estimate, col = term, 
             ymin = conf.low, ymax = conf.high)) +
  geom_point() +
  
  geom_errorbar(width = 0.2) +
  
  labs(title = "TukeyHSD Test Results", 
       x = "Comparisons", 
       y = "Difference in Means") +
  
  theme_economist() +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5),
    
    panel.grid.major.y = element_line(colour = "white", size = 1)
  )



#group the ratings into high and low
stock_dataset <- stock_dataset %>%
  mutate(rating_score = ifelse(ratings <= 2, 'low', 'high'))

stock_dataset$rating_score <- as.factor(stock_dataset$rating_score)

head(stock_dataset)

##### MODELLING #####
logistic_model <- glm(rating_score ~ stockExchange, family = binomial, data = stock_dataset)
summary(logistic_model)

pred_data <- data.frame(
  stockExchange = levels(stock_dataset$stockExchange))
  #divInfoMethod = levels(stock_dataset$divInfoMethod))

# Calculate predicted probabilities
pred_data$probability <- predict(logistic_model, newdata=pred_data, type="response")

merge_data <- merge(stock_dataset, pred_data, by="stockExchange")


ggplot(merge_data, aes(x = stockExchange, y = rating_score)) + 
  geom_jitter(width = 0.2, height = 0.0, alpha=0.5) +
  
  geom_point(aes(y = probability), color="blue", 
             size = 3, shape = 8) +
  labs(y="Probability of Rating", x="Stock Exchange", title="Logistic Regression Plot") +
  theme_economist()















library(ggplot2)
library(dplyr)








  
install.packages("ggpubr")

library(ggpubr)

stock_dataset %>% 
  ggplot(aes(stockExchange, ratings, col = stockExchange)) +
  geom_boxplot() +
  
  scale_fill_brewer(palette = "paste1") +
  
  stat_compare_means(method = "anova") +
  stat_compare_means(method = "tukey") + 
  
  labs(title = "TukeyHSD Test Results"
       ) +
  
  theme_economist() +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5),
    
    panel.grid.major.y = element_line(colour = "white", size = 1)
  )
  


