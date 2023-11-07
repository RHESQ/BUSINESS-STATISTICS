###### sTAT 440: BUSINESS STATISTICS ######
### AUTHOR: 10907327 ###
## DUE DATE: 6 / 11/ 2024

# loading the necessary libraries
library(tidyverse)
library(ggplot2)
library(readxl)
library(dplyr)
library(naniar)
library(ggthemes)


getwd() #checking the current directory

# set up the new directory to point to the data
setwd("H:/UG/Academics/L400/SEM 1/Assignments/STAT 440 - BUSINESS STATISTICS")

list.files()

AHA_dataset <- read_excel("AHA_1300_Database.xlsx") # read the excel file.
view(AHA_dataset) # display the dataset in tables

##### Initial Assessment of Data
names(AHA_dataset) # display the different data fields
glimpse(AHA_dataset) # display the STRUCTURE OF data
head(AHA_dataset, 10L)
attach(AHA_dataset)
unique(AHA_dataset)


##### DATA PROCESSING #####
# check for incomplete and empty data
AHA_dataset %>% 
  select(everything()) %>% 
  filter(!complete.cases(.)) %>% 
  view()

AHA_dataset %>% 
  select(Births, `Births or Not`) %>% 
  #filter(!complete.cases(.)) %>% 
  view()

vis_miss(AHA_dataset)

#visualization of relationship between Births and 'Births or not', also to identify incorrect values
p2 <- AHA_dataset %>% 
  ggplot(aes(Births, `Births or Not`)) +
  geom_point(aes(col = Births), size = 2) +
  scale_x_log10() +
  #scale_y_log10() +
  labs(title = "Relationship between Births and 'Births or Not'",
       x = "Births",
       y = "Births or Not") +
  theme_economist()

#finding issues between 'Service Convert' and 'Gen Med & Sug or Not'
p3 <- AHA_dataset %>% 
  ggplot(aes(`Service Convert`, `Gen Med & Sug or Not`)) +
  geom_point(aes(col = `Service Convert`), size = 2) +
  labs(title = "Relationship between 'Service Convert' and 'Gen Med & Sug or Not'",
       x = "Service Convert",
       y = "Gen Med & Sug or Not") +
  theme_economist() +
  theme(axis.text.x = element_text(angle = 90))

#fix missing and incorrect value
AHA_dataset <- AHA_dataset %>% 
  mutate(`Births or Not` = if_else(Births > 0, 1, 0))

#check if the values still exist
vis_miss(AHA_dataset)

##### Data Transformation #####
# conduct appropriate formatting
# change variables to lower case and replace spaces with _
names(AHA_dataset) <- tolower(gsub(" ", "_", names(AHA_dataset))) 

#give appropriate formatting to the datatypes of the variables
AHA_dataset <- AHA_dataset %>% 
  mutate_if(is.character, as.factor)

##### EDA #####
# descriptive statistics
# numeric data

d1 <- AHA_dataset %>% 
  summarise_if(is.numeric, list(
    Mean = ~mean(.),
    Median = ~median(.),
    SD = ~ sd(.),
    Variance = ~var(.),
    Range = range(.),
    Quantile25 = ~quantile(., 0.25, na.rm = TRUE),
    Quantile75 = ~quantile(., 0.75, na.rm = TRUE),
    Sum = ~sum(.),
    Count = ~sum(.)
  ))