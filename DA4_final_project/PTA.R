rm(list=ls())

library(tidyverse)
library(readxl)
library(haven)
library(fixest)
library(ggplot2) 
library(dplyr)     
library(lubridate)  
library(skimr)
library(stargazer)
library(kableExtra)
library(corrplot)
library(xtable)

getwd()
setwd("C:/Users/Пользователь/Desktop/MA1y/Data_Analysis_4/DA4_final_project")

data <- read_delim("total_data.csv", delim = ",", quote = '"')
data <- data %>%
  mutate(group = ifelse(league == "E0", "Control", "Treated"))

yellow_card_trends <- data %>%
  mutate(season = factor(season, levels = unique(season)))%>%
  group_by(season, league, group) %>%
  summarise(total_y = sum(HT_yellow + AT_yellow),
            .groups = "drop")

ggplot(yellow_card_trends, aes(x = season, y = total_y, color = group, group = league)) +
  geom_line(size = 1.2) +  # Line for each league
  geom_point(size = 3) +   # Points for visibility
  geom_vline(xintercept = which(levels(yellow_card_trends$season) == "2016/2017"), 
             linetype = "dotted", color = "black", size = 1) +  # VAR introduction
  annotate("text", x = which(levels(yellow_card_trends$season) == "2016/2017") + 0.5, 
           y = max(yellow_card_trends$total_y) * 0.9, 
           label = "VAR Introduced", angle = 90, vjust = -0.5, size = 4) +
  labs(title = "Parallel Trend Assumption: Yellow Card Trends by Season",
       x = "Season",
       y = "Total Yellow Cards",
       color = "Group") +
  scale_x_discrete() +  # Use discrete scale for categorical season labels
  theme_minimal(base_size = 14) +
  theme(legend.position = "top", axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values = c("Treated" = "orange", "Control" = "red"))


# ADJUSTING FOR SEASONABILITY
library(zoo)  # For rolling averages
data <- data %>%
  mutate(month = format(as.Date(date), "%Y-%m"))  # Format YYYY-MM for aggregation

#YELLOW CARDS PER TEAM
monthly_y_cards_trends <- data %>%
  group_by(month, league, group) %>%
  summarise(avg_y_cards_per_team = sum(HT_yellow + AT_yellow) / n_distinct(c(HomeTeam, AwayTeam)),
            .groups = "drop")

# Convert month to date format for better x-axis scaling
monthly_y_cards_trends$month <- as.Date(paste0(monthly_y_cards_trends$month, "-01"))  

# Compute rolling mean (3-month window)
monthly_y_cards_trends <- monthly_y_cards_trends %>%
  arrange(month) %>%
  group_by(league, group) %>%
  mutate(rolling_avg_y_cards = rollmean(avg_y_cards_per_team, k = 3, fill = NA, align = "right"))

ggplot(monthly_y_cards_trends, aes(x = month, y = rolling_avg_y_cards, color = group, group = interaction(league, group))) +
  geom_line(size = 1) +
  geom_vline(xintercept = as.Date("2017-08-01"), linetype = "dotted", color = "black", size = 1) +  
  annotate("text", x = as.Date("2017-08-15"), 
           y = max(monthly_y_cards_trends$rolling_avg_y_cards, na.rm = TRUE) * 0.53, 
           label = "VAR Intro", angle = 90, vjust = -1, size = 4) +
  labs(title = "3-Month Rolling Average yellow cards per Team",
       x = "Month",
       y = "Yellow cards per Team",
       color = "Group") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")+
  scale_color_manual(values = c("Treated" = "orange", "Control" = "darkgreen"))



#Yellow Card TOTAL
monthly_y_cards_trends <- data %>%
  group_by(month, league, group) %>%
  summarise(avg_y_cards = sum(HT_yellow + AT_yellow), .groups = "drop")

# Convert month to date format for better x-axis scaling
monthly_y_cards_trends$month <- as.Date(paste0(monthly_y_cards_trends$month, "-01"))  

# Compute rolling mean (e.g., 3-month window)
monthly_y_cards_trends <- monthly_y_cards_trends %>%
  arrange(month) %>%
  group_by(league, group) %>%
  mutate(rolling_avg_y_cards = rollmean(avg_y_cards, k = 3, fill = NA, align = "right"))

ggplot(monthly_y_cards_trends, aes(x = month, y = rolling_avg_y_cards, color = group, group = interaction(league, group))) +
  geom_line(size = 1) +
  geom_vline(xintercept = as.Date("2017-08-01"), linetype = "dotted", color = "black", size = 1) +  
  annotate("text", x = as.Date("2017-08-15"), 
           y = max(monthly_y_cards_trends$rolling_avg_y_cards, na.rm = TRUE) * 0.5, 
           label = "VAR Intro", angle = 90, vjust = -1, size = 4) +
  labs(title = "3-Month Rolling Average yellow cards within teams",
       x = "Month",
       y = "Avg yellow cards",
       color = "Group") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")+
  scale_color_manual(values = c("Treated" = "orange", "Control" = "darkgreen"))



