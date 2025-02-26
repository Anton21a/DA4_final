library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(sjlabelled)
library(ggrepel)
library(scales)
library(ggpubr)
library(fixest)
library(lmtest)
library(modelsummary)
library(skimr)
library(corrplot)
library(lubridate)
library(plm)
rm(list=ls())

setwd("C:/Users/Пользователь/Desktop/MA1y/Data_Analysis_4/DA4_final_project")
data <- read_delim("data2.csv", delim = ",", quote = '"')

data$...1 <- NULL

data0 <- data %>%
  group_by(id_match) %>%
  mutate(match_pair = paste(sort(team), collapse = " vs ")) %>%
  ungroup()

away_subset <- data0 %>%
  filter(status == 'AwayTeam') %>%
  select(id_match, team, yellow_cards, red_cards)
colnames(away_subset) <- c('id_match', 'A_team', 'A_yellow_cards',
                           'A_red_cards')
data0 <- data0 %>%
  filter(status == 'HomeTeam') 
data0 <- left_join(data0, away_subset, by = "id_match")

str(data0)

data0 <- data0 %>%
  select(id_match, league, season, date, match_pair, team, A_team, 
         fouls_commited, fouls_opponent, yellow_cards, A_yellow_cards,
         red_cards, A_red_cards, everything()) 
data0 <- data0 %>%
  rename(
    HT = team,
    AT = A_team,
    H_fouls = fouls_commited,
    A_fouls = fouls_opponent
  )
var_start_season = '2019/2020'

match_counts <- data0 %>%
  group_by(match_pair) %>%
  summarise(
    season_count = n_distinct(season),
    total_matches_after_VAR = sum(season >= var_start_season, na.rm = TRUE),  # Count occurrences after VAR
    total_matches_before_VAR = sum(season < var_start_season, na.rm = TRUE)   # Count occurrences before VAR
  )
match_counts <- match_counts %>%
  filter(total_matches_after_VAR != 0 & total_matches_before_VAR != 0)

valid_matches <- match_counts %>%
  pull(match_pair)

data0 <- data0 %>%
  filter(match_pair %in% valid_matches) %>%
  select(-match_pair)

data0 <- data0 %>%
  mutate(match = paste(HT, AT, sep = " vs "))

data0 %>%
  summarize(n_distinct(match))

teamd <- data %>%
  group_by(team, league, season, postVar)%>%
  summarize(avg_red = mean(red_cards))

ggplot(teamd, aes(x = as.factor(postVar), y = avg_red, fill = as.factor(postVar))) +
  geom_boxplot(alpha = 0.7, outlier.color = "red", outlier.shape = 16) +  # Adds boxplot with outlier settings
  facet_wrap(~league) +  # Divides the plot by league
  labs(title = "Average Red Cards Distribution by Post VAR and League",
       x = "Post VAR (0 = Before, 1 = After)",
       y = "Total Red Cards") +
  scale_fill_manual(values = c("blue", "orange"), name = "Post VAR") +  # Custom colors
  theme_minimal() +
  theme(legend.position = "top") +
  coord_cartesian(ylim = c(0, 0.3))

#---------------------Analysis-by-Matches-Unbalanced-Data-----------------------

match_season <- data0 %>%
  group_by(match, season, treated, postVar) %>%
  summarise(
    yellow_cards = sum(yellow_cards + A_yellow_cards, na.rm = TRUE),
    red_cards = sum(red_cards + A_red_cards, na.rm = TRUE),
    fouls = sum(H_fouls + A_fouls, na.rm = TRUE),
  ) %>%
  mutate(DID = postVar * treated,
         league = ifelse(treated == 1, 'Bundesliga 2', 'Championship'))%>%
  ungroup()

trend_data <- data0 %>%
  group_by(season, league) %>%
  summarise(avg_red_cards = mean(red_cards, na.rm = TRUE), .groups = "drop")
pre_VAR_data <- trend_data %>%
  filter(season < "2019/2020") 


ggplot(trend_data, aes(x = season, y = avg_red_cards, color = league, group = league)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +  # Emphasize season points
  geom_vline(xintercept = "2019/2020", linetype = "dashed", color = "red", size = 1) +  # VAR introduction
  annotate("text", x = "2019/2020", y = max(trend_data$avg_red_cards, na.rm = TRUE), 
           label = "VAR Introduced (2019/2020)", color = "red", angle = 90, vjust = -1.5, hjust = 1) +
  geom_smooth(data = pre_VAR_data, aes(x = season, y = avg_red_cards, color = league), 
              method = "lm", formula = y ~ as.numeric(as.character(x)), 
              se = FALSE, linetype = "dotted") +
  labs(title = "Parallel Trends Check: Avg Red Cards per Match of stable Teams (Bundesliga 2 vs Champonship)",
       x = "Season", y = "Avg Red Cards per Match", color = "League") +
  scale_x_discrete(limits = unique(trend_data$season)) +  # Keep full season labels on x-axis
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

lm1 <- lm(red_cards ~ treated + postVar + DID,
          data = match_season)
summary(lm1)

lm2 <- lm(red_cards ~ treated + postVar + DID +
            fouls,
          data = match_season)
summary(lm2)

lm3 <- lm(red_cards ~ treated + postVar + DID +
            fouls + yellow_cards,
          data = match_season)
summary(lm3)

models <- list("w/o controls" = lm1, "w controls" = lm2, "w controls" = lm3)
modelsummary(models, 
             stars = TRUE, 
             title = "Regression Models")


panel_data <- pdata.frame(match_season, index = c("match", "season"))

FE1 <- feols(red_cards ~ DID| match + season,
             data = panel_data, cluster = ~match)
summary(FE1)


FE2 <- feols(red_cards ~ treated + postVar + DID + 
               fouls | season,
             data = panel_data, cluster = ~match)
summary(FE2)

FE3 <- feols(red_cards ~ treated + postVar +DID + 
               fouls + yellow_cards | season,
             data = panel_data, cluster = ~match)
summary(FE3)

FE4 <- feols(red_cards ~ treated + postVar +DID + 
               fouls | match + season,
             data = panel_data, cluster = ~match)
summary(FE4)

models <- list("w/o controls + FE" = FE1, "w controls + time FE" = FE2,
               "w all controls + time FE" = FE3, "w controls + FE" = FE4)
modelsummary(models, 
             stars = TRUE, 
             title = "Regression Models",
             output = "fe2_matches_unbalanced_table.tex")

#---------------------Analysis-by-Teams-Unbalanced-Data-------------------------
var_start_season = '2019/2020'

team_counts <- data %>%
  group_by(team) %>%
  summarise(
    season_count = n_distinct(season),
    total_matches_after_VAR = sum(season >= var_start_season, na.rm = TRUE),  
    total_matches_before_VAR = sum(season < var_start_season, na.rm = TRUE)  
  )

team_counts <- team_counts %>%
  filter(total_matches_after_VAR != 0 & total_matches_before_VAR != 0)

valid_team <- team_counts %>%
  pull(team)

data_team <- data %>%
  filter(team %in% valid_team) 

team_season <- data_team %>%
  group_by(team, season, treated, postVar) %>%
  summarise(
    total_matches = n(),
    total_yellow_cards = sum(yellow_cards, na.rm = TRUE),
    total_red_cards = sum(red_cards, na.rm = TRUE),
    total_fouls_committed = sum(fouls_commited, na.rm = TRUE),
    total_points = sum(case_when(result == "W" ~ 3, result == "D" ~ 1, result == "L" ~ 0, TRUE ~ 0)),
    
    avg_fouls_commited_per_match = mean(fouls_commited, na.rm = TRUE),
    avg_fouls_total_per_match = mean(fouls_commited + fouls_opponent, na.rm = TRUE),
    
    avg_yellow_cards_per_match = mean(yellow_cards, na.rm = TRUE),
    avg_red_cards_per_match = mean(red_cards, na.rm = TRUE),
  ) %>%
  mutate(DID = postVar * treated)%>%
  ungroup()

str(team_season)
team_season <- team_season %>%
  rename(
    avg_fouls = avg_fouls_commited_per_match,
    avg_yellow = avg_yellow_cards_per_match,
    avg_red = avg_red_cards_per_match,
    avg_foul = avg_fouls_commited_per_match,
    matches = total_matches,
    yellow_cards = total_yellow_cards,
    red_cards= total_red_cards,
    fouls = total_fouls_committed 
  )

team_season %>%
  group_by(treated)%>%
  summarize(n(),
            n_distinct(team))

trend_data <- data_team %>%
  group_by(season, league) %>%
  summarise(avg_red_cards = sum(red_cards, na.rm = TRUE), .groups = "drop")

pre_VAR_data <- trend_data %>%
  filter(season < "2019/2020")

ggplot(trend_data, aes(x = season, y = avg_red_cards, color = league, group = league)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +  # Emphasize season points
  geom_vline(xintercept = "2019/2020", linetype = "dashed", color = "red", size = 1) +  # VAR introduction
  annotate("text", x = "2019/2020", y = max(trend_data$avg_red_cards, na.rm = TRUE), 
           label = "VAR Introduced (2019/2020)", color = "red", angle = 90, vjust = -1.5, hjust = 1) +
  geom_smooth(data = pre_VAR_data, aes(x = season, y = avg_red_cards, color = league), 
              method = "lm", formula = y ~ as.numeric(as.character(x)), 
              se = FALSE, linetype = "dotted") +
  labs(title = "Parallel Trends Check: Avg Red Cards per Match of stable Teams (Bundesliga 2 vs Championship)",
       x = "Season", y = "Avg Red Cards per Match", color = "League") +
  scale_x_discrete(limits = unique(trend_data$season)) +  # Keep full season labels on x-axis
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

lm1 <- lm(red_cards ~ treated + postVar + DID,
          data = team_season)
summary(lm1)

lm2 <- lm(red_cards ~ treated + postVar + DID +
            fouls,
          data = team_season)
summary(lm2)

lm3 <- lm(red_cards ~ treated + postVar + DID +
            fouls + yellow_cards,
          data = team_season)
summary(lm3)

models <- list("w/o controls" = lm1, "w controls" = lm2, "w controls" = lm3)
modelsummary(models, 
             stars = TRUE, 
             title = "Regression Models")


panel_data_team <- pdata.frame(team_season, index = c("team", "season"))

FE1 <- feols(red_cards ~ DID| team + season,
             data = panel_data_team, cluster = ~team)
summary(FE1)


FE2 <- feols(red_cards ~ treated + postVar + DID + 
               fouls | team + season,
             data = panel_data_team, cluster = ~team)
summary(FE2)

FE3 <- feols(red_cards ~ treated + postVar + DID + 
               fouls + yellow_cards | team + season,
             data = panel_data_team, cluster = ~team)
summary(FE3)

FE4 <- feols(red_cards ~ treated + postVar +DID + 
               fouls | team + season,
             data = panel_data_team, cluster = ~team)
summary(FE4)

models <- list("w/o controls + FE" = FE1, "w controls + time FE" = FE2,
               "w all controls + time FE" = FE3, "w controls + FE" = FE4)
modelsummary(models, 
             stars = TRUE, 
             title = "Regression Models")



#---------------------Analysis-by-Matches-Balanced-Data-------------------------

View(data0[data0$season == "2023/2024", ])

balanced_data <- data0 %>%
  filter(date <= '2023-07-29')

match_counts <- balanced_data %>%
  group_by(match) %>%
  summarise(season_count = n_distinct(season))

valid_matches <- match_counts %>%
  filter(season_count > 5) %>%
  pull(match)

balanced_data <- balanced_data %>%
  filter(match %in% valid_matches) %>%
  arrange(match)

balanced_data %>%
  group_by(league) %>%
  summarize(n(),
            n_distinct(match))


match_season_balanced <- balanced_data %>%
  group_by(match, season, treated, postVar) %>%
  summarise(
    yellow_cards = sum(yellow_cards + A_yellow_cards, na.rm = TRUE),
    red_cards = sum(red_cards + A_red_cards, na.rm = TRUE),
    fouls = sum(H_fouls + A_fouls, na.rm = TRUE),
  ) %>%
  mutate(DID = postVar * treated,
         league = ifelse(treated == 1, 'Bundesliga', 'EPL'))%>%
  ungroup()

trend_data <- balanced_data %>%
  group_by(season, league) %>%
  summarise(avg_red_cards = mean(red_cards, na.rm = TRUE), .groups = "drop")

pre_VAR_data <- trend_data %>%
  filter(season < "2019/2020") 


ggplot(trend_data, aes(x = season, y = avg_red_cards, color = league, group = league)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +  # Emphasize season points
  geom_vline(xintercept = "2019/2020", linetype = "dashed", color = "red", size = 1) +  # VAR introduction
  annotate("text", x = "2019/2020", y = max(trend_data$avg_red_cards, na.rm = TRUE), 
           label = "VAR Introduced (2019/2020)", color = "red", angle = 90, vjust = -1.5, hjust = 1) +
  geom_smooth(data = pre_VAR_data, aes(x = season, y = avg_red_cards, color = league), 
              method = "lm", formula = y ~ as.numeric(as.character(x)), 
              se = FALSE, linetype = "dotted") +
  labs(title = "Parallel Trends Check: Avg Red Cards per Match of stable Teams (Bundesliga vs EPL)",
       x = "Season", y = "Avg Red Cards per Match", color = "League") +
  scale_x_discrete(limits = unique(trend_data$season)) +  # Keep full season labels on x-axis
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

lm1 <- lm(red_cards ~ treated + postVar + DID,
          data = match_season_balanced)
summary(lm1)

lm2 <- lm(red_cards ~ treated + postVar + DID +
            fouls,
          data = match_season_balanced)
summary(lm2)

lm3 <- lm(red_cards ~ treated + postVar + DID +
            fouls + yellow_cards,
          data = match_season_balanced)
summary(lm3)

models <- list("w/o controls" = lm1, "w controls" = lm2, "w controls" = lm3)
modelsummary(models, 
             stars = TRUE, 
             title = "Regression Models")


balanced_panel_data <- pdata.frame(match_season_balanced, index = c("match", "season"))

FE1 <- feols(red_cards ~ DID| match + season,
             data = balanced_panel_data, cluster = ~match)
summary(FE1)


FE2 <- feols(red_cards ~ treated + postVar + DID + 
               fouls | season,
             data = balanced_panel_data, cluster = ~match)
summary(FE2)

FE3 <- feols(red_cards ~ treated + postVar +DID + 
               fouls + yellow_cards | season,
             data = balanced_panel_data, cluster = ~match)
summary(FE3)

FE4 <- feols(red_cards ~ treated + postVar +DID + 
               fouls | match + season,
             data = balanced_panel_data, cluster = ~match)
summary(FE4)

models <- list("w/o controls + FE" = FE1, "w controls + time FE" = FE2,
               "w all controls + time FE" = FE3, "w controls + FE" = FE4)
modelsummary(models, 
             stars = TRUE, 
             title = "Regression Models",
             output = "fe2_matches_balanced_table.tex")

