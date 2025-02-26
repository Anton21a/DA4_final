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
library(car)
library(zoo)
library(xtable)
rm(list=ls())

setwd("C:/Users/Пользователь/Desktop/MA1y/Data_Analysis_4/DA4_final_project")
data <- read_delim("data.csv", delim = ",", quote = '"')

data$...1 <- NULL

data0 <- data %>%
  group_by(id_match) %>%
  mutate(match_pair = paste(sort(team), collapse = " vs ")) %>%
  ungroup()

away_subset <- data0 %>%
  filter(status == 'AwayTeam') %>%
  select(id_match, team, yellow_cards, red_cards, top)
colnames(away_subset) <- c('id_match', 'A_team', 'A_yellow_cards',
                           'A_red_cards', 'A_top')
data0 <- data0 %>%
  filter(status == 'HomeTeam') 
data0 <- left_join(data0, away_subset, by = "id_match")

str(data0)
#is_top: 
#0: 0 top teams
#1: 1 top team
#2: both teams are top
data0$is_top <- ifelse(data0$top == 1 & data0$A_top == 1, 2,
                       ifelse((data0$top == 1 & data0$A_top == 0)|(data0$top == 0 & data0$A_top == 1), 1, 0))

data0 <- data0 %>%
  select(id_match, league, season, date, match_pair, team, A_team, 
         fouls_commited, fouls_opponent, yellow_cards, A_yellow_cards,
         red_cards, A_red_cards, top, A_top, is_top, everything()) 
data0 <- data0 %>%
  rename(
    HT = team,
    AT = A_team,
    H_fouls = fouls_commited,
    A_fouls = fouls_opponent,
    top_level = is_top 
  )
var_start_season = '2017/2018'
str(data0)

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

#---------------------Analysis-by-Matches-Unbalanced-Data-----------------------

match_season <- data0 %>%
  group_by(match, season, treated, postVar, top_level) %>%
  summarise(
    yellow_cards = sum(yellow_cards + A_yellow_cards, na.rm = TRUE),
    red_cards = sum(red_cards + A_red_cards, na.rm = TRUE),
    fouls = sum(H_fouls + A_fouls, na.rm = TRUE),
  ) %>%
  mutate(DID = postVar * treated,
         league = ifelse(treated == 1, 'Bundesliga', 'EPL'))%>%
  ungroup()


match_season2 <- match_season
match_season2 <- match_season2 %>%
  group_by(match) %>%
  summarize(avg_red_cards = mean(red_cards),
            avg_yellow_cards = mean(yellow_cards),
            avg_fouls = mean(fouls),
            DID = sum(DID),
            top_level = sum(top_level))

cor_data <- subset(match_season2, select = c('top_level',
                                            'avg_yellow_cards',
                                            'avg_fouls',
                                            'avg_red_cards',
                                            "DID"
))


corr_matrix <- cor(cor_data, use = "complete.obs", method = "pearson")

corrplot(corr_matrix, 
         method = "color",  # Use colors instead of numbers for a cleaner look
         type = "upper",     # Show only the upper triangle to save space
         tl.cex = 0.8,       # Reduce text size of variable names
         number.cex = 0.7,   # Reduce correlation value size
         col = colorRampPalette(c("blue", "white", "red"))(200),  # Gradient colors
         addCoef.col = "black",  # Show numbers in black on top of colors
         tl.col = "black",   # Keep variable labels in black for readability
         cl.cex = 0.8        # Reduce legend text size
)


print(xtable(head(match_season)), include.rownames = FALSE)

match_season %>%
  summarize(n_distinct(match))

descr <- skim(match_season)
print(xtable(descr), include.rownames = FALSE)

ggplot(match_season, aes(x = league, fill = league)) +
  geom_bar() +
  labs(title = "Number of Matches per League", x = "League", y = "Count") +
  theme_minimal()

ggplot(match_season, aes(x = league, y = red_cards, fill = factor(postVar))) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Total Red Cards per League (Pre-VAR vs. Post-VAR)",
       x = "League",
       y = "Total Red Cards",
       fill = "VAR Implementation") +
  theme_minimal()

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

library(slider)

trend_data <- data0 %>%
  group_by(league, season, month) %>%
  summarise(total_red_cards = sum(red_cards + A_red_cards, na.rm = TRUE), .groups = "drop") %>%
  arrange(league, month) %>%
  group_by(league) %>%
  mutate(avg_red_cards = slide_mean(total_red_cards, before = 2, complete = TRUE))

pre_VAR_dataB <- trend_data %>%
  filter(month <= "2016/2017",
         league == "Bundesliga") 

after_VAR_dataB <- trend_data %>%
  filter(month > "2016/2017",
         league == "Bundesliga")

pre_VAR_data <- trend_data %>%
  filter(month <= "2018/2019",
         league == "EPL") 

after_VAR_data <- trend_data %>%
  filter(month > "2016/2017",
         league == "EPL")


ggplot(trend_data, aes(x = month, y = avg_red_cards, color = league, group = league)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +  # Emphasize season points
  geom_vline(xintercept = "2016/2017", linetype = "dashed", color = "red", size = 1) +  # VAR introduction
  annotate("text", x = "2016/2017", y = max(trend_data$avg_red_cards, na.rm = TRUE), 
           label = "VAR Introduced (2016/2017)", color = "red", angle = 90, vjust = -1.5, hjust = 1) +
  geom_smooth(data = pre_VAR_data, aes(x = season, y = avg_red_cards, color = league), 
              method = "lm", formula = y ~ as.numeric(as.character(x)), 
              se = FALSE, linetype = "dotted") +
  geom_smooth(data = after_VAR_dataB, aes(x = season, y = avg_red_cards, color = league), 
              method = "lm", formula = y ~ as.numeric(as.character(x)), 
              se = FALSE, linetype = "dotted") +
  geom_smooth(data = pre_VAR_dataB, aes(x = season, y = avg_red_cards, color = league), 
              method = "lm", formula = y ~ as.numeric(as.character(x)), 
              se = FALSE, linetype = "dotted") +
  labs(title = "Parallel Trends Check: Avg Red Cards per Match of all Teams",
       x = "Season", y = "Avg Red Cards per Match", color = "League") +
  scale_x_discrete(limits = unique(trend_data$season)) +  # Keep full season labels on x-axis
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


trend_data <- data0 %>%
  group_by(season, league, top_level) %>%
  summarise(avg_red_cards = mean(red_cards, na.rm = TRUE), .groups = "drop")


trend_data <- trend_data %>%
  filter(top_level == 2)

pre_VAR_data <- trend_data %>%
  filter(season < "2017/2018") 

ggplot(trend_data, aes(x = season, y = avg_red_cards, color = league, group = league)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +  # Emphasize season points
  geom_vline(xintercept = "2016/2017", linetype = "dashed", color = "red", size = 1) +  # VAR introduction
  annotate("text", x = "2016/2017", y = max(trend_data$avg_red_cards, na.rm = TRUE), 
           label = "VAR Introduced (2016/2017)", color = "red", angle = 90, vjust = -1.5, hjust = 1) +
  geom_smooth(data = pre_VAR_data, aes(x = season, y = avg_red_cards, color = league), 
              method = "lm", formula = y ~ as.numeric(as.character(x)), 
              se = FALSE, linetype = "dotted") +
  labs(title = "Parallel Trends Check: Avg Red Cards per Match of unstable non-top Teams ",
       x = "Season", y = "Avg Red Cards per Match", color = "League") +
  scale_x_discrete(limits = unique(trend_data$season)) +  # Keep full season labels on x-axis
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

match_season$top_level <- as.factor(match_season$top_level)

lm1 <- lm(red_cards ~ treated + postVar + DID,
               data = match_season)
summary(lm1)


lm2 <- lm(red_cards ~ treated + postVar + DID +
                 fouls + top_level,
               data = match_season)
summary(lm2)

lm3 <- lm(red_cards ~ treated + postVar + DID +
            fouls + top_level + yellow_cards,
          data = match_season)
summary(lm3)

models <- list("w/o controls" = lm1, "w controls" = lm2, "w controls" = lm3)
modelsummary(models, 
             stars = TRUE, 
             title = "Regression Models",
             output = "lm_matches_unbalanced_table.tex")


panel_data <- pdata.frame(match_season, index = c("match", "season"))

FE1 <- feols(red_cards ~ treated + postVar + DID| match + season,
              data = panel_data, cluster = ~match)
summary(FE1)

FE2 <- feols(red_cards ~ treated + postVar + DID + 
                fouls + top_level | season,
                  data = panel_data, cluster = ~match)
summary(FE2)


FE3 <- feols(red_cards ~ treated + postVar + DID + 
               fouls + top_level + yellow_cards | season,
             data = panel_data, cluster = ~match)
summary(FE3)


FE4 <- feols(red_cards ~ treated + postVar + DID + 
               fouls + top_level + yellow_cards| match + season,
             data = panel_data, cluster = ~match)
summary(FE4)

models <- list("w/o controls+FE" = FE1, "w controls+time FE" = FE2,
               "w all controls+time FE" = FE3, "w controls+FE" = FE4)
modelsummary(models, 
             stars = TRUE, 
             title = "Regression Models",
             output = "fe_matches_unbalanced_table.tex")

#---------------------Analysis-by-Teams-Unbalanced-Data-------------------------
var_start_season = '2017/2018'

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
  group_by(team, season, treated, postVar, top) %>%
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

str(team_season)

cor_data <- subset(team_season, select = c('treated',
                                             'postVar',
                                             'top',
                                             'avg_foul',
                                             'total_points',
                                             'avg_yellow',
                                             "avg_red",
                                             'DID'
))


corr_matrix <- cor(cor_data, use = "complete.obs", method = "pearson")

corrplot(corr_matrix, 
         method = "color",  # Use colors instead of numbers for a cleaner look
         type = "upper",     # Show only the upper triangle to save space
         tl.cex = 0.8,       # Reduce text size of variable names
         number.cex = 0.7,   # Reduce correlation value size
         col = colorRampPalette(c("blue", "white", "red"))(200),  # Gradient colors
         addCoef.col = "black",  # Show numbers in black on top of colors
         tl.col = "black",   # Keep variable labels in black for readability
         cl.cex = 0.8        # Reduce legend text size
)


print(xtable(head(team_season)), include.rownames = FALSE)

descr <- skim(team_season)
print(xtable(descr), include.rownames = FALSE)

trend_data <- data_team %>%
  group_by(season, league) %>%
  summarise(avg_red_cards = sum(red_cards, na.rm = TRUE), .groups = "drop")
 
pre_VAR_data <- trend_data %>%
  filter(season < "2017/2018")

#trend_data <- trend_data %>%
#  filter(top_level == 0)

ggplot(trend_data, aes(x = season, y = avg_red_cards, color = league, group = league)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +  # Emphasize season points
  geom_vline(xintercept = "2016/2017", linetype = "dashed", color = "red", size = 1) +  # VAR introduction
  annotate("text", x = "2016/2017", y = max(trend_data$avg_red_cards, na.rm = TRUE), 
           label = "VAR Introduced (2016/2017)", color = "red", angle = 90, vjust = -1.5, hjust = 1) +
  geom_smooth(data = pre_VAR_data, aes(x = season, y = avg_red_cards, color = league), 
              method = "lm", formula = y ~ as.numeric(as.character(x)), 
              se = FALSE, linetype = "dotted") +
  labs(title = "Parallel Trends Check: Avg Red Cards per Match of stable Teams (Bundesliga vs EPL)",
       x = "Season", y = "Avg Red Cards per Match", color = "League") +
  scale_x_discrete(limits = unique(trend_data$season)) +  # Keep full season labels on x-axis
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

team_season$top <- as.factor(team_season$top)

team_season <- team_season %>%
  filter(matches > 1)

lm1 <- lm(avg_red ~ treated + postVar + DID,
          data = team_season)
summary(lm1)

lm2 <- lm(avg_red ~ treated + postVar + DID +
            avg_foul + top + avg_yellow,
          data = team_season)
summary(lm2)

lm3 <- lm(avg_red ~ treated + postVar + DID +
            avg_foul + top + avg_yellow + avg_yellow*top + avg_foul*top,
          data = team_season)
summary(lm3)

vif(lm3)

models <- list("w/o controls" = lm1, "w controls" = lm2, "w controls" = lm3)
modelsummary(models, 
             stars = TRUE, 
             title = "Regression Models",
             output = "lm_teams_unbalanced_table.tex")


panel_data_team <- pdata.frame(team_season, index = c("team", "season"))

FE1 <- feols(avg_red ~ DID| team + season,
             data = panel_data_team, cluster = ~team)
summary(FE1)


FE2 <- feols(avg_red ~ treated + postVar + DID + 
               avg_foul + top|season,
             data = panel_data_team, cluster = ~team)
summary(FE2)

FE3 <- feols(avg_red ~ treated + postVar + DID + 
               avg_foul + top + avg_yellow |team + season,
             data = panel_data_team, cluster = ~team)
summary(FE3)


FE4 <- feols(avg_red ~ DID + 
               avg_foul + top + avg_yellow  | team + season,
             data = panel_data_team, cluster = ~team)
summary(FE4)


models <- list("w/o controls+FE" = FE1, "w controls+time FE" = FE2,
               "w all controls+time FE" = FE3, "w controls+FE" = FE4)
modelsummary(models, 
             stars = TRUE, 
             title = "Regression Models",
             output = "fe_teams_unbalanced_table.tex")

#---------------------Analysis-by-Matches-Balanced-Data-------------------------

View(data0[data0$season == "2012/2013", ])

balanced_data <- data0 %>%
  filter(date >= '2012-08-24')

match_counts <- balanced_data %>%
  group_by(match) %>%
  summarise(season_count = n_distinct(season))

valid_matches <- match_counts %>%
  filter(season_count > 6) %>%
  pull(match)

balanced_data <- balanced_data %>%
  filter(match %in% valid_matches) %>%
  arrange(match)

balanced_data %>%
  group_by(league) %>%
  summarize(n(),
            n_distinct(match))


match_season_balanced <- balanced_data %>%
  group_by(match, season, treated, postVar, top_level) %>%
  summarise(
    yellow_cards = sum(yellow_cards + A_yellow_cards, na.rm = TRUE),
    red_cards = sum(red_cards + A_red_cards, na.rm = TRUE),
    fouls = sum(H_fouls + A_fouls, na.rm = TRUE),
  ) %>%
  mutate(DID = postVar * treated,
         league = ifelse(treated == 1, 'Bundesliga', 'EPL'))%>%
  ungroup()


trend_data <- balanced_data %>%
  group_by(league, season, month, top_level) %>%
  summarise(total_red_cards = sum(red_cards + A_red_cards, na.rm = TRUE), .groups = "drop") %>%
  arrange(league, month) %>%
  group_by(league) %>%
  mutate(avg_red_cards = slide_mean(total_red_cards, before = 3, complete = TRUE))

trend_data <- trend_data %>%
  filter(top_level == 2)

pre_VAR_dataB <- trend_data %>%
  filter(month <= "2016/2017",
         league == "Bundesliga") 

after_VAR_dataB <- trend_data %>%
  filter(month > "2016/2017",
         league == "Bundesliga")

pre_VAR_data <- trend_data %>%
  filter(month <= "2016/2017",
         league == "EPL") 

after_VAR_data <- trend_data %>%
  filter(month > "2016/2017",
         league == "EPL")


ggplot(trend_data, aes(x = month, y = avg_red_cards, color = league, group = league)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +  # Emphasize season points
  geom_vline(xintercept = "2016/2017", linetype = "dashed", color = "red", size = 1) +  # VAR introduction
  annotate("text", x = "2016/2017", y = max(trend_data$avg_red_cards, na.rm = TRUE), 
           label = "VAR Introduced (2016/2017)", color = "red", angle = 90, vjust = -1.5, hjust = 1) +
  geom_smooth(data = pre_VAR_data, aes(x = season, y = avg_red_cards, color = league), 
              method = "lm", formula = y ~ as.numeric(as.character(x)), 
              se = FALSE, linetype = "dotted") +
  geom_smooth(data = pre_VAR_dataB, aes(x = season, y = avg_red_cards, color = league), 
              method = "lm", formula = y ~ as.numeric(as.character(x)), 
              se = FALSE, linetype = "dotted") +
  labs(title = "Parallel Trends Check: Avg Red Cards per Match of stable Teams",
       x = "Season", y = "Avg Red Cards per Match", color = "League") +
  scale_x_discrete(limits = unique(trend_data$season)) +  # Keep full season labels on x-axis
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


trend_data <- balanced_data %>%
  group_by(league, season) %>%
  summarise(avg_red_cards = mean(red_cards + A_red_cards, na.rm = TRUE), .groups = "drop")

#trend_data <- trend_data %>%
 # filter(top_level == 2)

pre_VAR_dataB <- trend_data %>%
  filter(season <= "2016/2017",
         league == "Bundesliga") 

after_VAR_dataB <- trend_data %>%
  filter(season > "2016/2017",
         league == "Bundesliga")

pre_VAR_data <- trend_data %>%
  filter(season <= "2016/2017",
         league == "EPL") 

after_VAR_data <- trend_data %>%
  filter(season > "2016/2017",
         league == "EPL")


ggplot(trend_data, aes(x = season, y = avg_red_cards, color = league, group = league)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +  # Emphasize season points
  geom_vline(xintercept = "2016/2017", linetype = "dashed", color = "red", size = 1) +  # VAR introduction
  annotate("text", x = "2016/2017", y = max(trend_data$avg_red_cards, na.rm = TRUE), 
           label = "VAR Introduced (2016/2017)", color = "red", angle = 90, vjust = -1.5, hjust = 1) +
  geom_smooth(data = pre_VAR_data, aes(x = season, y = avg_red_cards, color = league), 
              method = "lm", formula = y ~ as.numeric(as.character(x)), 
              se = FALSE, linetype = "dotted") +
  geom_smooth(data = pre_VAR_dataB, aes(x = season, y = avg_red_cards, color = league), 
              method = "lm", formula = y ~ as.numeric(as.character(x)), 
              se = FALSE, linetype = "dotted") +
  labs(title = "Parallel Trends Check: Avg Red Cards per Match of stable Teams",
       x = "Season", y = "Avg Red Cards per Match", color = "League") +
  scale_x_discrete(limits = unique(trend_data$season)) +  # Keep full season labels on x-axis
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


match_season_balanced$top_level <- as.factor(match_season_balanced$top_level)

lm1 <- lm(red_cards ~ treated + postVar + DID,
          data = match_season_balanced)
summary(lm1)

lm2 <- lm(red_cards ~ treated + postVar + DID +
            fouls + top_level + yellow_cards,
          data = match_season_balanced)
summary(lm2)

lm3 <- lm(red_cards ~ treated + postVar + DID +
            fouls + top_level +
            yellow_cards + yellow_cards*top_level,
          data = match_season_balanced)
summary(lm3)

models <- list("w/o controls" = lm1, "w controls" = lm2, "w controls" = lm3)
modelsummary(models, 
             stars = TRUE, 
             title = "Regression Models",
             output = "lm_matches_balanced_table.tex")

match_season_balanced %>%
  group_by(top_level) %>%
  summarize(n())

balanced_panel_data <- pdata.frame(match_season_balanced, index = c("match", "season"))

FE1 <- feols(red_cards ~ DID| match + season,
             data = balanced_panel_data, cluster = ~match)
summary(FE1)


FE2 <- feols(red_cards ~ DID + 
               fouls + top_level | match + season,
             data = balanced_panel_data, cluster = ~match)
summary(FE2)

FE3 <- feols(red_cards ~ DID + 
               fouls + top_level  +
               yellow_cards| match + season,
             data = balanced_panel_data, cluster = ~match)
summary(FE3)


FE4 <- feols(red_cards ~ DID + 
               fouls + top_level + fouls*top_level +
               yellow_cards + yellow_cards*top_level | match + season,
             data = balanced_panel_data, cluster = ~match)
summary(FE4)

vif(FE4)

models <- list("w/o yellow cards & interactions" = FE2,
               "w/o interactions" = FE3, "w interactions" = FE4)


modelsummary(models, 
             stars = TRUE, 
             title = "Regression Models",
             output = "fe_matches_balanced_table.tex")  


control_vars <- c("fouls", "top_level", "yellow_cards", "postVar")

modelsummary(models, 
             stars = TRUE, 
             title = "Regression Models",
             output = "fe_matches_balanced_reduced_table.tex",
             coef_omit = paste(control_vars, collapse = "|"),  
             add_rows = data.frame(
               term = "Controls", 
               `w/o interactions` = "YES",
               `with all` = "YES",
               `w/o yellow cards` = "YES"
             ))


