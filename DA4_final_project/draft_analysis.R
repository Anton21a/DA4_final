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
rm(list=ls())

setwd("C:/Users/Пользователь/Desktop/MA1y/Data_Analysis_4/DA4_final_project")
data <- read_delim("data.csv", delim = ",", quote = '"')

data$...1 <- NULL

#-------------------------------Team-Season-SETUP-------------------------------

team_season_data <- data %>%
  group_by(team, season, treated, postVar, top) %>%
  summarise(
    total_matches = n(),
    
    total_yellow_cards = sum(yellow_cards, na.rm = TRUE),
    total_red_cards = sum(red_cards, na.rm = TRUE),
    total_fouls_committed = sum(fouls_commited, na.rm = TRUE),
    total_opponents_fouls = sum(fouls_opponent, na.rm = TRUE),
    total_fouls = sum(total_fouls_committed + total_opponents_fouls, na.rm = TRUE),
    total_goals_scored = sum(goals_scored, na.rm = TRUE),
    total_goals_conceded = sum(goals_conceded, na.rm = TRUE),
    total_points = sum(case_when(result == "W" ~ 3, result == "D" ~ 1, result == "L" ~ 0, TRUE ~ 0)),
    
    avg_fouls_commited_per_match = mean(fouls_commited, na.rm = TRUE),
    avg_fouls_suffered_per_match = mean(fouls_opponent, na.rm = TRUE),
    avg_fouls_total_per_match = mean(fouls_commited + fouls_opponent, na.rm = TRUE),
    
    avg_yellow_cards_per_match = mean(yellow_cards, na.rm = TRUE),
    avg_red_cards_per_match = mean(red_cards, na.rm = TRUE),
    avg_scored_goals_per_match = mean(goals_scored, na.rm = TRUE),
    avg_conceded_goals_per_match = mean(goals_conceded, na.rm = TRUE)
  
  ) %>%
  mutate(DID = postVar * treated)%>%
ungroup()

#descriptive analysis
skim(team_season_data)

#graphs
team_season_data <- team_season_data %>%
  mutate(league_label = ifelse(treated == 1, "Bundesliga", "EPL"))

ggplot(team_season_data, aes(x = as.factor(postVar), y = total_red_cards, fill = as.factor(postVar))) +
  geom_boxplot(alpha = 0.7, outlier.color = "red", outlier.shape = 16) +  # Adds boxplot with outlier settings
  facet_wrap(~league_label) +  # Divides the plot by league
  labs(title = "Boxplot: Total Red Cards Distribution by Post VAR and League",
       x = "Post VAR (0 = Before, 1 = After)",
       y = "Total Red Cards") +
  scale_fill_manual(values = c("blue", "orange"), name = "Post VAR") +  # Custom colors
  theme_minimal() +
  theme(legend.position = "top")

# Create scatter plot
ggplot(team_season_data, aes(x = avg_red_cards_per_match, y = avg_fouls_commited_per_match)) +
  geom_point(color = "blue", alpha = 0.7) +  # Scatter plot points
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "red") +  # Linear regression trend line
  labs(title = "Scatter Plot: Avg Red Cards vs. Avg fouls commited",
       x = "Total Red Cards",
       y = "Total Points") +
  theme_minimal()



team_season_data <- team_season_data %>%
  filter(total_matches > 1)

str(team_season_data)


cor_data <- subset(team_season_data, select = c('avg_red_cards_per_match',
                                                'avg_fouls_total_per_match',
                                                'avg_yellow_cards_per_match',
                                                'avg_fouls_commited_per_match',
                                                'avg_fouls_suffered_per_match',
                                                'avg_scored_goals_per_match',
                                                'avg_conceded_goals_per_match',
                                                'total_points',
                                                'top',
                                                "treated",
                                                'postVar',
                                                "DID"
                                                ))
corr_matrix <- cor(cor_data, use = "complete.obs", method = "pearson")
colnames(corr_matrix) <- rownames(corr_matrix) <- c(
  "Red Cards (Avg)",
  "Total Fouls (Avg)",
  "Yellow Cards (Avg)",
  "Fouls Committed (Avg)",
  "Fouls Suffered (Avg)",
  "Goals Scored (Avg)",
  "Goals Conceded (Avg)",
  "Total points",
  "Top Team",
  "league",
  "Post VAR",
  "DID"
)
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

n_distinct(team_season_data$team)


lm_reg1 <- lm(avg_red_cards_per_match ~ treated + postVar + DID,
              data = team_season_data)
summary(lm_reg1)

lm_reg2 <- lm(avg_red_cards_per_match ~ treated + postVar + DID +
              avg_fouls_commited_per_match +
              total_points + top,
              data = team_season_data)
summary(lm_reg2)

models <- list("w/o controls" = lm_reg1, "w controls" = lm_reg2)
modelsummary(models, 
             stars = TRUE, 
             title = "Regression Models")


panel_data <- pdata.frame(team_season_data, index = c("team", "season"))

FE1 <- feols(avg_red_cards_per_match ~ treated + postVar + DID| team + season,
             data = panel_data, cluster = ~team)
summary(FE1)


FE2 <- feols(avg_red_cards_per_match ~ DID + 
               avg_fouls_commited_per_match +
               total_points + top | team + season,
              data = panel_data, cluster = ~team)
summary(FE2)

FE3 <- feols(avg_red_cards_per_match ~ DID + 
               avg_fouls_commited_per_match +
               total_points + top + avg_yellow_cards_per_match| team + season,
             data = panel_data, cluster = ~team)
summary(FE3)

models <- list("w/o controls + FE" = FE1, "w controls + FE" = FE2, "w controls + FE" = FE3)
modelsummary(models, 
             stars = TRUE, 
             title = "Regression Models")


#-------------------------------Team-month-SETUP--------------------------------

team_month_data <- data %>%
  group_by(team, month, treated, postVar, top) %>%
  summarise(
    total_matches = n(),
    
    total_yellow_cards = sum(yellow_cards, na.rm = TRUE),
    total_red_cards = sum(red_cards, na.rm = TRUE),
    total_fouls_committed = sum(fouls_commited, na.rm = TRUE),
    total_opponents_fouls = sum(fouls_opponent, na.rm = TRUE),
    total_fouls = sum(total_fouls_committed + total_opponents_fouls, na.rm = TRUE),
    total_goals_scored = sum(goals_scored, na.rm = TRUE),
    total_goals_conceded = sum(goals_conceded, na.rm = TRUE),
    total_points = sum(case_when(result == "W" ~ 3, result == "D" ~ 1, result == "L" ~ 0, TRUE ~ 0)),
    
    avg_fouls_commited_per_match = mean(fouls_commited, na.rm = TRUE),
    avg_fouls_suffered_per_match = mean(fouls_opponent, na.rm = TRUE),
    avg_fouls_total_per_match = mean(fouls_commited + fouls_opponent, na.rm = TRUE),
    
    avg_yellow_cards_per_match = mean(yellow_cards, na.rm = TRUE),
    avg_red_cards_per_match = mean(red_cards, na.rm = TRUE),
    avg_scored_goals_per_match = mean(goals_scored, na.rm = TRUE),
    avg_conceded_goals_per_match = mean(goals_conceded, na.rm = TRUE)
    
  ) %>%
  mutate(DID = postVar * treated)%>%
  ungroup()

cor_data <- subset(team_month_data, select = c('avg_red_cards_per_match',
                                                'avg_fouls_total_per_match',
                                                'avg_yellow_cards_per_match',
                                                'avg_fouls_commited_per_match',
                                                'avg_fouls_suffered_per_match',
                                                'avg_scored_goals_per_match',
                                                'avg_conceded_goals_per_match',
                                                'total_points',
                                                'top',
                                                "treated",
                                                'postVar',
                                                "DID"
))
corr_matrix <- cor(cor_data, use = "complete.obs", method = "pearson")
colnames(corr_matrix) <- rownames(corr_matrix) <- c(
  "Red Cards (Avg)",
  "Total Fouls (Avg)",
  "Yellow Cards (Avg)",
  "Fouls Committed (Avg)",
  "Fouls Suffered (Avg)",
  "Goals Scored (Avg)",
  "Goals Conceded (Avg)",
  "Total points",
  "Top Team",
  "league",
  "Post VAR",
  "DID"
)
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

lm_reg1 <- lm(avg_red_cards_per_match ~ treated + postVar + DID,
              data = team_month_data)
summary(lm_reg1)

lm_reg2 <- lm(avg_red_cards_per_match ~ treated + postVar + DID +
                avg_fouls_commited_per_match +
                total_points + top,
              data = team_month_data)
summary(lm_reg2)

models <- list("w/o controls" = lm_reg1, "w controls" = lm_reg2)
modelsummary(models, 
             stars = TRUE, 
             title = "Regression Models")

monthlypanel_data <- pdata.frame(team_month_data, index = c("team", "month"))

FE1 <- feols(avg_red_cards_per_match ~ DID| team + month,
             data = monthlypanel_data, cluster = ~team)
summary(FE1)


FE2 <- feols(avg_red_cards_per_match ~ DID + 
               avg_fouls_commited_per_match +
               total_points + top | team + month,
             data = monthlypanel_data, cluster = ~team)
summary(FE2)

FE3 <- feols(avg_red_cards_per_match ~ DID + 
               avg_fouls_commited_per_match +
               total_points + top + avg_yellow_cards_per_match| team + month,
             data = monthlypanel_data, cluster = ~team)
summary(FE3)

models <- list("w/o controls + FE" = FE1, "w controls + FE" = FE2, "w controls + FE" = FE3)
modelsummary(models, 
             stars = TRUE, 
             title = "Regression Models")

#-----------------------Team-month-SETUP-Rolling-average------------------------

team_roll_month_data <- data %>%
  group_by(team, month, treated, postVar, top) %>%
  summarise(
    total_matches = n(),
    
    total_yellow_cards = sum(yellow_cards, na.rm = TRUE),
    total_red_cards = sum(red_cards, na.rm = TRUE),
    total_fouls_committed = sum(fouls_commited, na.rm = TRUE),
    total_opponents_fouls = sum(fouls_opponent, na.rm = TRUE),
    total_fouls = sum(total_fouls_committed + total_opponents_fouls, na.rm = TRUE),
    total_goals_scored = sum(goals_scored, na.rm = TRUE),
    total_goals_conceded = sum(goals_conceded, na.rm = TRUE),
    total_points = sum(case_when(result == "W" ~ 3, result == "D" ~ 1, result == "L" ~ 0, TRUE ~ 0)),
    
    avg_fouls_commited_per_match = mean(fouls_commited, na.rm = TRUE),
    avg_fouls_suffered_per_match = mean(fouls_opponent, na.rm = TRUE),
    avg_fouls_total_per_match = mean(fouls_commited + fouls_opponent, na.rm = TRUE),
    
    avg_yellow_cards_per_match = mean(yellow_cards, na.rm = TRUE),
    avg_red_cards_per_match = mean(red_cards, na.rm = TRUE),
    avg_scored_goals_per_match = mean(goals_scored, na.rm = TRUE),
    avg_conceded_goals_per_match = mean(goals_conceded, na.rm = TRUE)
    
  ) %>%
  mutate(DID = postVar * treated)%>%
  ungroup()

team_roll_month_data <- team_roll_month_data %>%
  arrange(team, month) %>%  # Ensure ordering within each team
  group_by(team) %>%  # Apply rolling mean per team
  mutate(roll_avg_red_cards_per_match = rollmeanr(avg_red_cards_per_match, k = 4, fill = NA)) %>%
  ungroup()

team_roll_month_data <- na.omit(team_roll_month_data)

cor_data <- subset(team_roll_month_data, select = c('roll_avg_red_cards_per_match',
                                               'avg_fouls_total_per_match',
                                               'avg_yellow_cards_per_match',
                                               'avg_fouls_commited_per_match',
                                               'avg_fouls_suffered_per_match',
                                               'avg_scored_goals_per_match',
                                               'avg_conceded_goals_per_match',
                                               'total_points',
                                               'top',
                                               "treated",
                                               'postVar',
                                               "DID"
))
corr_matrix <- cor(cor_data, use = "complete.obs", method = "pearson")
colnames(corr_matrix) <- rownames(corr_matrix) <- c(
  "Red Cards (Avg)",
  "Total Fouls (Avg)",
  "Yellow Cards (Avg)",
  "Fouls Committed (Avg)",
  "Fouls Suffered (Avg)",
  "Goals Scored (Avg)",
  "Goals Conceded (Avg)",
  "Total points",
  "Top Team",
  "league",
  "Post VAR",
  "DID"
)
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

lm_reg1 <- lm(avg_red_cards_per_match ~ treated + postVar + DID,
              data = team_roll_month_data)
summary(lm_reg1)

lm_reg2 <- lm(avg_red_cards_per_match ~ treated + postVar + DID +
                avg_fouls_commited_per_match +
                total_points + top,
              data = team_roll_month_data)
summary(lm_reg2)

models <- list("w/o controls" = lm_reg1, "w controls" = lm_reg2)
modelsummary(models, 
             stars = TRUE, 
             title = "Regression Models")

rollmonthlypanel_data <- pdata.frame(team_roll_month_data, index = c("team", "month"))

FE1 <- feols(avg_red_cards_per_match ~ DID| team + month,
             data = rollmonthlypanel_data, cluster = ~team)
summary(FE1)


FE2 <- feols(avg_red_cards_per_match ~ DID + 
               avg_fouls_commited_per_match +
               total_points + top | team + month,
             data = rollmonthlypanel_data, cluster = ~team)
summary(FE2)

FE3 <- feols(avg_red_cards_per_match ~ DID + 
               avg_fouls_commited_per_match +
               total_points + top + avg_yellow_cards_per_match| team + month,
             data = rollmonthlypanel_data, cluster = ~team)
summary(FE3)

models <- list("w/o controls + FE" = FE1, "w controls + FE" = FE2, "w controls + FE" = FE3)
modelsummary(models, 
             stars = TRUE, 
             title = "Regression Models")

#--------------------------------Checking_PTA-----------------------------------
trend_data <- team_season_data %>%
  mutate(season_numeric = as.numeric(substr(season, 6, 9))) %>%  # Extract second year (e.g., "2008/2009" → 2009)
  group_by(season_numeric, treated) %>%
  summarise(avg_red_cards = mean(avg_red_cards_per_match, na.rm = TRUE))

# Convert treated to factor for legend clarity
trend_data$treated <- factor(trend_data$treated, labels = c("EPL (Control)", "Bundesliga (Treated)"))

# Plot trends with correctly formatted seasons
ggplot(trend_data, aes(x = season_numeric, y = avg_red_cards, color = treated, group = treated)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +  # Emphasize season points
  geom_vline(xintercept = 2017.5, linetype = "dashed", color = "red", size = 1) +  # VAR in Aug 2017 (start of 2017/18)
  annotate("text", x = 2017.7, y = max(trend_data$avg_red_cards, na.rm = TRUE), 
           label = "VAR Introduced (Aug 2017)", color = "red", angle = 90, vjust = -0.5, hjust = 1) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, linetype = "dotted") +  # Trend lines
  labs(title = "Parallel Trends Check: Avg Red Cards per Match (Bundesliga vs EPL)",
       x = "Season", y = "Avg Red Cards per Match", color = "League") +
  scale_x_continuous(breaks = seq(min(trend_data$season_numeric, na.rm = TRUE), 
                                  max(trend_data$season_numeric, na.rm = TRUE), by = 1)) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#-------------------------------------------------------------------------------
str(team_month_data)
team_month_data <- team_month_data %>%
  mutate(month_date = ym(month))  # Convert month column to Date format YYYY-MM

# Compute average red cards per match by month and treatment group
trend_monthly_data <- team_month_data %>%
  group_by(month_date, treated) %>%
  summarise(avg_red_cards = mean(avg_red_cards_per_match, na.rm = TRUE))

# Convert treated to factor for better plotting labels
trend_monthly_data$treated <- factor(trend_monthly_data$treated, labels = c("EPL (Control)", "Bundesliga (Treated)"))

ggplot(trend_monthly_data, aes(x = month_date, y = avg_red_cards, color = treated, group = treated)) +
  geom_line(size = 0.75) +
  geom_point(size = 1) +  # Add points to emphasize monthly values
  geom_vline(xintercept = as.Date("2017-08-01"), linetype = "dashed", color = "red", size = 1) +  # VAR in Aug 2017
  annotate("text", x = as.Date("2017-09-01"), y = max(trend_monthly_data$avg_red_cards, na.rm = TRUE), 
           label = "VAR Introduced (Aug 2017)", color = "red", angle = 90, vjust = -1.5, hjust = 1) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, linetype = "dotted") +  # Add separate trend lines
  labs(title = "Parallel Trends Check: Avg Red Cards per Match (Monthly, Bundesliga vs EPL)",
       x = "Month", y = "Avg Red Cards per Match", color = "League") +
  scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m") +  # Adjust x-axis labels
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#-------------------------------------------------------------------------------

team_roll_month_data <- team_roll_month_data %>%
  mutate(month_date = ym(month))  # Convert month column to Date format YYYY-MM

# Compute average red cards per match by month and treatment group
team_roll_month_data <- team_roll_month_data %>%
  group_by(month_date, treated) %>%
  summarise(avg_red_cards = mean(avg_red_cards_per_match, na.rm = TRUE))

# Convert treated to factor for better plotting labels
team_roll_month_data$treated <- factor(team_roll_month_data$treated, labels = c("EPL (Control)", "Bundesliga (Treated)"))

ggplot(team_roll_month_data, aes(x = month_date, y = avg_red_cards, color = treated, group = treated)) +
  geom_line(size = 0.75) +
  geom_point(size = 1) +  # Add points to emphasize monthly values
  geom_vline(xintercept = as.Date("2017-08-01"), linetype = "dashed", color = "red", size = 1) +  # VAR in Aug 2017
  annotate("text", x = as.Date("2017-09-01"), y = max(team_roll_month_data$avg_red_cards, na.rm = TRUE), 
           label = "VAR Introduced (Aug 2017)", color = "red", angle = 90, vjust = -1.5, hjust = 1) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, linetype = "dotted") +  # Add separate trend lines
  labs(title = "Parallel Trends Check: Avg Red Cards per Match (Monthly, Bundesliga vs EPL)",
       x = "Month", y = "Avg Red Cards per Match", color = "League") +
  scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m") +  # Adjust x-axis labels
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#------------------------Analysis-for-stable-teams------------------------------
data0 <- data %>%
  group_by(id_match) %>%
  mutate(match_pair = paste(sort(team), collapse = " vs ")) %>%
  ungroup()

match_counts <- data0 %>%
  group_by(match_pair) %>%
  summarise(season_count = n_distinct(season))

valid_matches <- match_counts %>%
  filter(season_count > 10) %>%
  pull(match_pair)

filtered_data <- data0 %>%
  filter(match_pair %in% valid_matches) %>%
  select(-match_pair)  # Remove temporary column

filtered_data <- filtered_data %>%
  group_by(id_match) %>%
  mutate(match_pair = paste(sort(team), collapse = " vs "))

away_subset <- filtered_data %>%
  filter(status == 'AwayTeam') %>%
  select(id_match, team, yellow_cards, red_cards, top)

colnames(away_subset) <- c('id_match', 'A_team', 'A_yellow_cards',
                           'A_red_cards', 'A_top')

filtered_data <- filtered_data %>%
  filter(status == 'HomeTeam') 

filtered_data <- left_join(filtered_data, away_subset, by = "id_match")

filtered_data <- filtered_data %>%
  mutate(
    total_yellow_cards = yellow_cards + A_yellow_cards,
    total_red_cards = red_cards + A_red_cards,
    total_fouls = fouls_commited + fouls_opponent,
    is_top = ifelse(top | A_top == 1, 1, 0)
  )

team_season_filter_data <- filtered_data %>%
  group_by(match_pair, season, treated, postVar, is_top) %>%
  summarise(
    yellow_cards = sum(total_yellow_cards, na.rm = TRUE),
    red_cards = sum(total_red_cards, na.rm = TRUE),
    fouls = sum(total_fouls, na.rm = TRUE),
  ) %>%
  mutate(DID = postVar * treated,
         league = ifelse(treated == 1, 'Bundesliga', 'EPL'))%>%
  ungroup()


trend_filtered_data <- filtered_data %>%
  group_by(season, league) %>%
  summarise(avg_red_cards = mean(red_cards, na.rm = TRUE), .groups = "drop")

ggplot(trend_filtered_data, aes(x = season, y = avg_red_cards, color = league, group = league)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +  # Emphasize season points
  geom_vline(xintercept = "2016/2017", linetype = "dashed", color = "red", size = 1) +  # VAR introduction
  annotate("text", x = "2016/2017", y = max(trend_filtered_data$avg_red_cards, na.rm = TRUE), 
           label = "VAR Introduced (2016/2017)", color = "red", angle = 90, vjust = -1.5, hjust = 1) +
  geom_smooth(method = "lm", formula = y ~ as.numeric(as.character(x)), se = FALSE, linetype = "dotted") +  # Trend lines
  labs(title = "Parallel Trends Check: Avg Red Cards per Match of stable Teams (Bundesliga vs EPL)",
       x = "Season", y = "Avg Red Cards per Match", color = "League") +
  scale_x_discrete(limits = unique(trend_filtered_data$season)) +  # Keep full season labels on x-axis
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


slm_reg1 <- lm(red_cards ~ treated + postVar + DID,
              data = team_season_filter_data)
summary(slm_reg1)

slm_reg2 <- lm(red_cards ~ treated + postVar + DID +
                fouls + is_top,
              data = team_season_filter_data)
summary(slm_reg2)

models <- list("w/o controls" = slm_reg1, "w controls" = slm_reg2)
modelsummary(models, 
             stars = TRUE, 
             title = "Regression Models")

stable_panel_data <- pdata.frame(team_season_filter_data, index = c("match_pair", "season"))

sFE1 <- feols(red_cards ~ DID| match_pair + season,
             data = stable_panel_data, cluster = ~match_pair)
summary(sFE1)


sFE2 <- feols(red_cards ~ DID + 
               fouls + is_top | match_pair + season,
               data = stable_panel_data, cluster = ~match_pair)
summary(sFE2)

sFE3 <- feols(red_cards ~ DID + 
               fouls + is_top + yellow_cards| match_pair + season,
               data = stable_panel_data, cluster = ~match_pair)
summary(sFE3)

models <- list("w/o controls + FE" = sFE1, "w controls + FE" = sFE2, "w controls + FE" = sFE3)
modelsummary(models, 
             stars = TRUE, 
             title = "Regression Models")


#------------------------Analysis-for-stable-teams-2-part-----------------------
View(data0[data0$season == "2012/2013", ])

data00 <- data0 %>%
  filter(date >= '2012-08-24')

match_counts1 <- data00 %>%
  group_by(match_pair) %>%
  summarise(season_count = n_distinct(season))

valid_matches1 <- match_counts1 %>%
  filter(season_count > 6) %>%
  pull(match_pair)

filtered_data1 <- data00 %>%
  filter(match_pair %in% valid_matches1) %>%
  select(-match_pair)  # Remove temporary column

filtered_data1 <- filtered_data1 %>%
  group_by(id_match) %>%
  mutate(match_pair = paste(sort(team), collapse = " vs "))

away_subset1 <- filtered_data1 %>%
  filter(status == 'AwayTeam') %>%
  select(id_match, team, yellow_cards, red_cards, top)

colnames(away_subset1) <- c('id_match', 'A_team', 'A_yellow_cards',
                           'A_red_cards', 'A_top')

filtered_data1 <- filtered_data1 %>%
  filter(status == 'HomeTeam') 

filtered_data1 <- left_join(filtered_data1, away_subset1, by = "id_match")

filtered_data1 <- filtered_data1 %>%
  mutate(
    total_yellow_cards = yellow_cards + A_yellow_cards,
    total_red_cards = red_cards + A_red_cards,
    total_fouls = fouls_commited + fouls_opponent,
    is_top = ifelse(top | A_top == 1, 1, 0)
  )

team_season_filter_data1 <- filtered_data1 %>%
  group_by(match_pair, season, treated, postVar, is_top) %>%
  summarise(
    yellow_cards = sum(total_yellow_cards, na.rm = TRUE),
    red_cards = sum(total_red_cards, na.rm = TRUE),
    fouls = sum(total_fouls, na.rm = TRUE),
  ) %>%
  mutate(DID = postVar * treated,
         league = ifelse(treated == 1, 'Bundesliga', 'EPL'))%>%
  ungroup()


trend_filtered_data1 <- filtered_data1 %>%
  group_by(season, league) %>%
  summarise(avg_red_cards = mean(red_cards, na.rm = TRUE), .groups = "drop")

pre_VAR_data <- trend_filtered_data1 %>%
  filter(season < "2017/2018")  # Select only pre-VAR seasons

# Plot with separate trend line dataset
ggplot(trend_filtered_data1, aes(x = season, y = avg_red_cards, color = league, group = league)) +
  geom_line(size = 1.2) +  # Actual trend line
  geom_point(size = 2) +  # Emphasize season points
  geom_vline(xintercept = "2016/2017", linetype = "dashed", color = "red", size = 1) +  # VAR introduction
  annotate("text", x = "2016/2017", y = max(trend_filtered_data1$avg_red_cards, na.rm = TRUE), 
           label = "VAR Introduced (2016/2017)", color = "red", angle = 90, vjust = -1.5, hjust = 1) +
  # Trend line stops at 2016/2017
  geom_smooth(data = pre_VAR_data, aes(x = season, y = avg_red_cards, color = league), 
              method = "lm", formula = y ~ as.numeric(as.character(x)), 
              se = FALSE, linetype = "dotted") +  
  labs(title = "Parallel Trends Check: Avg Red Cards per Match of Stable Teams (Bundesliga vs EPL)",
       x = "Season", y = "Avg Red Cards per Match", color = "League") +
  scale_x_discrete(limits = unique(trend_filtered_data1$season)) +  # Keep full season labels on x-axis
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

slm_reg1 <- lm(red_cards ~ treated + postVar + DID,
               data = team_season_filter_data1)
summary(slm_reg1)

slm_reg2 <- lm(red_cards ~ treated + postVar + DID +
                 fouls + is_top,
               data = team_season_filter_data1)
summary(slm_reg2)

models <- list("w/o controls" = slm_reg1, "w controls" = slm_reg2)
modelsummary(models, 
             stars = TRUE, 
             title = "Regression Models")

stable_panel_data1 <- pdata.frame(team_season_filter_data1, index = c("match_pair", "season"))

sFE1 <- feols(red_cards ~ DID| match_pair + season,
              data = stable_panel_data1, cluster = ~match_pair)
summary(sFE1)


sFE2 <- feols(red_cards ~ DID + 
                fouls + is_top | match_pair + season,
              data = stable_panel_data1, cluster = ~match_pair)
summary(sFE2)

sFE3 <- feols(red_cards ~ DID + 
                fouls + is_top + yellow_cards| match_pair + season,
              data = stable_panel_data1, cluster = ~match_pair)
summary(sFE3)

models <- list("w/o controls + FE" = sFE1, "w controls + FE" = sFE2, "w controls + FE" = sFE3)
modelsummary(models, 
             stars = TRUE, 
             title = "Regression Models")



#-------------------------------Analysis-matches---------------------------------
var_start_season <- "2017/2018"
var_before_season <- "2017/2018"

match_counts2 <- data0 %>%
  group_by(match_pair) %>%
  summarise(
    season_count = n_distinct(season),
    total_matches_after_VAR = sum(season >= var_start_season, na.rm = TRUE),  # Count occurrences after VAR
    total_matches_before_VAR = sum(season < var_start_season, na.rm = TRUE)   # Count occurrences before VAR
  )

match_counts2 <- match_counts2 %>%
  filter(total_matches_after_VAR != 0 & total_matches_before_VAR != 0)

valid_matches2 <- match_counts2 %>%
  pull(match_pair)

filtered_data2 <- data0 %>%
  filter(match_pair %in% valid_matches2) %>%
  select(-match_pair)  # Remove temporary column

filtered_data2 <- filtered_data2 %>%
  group_by(id_match) %>%
  mutate(match_pair = paste(sort(team), collapse = " vs "))

away_subset2 <- filtered_data2 %>%
  filter(status == 'AwayTeam') %>%
  select(id_match, team, yellow_cards, red_cards, top)

colnames(away_subset2) <- c('id_match', 'A_team', 'A_yellow_cards',
                           'A_red_cards', 'A_top')

filtered_data2 <- filtered_data2 %>%
  filter(status == 'HomeTeam') 

filtered_data2 <- left_join(filtered_data2, away_subset2, by = "id_match")

filtered_data2 <- filtered_data2 %>%
  mutate(
    total_yellow_cards = yellow_cards + A_yellow_cards,
    total_red_cards = red_cards + A_red_cards,
    total_fouls = fouls_commited + fouls_opponent,
    is_top = ifelse(top | A_top == 1, 1, 0)
  )

team_season_filter_data2 <- filtered_data2 %>%
  group_by(match_pair, season, treated, postVar, is_top) %>%
  summarise(
    yellow_cards = sum(total_yellow_cards, na.rm = TRUE),
    red_cards = sum(total_red_cards, na.rm = TRUE),
    fouls = sum(total_fouls, na.rm = TRUE),
  ) %>%
  mutate(DID = postVar * treated,
         league = ifelse(treated == 1, 'Bundesliga', 'EPL'))%>%
  ungroup()

team_season_filter_data2 %>%
  group_by(league) %>%
  summarize(total_matches = n(),
            unique_matches = n_distinct(match_pair))

trend_filtered_data2 <- filtered_data2 %>%
  group_by(season, league) %>%
  summarise(avg_red_cards = mean(red_cards, na.rm = TRUE), .groups = "drop")

pre_VAR_data2 <- trend_filtered_data2 %>%
  filter(season < "2017/2018")  # Select only pre-VAR seasons

# Plot with separate trend line dataset
ggplot(trend_filtered_data2, aes(x = season, y = avg_red_cards, color = league, group = league)) +
  geom_line(size = 1.2) +  # Actual trend line
  geom_point(size = 2) +  # Emphasize season points
  geom_vline(xintercept = "2016/2017", linetype = "dashed", color = "red", size = 1) +  # VAR introduction
  annotate("text", x = "2016/2017", y = max(trend_filtered_data2$avg_red_cards, na.rm = TRUE), 
           label = "VAR Introduced (2016/2017)", color = "red", angle = 90, vjust = -1.5, hjust = 1) +
  # Trend line stops at 2016/2017
  geom_smooth(data = pre_VAR_data2, aes(x = season, y = avg_red_cards, color = league), 
              method = "lm", formula = y ~ as.numeric(as.character(x)), 
              se = FALSE, linetype = "dotted") +  
  labs(title = "Parallel Trends Check: Avg Red Cards per Match of Stable Teams (Bundesliga vs EPL)",
       x = "Season", y = "Avg Red Cards per Match", color = "League") +
  scale_x_discrete(limits = unique(trend_filtered_data2$season)) +  # Keep full season labels on x-axis
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
 