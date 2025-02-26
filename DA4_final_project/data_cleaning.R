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

Deutschdata <- list()
Englishdata <- list()

generate_season_label <- function(i) {
  start_year <- 2000 + i - 1  # Adjust starting year dynamically
  end_year <- start_year + 1  # Next year
  return(paste0(start_year, "/", end_year))  # Format: YYYY/YYYY
}

for (i in 9:19) {
  file_name <- paste0("D", i, ".csv")  
  Deutschdata[[as.character(i)]] <- read_delim(file_name, delim = ",", quote = '"')%>%
  mutate(season = generate_season_label(i))
}

for (i in 9:19) {
  file_name <- paste0("E", i, ".csv")  
  Englishdata[[as.character(i)]] <- read_delim(file_name, delim = ",", quote = '"')%>%
    mutate(season = generate_season_label(i))
}


for (i in names(Deutschdata)) {
  Deutschdata[[i]] <- select(Deutschdata[[i]], c("Div", "season", "Date", "HomeTeam", "AwayTeam", 
                                             "FTHG", "FTAG", "FTR", "HF", "AF", 
                                             "HY", "AY", "HR", "AR"))
}

for (i in names(Englishdata)) {
  Englishdata[[i]] <- select(Englishdata[[i]], c("Div", "season", "Date", "HomeTeam", "AwayTeam", 
                                                 "FTHG", "FTAG", "FTR", "HF", "AF", 
                                                 "HY", "AY", "HR", "AR"))
}

combined_Deutschdata <- bind_rows(Deutschdata)
combined_Englishdata <- bind_rows(Englishdata)

total_data <- bind_rows(combined_Deutschdata, combined_Englishdata)


colnames(total_data) <- c("league", "season", "date", "HomeTeam", "AwayTeam",
                     "HT_Goals", "AT_Goals", "Result", "HT_fouls",
                     "AT_fouls", "HT_yellow", "AT_yellow", "HT_red", "AT_red")

str(total_data)

total_data <- total_data %>%
  mutate(date = ifelse(nchar(date) == 10, format(as.Date(date, format = "%d/%m/%Y"), "%d/%m/%y"), date))

total_data$date = as.Date(total_data$date, format = "%d/%m/%y")
str(total_data)

sum(is.na(total_data))
which(is.na(total_data), arr.ind = TRUE)
total_data <- na.omit(total_data)

names(total_data)

total_data <- total_data %>%
  mutate(VAR = ifelse(league == "D1" & date >= "2017-08-18", 1, 0))

str(total_data)

total_data %>%
  group_by(league, VAR) %>%
  summarise(count = dplyr::n(), .groups = "drop")
conflicts()

total_data <- total_data %>%
  mutate(HT_result = ifelse(HT_Goals > AT_Goals, "W", ifelse(HT_Goals < AT_Goals, "L", "D")),
         AT_result = ifelse(HT_Goals < AT_Goals, "W", ifelse(HT_Goals > AT_Goals, "L", "D")),
         id_match = seq_len(nrow(total_data))) %>%
  select(id_match, everything())
    

HT_dataset <- total_data %>%
  select('id_match', 'league', 'season', 'date', 'HomeTeam', 'HT_Goals', 'AT_Goals', 'HT_fouls',
         'AT_fouls', 'HT_yellow', 'HT_red', 'HT_result', 'VAR') %>%
  mutate(status = "HomeTeam")

AT_dataset <- total_data %>%
  select('id_match', 'league', 'season', 'date', 'AwayTeam', 'AT_Goals', 'HT_Goals', 'AT_fouls',
         'HT_fouls', 'AT_yellow', 'AT_red', 'AT_result', 'VAR') %>%
  mutate(status = "AwayTeam")


colnames(HT_dataset) <- c('id_match', "league", "season", "date", "team", "goals_scored",
                          "goals_conceded", "fouls_commited", "fouls_opponent", "yellow_cards", "red_cards", "result",
                          "VAR", "status")
colnames(AT_dataset) <- c('id_match', "league", "season", "date", "team", "goals_scored",
                          "goals_conceded", "fouls_commited", "fouls_opponent", "yellow_cards", "red_cards", "result",
                          "VAR", "status")

data <- rbind(HT_dataset, AT_dataset) %>%
  arrange(id_match, date)

data <- data %>%
  mutate(month = format(as.Date(date), "%Y-%m"),
         team_status = ifelse(status == 'HomeTeam', 1, 0))

data <- data %>%
  mutate(
    top = case_when(
      league == "E0" & season == "2008/2009" & team %in% c("Man United", "Chelsea", "Arsenal", "Liverpool") ~ 1,
      league == "E0" & season == "2009/2010" & team %in% c("Man United", "Chelsea", "Arsenal", "Tottenham") ~ 1,
      league == "E0" & season == "2010/2011" & team %in% c("Man United", "Chelsea", "Arsenal", "Man City") ~ 1,
      league == "E0" & season == "2011/2012" & team %in% c("Man United", "Tottenham", "Arsenal", "Liverpool") ~ 1,
      league == "E0" & season == "2012/2013" & team %in% c("Man United", "Chelsea", "Arsenal", "Man City") ~ 1,
      league == "E0" & season == "2013/2014" & team %in% c("Man City", "Chelsea", "Arsenal", "Liverpool") ~ 1,
      league == "E0" & season == "2014/2015" & team %in% c("Man United", "Chelsea", "Arsenal", "Man City") ~ 1,
      league == "E0" & season == "2015/2016" & team %in% c("Man City", "Leicester", "Arsenal", "Tottenham") ~ 1,
      league == "E0" & season == "2016/2017" & team %in% c("Man City", "Chelsea", "Tottenham", "Liverpool") ~ 1,
      league == "E0" & season == "2017/2018" & team %in% c("Man United", "Man City", "Tottenham", "Liverpool") ~ 1,
      league == "E0" & season == "2018/2019" & team %in% c("Man City", "Chelsea", "Tottenham", "Liverpool") ~ 1,
      league == "D1" & season == "2008/2009" & team %in% c("Bayern Munich", "Wolfsburg", "Stuttgart") ~ 1,
      league == "D1" & season == "2009/2010" & team %in% c("Bayern Munich", "Schalke 04", "Werder Bremen") ~ 1,
      league == "D1" & season == "2010/2011" & team %in% c("Bayern Munich", "Leverkusen", "Dortmund") ~ 1,
      league == "D1" & season == "2011/2012" & team %in% c("Bayern Munich", "Schalke 04", "Dortmund") ~ 1,
      league == "D1" & season == "2012/2013" & team %in% c("Bayern Munich", "Schalke 04", "Dortmund", "Leverkusen") ~ 1,
      league == "D1" & season == "2013/2014" & team %in% c("Bayern Munich", "Schalke 04", "Dortmund", "Leverkusen") ~ 1,
      league == "D1" & season == "2014/2015" & team %in% c("Bayern Munich", "Wolfsburg", "M'gladbach", "Leverkusen") ~ 1,
      league == "D1" & season == "2015/2016" & team %in% c("Bayern Munich", "M'gladbach", "Dortmund", "Leverkusen") ~ 1,
      league == "D1" & season == "2016/2017" & team %in% c("Bayern Munich", "Hoffenheim", "Dortmund", "RB Leipzig") ~ 1,
      league == "D1" & season == "2017/2018" & team %in% c("Bayern Munich", "Schalke 04", "Dortmund", "Hoffenheim") ~ 1,
      league == "D1" & season == "2018/2019" & team %in% c("Bayern Munich", "RB Leipzig", "Dortmund", "Leverkusen") ~ 1,
      TRUE ~ 0  
    )
  )

data <- data %>%
  mutate(league = case_when(
    league == "E0" ~ "EPL",
    league == "D1" ~ "Bundesliga" 
  ),
  postVar = ifelse(date >= "2017-08-18", 1, 0)
  )

data <- data %>%
  mutate(treated = ifelse(league == "Bundesliga", 1, 0))
write.csv(data, "data.csv")



Deutschdata2 <- list()
Englishdata2 <- list()

generate_season_label <- function(i) {
  start_year <- 2000 + i - 1  # Adjust starting year dynamically
  end_year <- start_year + 1  # Next year
  return(paste0(start_year, "/", end_year))  # Format: YYYY/YYYY
}

for (i in 18:24) {
  file_name <- paste0("B", i, ".csv")  
  Deutschdata2[[as.character(i)]] <- read_delim(file_name, delim = ",", quote = '"')%>%
    mutate(season = generate_season_label(i))
}

for (i in 18:24) {
  file_name <- paste0("C", i, ".csv")  
  Englishdata2[[as.character(i)]] <- read_delim(file_name, delim = ",", quote = '"')%>%
    mutate(season = generate_season_label(i))
}


for (i in names(Deutschdata2)) {
  Deutschdata2[[i]] <- select(Deutschdata2[[i]], c("Div", "season", "Date", "HomeTeam", "AwayTeam", 
                                                 "FTHG", "FTAG", "FTR", "HF", "AF", 
                                                 "HY", "AY", "HR", "AR"))
}

for (i in names(Englishdata2)) {
  Englishdata2[[i]] <- select(Englishdata2[[i]], c("Div", "season", "Date", "HomeTeam", "AwayTeam", 
                                                 "FTHG", "FTAG", "FTR", "HF", "AF", 
                                                 "HY", "AY", "HR", "AR"))
}

combined_Deutschdata2 <- bind_rows(Deutschdata2)
combined_Englishdata2 <- bind_rows(Englishdata2)

total_data2 <- bind_rows(combined_Deutschdata2, combined_Englishdata2)


colnames(total_data2) <- c("league", "season", "date", "HomeTeam", "AwayTeam",
                          "HT_Goals", "AT_Goals", "Result", "HT_fouls",
                          "AT_fouls", "HT_yellow", "AT_yellow", "HT_red", "AT_red")

total_data2 <- total_data2 %>%
  mutate(date = ifelse(nchar(date) == 10, format(as.Date(date, format = "%d/%m/%Y"), "%d/%m/%y"), date))

total_data2$date = as.Date(total_data2$date, format = "%d/%m/%y")
str(total_data2)

sum(is.na(total_data2))
which(is.na(total_data2), arr.ind = TRUE)
total_data <- na.omit(total_data2)

names(total_data2)

total_data2 <- total_data2 %>%
  mutate(VAR = ifelse(league == "D2" & date >= "2019-07-26", 1, 0))


total_data2 %>%
  group_by(league, VAR) %>%
  summarise(count = dplyr::n(), .groups = "drop")
conflicts()

total_data2 <- total_data2 %>%
  mutate(HT_result = ifelse(HT_Goals > AT_Goals, "W", ifelse(HT_Goals < AT_Goals, "L", "D")),
         AT_result = ifelse(HT_Goals < AT_Goals, "W", ifelse(HT_Goals > AT_Goals, "L", "D")),
         id_match = seq_len(nrow(total_data2))) %>%
  select(id_match, everything())


HT_dataset2 <- total_data2 %>%
  select('id_match', 'league', 'season', 'date', 'HomeTeam', 'HT_Goals', 'AT_Goals', 'HT_fouls',
         'AT_fouls', 'HT_yellow', 'HT_red', 'HT_result', 'VAR') %>%
  mutate(status = "HomeTeam")

AT_dataset2 <- total_data2 %>%
  select('id_match', 'league', 'season', 'date', 'AwayTeam', 'AT_Goals', 'HT_Goals', 'AT_fouls',
         'HT_fouls', 'AT_yellow', 'AT_red', 'AT_result', 'VAR') %>%
  mutate(status = "AwayTeam")


colnames(HT_dataset2) <- c('id_match', "league", "season", "date", "team", "goals_scored",
                          "goals_conceded", "fouls_commited", "fouls_opponent", "yellow_cards", "red_cards", "result",
                          "VAR", "status")
colnames(AT_dataset2) <- c('id_match', "league", "season", "date", "team", "goals_scored",
                          "goals_conceded", "fouls_commited", "fouls_opponent", "yellow_cards", "red_cards", "result",
                          "VAR", "status")

data2 <- rbind(HT_dataset2, AT_dataset2) %>%
  arrange(id_match, date)

data2 <- data2 %>%
  mutate(month = format(as.Date(date), "%Y-%m"),
         team_status = ifelse(status == 'HomeTeam', 1, 0))

data2 <- data2 %>%
  mutate(league = case_when(
    league == "E1" ~ "Championship",
    league == "D2" ~ "Bundesliga 2" 
  ),
  postVar = ifelse(date >= "2019-07-26", 1, 0)
  )

data2 <- data2 %>%
  mutate(treated = ifelse(league == "Bundesliga 2", 1, 0))
write.csv(data2, "data2.csv")


data$date <- as.character(data$date)

print(xtable(head(data)), include.rownames = FALSE)
