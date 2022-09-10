library(tidyverse)
library(stringr)

# Data manipulation of table from fitzRoy function
scores_2022 <- score_progression_raw %>% 
  mutate(
    Heading = `Scoring progression...2`=="Time", # heading row of each game
    Timings = ifelse(!Heading&nchar(`Scoring progression...2`)<8,paste0(`Scoring progression...2`,`Scoring progression...4`),NA),
    PlayerScore = ifelse(!Heading&nchar(`Scoring progression...2`)<8,paste0(`Scoring progression...1`,`Scoring progression...5`),NA),
    GameID = cumsum(Heading),
    ) %>% 
  filter(!grepl("Biggest",PlayerScore),!grepl("Biggest",Timings)) %>% 
  group_by(GameID) %>% 
  mutate(
    Team = ifelse(PlayerScore == `Scoring progression...1`,first(`Scoring progression...1`),first(`Scoring progression...5`)),
    Home.Team = first(`Scoring progression...1`),
    Away.Team = first(`Scoring progression...5`),
    Quarter.Length = ifelse(grepl("quarter",`Scoring progression...1`),sub(").*", "", `Scoring progression...1`),NA),
    Quarter.Number = cumsum(grepl("quarter",Quarter.Length))
    ) %>%
  ungroup() %>% 
  filter(!Heading) %>% 
  mutate(Quarter.Length = sub(".*\\(", "", Quarter.Length)) %>%
  mutate(Quarter.Length = 60*parse_number(sub("m .*","",Quarter.Length)) + parse_number(sub(".*m ","",Quarter.Length))) %>%
  group_by(GameID,Quarter.Number) %>% 
  fill(Quarter.Length, .direction = "downup") %>% 
  ungroup() %>% 
  filter(!is.na(PlayerScore)) %>% 
  select(GameID, Home.Team, Away.Team, Timings, PlayerScore, Team, Quarter.Number, Quarter.Length) %>% 
  mutate(Score = ifelse(grepl("goal",PlayerScore), 6, 1)) %>% 
  mutate(Type = ifelse(Home.Team==Team, "Home.Score", "Away.Score")) %>% 
  group_by(GameID,Team) %>% 
  mutate(Total = cumsum(Score)) %>% 
  pivot_wider(names_from = Type, values_from = Total) %>%
  ungroup() %>% 
  group_by(GameID) %>% 
  fill(Home.Score) %>% 
  fill(Away.Score) %>% 
  ungroup() %>% 
  mutate(
    Home.Score = ifelse(is.na(Home.Score),0,Home.Score),
    Away.Score = ifelse(is.na(Away.Score),0,Away.Score),
    Player = str_replace(str_replace(PlayerScore," goal", "")," behind", ""),
    Score.Type = ifelse(Score == 6, "Goal","Behind"),
    Timings = str_replace(str_replace(Timings,"s",""),"m ",":"),
    Elapsed = 60*as.integer(sub(":.*","",Timings)) + as.integer(sub(".*:","",Timings)),
    Opposition = ifelse(Home.Team == Team, Away.Team, Home.Team),
    Leader = ifelse(Home.Score == Away.Score,"Draw",ifelse(Home.Score > Away.Score, Home.Team, Away.Team)),
    Margin = ifelse(Team == Home.Team, Home.Score-Away.Score, Away.Score-Home.Score)
    ) %>% 
  group_by(GameID) %>% 
  mutate(Winner = last(Leader)) %>% 
  ungroup() %>% 
  select(GameID, Quarter.Number, Quarter.Length, Elapsed, Home.Team, Home.Score, Away.Team, Away.Score, Score.Type, Player, Team, Opposition, Leader, Margin, Winner)
