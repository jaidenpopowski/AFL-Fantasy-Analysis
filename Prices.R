# Fetch AFL Fantasy/SuperCoach player prices from Footywire
library(fitzRoy) # AFL data
library(tidyverse) # data manipulation
library(rvest) # static web scraping

fetch_player_prices <- function(season = year(Sys.Date()), round = c(1:24), positions = c("DE","MI","RU","FO"), game = "dream_team") {
  
  if(!is.numeric(season)) {season = year(Sys.Date)}
  
  if(!is.character(positions)) {positions = c("DE","MI","RU","FO")}
  
  if(!is.character(game)) {game = "dream_team"}
  
  rounds <- data.frame(Season = rep(season,4*length(round)), Round = rep(c(round),4), Position = c(rep("DE",length(round)),rep("MI",length(round)),rep("RU",length(round)),rep("FO",length(round)))) %>% 
    filter(Position %in% positions) %>% 
    mutate(Game = game) %>% 
    arrange(Season,Round)
  
  message(paste("Getting prices from", min(rounds$Season), ifelse(min(rounds$Season)!=max(rounds$Season),paste("to", max(rounds$Season)),""))) # print start message
  
  pb <- suppressWarnings(progress_estimated(length(rounds$Season)))  # Create progress bar
  
  tables <-
    purrr::pmap_df(list(rounds$Season,rounds$Round,rounds$Position, rounds$Game), ~ {
      pb$tick()$print() # progress bar updates
      fetch_round_prices({..1}, {..2}, {..3}, {..4}) # scrape individual coach URLs
    })
  
  final <- tables %>% 
    mutate(Score = as.integer(Score),Value = as.double(Value),Season = as.integer(Season),Round = as.integer(Round)) %>% 
    group_by(Season,Round) %>% 
    mutate(Rank = rank(-Score,ties.method = "min")) %>% 
    ungroup() %>% 
    select(Season,Round,Rank,Player,Team,Position,Salary,Score,Value,Salary_current)

  return(final)
}

# Function to retrieve the table for an individual coach
fetch_round_prices <- function(Season,Round,Position,Game) {
  
  round_list <- read_html(x = paste0("https://www.footywire.com/afl/footy/",Game,"_round?year=",Season,"&round=",Round,"&p=",Position,"&s=T")) %>% 
    html_element(xpath = '//table[@width = "688"]')
  
  if (class(round_list)!="xml_missing") {
  return(
    round_list %>% 
      html_table() %>% 
      janitor::row_to_names(row_number = 1) %>% 
      mutate(Season = Season, Round = Round, Position = Position) %>%
      select(Season, Round, Rank, Player, Position, Team, Salary_current = 4, Salary = 5, Score = 6, Value = 7)
    )
  }
}

prices <- fetch_player_prices(season = 2016, game = "dream_team")
