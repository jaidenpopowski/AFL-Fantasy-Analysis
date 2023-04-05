# Web scraping functions for AFL Fantasy and SuperCoach

## use `source("https://raw.githubusercontent.com/jaidenpopowski/AFL-Fantasy-Analysis/main/fantasyR/functions.R")` to load these functions into your R environment

fetch_player_prices_footywire <- function(season = lubridate::year(Sys.Date()), round = c(1:24), game = "dream_team") {
  # example... fetch_player_prices(season = 2019, round = 1:23, game = "supercoach")
  
  if(mean(season %in% c(2010:lubridate::year(Sys.Date())))<1) {
    rlang::abort(glue::glue("{season[!(season %in% 2010:lubridate::year(Sys.Date()))]} is not a valid season. Must be seasons between 2010 and {lubridate::year(Sys.Date())}."))
  }
  
  if(mean(round %in% c(1:24))<1) {
    rlang::abort(glue::glue("{round[!(round %in% 1:24)]} is not a valid round. Must be rounds between 1 and 24."))
  }
  
  if(!(game %in% c("dream_team","supercoach"))) {
    rlang::abort(glue::glue("{game} is not a valid input for game. Should be one of dream_team, supercoach."))
  }
  
  message(paste("Getting prices from season/s", min(season), ifelse(min(season)!=max(season),paste("to", max(season)),""))) # print start message
  
  purrr::pmap_df(
    list(game,rep(season,each = length(round)),rep(round,times = length(season))), 
    purrr::possibly(
      ~ rvest::read_html(x = paste0("https://www.footywire.com/afl/footy/",{..1},"_round?year=",{..2},"&round=",{..3},"&p=&s=T")) |>
        rvest::html_element(xpath = '//table[@width = "688"]') |> 
        rvest::html_table() |>
        janitor::row_to_names(row_number = 1) |> 
        dplyr::select(Player, Team, Current_Price = 4, Price = 5, Score = 6, Value = 7) |>
        dplyr::transmute(
          Season = {..2}, 
          Round = {..3}, 
          Player = sub('\n.*','',Player), 
          Team,
          Current_Price = as.numeric(gsub('[$,]', '',Current_Price)), 
          Price = as.numeric(gsub('[$,]', '',Price)), 
          Score = as.integer(Score), 
          Value = as.double(Value)),
      otherwise = data.frame()))
}
