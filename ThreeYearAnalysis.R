library(fitzRoy)
library(tidyverse)
library(ggrepel)

average_comparison <- function(season,round) {
  
  fw_hist <- fetch_player_stats_footywire((season-3):season)
  
  three_year_averages <- fw_hist %>% 
    select(Season,Player,Team,AF) %>% 
    filter(Season!=season) %>% 
    group_by(Player) %>% 
    summarise(
      three_year_average = mean(AF),
      .groups = 'drop')
  
  current_year_averages <- fw_hist %>% 
    select(Season,Round,Player,Team,AF) %>% 
    filter(!grepl("Final",Round)) %>% 
    filter(Season==season,round>=parse_number(Round)) %>% 
    group_by(Player) %>% 
    summarise(
      Team,
      current_year_average = mean(AF),
      .groups = 'drop')
  
  df_compare <- three_year_averages %>% 
    left_join(current_year_averages,by='Player') %>% 
    filter(!is.na(current_year_average)) %>% 
    distinct(Player,.keep_all=TRUE) %>% 
    mutate(diff = current_year_average - three_year_average) %>% 
    select(Player,Team,three_year_average,current_year_average,diff) %>% 
    arrange(desc(diff))
  View(df_compare)
  
  # Team colours from @crow_data_sci
  team_colours <- c(
    'Carlton'= '#0e1e2d',
    'Geelong'= '#1c3c63',
    'Adelaide'= '#002b5c',
    'Sydney' = '#ed171f',
    'St Kilda' = '#ed0f05',
    'Essendon' = '#cc2031',
    'Richmond'= '#D8D800',
    'Melbourne'= '#0f1131',
    'Hawthorn' = '#4d2004',
    'Brisbane' = '#a30046',
    'Gold Coast' = '#d93e39',
    'Fremantle'= '#2a1a54',
    'Collingwood'= '#000000',
    'Port Adelaide'  = '#01b5b6',
    'Western Bulldogs'  = '#014896',
    'West Coast' = '#062ee2',
    'North Melbourne'= '#013b9f',
    'GWS' = '#f15c22')
  
  ggplot(df_compare) +
    geom_point(aes(x=three_year_average,y=current_year_average,color=Team)) +
    geom_smooth(aes(x=three_year_average,y=current_year_average,color='gray'),formula = y~x,method="lm",se=FALSE,size=1) +
    geom_text_repel(aes(x=three_year_average,y=current_year_average,color=Team,label=Player),cex=2.5) +
    labs(x=paste("Three year AFL Fantasy average (",season-3,"-",season-1,")",sep=""),
         y=paste(season,"AFL Fantasy Average"),
         title="AFL Fantasy Average compared to last three years",
         subtitle=paste("Data from www.footywire.com. Statistics up to Round ",round,", ",season,sep=""),
         caption="Code from jaidenpopowski (GitHub)") +
    scale_colour_manual(values=team_colours) +
    theme_bw() +
    theme(legend.position = 'none') 
}
