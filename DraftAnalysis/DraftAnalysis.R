library(tidyverse) 
library(ggrepel)
library(fitzRoy)

afl_2022 <- fetch_player_stats_afl()
adps_2022 <- read_csv("2022adps.csv",show_col_types = FALSE)

team_colours_full <- c(
  'Carlton'= '#0e1e2d',
  'Geelong Cats'= '#1c3c63',
  'Adelaide Crows'= '#002b5c',
  'Sydney Swans' = '#ed171f',
  'St Kilda' = '#ed0f05',
  'Essendon' = '#cc2031',
  'Richmond'= '#D8D800',
  'Melbourne'= '#0f1131',
  'Hawthorn' = '#4d2004',
  'Gold Coast Suns' = '#d93e39',
  'Fremantle'= '#2a1a54',
  'Collingwood'= '#000000',
  'Port Adelaide'  = '#01b5b6',
  'Western Bulldogs'  = '#014896',
  'West Coast Eagles' = '#062ee2',
  'North Melbourne'= '#013b9f',
  'GWS Giants' = '#f15c22',
  'Brisbane Lions' = '#a30046'
)

draft_analysis <- function(uptoround) {
  df_adps <- afl_2022 %>% 
    mutate(player=paste(player.givenName,player.surname)) %>% 
    filter(round.roundNumber <= uptoround) %>% 
    group_by(player.playerId,player,team.name) %>% 
    summarise(
      total_points=sum(dreamTeamPoints),
      .groups='drop'
      ) %>% 
    left_join(adps_2022,by="player") %>% 
    mutate(adp=ifelse(adp>=1,adp,NA)) %>% 
    filter(!is.na(adp)) %>% 
    arrange(desc(total_points)) 
  
  fit<-loess(total_points~1+adp,data=df_adps)
  df_adps$predicted <- round(predict(fit),1)
  df_adps$points_diff <- round(df_adps$total_points - df_adps$predicted,1)
  
  output <- df_adps %>% 
    select(player,team.name,adp,total_points,predicted,points_diff) %>% 
    arrange(desc(points_diff))
  
  View(output)
  
  ggplot(df_adps,aes(x=adp,y=total_points,color=team.name)) +
    geom_point() +
    geom_text_repel(aes(label=player),cex=3) +
    geom_smooth(color="gray",formula = y~x,method='loess',se=FALSE)+
    scale_color_manual(values = team_colours_full) +
    labs(x="2022 Average Draft Position",y="2022 Total Points",
         title=paste("2022 Total Points by Average Draft Position: up to Round",uptoround),
         subtitle="Above the line = good value for that pick") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),plot.subtitle =element_text(hjust = 0.5),legend.position = 'none' )
}
