library(tidyverse)

# data is the best 22 players by season, arranged by onfield position (first 6 rows are DEF, ...)

#> names(data)
#[1] "Season"   "ID"       "Player"   "Position" "Games"    "Total"    "Average" 

data %>% 
  group_by(Season) %>% 
  mutate(group = case_when( # assign position
    row_number() <= 6 ~ "DEF", 
    row_number() <= 14 ~ "MID",
    row_number() <=  16 ~ "RUC",
    row_number() <= 22 ~ "FWD"
  )) %>%
  select(Season,ID,Player,group) %>% 
  distinct() %>% 
  group_by(ID) %>% 
  summarise(
    Player = last(Player), # choose most recent name
    group,
    Season,
    ongoing = Season == lag(Season+1, default = 0), # was this player in best 22 last season?
    .groups = 'drop'
  ) %>%
  filter(Season>2014) %>% # no previous data from 2013
  group_by(Season) %>% 
  mutate(n_season = sum(ongoing)) %>% # number label by season
  ungroup() %>% 
  filter(ongoing) %>% # only keep players that went back to back
  arrange(Season,group) %>% # arrange players for output (sorting by total points would be better, it's by ID in my chart)
  group_by(Season) %>% 
  mutate(row = 1+n()-row_number()) %>% # assign y-value for plotting player name
  ungroup() %>% 
  ggplot(aes(x=Season)) +
  geom_bar(aes(fill = group) ,color = "black",alpha = 0.8) + # create coloured bars
  geom_hline(yintercept = 0) + # bottom line
  geom_text(aes(y = n_season, label = n_season),color="black", vjust = -0.5, size=5) + # number per season value
  geom_text(aes(y = row, label = Player),color="black", vjust = 2,size=3) + # player names
  geom_text( # season labels
    mapping = aes(y = 0, x = Season, label = paste0(Season-1,"-",Season-2000)),
    hjust = 0.5,
    nudge_y = -1,
    color = 'black',
    fontface = 'bold',
    size = 5
  ) +
  labs(
    x = NULL,
    y= NULL, 
    fill = NULL,
    title = "Rolling 22 retention rate between seasons",
    subtitle = "22 best players by season ranked on total points. Traditional AFL Fantasy structure 6-8-2-6."
    ) +
  scale_y_continuous(breaks = NULL, limits = c(-1,11)) +
  scale_x_continuous(breaks = NULL) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5,face="bold",size=20),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = 'top'
    )
