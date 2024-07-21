# AFL Projections Testing

avg.last.n <- function (x,n) zoo::rollapply(x, width=n, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right")
weight.last.n <- function (x,n) {
  if (length(x)-1<n) {return(avg.last.n(x,n))}
  pracma::movavg(x,n,type = "w")
}

# Save this as it takes a while
afl_stats <- purrr::map_dfr(
  2012:2024,
  ~ fetch_player_stats_afl(season = .x) %>% filter(status!="LIVE")
)

extendedStats <- purrr::map_dfr(
  c(2012:2024),
  purrr::possibly(
    ~httr::content(
      httr::GET(
        paste0('https://api.afl.com.au/statspro/playersStats/seasons/CD_S',.x,'014'),
        config = httr::add_headers("x-media-mis-token" = fitzRoy::get_afl_cookie())
      ),as = 'text',encoding = 'UTF-8') %>%
      jsonlite::fromJSON(flatten = T) %>% .[['players']] %>% mutate(season = .x),otherwise = data.frame())
) %>% 
  select(season,playerId,gamesPlayed,starts_with("totals.")) %>% 
  rename_with(~str_replace_all(.x,"totals.","")) %>% 
  filter(gamesPlayed>0) %>% 
  group_by(playerId,season) %>% 
  summarise(
    cbas_total = centreBounceAttendances/gamesPlayed,
    kickins_total = kickins/gamesPlayed,
    .groups = 'drop'
  )

input <- afl_stats %>% 
  mutate(cba_pc = replace_na(400*extendedStats.centreBounceAttendances/sum(extendedStats.centreBounceAttendances),0L), .by = c(providerId,teamId)) %>%
  filter(timeOnGroundPercentage>=50) %>% 
  rename_with(~ str_replace_all(.x,c("extendedStats."="","clearances."=""))) %>% 
  mutate(oppoId = ifelse(teamId == first(teamId), first(teamId[teamId != first(teamId)]), first(teamId)), .by = c(providerId)) %>%
  transmute(
    match_id = providerId,
    season = as.integer(substr(providerId,5,8)),
    round = round.roundNumber,
    teamId, team = team.name,
    oppoId, oppo = ifelse(team.name == home.team.club.name, away.team.club.name, home.team.club.name),
    lineup = player.player.position, fantasy = dreamTeamPoints, 
    id = player.playerId,player=paste(player.givenName,player.surname), team=team.name, 
    timeOnGroundPercentage, goals, behinds, onePercenters, hitouts, shotsAtGoal, kicks, handballs,
    inside50s, marksInside50, disposalEfficiency, rebound50s, goalAssists, goalAccuracy, turnovers, tacklesInside50, scoreInvolvements, metresGained, centreClearances, stoppageClearances, effectiveKicks, kickEfficiency, marksOnLead, interceptMarks, contestedPossessionRate, d50GroundBallGets = groundBallGets - f50GroundBallGets, f50GroundBallGets, defHalfPressureActs, fwdHalfPressureActs = pressureActs-defHalfPressureActs, spoils, contestDefLosses, contestOffWins, ruckContests, centreBounceAttendances,kickins,
    uncontestedMarks = marks - contestedMarks - marksOnLead,
    hitoutReceives = contestedPossessions - freesFor - groundBallGets - contestedMarks,
    receives = uncontestedPossessions - marks + contestedMarks,
    marksOutside50 = marks - marksInside50,
    tacklesOutside50 = tackles - tacklesInside50,
    contestDefWins = contestDefOneOnOnes - contestDefLosses,
    contestOffLosses = contestOffOneOnOnes - contestOffWins,
    interceptPossessions = intercepts - interceptMarks,
    #ineffectiveKicks = kicks - effectiveKicks,
    effectiveHandballs = effectiveDisposals - effectiveKicks,
    #ineffectiveHandballs = handballs - effectiveHandballs,
    disposalClangers = clangers - freesAgainst,
    cbas = centreBounceAttendances, cba_pc, kickin = kickins,ruckcont = ruckContests, time_on_ground = timeOnGroundPercentage
  ) %>% 
  left_join(extendedStats, by = join_by(season,id == playerId)) %>% 
  mutate(centreBounceAttendances = coalesce(centreBounceAttendances,cbas_total),kickins = coalesce(kickins,kickins_total),cbas = coalesce(cbas,cbas_total), kickin = coalesce(kickin,kickins_total)) %>% 
  select(-cbas_total, -kickins_total) %>% 
  mutate(across(timeOnGroundPercentage:disposalClangers, ~replace_na(.x,replace_na(mean(.x,na.rm=T),0L))),.by = c(id,season)) %>% #-> df
  mutate(across(c(goals:marksInside50,rebound50s,goalAssists,turnovers:effectiveKicks,marksOnLead,interceptMarks,d50GroundBallGets:cbas,kickin,ruckcont), ~ ifelse(season == 2020, 1.25*.x, .x))) %>% #-> df
  mutate(
    player = last(player), 
    team = last(team), 
    across(timeOnGroundPercentage:disposalClangers, ~ weight.last.n(.x,5)), 
    .by = c(season,id)
  ) %>% 
  mutate(across(timeOnGroundPercentage:disposalClangers, ~ as.numeric(scale(.x))))

km <- stats::kmeans(input[12:56], centers = 11, nstart = 30, iter.max = 50)

centroids <- km$centers %>% 
  as.data.frame() %>% 
  mutate(group_c = row_number()) %>% 
  pivot_longer(cols = -group_c, names_to = "stat", values_to = "center")

input %>% 
  mutate(group = km$cluster) %>% 
  filter(season==2024) %>% 
  group_by(id,player) %>% 
  slice_tail(n=1) %>% 
  ungroup() %>% 
  pivot_longer(cols = timeOnGroundPercentage:disposalClangers, names_to = "stat", values_to = "value") %>% 
  left_join(centroids, by = join_by(stat), relationship = "many-to-many") %>% 
  summarise(dist = sum(value*center)/(sqrt(sum(value^2))*sqrt(sum(center^2))),.by = c(match_id,id,player,team,group_c)) %>% 
  #pivot_wider(names_from = group_c, names_prefix = "Pos", values_from = dist) %>% -> positions
  # select(-value,-center) %>% 
  # distinct() %>% View()
  #left_join(data.frame(group_c = 1:11,Position = c("Midfield","KeyDef","GenFwd","Distributor","GenDef","Utility","Ruck","KeyFwd","Wing","MidRotation","Fringe")), by = "group_c") %>%
  #select(-group_c) %>% 
  #filter(Position!="Fringe") %>% 
  mutate(photo = paste0('https://s.afl.com.au/staticfile/AFL%20Tenant/AFL/Players/ChampIDImages/AFL/2024014/',parse_number(id),'.png')) %>%
  select(photo,player,team,Position=group_c,dist) %>% 
  arrange(Position) %>% 
  #filter(player %in% players$Player[players$Average>=100]) %>% 
  pivot_wider(names_from = Position,names_prefix="Pos",values_from = dist) %>% 
  select(photo,player,team,Pos1:Pos11) %>%
  View()
  
gt() %>% 
  gt_img_rows(columns = 1) %>% 
  cols_label(photo = "",player = "") %>% 
  data_color(-c(photo,player),palette = c("red","white","blue"),domain = c(-1,0,1)) %>% 
  fmt_number(-c(photo,player),decimals = 2) %>% 
  tab_options(data_row.padding = px(0)) %>% 
  gtsave(filename = "temp.png",expand = 20)
