#install.packages('fitzRoy')
library(fitzRoy)
#install.packages('tidyverse')
library(tidyverse)
#install.packages('ggrepel')
library(ggrepel)
#install.packages('lpSolve')
library(lpSolve)

# import data
import <- read_csv("2022 Player Prices and Positions.csv",show_col_types = FALSE) # prices and positions from GitHub file
df_afl_current <- fetch_player_stats_afl(season=2022) # get latest AFL stats (takes ~30 seconds)

actualranks <- c(2281,4479,6620,8803,10948,13107,15434,17528,19679,22014,24374,26290,28022,29860,32187,34575,36953,39296,41796,44173,46495,48805,50996) # total points of overall leader

# function for finding and showing the optimal AFL Fantasy team selection.
# usage example: 'optimalteam(4) shows the optimal team as at the end of Round 4'
optimalteam <- function(uptoround) {
  # Setting the framework
  numOfPlayers <- 22 # players on field
  maxPrice <- 14800000 # total salary cap 2022
  maxPriceOnField <- maxPrice - 8*190000 # maximum salary cap on field (8 bench players worth $190,000 each)
  maxdef <- 6 # number of defenders in classic team
  maxmid <- 8 # number of midfielders in classic team
  maxruc <- 2 # number of rucks in classic team
  maxfwd <- 6 # number of forwards in classic team
  
  # Import data and stitch with current scores
  player_data <- df_afl_current %>% 
    mutate(Player = paste(player.givenName,player.surname)) %>% 
    filter((round.roundNumber<=uptoround)) %>% 
    group_by(player.playerId,Player) %>% 
    summarise(
      total_points = sum(dreamTeamPoints),
      games=n(),
      .groups='drop') %>% 
    left_join(import,by="Player") %>% # add prices/positions
    filter(!is.na(salary_start),total_points>0) %>% 
    mutate(isplayer=1) %>% 
    arrange(desc(total_points)) %>% 
    select(Player,total_points,salary_start,pos_def,pos_mid,pos_ruc,pos_fwd,isplayer)
    
    df_final <- t(player_data) # transpose the data to get horizontal condition equations
  
  # setting in/equalities for constraints
  const.direction <- c("<=",">=",">=",">=",">=","==")
  
  # setting right-hand-side values for constraints
  rhs.const <- c(maxPriceOnField,maxdef,maxmid,maxruc,maxfwd,numOfPlayers)
  
  # linear programming algorithm
  mod <- lp(direction="max", df_final[2,], df_final[3:nrow(df_final),], const.direction, rhs.const, all.bin = TRUE)
  
  answer <- mod[["solution"]] # row numbers of players in optimal solution
  solution <-  cbind(answer,player_data) # add the binary solution column to the original dataset
  
  # show the final team
  finalteam <- solution %>% 
    filter(answer==1) %>% # only show players in solution
    select(-answer,-isplayer) %>% 
    arrange(desc(pos_def),(pos_fwd),desc(pos_mid),(pos_fwd),desc(pos_ruc),desc(pos_fwd),desc(salary_start)) # fancy arrangement to try and get team structure
  View(finalteam)
  
  # print Total Points and Remaining Salary
  solution_points = sum(finalteam$total_points)+max(finalteam$total_points) # team total points plus the best player to be captain (x2)
  salary_remaining = maxPriceOnField-sum(finalteam$salary_start)
  print(paste0("Remaining Salary from Round 1: $",salary_remaining))
  print(paste0("Total Points after Round ",uptoround,": ",solution_points))
  print(paste("Currently",abs(solution_points-actualranks[uptoround]),"points",ifelse(solution_points>=actualranks[uptoround],"in front of","behind"),"the #1 ranked team"))
  
  
  # plot all players and add names to the ones in the optimal team
  ggplot(solution) +
    geom_point(aes(x=salary_start,y=total_points,color=paste(pos_def,pos_ruc,pos_fwd))) +
    geom_text_repel(aes(x=salary_start,y=total_points,color=paste(pos_def,pos_ruc,pos_fwd),label=ifelse(answer==1,Player,element_blank()))) +
    labs(x="Starting Price ($)",y="Total Points",caption="#rstats @jaiden_popowski",
         title=paste("Set and Forget AFL Fantasy starting team up to Round",uptoround),
         subtitle=paste0(finalteam$Player[finalteam$total_points==max(finalteam$total_points)],
                         " captain; no trades. $190k players on bench leaves $",salary_remaining/1000,
                         "k spare. Currently ",ifelse(solution_points>=actualranks[uptoround],"winning","behind"), " overall by ",abs(solution_points-actualranks[uptoround])," points.")) +
    theme_bw() +
    theme(legend.position = 'none')
}
