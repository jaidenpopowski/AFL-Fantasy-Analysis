#install.packages('fitzRoy')
library(fitzRoy)
#install.packages('tidyverse')
library(tidyverse)
#install.packages('ggrepel')
library(ggrepel)
#install.packages('lpSolve')
library(lpSolve)

# import file from GitHub of 2022 Player Prices and respective positions
import <- read_csv("2022 Player Prices and Positions.csv",show_col_types = FALSE)
df_afl_current <- fetch_player_stats_afl(season=2022) # get latest AFL stats

actualranks <- c(2281,4479,6620,8803,10948,13107,15434,17528,19679,22014,24374)

# function for finding and showing the optimal AFL Fantasy team selection.
# usage example: 'optimalteam(4)' shows the optimal team as at the end of Round 4
optimalteam <- function(uptoround) {
  # Setting the framework
  numOfPlayers <- 22 # players on field
  maxPrice <- 14800000 # total salary cap 2022
  maxPriceOnField <- 13280000 # maximum salary cap on field (8 bench players worth $190,000 each)
  maxdef <- 6 # number of defenders in classic team
  maxmid <- 8 # number of midfielders in classic team
  maxruc <- 2 # number of rucks in classic team
  maxfwd <- 6 # number of forwards in classic team
  
  # Import data and stitch with current scores
  player_data <- df_afl_current %>% 
    mutate(Player = paste(player.givenName,player.surname)) %>% 
    filter(as.integer(round.roundNumber)<=uptoround) %>% 
    group_by(player.playerId,Player) %>% 
    summarise(
      total_points = sum(dreamTeamPoints),
      games=n(),
      .groups='drop') %>% 
    left_join(import,by="Player") %>% 
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
  
  # show the final team and print Total Points and Remaining Salary
  finalteam <- solution %>% 
    filter(answer==1) %>% # only show players in solution
    select(-answer) %>% 
    arrange(desc(pos_def),(pos_fwd),desc(pos_mid),(pos_fwd),desc(pos_ruc),desc(pos_fwd),desc(salary_start)) # fancy arrangement to try and get team structure
  solution_points = sum(finalteam$total_points)+max(finalteam$total_points) #
  salaryremaining <- maxPriceOnField-sum(finalteam$salary_start)
  print(paste("Remaining Salary from Round 1: $",salaryremaining,sep=""))
  print(paste("Total Points after Round ",uptoround,": ",solution_points,sep=""))
  print(paste("Currently",solution_points-actualranks[uptoround],"points in front of the #1 ranked team"))
  View(finalteam)
  
  # plot all players and add names to the ones in the optimal team
  ggplot(solution) +
    geom_point(aes(x=salary_start,y=total_points,color=paste(pos_def,pos_ruc,pos_fwd))) +
    geom_smooth(aes(x=salary_start,y=total_points),color='gray',formula=y~x,method='lm',se=FALSE) +
    geom_text_repel(aes(x=salary_start,y=total_points,color=paste(pos_def,pos_ruc,pos_fwd),label=ifelse(answer==1,Player,element_blank()))) +
    labs(x="Starting Price ($)",y="Total Points",caption="#rstats @jaiden_popowski",
         title=paste("Set and Forget AFL Fantasy starting team up to Round",uptoround),
         subtitle=paste0(finalteam$Player[finalteam$total_points==max(finalteam$total_points)],
                         " captain; no trades. $190k players on bench leaves $",salaryremaining/1000,
                         "k spare. Currently winning overall by ",solution_points-actualranks[uptoround]," points.")) +
    theme_bw() +
    theme(legend.position = 'none')
}
