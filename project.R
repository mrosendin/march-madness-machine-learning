library(readr)
library(dplyr)

season = read_csv("./data/RegularSeasonDetailedResults.csv")
teams = read_csv("./data/Teams.csv")

win_stats = function(season, team){
  #First filter for the team prior and propr columns
  season %>% filter(Wteam == team, Daynum < 132) %>% select(Season, starts_with("W"), -Wloc) %>% group_by(Season)
  # Aggregate by season year
  #aggregate(. ~ Season + Wteam, FUN = sum, data = wins)
  
}

loss_stats = function(season, team){
  #First filter for the team prior and propr columns
  season %>% filter(Lteam == team, Daynum < 132) %>% select(Season, starts_with("L")) %>% group_by(Season)
  # Aggregate by season year
  #aggregate(. ~ Season + Lteam, FUN = sum, data = wins)
  
}

seasonRecord = function(season){
  winRecord = season %>% filter(Daynum < 132) %>% group_by(Season, Wteam) %>% summarise(gamesWon = n())
  names(winRecord)[2] = "TeamID"
  lossRecord = season %>% filter(Daynum < 132) %>% group_by(Season, Lteam) %>% summarise(gamesLoss = n())
  names(lossRecord)[2] = "TeamID"
  return(list(winRecord, lossRecord))
}

combineStats = function(winStats, lossStats, team){
  #PROVIDE COLUMN NAMES
  # Set names equal
  names(winStats) = names(lossStats)
  z = rbind(winStats,lossStats) #combine
  #stats = aggregate(. ~ Season + Lteam, FUN = mean, data = z) #aggregate
  return(as.data.frame(z))
  #return(cbind(z, seasonRecord(team)))
}

combineFinal = function(final, finalStats){
  names(finalStats) = names(final)
  final = rbind(final, finalStats)
  return(final)
}

winPercentage = function(season){
  gamesWon = group_by(season, Season, Wteam)%>% summarise(won = n())
  gamesLost =  group_by(season, Season, Lteam) %>% summarise(lost = n())
  names(gamesWon)[2] = names(gamesLost)[2] = "TeamID"
  return(full_join(gamesWon, gamesLost, by = c("Season", "TeamID")) %>% mutate(PCT = won/(lost+won)) %>%
    select(Season, TeamID, PCT))
}



final = data.frame(matrix(ncol = 16))
colnames(final) = c("Season", "TeamID", "PPG", "FGMPG", "FGAPG", "3MPG", "3APG", "FTMPG", "FTAPG", "ORPG", "DRPG",
                    "APG", "TOPG", "SPG", "BPG","PFPG")


for (i in teams$Team_Id){
  hold1 = win_stats(season, i)
  hold2 = loss_stats(season, i)
  final = combineFinal(final, combineStats(hold1, hold2, i))
}
head(final)

# Aggregate Season Stats
final2 = aggregate(. ~ Season + TeamID, data = final, FUN = mean)
head(final2)
# Add Season win percentages
final2 = left_join(final2, winPercentage(season), by = c("Season", "TeamID"))
head(final2)

percents = aggregate(. ~ Season + TeamID, data = final, FUN = sum)
percents = percents %>% group_by(Season, TeamID) %>% mutate(FGP = sum(FGMPG)/ sum(FGAPG), TPP = sum(`3MPG`)/sum(`3APG`), FTP = sum(FTMPG)/sum(FTAPG)) %>%
  select(Season, TeamID, FGP, TPP, FTP)

final2 = left_join(final2, percents, by = c("Season", "TeamID"))
final2[,4:9] = NULL
final2 = round(select(final2, Season, TeamID, PPG, FGP, TPP, FTP, everything()), digits = 6)
final2 = final2 %>% arrange(TeamID, Season)
head(final2)


write.csv(final2, "./data/FinalStats.csv")



