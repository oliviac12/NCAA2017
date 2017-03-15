library(dplyr)

season_c <- read.csv('RegularSeasonCompactResults.csv')
season_d <- read.csv('RegularSeasonDetailedResults.csv')
season <- read.csv('Seasons.csv')
team <- read.csv('Teams.csv')
tourney_c <- read.csv('TourneyCompactResults.csv')
tourney_d <- read.csv('TourneyDetailedResults.csv')
tourneySeed <- read.csv('TourneySeeds.csv')
tourneySlot <- read.csv('TourneySlots.csv')

y <- season_d %>% distinct(Season)
y$Season[1:2]

train_data = data.frame(Matchup = character(), 
                        Win = integer())

get_training <- function(year){
season_matches <- season_d[which(season_d$Season == year), ]
team <- vector()
result <- vector()
for(i in c(1:nrow(season_matches))) {
  row <- season_matches[i, ]
  if(season_matches[i, 3] < season_matches[i, 5]) {
    vector <- paste(year,"_",row$Wteam,"_", row$Lteam, sep ="")
    team <- c(team, vector)
    result <- c(result, 1)
  } else {
    oth <- paste(year, "_", row$Lteam, "_", row$Wteam, sep ="")
    team <- c(team, oth)
    result <- c(result, 0)
  }
}
train_data_frame <- data.frame("Matchup" = team, "Win" = result)
return(train_data_frame)
}



data2012 <- get_training(2012)
#Installing package
install.packages(stringr)
library(stringr)
#Selecting and sorting the playoff teamIDs least to greatest for season A
playoff_teams <- sort(tourneySeed$Team[which(tourneySeed$Season == 2012)])

#Selecting the seeds for season A
playoff_seeds <- tourneySeed[which(tourneySeed$Season == 2012), ]

#Selecting the regular season statistics for season A
season <- season_d[which(season_d$Season == 2012), ]

#Wins by team that in the tourney that year
win_freq_table <- as.data.frame(table(season$Wteam))
wins_by_team <- win_freq_table[win_freq_table$Var1 %in% playoff_teams, ]

#Losses by team that in the tourney that year
loss_freq_table <- as.data.frame(table(season$Lteam))
loss_by_team <- loss_freq_table[loss_freq_table$Var1 %in% playoff_teams, ]

#Total Win Percentage
gamesplayed <- as.vector(wins_by_team$Freq + loss_by_team$Freq)
total_winpct <- round(wins_by_team$Freq / gamesplayed, digits = 3)
total_winpct_by_team <- as.data.frame(cbind(as.vector(loss_by_team$Var1), total_winpct))
colnames(total_winpct_by_team) <- c("Var1", "Freq")

#Num of wins in last 6 games
wins_last_six_games_by_team <- data.frame()
for(i in playoff_teams) {
  games <- season[which(season$Wteam == i | season$Lteam == i), ]
  numwins <- sum(tail(games$Wteam) == i)
  put <- c(i, numwins)
  wins_last_six_games_by_team <- rbind(wins_last_six_games_by_team, put)
}
colnames(wins_last_six_games_by_team) <- c("Var1", "Freq")

#Seed in tournament
pattern <- "[A-Z]([0-9][0-9])"
team_seeds <- as.data.frame(str_match(playoff_seeds$Seed, pattern))
seeds <- as.numeric(team_seeds$V2)
playoff_seeds$seed  <- seeds
seed_col <- vector()
for(i in playoff_teams) {
  val <- match(i, playoff_seeds$Team)
  seed_col <- c(seed_col, playoff_seeds$Seed[val])
}
team_seed <- data.frame("Var1" = playoff_teams, "Freq" =seed_col)

#Combining columns together
team_metrics <- data.frame()
team_metrics <- cbind(total_winpct_by_team, wins_last_six_games_by_team$Freq, team_seed$Freq)
colnames(team_metrics) <- c("TEAMID", "A_TWPCT","A_WST6", "A_SEED")

head(wins_by_team)
head(team_metrics)

tourney_d %>%
  mutate(wfr = Wfgm/Wfga, 
         wf3r =Wfgm3/Wfga3,
         wftr = Wftm/Wfta)

train_data <- train_data%>%
  add_row(Matchup = team, Win = result)

for (i in y$Season[1:2]){
  get_training(i)
  train_data <- train_data%>%
                  add_row(Matchup = team, Win = result)
}















season <- merge(season, team, by = "Wteam")
library(dplyr)
team <- rename(team, Wteam = Lteam )
season <- rename(season, Win_teamname = Team_Name)

journey_winner <- journey %>% 
                  filter(Daynum == 154)

journey_winner <- merge(journey_winner, team, by = "Wteam")
library(ggplot2)

filterd_season <- season %>% 
                  filter(Season > 2005) 

grouped <- group_by(filterd_season, Season, Win_teamname)
data <- summarise(grouped, Wmean=mean(Wscore), Lmean=mean(Lscore))


p <- ggplot(data, aes(x=Season, y=Wmean, colour = Win_teamname))
                                #group=Season, colour = as.factor(Season)))
p + geom_line()
