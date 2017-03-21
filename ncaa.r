library(dplyr)

season_c <- read.csv('RegularSeasonCompactResults.csv')
season_d <- read.csv('RegularSeasonDetailedResults.csv')
season <- read.csv('Seasons.csv')
team <- read.csv('Teams.csv')
tourney_c <- read.csv('TourneyCompactResults.csv')
tourney_d <- read.csv('TourneyDetailedResults.csv')
tourneySeed <- read.csv('TourneySeeds.csv')
tourneySlot <- read.csv('TourneySlots.csv')
realtest <- read.csv('2017matchup.csv')

y <- tourney_d %>% distinct(Season)

train_data = data.frame(Matchup = character(),
                        Win = integer())

# Response Match Up Data (Y)
get_Matchup <- function(year){
tourney_matches <- tourney_d[which(tourney_d$Season == year), ]

team <- vector()
result <- vector()
for(i in c(1:nrow(tourney_matches))) {
  row <- tourney_matches[i, ]
  if(tourney_matches[i, 3] < tourney_matches[i, 5]) {
    vector <- paste(year,"_",row$Wteam,"_", row$Lteam, sep ="")
    team <- c(team, vector)
    result <- c(result, 1)
  } else {
    oth <- paste(year, "_", row$Lteam, "_", row$Wteam, sep ="")
    team <- c(team, oth)
    result <- c(result, 0)
  }
}
matchup <- data.frame("Matchup" = team, "Win" = result)
Ateam <- vector()
Bteam <- vector()
for (i in c(1:nrow(matchup))) {
  row <- matchup[i, ]
  first <- unlist(str_split(row$Matchup, "_"))[2]
  later <- unlist(str_split(row$Matchup, "_"))[3]
  Ateam <- as.numeric(c(Ateam, first))
  Bteam <- as.numeric(c(Bteam, later))
}
matchup$Ateam = Ateam
matchup$Bteam = Bteam
return(matchup)
}

getTrainig <- function(year) {
  y = get_Matchup(year)
  df = getPredictor(year)
  df_b = getPredictor(year)
  df = rename(df, Ateam = TEAMID)
  df_b = rename(df_b, Bteam = TEAMID)
  df$Ateam = as.numeric(df$Ateam)
  df_b$Bteam = as.numeric(df_b$Bteam)
  colnames(df_b) <- c("Bteam", "B_TWPCT", "B_WST6", "B_SEED", "B_WL3_Freq", "B_LL3_Freq",
                      "B_WG7_Freq","B_LG7_Freq","B_TWPCT_W", "B_WPCTL4", "B_WPCT_play")
  train = inner_join(y, df)
  train = inner_join(train, df_b)
  return(train)
}

colnames(df)

getTrainig(2003)
a <- getPredictor(2010)

df_2017 <- getPredictor(2017)


BigTrain = getTrainig(2003)
for (i in c(2004 : 2016)){
  print(i)
  matchup = getTrainig(i)
  BigTrain = rbind(BigTrain, matchup)
}




#Installing package
install.packages(stringr)
library(stringr)

getPredictor <- function(year){
  
  #Selecting and sorting the playoff teamIDs least to greatest for season 
  playoff_teams <- sort(tourneySeed$Team[which(tourneySeed$Season == year)])

  #Selecting the seeds for season 
  playoff_seeds <- tourneySeed[which(tourneySeed$Season == year), ]

  #Selecting the regular season statistics for season 
  season <- season_d[which(season_d$Season == year), ]
  season_w <- season_d[which(season_d$Season == year & season_d$Wloc == 'A'),] # away game
  season_last4 <- season[which(season$Daynum >= season$Daynum[length(season$Daynum)] - 30),] # last four weeks


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
  team_seed <- data.frame("Var1" = playoff_teams, "Freq" =seed_col)w
  
  #Combining columns together
  team_metrics <- data.frame()
  team_metrics <- cbind(total_winpct_by_team, team_seed$Freq)
  # total_winpct_by_team_w$Freq,
  colnames(team_metrics) <- c("TEAMID", "TWPCT","SEED")
                              # ,"TWPCT_W" )

  # playoff_teams <- sort(tourneySeed$Team[which(tourneySeed$Season == 2010)])
  #Num of wins in last 6 games
  wins_last_six_games_by_team <- data.frame()
  for(i in playoff_teams) {
    games <- season[which(season$Wteam == i | season$Lteam == i), ]
    numwins <- sum(tail(games$Wteam) == i)
    put <- c(i, numwins)
    wins_last_six_games_by_team <- rbind(wins_last_six_games_by_team, put)
  }
  colnames(wins_last_six_games_by_team) <- c("TEAMID", "Freq")
  # wins_last_six_games_by_team = rename(wins_last_six_games_by_team, TEAMID = X1115L)
  wins_last_six_games_by_team$TEAMID = as.character(wins_last_six_games_by_team$TEAMID)
  team_metrics = left_join(team_metrics, wins_last_six_games_by_team)
  team_metrics[is.na(team_metrics)] <- 0
  team_metrics = rename(team_metrics, WST6 = Freq)
  
  

#Wins by margin less than 3
season$win_margin <- season$Wscore - season$Lscore
season = rename(season, margin = win_margin)
margin_l2 = season[which(season$margin < 3), ]
win_marginle_freq <- as.data.frame(table(margin_l2$Wteam))
winsl2_by_team <- win_marginle_freq[win_marginle_freq$Var1 %in% playoff_teams, ]
winsl2_by_team = rename(winsl2_by_team, TEAMID = Var1)
team_metrics = left_join(team_metrics, winsl2_by_team)
team_metrics[is.na(team_metrics)] <- 0
team_metrics = rename(team_metrics, WL3_Freq = Freq)

#Lose by margin less than 3
lose_marginle_freq <- as.data.frame(table(margin_l2$Lteam))
losel2_by_team <- lose_marginle_freq[lose_marginle_freq$Var1 %in% playoff_teams, ]
losel2_by_team = rename(losel2_by_team, TEAMID = Var1)
team_metrics = left_join(team_metrics, losel2_by_team)
team_metrics[is.na(team_metrics)] <- 0
team_metrics = rename(team_metrics, LL3_Freq = Freq)

#Win by margin great than 7
margin_7 = season[which(season$margin > 7), ]
win_margin7_freq <- as.data.frame(table(margin_7$Wteam))
win7_by_team <- win_margin7_freq[win_margin7_freq$Var1 %in% playoff_teams, ]
win7_by_team = rename(win7_by_team, TEAMID = Var1)
team_metrics = left_join(team_metrics, win7_by_team)
team_metrics[is.na(team_metrics)] <- 0
team_metrics = rename(team_metrics, WG7_Freq = Freq)

#Lose by margin greater than 7
lose_margin7_freq <- as.data.frame(table(margin_7$Lteam))
lose7_by_team <- lose_margin7_freq[lose_margin7_freq$Var1 %in% playoff_teams, ]
lose7_by_team = rename(lose7_by_team, TEAMID = Var1)
team_metrics = left_join(team_metrics, lose7_by_team)
team_metrics[is.na(team_metrics)] <- 0
team_metrics = rename(team_metrics, LG7_Freq = Freq)

#Away Wins by team that in tourney that year
win_freq_table_w <- as.data.frame(table(season_w$Wteam))
wins_by_team_w <- win_freq_table_w[win_freq_table_w$Var1 %in% playoff_teams, ]
wins_by_team_w = rename(wins_by_team_w , freq_w = Freq)


#last 4 weeks wins by team that in tourney that year
win_freq_table_last4 <- as.data.frame(table(season_last4$Wteam))
wins_by_team_last4 <- win_freq_table_last4[win_freq_table_last4$Var1 %in% playoff_teams, ]
wins_by_team_last4 = rename(wins_by_team_last4, freq_w = Freq)

#Away Losses by team that in the tourney that year
loss_freq_table_w <- as.data.frame(table(season_w$Lteam))
loss_by_team_w <- loss_freq_table_w[loss_freq_table_w$Var1 %in% playoff_teams, ]
loss_by_team_w = rename(loss_by_team_w , freq_l = Freq)

#last 4 weeks Losses by team that in the tourney that year
loss_freq_table_last4 <- as.data.frame(table(season_last4$Lteam))
loss_by_team_last4 <- loss_freq_table_last4[loss_freq_table_last4$Var1 %in% playoff_teams, ]
loss_by_team_last4 = rename(loss_by_team_last4, freq_l = Freq)

#Total Away Win Percentage
gamesplayed_w <- left_join(wins_by_team_w, loss_by_team_w)
gamesplayed_w [is.na(gamesplayed_w)] <- 0
gamesplayed_w$total_game <- gamesplayed_w$freq_w + gamesplayed_w$freq_l
total_winpct_w <- round(gamesplayed_w$freq_w / gamesplayed_w$total_game, digits = 3)
team_metrics[ , "TWPCT_W"] <- total_winpct_w


#last 4 weeks Win Percentage
gamesplayed_last4 <- left_join(wins_by_team_last4, loss_by_team_last4)
gamesplayed_last4 [is.na(gamesplayed_last4 )] <- 0
gamesplayed_last4$total_game <- gamesplayed_last4$freq_w + gamesplayed_last4$freq_l
total_winpct_last4 <- round(gamesplayed_last4$freq_w / gamesplayed_last4$total_game, digits = 3)
team_metrics[ , "WPCTL4"] <- total_winpct_last4

#Win Percentage against playoff teams
wins_percentage_againplayoff <- data.frame()
for (i in playoff_teams) {
  Wgames <- season[which(season$Wteam == i & season$Lteam %in% playoff_teams), ]
  Lgames <- season[which(season$Lteam == i & season$Wteam %in% playoff_teams), ]
  put <- c(i, round(dim(Wgames)[1]/(dim(Wgames)[1] + dim(Lgames)[1]), digits = 3))
  wins_percentage_againplayoff <- rbind(wins_percentage_againplayoff, put)
}
colnames(wins_percentage_againplayoff) <- c("Var1", "WPCT_play")
team_metrics[ , "WPCT_play"] <- wins_percentage_againplayoff$WPCT_play

return(team_metrics)
}






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






#DecisionTree
set.seed(1010)
head(BigTrain)
df = BigTrain[c(-1, -3,-4)]
df$TWPCT = as.numeric(df$TWPCT)
df$B_TWPCT= as.numeric(df$B_TWPCT)
head(df)
train = sample(dim(df)[1], 720)
Mad.train = df[train, ]
Mad.test = df[-train, ]


library(tree)
Mad.tree = tree(as.factor(Win) ~ ., data = Mad.train)
summary(Mad.tree)
plot(Mad.tree)
text(Mad.tree, pretty = 0)


Mad.pred = predict(Mad.tree, Mad.test, type = "class")
table(Mad.test$Win, Mad.pred)
mean(Mad.pred != Mad.test$Win)

## logistic 
glm.fit = glm(as.factor(Win) ~ ., data = Mad.train, family = "binomial")
glm.probs = predict(glm.fit, newdata = Mad.test, type = "response")
glm.pred = rep(0, length(glm.probs))
glm.pred[glm.probs > 0.5] = 1
table(glm.pred, Mad.test$Win)
mean(glm.pred != Mad.test$Win)

## Random Forest
library(randomForest)
set.seed(10)
Mad.train[is.na(Mad.train)] = 0
rf.weekly = randomForest(as.factor(Win) ~ ., data = Mad.train , mtry = 10)
yhat.bag = predict(rf.weekly, newdata = Mad.test)
table(yhat.bag, Mad.test$Win)
mean(yhat.bag != Mad.test$Win)

head(realtest)
df_2017 <- getPredictor(2017)
df_2017 = rename(df_2017, Ateam = TEAMID)
df_2017$Ateam = as.numeric(df_2017$Ateam)
test = inner_join(realtest, df_2017)
colnames(df_2017) <- c("Bteam", "B_TWPCT", "B_SEED", "B_WST6","B_WL3_Freq", "B_LL3_Freq",
                    "B_WG7_Freq","B_LG7_Freq","B_TWPCT_W", "B_WPCTL4", "B_WPCT_play")
df_2017$Bteam = as.numeric(df_2017$Bteam)
test = inner_join(test, df_2017)

df_test = test[c(-1, -2, -3)]
df_test$TWPCT = as.numeric(df_test$TWPCT)
df_test$B_TWPCT= as.numeric(df_test$B_TWPCT)
## logistic 
glm.fit = glm(as.factor(Win) ~ ., data = Mad.train, family = "binomial")
glm.probs = predict(glm.fit, newdata = df_test, type = "response")
glm.pred = rep(0, length(glm.probs))
glm.pred[glm.probs > 0.5] = 1
table(glm.pred, Mad.test$Win)
mean(glm.pred != Mad.test$Win)

check <- tourney_matches <- tourney_d[which(tourney_d$Season == 2003), ]
test$prod = glm.probs
test$pred = glm.pred
dim(df_test)
dim(test)
