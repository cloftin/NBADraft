library(plyr)
library(dplyr)
library(DBI)
library(randomForest)
library(ggplot2)
library(tidyr)

options(scipen = 999)

source("seasonWeighting.R")


runCollegeModels <- function(draftYearToTest = 2017, dx = F, yearsToExclude = c(2016)) {
  
  yearsToExclude <- c(yearsToExclude, draftYearToTest, 2016)
  
  cn <- dbConnect(RSQLite::SQLite(), "NBADraft.sqlite3")
  
  if(dx) {
    collegePlayers <- dbGetQuery(cn, "Select a.*, b.*, e.draftAge, e.Position, c.School, c.SchoolSRS, c.SchoolSOS, d.Rank
                                    from DXStats a
                               inner join PlayerStats b
                               inner join SchoolStats c
                               inner join DXRankings d
                               inner join CollegePlayers e
                               on a.Name = b.Player
                               and abs(a.Year - b.draftYear) <=4
                               and a.Year <= b.draftYear
                               and a.Team = c.School
                               and a.Year = c.Year
                               and a.Name = d.Player
                               and b.draftYear = d.Year
                               and e.link = b.link")
    collegePlayers <- unique(collegePlayers)
    collegePlayers$MP <- as.numeric(collegePlayers$MP)
    collegePlayers <- collegePlayers[order(-collegePlayers$draftYear, collegePlayers$Player, -collegePlayers$Year),]
    
    test <- collegePlayers %>% filter(draftYear == draftYearToTest)
    
    test <- test %>% select(Player, Position, MP, VORP, Rank, games, DraftAge, ts_pct, fta_per_fga_pct, fg3a_per_fga_pct,
                            ast_per_fga_pct, ast_to_ratio, blk_g, stl_g, pts_play, SchoolSOS)
    # test$fg3_pct[is.na(test$fg3_pct)] <- 0
    
    train <- collegePlayers %>% filter(draftYear < draftYearToTest & !is.na(VORP) & !(draftYear %in% yearsToExclude))
    train <- train %>% select(Player, Position, MP, VORP, Rank, games, DraftAge, ts_pct, fta_per_fga_pct, fg3a_per_fga_pct,
                              ast_per_fga_pct, ast_to_ratio, blk_g, stl_g, pts_play, SchoolSOS)
    # train$fg3_pct[is.na(train$fg3_pct)] <- 0
    
  } else {
    collegePlayers <- dbGetQuery(cn, "Select a.*, e.Rank, e.Year, b.Season, b.ConfSRS, b.ConfSOS, 
                                  c.School, c.SchoolSRS, c.SchoolSOS,
                                  d.Height, d.Weight, d.Wingspan
                                  from CollegePlayers a
                                  inner join ConferenceStats b
                                  inner join SchoolStats c
                                  inner join Measurements d
                                  inner join DXRankings e
                                  on a.conf_link = b.ConfLink 
                                  and a.school_link = c.school_link
                                  and a.Player = d.Player
                                  and a.Player = e.Player")
    
    collegePlayers$MP <- as.numeric(collegePlayers$MP)
    collegePlayers <- collegePlayers[order(-collegePlayers$draftYear, collegePlayers$Player, -collegePlayers$Year),]
    
    test <- collegePlayers %>% filter(draftYear == draftYearToTest)
    
    test <- test %>% select(Player, Position, MP, VORP, season, Rank, games, DraftAge, fg2_pct, fg3_pct, ft_pct, orb_pct, drb_pct,
                            ast_per_poss, stl_per_poss, blk_per_poss, blk_pct, fg3a_per_fga_pct, fta_per_fga_pct,
                            tov_pct, usg_pct, SchoolSOS)
    test$fg3_pct[is.na(test$fg3_pct)] <- 0
    
    train <- collegePlayers %>% filter(!is.na(VORP) & !(draftYear %in% yearsToExclude))
    train <- train %>% select(Player, Position, MP, VORP, season, Rank, games, DraftAge, fg2_pct, fg3_pct, ft_pct, orb_pct, drb_pct,
                              ast_per_poss, stl_per_poss, blk_per_poss, blk_pct, fg3a_per_fga_pct, fta_per_fga_pct,
                              tov_pct, usg_pct, SchoolSOS)
    train$fg3_pct[is.na(train$fg3_pct)] <- 0
    
  }
  
  
  if(draftYearToTest == 2017) {
    test <- dbGetQuery(cn, "Select a.*, d.Rank, d.Year, b.Season, b.ConfSRS, b.ConfSOS,
                               c.School, c.SchoolSRS, c.SchoolSOS
                   from CollegeDraftProspects a
                   inner join ConferenceStats b
                   inner join SchoolStats c
                   inner join DXRankings d
                   on a.conf_link = b.ConfLink
                   and a.school_link = c.school_link
                   and a.Player = d.Player
                   order by d.Rank asc")
    
    test <- test %>% select(Player, Position, season, Rank, games, DraftAge, fg2_pct, fg3_pct, ft_pct, orb_pct, drb_pct,
                            ast_per_poss, stl_per_poss, blk_per_poss, blk_pct, fg3a_per_fga_pct, fta_per_fga_pct,
                            tov_pct, usg_pct, SchoolSOS)
    test$fg3_pct[is.nan(test$fg3_pct)] <- 0
    test$fg3_pct[is.na(test$fg3_pct)] <- 0
    
  }
  
  train$Position[grepl("Center", train$Position)] <- "Center"
  train$Position <- unlist(lapply(strsplit(train$Position, " and "), function(x) {head(x,1)}))
  train$Position[grepl("Guard", train$Position)] <- "Guard"
  train$Position[grepl("Forward", train$Position)] <- "Forward"
  
  test$Position[grepl("Center", test$Position)] <- "Center"
  test$Position <- unlist(lapply(strsplit(test$Position, " and "), function(x) {head(x,1)}))
  test$Position[grepl("Guard", test$Position)] <- "Guard"
  test$Position[grepl("Forward", test$Position)] <- "Forward"
  
  
  
  test <- ldply(unique(test$Player), function(x) {
    return(seasonWeighting(test %>% filter(Player == x), F, F))
  })
  
  try(test <- test[order(-test$VORP),])
  rownames(test) <- c(1:nrow(test))
  
  
  train <- ldply(unique(train$Player), function(x) {
    return(seasonWeighting(train %>% filter(Player == x), F))
  })
  
  train <- train %>% filter(!is.nan(orb_pct) & !is.nan(ast_per_poss))
  
  test$games <- NULL
  train$games <- NULL
  
  colStart <- which(colnames(train) == "Rank")
  
  test$RanksRF <- NA
  test$RanksLM <- NA
  test$StatsRF <- NA
  test$StatsLM <- NA
  
  train_temp <- train
  test_temp <- test
  
  results <- data.frame()
  
  for(i in 1:length(unique(train$Position))) {
    
    train <- train_temp %>% filter(Position == unique(train_temp$Position)[i])
    test <- test_temp %>% filter(Position == unique(test_temp$Position)[i])
    
    Ranks_RF <- paste0("ranksRF <- randomForest(VORP/MP ~ ", 
                       paste(colnames(train)[c(colStart:ncol(train))], collapse = " + "), 
                       ", data = train, na.action = na.roughfix)")
    Ranks_LM <- paste0("ranksLM <- lm(VORP/MP ~ ", 
                       paste(colnames(train)[c(colStart:ncol(train))], collapse = " + "), 
                       ", data = train)")
    
    Stats_RF <- paste0("statsRF <- randomForest(VORP/MP ~ ", 
                       paste(colnames(train)[c((colStart + 1):ncol(train))], collapse = " + "), 
                       ", data = train, na.action = na.roughfix)")
    Stats_LM <- paste0("statsLM <- lm(VORP/MP ~ ", 
                       paste(colnames(train)[c((colStart + 1):ncol(train))], collapse = " + "), 
                       ", data = train)")
    eval(parse(text = Ranks_RF))
    eval(parse(text = Ranks_LM))
    eval(parse(text = Stats_RF))
    eval(parse(text = Stats_LM))
    
    predRanksRF <- predict(ranksRF, newdata = test)
    predRanksLM <- predict(ranksLM, newdata = test)
    predStatsRF <- predict(statsRF, newdata = test)
    predStatsLM <- predict(statsLM, newdata = test)
    
    test$RanksRF <- predRanksRF
    test$RanksLM <- predRanksLM
    test$StatsRF <- predStatsRF
    test$StatsLM <- predStatsLM
    
    results <- rbind(results, test)
  }
  
  View(results %>% select(Player, Position, Rank, DraftAge, RanksRF, StatsRF) %>% .[order(-.$StatsRF),])


  
  # test$perGamePred <- predPerGame * test$MP
  # test$perMinPred  <- predPerMin * test$MP
  # test$perPossPred <- predPerPoss * test$MP
  # test$AdvancedPred <- predAdvanced * test$MP
  
  try(test$VORP_MP <- test$VORP/test$MP)
  test$RF_Blend <- (test$RanksRF + test$StatsRF)/2
  
  # plot_data <- test %>% .[order(-.$RanksLM),] %>% 
  # select(Player, VORP, Rank, RanksRF, RanksLM, StatsRF, StatsLM)
  
  # View(test %>% .[order(-.$RF_Blend),] %>% 
  # select(Player, SchoolSOS, Rank, RanksRF, StatsRF, RF_Blend))
  
  return(results %>% select(Player, Position, Rank, DraftAge, RanksRF, StatsRF) %>% .[order(-.$StatsRF),])
  
}
# 
# test$VORP_RRF <- test$MP * test$RanksRF
# test$VORP_RLM <- test$MP * test$RanksLM
# test$VORP_SRF <- test$MP * test$StatsRF
# test$VORP_SLM <- test$MP * test$StatsLM
# 
# plot_data$RanksRF_Rank[order(-plot_data$RanksRF)] <- c(1:nrow(plot_data))
# plot_data$RanksLM_Rank[order(-plot_data$RanksLM)] <- c(1:nrow(plot_data))
# plot_data$StatsRF_Rank[order(-plot_data$StatsRF)] <- c(1:nrow(plot_data))
# plot_data$StatsLM_Rank[order(-plot_data$StatsLM)] <- c(1:nrow(plot_data))
# 
# plot_data <- gather(plot_data, Method, MethodRank, RanksRF_Rank:StatsLM_Rank) %>% select(Player, Rank, Method, MethodRank)
# 
# ggplot(data = plot_data %>% filter(Method %in% c("RanksRF_Rank", "StatsRF_Rank")), 
#        aes(x = factor(Method), y = MethodRank, group = Player, colour = factor(Player))) + 
#   geom_line() + geom_point() +
#   scale_y_reverse(breaks = c(1:max(plot_data$MethodRank)), 
#                   labels = unlist(plot_data %>% filter(Method == "RanksRF_Rank") %>% select(Player)), 
#                   lim = c(max(plot_data$MethodRank),1)) + 
#   theme_bw() + theme(panel.grid.minor = element_blank(), 
#                      panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
#   ylab("Player") + theme(legend.position="none")

