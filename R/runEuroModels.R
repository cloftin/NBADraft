
#' @export
runEuroModels <- function() {
  options(scipen = 999)
  
  # source("seasonWeighting.R")
  
  draftYearToTest <- 2015
  yearsToExclude <- c(draftYearToTest)
  
  cn <- dbConnect(RSQLite::SQLite(), "NBADraft.sqlite3")
  
  euroPlayers <- dbGetQuery(cn, "Select a.*, e.Rank, e.Year, 
                             d.Height, d.Weight, d.Wingspan
                             from EuroPlayers a
                             inner join Measurements d
                             inner join DXRankings e
                             on a.Player = d.Player
                             and a.Player = e.Player")
  
  euroPlayers$season <- as.numeric(substr(euroPlayers$season, nchar(euroPlayers$season) - 1, nchar(euroPlayers$season)))
  euroPlayers$season <- 2000 + euroPlayers$season
  euroPlayers <- euroPlayers[order(-euroPlayers$draftYear, euroPlayers$Player, -euroPlayers$season),]
  euroPlayers$fg2_pct <- euroPlayers$fg2_per_mp/euroPlayers$fg2a_per_mp
  euroPlayers$fg3_pct <- euroPlayers$fg3_per_mp/euroPlayers$fg3a_per_mp
  euroPlayers$ft_pct  <- euroPlayers$ft_per_mp/euroPlayers$fta_per_mp
  euroPlayers$fg3a_per_fga_pct <- euroPlayers$fg3_per_mp/euroPlayers$fg_per_mp
  euroPlayers$fta_per_fga_pct <- euroPlayers$fta_per_mp/euroPlayers$fg_per_mp
  
  a <- euroPlayers[!duplicated(euroPlayers$Player), ]
  a$vorpMin <- a$VORP/a$MP
  a <- a[order(-a$draftYear, -a$VORP),]
  
  test <- euroPlayers %>% filter(draftYear == draftYearToTest)
  test <- test %>% select(Player, MP, VORP, season, Rank, games, DraftAge, fg2_pct, fg3_pct, ft_pct, efg_pct,
                          orb_per_mp, drb_per_mp, ast_per_mp, stl_per_mp, blk_per_mp, fg3a_per_fga_pct, 
                          fta_per_fga_pct, tov_per_mp)
  test$fg3_pct[is.nan(test$fg3_pct)] <- 0
  
  test <- ldply(unique(test$Player), function(x) {
    return(seasonWeighting(test %>% filter(Player == x), T))
  })
  
  test <- test[order(-test$VORP/test$MP),]
  rownames(test) <- c(1:nrow(test))
  # test <- dbGetQuery(cn, "Select a.*, b.Season, b.ConfSRS, b.ConfSOS, 
  #                                c.School, c.SchoolSRS, c.SchoolSOS
  #                                from CollegeDraftProspects a
  #                                inner join ConferenceStats b
  #                                inner join SchoolStats c
  #                                on a.conf_link = b.ConfLink 
  #                                and a.school_link = c.school_link")
  
  # test$season <- as.numeric(substr(test$season, nchar(test$season) - 1, nchar(test$season)))
  # test$season <- 2000 + test$season
  # test <- test[order(test$Player, -test$season),]
  # test <- test[!duplicated(test$Player),]
  
  train <- euroPlayers %>% filter(draftYear < draftYearToTest & !is.na(VORP) & !(draftYear %in% yearsToExclude))
  train <- train %>% select(Player, MP, VORP, season, Rank, games, DraftAge, fg2_pct, fg3_pct, ft_pct, 
                            orb_per_mp, drb_per_mp, ast_per_mp, stl_per_mp, blk_per_mp, fg3a_per_fga_pct, 
                            fta_per_fga_pct, tov_per_mp)
  train$fg3_pct[is.na(train$fg3_pct)] <- 0
  train$fg2_pct[is.na(train$fg2_pct)] <- 0
  train$ft_pct[is.na(train$ft_pct)] <- 0
  train$fg3a_per_fga_pct[is.na(train$fg3a_per_fga_pct)] <- 0
  train$fta_per_fga_pct[is.na(train$fta_per_fga_pct)] <- 0
  
  train <- ldply(unique(train$Player), function(x) {
    return(seasonWeighting(train %>% filter(Player == x) %>% head(.,4), T))
  })
  
  
  # pergame  <- train[complete.cases(train$pts_per_g),]
  # permin   <- train[complete.cases(train$pts_per_min),]
  # perposs  <- train[complete.cases(train$pts_per_poss),]
  # advanced <- train[complete.cases(train$bpm),]
  
  # pergame$pf_per_g <- NULL
  # pergame$tov_per_g <- NULL
  # pergame$fg3_pct <- NULL
  # perposs$fg3_pct <- NULL
  
  # advanced$fg3_pct[is.na(advanced$fg3_pct)] <- 0
  
  # toRunPerGame <- paste0("pergameModel <- randomForest(vorpMin ~ ", paste(colnames(pergame)[c(8, 10:23, 81:82)], collapse = " + "), ", data = pergame)")
  # eval(parse(text = toRunPerGame))
  # predPerGame <- predict(pergameModel, newdata = test)
  # 
  # toRunPerMin <- paste0("perminModel <- randomForest(vorpMin ~ ", paste(colnames(permin)[c(8, 10:40, 84:85)], collapse = " + "), ", data = permin)")
  # eval(parse(text = toRunPerMin))
  # predPerMin <- predict(perminModel, newdata = test)
  # 
  # toRunPerPoss <- paste0("perpossModel <- randomForest(vorpMin ~ ", paste(colnames(perposs)[c(8, 10:58, 83:84)], collapse = " + "), ", data = perposs)")
  # eval(parse(text = toRunPerPoss))
  # predPerPoss <- predict(perpossModel, newdata = test)
  
  
  # advanced <- advanced %>% select(Player, MP, VORP, season, DraftAge, fg2_pct, fg3_pct, ft_pct, orb_pct, drb_pct,
  #                                 ast_per_poss, stl_per_poss, blk_per_poss, blk_pct, fg3a_per_fga_pct, fta_per_fga_pct,
  #                                 tov_pct, usg_pct, SchoolSOS)
  ## Includes DX Rankings in model
  Ranks_RF <- paste0("ranksRF <- randomForest(VORP/MP ~ ", 
                     paste(colnames(train)[c(4:ncol(train))], collapse = " + "), 
                     ", data = train, na.action = na.roughfix)")
  Ranks_LM <- paste0("ranksLM <- lm(VORP/MP ~ ", 
                     paste(colnames(train)[c(4:ncol(train))], collapse = " + "), 
                     ", data = train)")
  
  ## Stats-only model
  Stats_RF <- paste0("statsRF <- randomForest(VORP/MP ~ ", 
                     paste(colnames(train)[c(5:ncol(train))], collapse = " + "), 
                     ", data = train, na.action = na.roughfix)")
  Stats_LM <- paste0("statsLM <- lm(VORP/MP ~ ", 
                     paste(colnames(train)[c(5:ncol(train))], collapse = " + "), 
                     ", data = train)")
  # toRunAdvanced <- paste0("advancedModel <- randomForest(vorpMin ~ ", 
  #                         paste(colnames(advanced)[c(8, 10:81, 86:87, 89:90)], collapse = " + "), 
  #                         ", data = advanced, mtry = 77)")
  eval(parse(text = Ranks_RF))
  eval(parse(text = Ranks_LM))
  eval(parse(text = Stats_RF))
  eval(parse(text = Stats_LM))
  
  predRanksRF <- predict(ranksRF, newdata = test)
  predRanksLM <- predict(ranksLM, newdata = test)
  predStatsRF <- predict(statsRF, newdata = test)
  predStatsLM <- predict(statsLM, newdata = test)
  
  # test$pGvorpMin <- predPerGame
  # test$pMvorpMin <- predPerMin
  # test$pPvorpMin <- predPerPoss
  test$RanksRF <- predRanksRF
  test$RanksLM <- predRanksLM
  test$StatsRF <- predStatsRF
  test$StatsLM <- predStatsLM
  
  # test$perGamePred <- predPerGame * test$MP
  # test$perMinPred  <- predPerMin * test$MP
  # test$perPossPred <- predPerPoss * test$MP
  # test$AdvancedPred <- predAdvanced * test$MP
  
  test %>% .[order(-.$RanksRF),] %>% 
    select(Player, Rank, RanksRF, RanksLM, StatsRF, StatsLM)
  
  plot_data <- test %>% .[order(-.$StatsLM),] %>% 
    select(Player, Rank, RanksRF, RanksLM, StatsRF, StatsLM)
  
  plot_data$RanksRF_Rank[order(-plot_data$RanksRF)] <- c(1:nrow(plot_data))
  plot_data$RanksLM_Rank[order(-plot_data$RanksLM)] <- c(1:nrow(plot_data))
  plot_data$StatsRF_Rank[order(-plot_data$StatsRF)] <- c(1:nrow(plot_data))
  plot_data$StatsLM_Rank[order(-plot_data$StatsLM)] <- c(1:nrow(plot_data))
  
  plot_data <- gather(plot_data, Method, MethodRank, RanksRF_Rank:StatsLM_Rank) %>% select(Player, Rank, Method, MethodRank)
  
  ggplot(data = plot_data, 
         aes(x = factor(Method), y = MethodRank, group = Player, colour = factor(Player))) + 
    geom_line() + 
    scale_y_reverse(breaks = c(1:max(plot_data$MethodRank)), 
                    labels = unlist(plot_data %>% filter(Method == "RanksLM_Rank") %>% 
                                      .[order(.$MethodRank),] %>% select(Player)), 
                    lim = c(max(plot_data$MethodRank),1)) + 
    theme_bw() + theme(panel.grid.minor = element_blank(), 
                       panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
    ylab("Player") + theme(legend.position="none")
  
}