library(plyr)
library(dplyr)
library(DBI)
library(randomForest)

options(scipen = 999)

draftYearToTest <- 2015
yearsToExclude <- c(draftYearToTest)

cn <- dbConnect(RSQLite::SQLite(), "NBADraft.sqlite3")

collegePlayers <- dbGetQuery(cn, "Select a.*, b.Season, b.ConfSRS, b.ConfSOS, 
                                  c.School, c.SchoolSRS, c.SchoolSOS,
                                  d.Height, d.Weight, d.Wingspan
                                  from CollegePlayers a
                                  inner join ConferenceStats b
                                  inner join SchoolStats c
                                  inner join Measurements d
                                  on a.conf_link = b.ConfLink 
                                  and a.school_link = c.school_link
                                  and a.Player = d.Player")

collegePlayers$season <- as.numeric(substr(collegePlayers$season, nchar(collegePlayers$season) - 1, nchar(collegePlayers$season)))
collegePlayers$season <- 2000 + collegePlayers$season
collegePlayers <- collegePlayers[order(-collegePlayers$draftYear, collegePlayers$Player, -collegePlayers$season),]

a <- collegePlayers[!duplicated(collegePlayers$Player), ]
a$vorpMin <- a$VORP/a$MP
a <- a[order(-a$draftYear, -a$VORP),]

test <- collegePlayers %>% filter(draftYear == draftYearToTest)
test <- test %>% select(Player, MP, VORP, season, games, DraftAge, fg2_pct, fg3_pct, ft_pct, orb_pct, drb_pct,
       ast_per_poss, stl_per_poss, blk_per_poss, blk_pct, fg3a_per_fga_pct, fta_per_fga_pct,
       tov_pct, usg_pct, SchoolSOS)
test$fg3_pct[is.na(test$fg3_pct)] <- 0

test <- ldply(unique(test$Player), function(x) {
  return(seasonWeighting(test %>% filter(Player == x)))
})

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

train <- a %>% filter(draftYear <= draftYearToTest & !is.na(VORP) & !(draftYear %in% yearsToExclude))

pergame  <- train[complete.cases(train$pts_per_g),]
permin   <- train[complete.cases(train$pts_per_min),]
perposs  <- train[complete.cases(train$pts_per_poss),]
advanced <- train[complete.cases(train$bpm),]

pergame$pf_per_g <- NULL
pergame$tov_per_g <- NULL
pergame$fg3_pct <- NULL
perposs$fg3_pct <- NULL

advanced$fg3_pct[is.na(advanced$fg3_pct)] <- 0

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


advanced <- advanced %>% select(Player, MP, VORP, season, DraftAge, fg2_pct, fg3_pct, ft_pct, orb_pct, drb_pct,
                                ast_per_poss, stl_per_poss, blk_per_poss, blk_pct, fg3a_per_fga_pct, fta_per_fga_pct,
                                tov_pct, usg_pct, SchoolSOS)
toRunAdvanced <- paste0("advancedModel <- randomForest(VORP/MP ~ ", 
                        paste(colnames(advanced)[c(5:19)], collapse = " + "), 
                        ", data = advanced)")
toRunAdvancedLM <- paste0("advancedModelLM <- lm(VORP/MP ~ ", 
                        paste(colnames(advanced)[c(5:19)], collapse = " + "), 
                        ", data = advanced)")
# toRunAdvanced <- paste0("advancedModel <- randomForest(vorpMin ~ ", 
#                         paste(colnames(advanced)[c(8, 10:81, 86:87, 89:90)], collapse = " + "), 
#                         ", data = advanced, mtry = 77)")
eval(parse(text = toRunAdvanced))
eval(parse(text = toRunAdvancedLM))

predAdvanced <- predict(advancedModel, newdata = test)
predAdvancedLM <- predict(advancedModelLM, newdata = test)

# test$pGvorpMin <- predPerGame
# test$pMvorpMin <- predPerMin
# test$pPvorpMin <- predPerPoss
test$avorpMin <- predAdvanced
test$avorpMinLM <- predAdvancedLM

# test$perGamePred <- predPerGame * test$MP
# test$perMinPred  <- predPerMin * test$MP
# test$perPossPred <- predPerPoss * test$MP
test$AdvancedPred <- predAdvanced * test$MP

test %>% .[order(-.$avorpMinLM),] %>% select(Player, avorpMin, avorpMinLM) %>% cbind(., c(1:nrow(.)))
