library(dplyr)
library(DBI)
library(randomForest)

draftYearToTest <- 2016

cn <- dbConnect(RSQLite::SQLite(), "NBADraft.sqlite3")

collegePlayers <- dbGetQuery(cn, "Select a.*, b.Height, b.Weight, b.Wingspan from CollegePlayers a
                                  inner join Measurements b
                                  on a.Player = b.Player")

collegePlayers$season <- as.numeric(substr(collegePlayers$season, nchar(collegePlayers$season) - 1, nchar(collegePlayers$season)))
collegePlayers$season <- 2000 + collegePlayers$season
collegePlayers <- collegePlayers[order(-collegePlayers$draftYear, collegePlayers$Player, -collegePlayers$season),]

a <- collegePlayers[!duplicated(collegePlayers$Player), ]
a$vorpMin <- a$VORP/a$MP
a <- a[order(-a$draftYear, -a$VORP),]

test <- a %>% filter(draftYear >= draftYearToTest)
train <- a %>% filter(draftYear < draftYearToTest & !is.na(VORP))

pergame  <- train[complete.cases(train$pts_per_g),]
permin   <- train[complete.cases(train$pts_per_min),]
perposs  <- train[complete.cases(train$pts_per_poss),]
advanced <- train[complete.cases(train$bpm),]

pergame$pf_per_g <- NULL
pergame$tov_per_g <- NULL
pergame$fg3_pct <- NULL
perposs$fg3_pct <- NULL
advanced$fg3_pct <- NULL

toRunPerGame <- paste0("pergameModel <- randomForest(vorpMin ~ ", paste(colnames(pergame)[c(8, 10:23, 81:82)], collapse = " + "), ", data = pergame)")
eval(parse(text = toRunPerGame))
predPerGame <- predict(pergameModel, newdata = test)

toRunPerMin <- paste0("perminModel <- randomForest(vorpMin ~ ", paste(colnames(permin)[c(8, 10:40, 84:85)], collapse = " + "), ", data = permin)")
eval(parse(text = toRunPerMin))
predPerMin <- predict(perminModel, newdata = test)

toRunPerPoss <- paste0("perpossModel <- randomForest(vorpMin ~ ", paste(colnames(perposs)[c(8, 10:58, 83:84)], collapse = " + "), ", data = perposs)")
eval(parse(text = toRunPerPoss))
predPerPoss <- predict(perpossModel, newdata = test)

toRunAdvanced <- paste0("advancedModel <- randomForest(vorpMin ~ ", paste(colnames(advanced)[c(8, 10:84)], collapse = " + "), ", data = advanced)")
eval(parse(text = toRunAdvanced))
predAdvanced <- predict(advancedModel, newdata = test)

test$pGvorpMin <- predPerGame
test$pMvorpMin <- predPerMin
test$pPvorpMin <- predPerPoss
test$avorpMin <- predAdvanced

test$perGamePred <- predPerGame * test$MP
test$perMinPred  <- predPerMin * test$MP
test$perPossPred <- predPerPoss * test$MP
test$AdvancedPred <- predAdvanced * test$MP

test %>% .[order(-.$avorpMin),] %>% select(Player, VORP, vorpMin, avorpMin)