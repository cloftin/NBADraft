library(XML)
library(dplyr)
library(lubridate)
library(rvest)
library(stringi)
library(randomForest)
library(RSQLite)
library(DBI)

source("get_College_Data.R")

options(stringsAsFactors = F)

draftYearToTest <- 2017

player_stats <- data.frame()

for(i in draftYearToTest:1980) {
  temp <- readHTMLTable(rawToChar(GET(paste0("http://www.basketball-reference.com/draft/NBA_", i, ".html"))$content), stringsAsFactors = F)$stats
  temp <- temp[,-c(15:18)]
  temp <- temp %>% filter(Pk != "Pk")
  temp <- temp %>% filter(Pk != "")
  temp$draftYear <- i
  temp <- temp %>% select(Player, VORP, G, MP, draftYear)
  temp$VORP <- as.numeric(temp$VORP)
  temp$link <- ""
  
  lines <- readLines(paste0("https://www.basketball-reference.com/draft/NBA_", i, ".html"))
  for(j in 1:nrow(temp)) {
    print(j/nrow(temp))
    page <- lines[grep(temp$Player[j], lines)]
    page <- strsplit(page, "a href=\\\"")[[1]]
    page <- page[grep(temp$Player[j], page)]
    page <- strsplit(page, ">")[[1]][1]
    page <- paste0("https://www.basketball-reference.com", gsub("\\\"", "", page))
    temp$link[j] <- page
  }
  
  player_stats <- rbind(player_stats, temp)
}

player_stats <- player_stats[which(substr(player_stats$link, nchar(player_stats$link) - 3, nchar(player_stats$link)) == "html"),]

cn <- dbConnect(RSQLite::SQLite(), "NBADraft.sqlite3")

dbGetQuery(cn, "Drop Table PlayerStats")
dbWriteTable(cn, "PlayerStats", player_stats)

# write.csv(player_stats, file = "player_stats.csv", row.names = F)


# player_stats <- read.csv("player_stats.csv", header = T, stringsAsFactors = F)

euroPlayers <- data.frame()
collegePlayers <- data.frame()

player_stats$Amateur <- ""
for(i in 1:nrow(player_stats)) {
  print(paste0(i, ": ", player_stats$Player[i]))
  lines <- readLines(player_stats$link[i])
  position <- stri_trim(lines[grep(pattern = "Position:", lines) + 2])
  if(length(position) == 0) {
    position <- NA
  }
  college <- lines[grep("College Basketball at|Euro Stats at", lines)][1]
  if(length(college) !=0 & !is.na(college)) {
    college <- strsplit(college, "<a href=\\\"")[[1]][2]
    college <- strsplit(college, "\\\"")[[1]][1]
    
    isEuro <- FALSE
    if(grepl("euro", college)) {
      isEuro <- TRUE
    }
    
    birth <- lines[grep("data-birth", lines)]
    birth <- as.Date(substr(strsplit(birth, "data-birth=\\\"")[[1]][2], 1, 10))
    
    if(isEuro) {
      player_stats$Amateur[i] <- "Euro"
      seasons <- tryCatch(read_html(college) %>% html_nodes("table") %>% html_table() %>% .[[1]])
      w <- which(seasons$Season == "Career")
      if(length(w) == 0) {
        seasons <- length(which(seasons$Season != ""))
      } else {
        seasons <- w - 1
      }
      
      per_game <- get_Euro_PerGame(college, seasons)
      per_minute <- get_Euro_PerMinute(college, seasons)
      
      games <- per_game$g
      
      college_stats <- merge(per_game, per_minute, by = "season")
      college_stats <- college_stats[, -c(grep(".x", colnames(college_stats)), grep(".y", colnames(college_stats)))]
      college_stats <- cbind(college_stats, games, player_stats$link[i])
      
    } else {
      player_stats$Amateur[i] <- "College"
      seasons <- tryCatch(read_html(college) %>% html_nodes("table") %>% html_table() %>% .[[1]],
                          error = function(e) {
                            e
                          })
      if(inherits(seasons, "error")) {
        next
      }
      w <- which(seasons$Season == "Career")
      if(length(w) == 0) {
        seasons <- length(which(seasons$Season != ""))
      } else {
        seasons <- w - 1
      }
      
      per_game <- NA
      per_minute <- NA
      per_poss <- NA
      advanced <- NA
      per_game <- get_College_PerGame(college, seasons)
      per_minute <- tryCatch(get_College_PerMinute(college, seasons), error = function(e) {
        per_minute <- NA
      })
      per_poss <- tryCatch(get_College_PerPoss(college, seasons), error = function(e) {
        per_poss <- NA
      })
      advanced <- tryCatch(get_College_Advanced(college, seasons), error = function(e) {
        advanced <- NA
      })
      
      games <- per_game$g
      school_link <- paste0("http://www.sports-reference.com", per_game$school_link)
      conf_link <- paste0("http://www.sports-reference.com", per_game$conf_link)
      
      if(!is.na(per_game)) {
        college_stats <- per_game
      }
      if(!is.na(per_minute)) {
        college_stats <- merge(per_game, per_minute, by = "season")
      }
      if(!is.na(per_poss)) {
        college_stats <- merge(college_stats, per_poss, by = "season")
      }
      if(!is.na(advanced)) {
        college_stats <- merge(college_stats, advanced, by = "season")
      }
      college_stats <- college_stats[, -c(grep(".x", colnames(college_stats)), grep(".y", colnames(college_stats)))]
      college_stats <- cbind(college_stats, games, school_link, conf_link, player_stats$link[i])
    }
    
    
    
    
    age <- 0
    if(month(birth) == 2 && day(birth) == 29) {
      birth <- birth + days(1)
    }
    if(month(birth) < 6) {
      years <- player_stats$draftYear[i] - year(birth)
      days <- as.numeric(difftime(paste0(player_stats$draftYear[i], "-06-01"), paste0(player_stats$draftYear[i], substr(birth, 5, 10))))
      age <- years + (days/365)
    } else {
      years <- player_stats$draftYear[i] - year(birth) - 1
      days <- as.numeric(difftime(paste0(player_stats$draftYear[i], substr(birth, 5, 10)), paste0(player_stats$draftYear[i], "-06-01")))
      age <- years + (days/365)
    }
    
    player <- data.frame(Player = player_stats$Player[i], DraftAge = age, Position = position)
    
    college_stats$Year <- 2000 + as.integer(substr(college_stats$season, nchar(college_stats$season) - 1, nchar(college_stats$season)))
    if(isEuro) {
      t <- tryCatch(bind_rows(euroPlayers, cbind(player, college_stats)), error = function(e) {
        e
      })
      if(inherits(t, "error")) {
        next
      } else {
        euroPlayers <- t
      }
    }
  } else {
    t <- tryCatch(bind_rows(collegePlayers, cbind(player, college_stats)), error = function(e) {
      e
    })
    if(inherits(t, "error")) {
      next
    } else {
      collegePlayers <- t
    } 
  }
}



collegePlayers <- merge(player_stats, collegePlayers, by="link") %>% .[order(-.$VORP),]
euroPlayers <- merge(player_stats, euroPlayers, by="link") %>% .[order(-.$VORP),]



write.csv(collegePlayers, file = "college_players.csv", row.names = F)
write.csv(euroPlayers, file = "euro_players.csv", row.names = F)

dbGetQuery(cn, "Drop Table CollegePlayers")
dbGetQuery(cn, "Drop Table EuroPlayers")

collegePlayers$mp <- NULL
euroPlayers$mp <- NULL

dbWriteTable(cn, "CollegePlayers", collegePlayers)
dbWriteTable(cn, "EuroPlayers", euroPlayers)

collegePlayers <- read.csv(file = "college_players.csv", header = T, stringsAsFactors = F)
euroPlayers <- read.csv(file = "euro_players.csv", header = T, stringsAsFactors = F)

collegePlayers$season <- as.numeric(substr(collegePlayers$season, nchar(collegePlayers$season) - 1, nchar(collegePlayers$season)))
collegePlayers$season <- 2000 + collegePlayers$season
collegePlayers <- collegePlayers[order(-collegePlayers$draftYear, collegePlayers$Player, -collegePlayers$season),]

a <- collegePlayers[!duplicated(collegePlayers$Player), ]
a$vorpMin <- a$VORP/a$MP
a <- a[order(-a$draftYear, -a$VORP),]

test <- a %>% filter(draftYear == draftYearToTest)
train <- a %>% filter(draftYear != draftYearToTest & !is.na(VORP))

pergame  <- train[complete.cases(train$pts_per_g),]
permin   <- train[complete.cases(train$pts_per_min),]
perposs  <- train[complete.cases(train$pts_per_poss),]
advanced <- train[complete.cases(train$bpm),]

pergame$pf_per_g <- NULL
pergame$tov_per_g <- NULL
pergame$fg3_pct <- NULL
perposs$fg3_pct <- NULL
advanced$fg3_pct <- NULL

toRunPerGame <- paste0("pergameModel <- randomForest(vorpMin ~ ", paste(colnames(pergame)[c(5:17)], collapse = " + "), ", data = pergame)")
eval(parse(text = toRunPerGame))
predPerGame <- predict(pergameModel, newdata = test)

toRunPerMin <- paste0("perminModel <- randomForest(vorpMin ~ ", paste(colnames(permin)[c(5:34)], collapse = " + "), ", data = permin)")
eval(parse(text = toRunPerMin))
predPerMin <- predict(perminModel, newdata = test)

toRunPerPoss <- paste0("perpossModel <- randomForest(vorpMin ~ ", paste(colnames(perposs)[c(5:52)], collapse = " + "), ", data = perposs)")
eval(parse(text = toRunPerPoss))
predPerPoss <- predict(perpossModel, newdata = test)

toRunAdvanced <- paste0("advancedModel <- randomForest(vorpMin ~ ", paste(colnames(advanced)[c(5:76)], collapse = " + "), ", data = advanced)")
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


test <- test %>% select(Player, perGamePred, perMinPred, perPossPred, AdvancedPred, VORP, vorpMin)
test[order(-test$AdvancedPred),]

# 
# ##Per Game
# bestPerGameCombo <- ""
# bestPerGameRMSE <- 100
# 
# for(i in 1:length(colnames(permin)[c(5:17)])) {
#   print(i)
#   t <- combn(colnames(pergame)[c(5:17)], m = i)
#   for(j in 1:ncol(t)) {
#     print(j/ncol(t))
#     toRunPerGame <- paste0("pergameModel <- randomForest(vorpMin ~ ", paste(t[,j], collapse = " + "), ", data = pergame)")
#     eval(parse(text = toRunPerGame))
#     predPerGame <- predict(pergameModel, newdata = test)
#     predPerGame <- predPerGame * test$MP
#     if(RMSE(test$VORP, predPerGame) < bestRMSE) {
#       bestPerGameRMSE <- RMSE(test$VORP, predPerGame)
#       bestPerGameCombo <- toRunPerGame
#     }
#   }
# }
# 
# 
# ##Per Minute
# bestPerMinCombo <- ""
# bestPerMinRMSE <- 100
# 
# for(i in 1:length(colnames(permin)[c(5:34)])) {
#   print(i)
#   t <- combn(colnames(permin)[c(5:34)], m = i)
#   for(j in 1:ncol(t)) {
#     print(j/ncol(t))
#     toRunPerMin <- paste0("perminModel <- randomForest(vorpMin ~ ", paste(colnames(permin)[c(5:34)], collapse = " + "), ", data = permin)")
#     eval(parse(text = toRunPerMin))
#     predPerMin <- predict(perminModel, newdata = test)
#     predPerMin <- predPerMin * test$MP
#     if(RMSE(test$VORP, predPerMin) < bestRMSE) {
#       bestPerMinRMSE <- RMSE(test$VORP, predPerMin)
#       bestPerMinCombo <- toRunPerMin
#     }
#   }
# }
# 
# 
# ##Per Possession
# bestPerPossCombo <- ""
# bestPerPossRMSE <- 100
# 
# for(i in 1:length(colnames(perposs)[c(5:52)])) {
#   print(i)
#   t <- combn(colnames(perposs)[c(5:52)], m = i)
#   for(j in 1:ncol(t)) {
#     print(j/ncol(t))
#     toRunPerPoss <- paste0("perpossModel <- randomForest(vorpMin ~ ", paste(colnames(perposs)[c(5:52)], collapse = " + "), ", data = perposs)")
#     eval(parse(text = toRunPerPoss))
#     predPerPoss <- predict(perpossModel, newdata = test)
#     predPerPoss <- predPerPoss * test$MP
#     if(RMSE(test$VORP, predPerPoss) < bestRMSE) {
#       bestPerPossRMSE <- RMSE(test$VORP, predPerPoss)
#       bestPerPossCombo <- toRunPerPoss
#     }
#   }
# }
# 
# 
# ##Advanced
# bestAdvancedCombo <- ""
# bestAdvancedRMSE <- 100
# 
# for(i in 1:length(colnames(advanced)[c(5:52)])) {
#   print(i)
#   t <- combn(colnames(advanced)[c(5:52)], m = i)
#   for(j in 1:ncol(t)) {
#     print(j/ncol(t))
#     toRunAdvanced <- paste0("advancedModel <- randomForest(vorpMin ~ ", paste(colnames(advanced)[c(5:76)], collapse = " + "), ", data = advanced)")
#     eval(parse(text = toRunAdvanced))
#     predAdvanced <- predict(advancedModel, newdata = test)
#     predAdvanced <- predAdvanced * test$MP
#     if(RMSE(test$VORP, predAdvanced) < bestRMSE) {
#       bestAdvancedRMSE <- RMSE(test$VORP, predAdvanced)
#       bestAdvancedCombo <- toRunAdvanced
#     }
#   }
# }