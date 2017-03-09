library(DBI)
library(dplyr)
library(rvest)
library(lubridate)
library(stringi)

source("get_College_Data.R")

getDraftClassData <- function() {
  cn <- dbConnect(RSQLite::SQLite(), "NBADraft.sqlite3")
  players <- dbGetQuery(cn, "Select * from DraftProspects")
  players$isEuro <- grepl("basketball", players$Link)
  
  players$Amateur <- ""
  collegePlayers <- data.frame()
  euroPlayers <- data.frame()
  
  for(i in 1:nrow(players)) {
    print(paste0(i, ": ", players$Player[i]))
    lines <- readLines(players$DXLink[i])

    birth <- lines[grep("Bday", lines)]
    birth <- as.Date(substr(strsplit(birth, "b> ")[[1]][2], 1, 10), format = "%m/%d/%Y")
    
    lines <- readLines(players$Link[i])
    position <- stri_trim(lines[grep(pattern = "Position:", lines) + 2])
    if(length(position) == 0) {
      position <- NA
    }
    if(players$isEuro[i]) {
      
      players$Amateur[i] <- "Euro"
      
      seasons <- tryCatch(read_html(players$Link[i]) %>% html_nodes("table") %>% html_table() %>% .[[1]])
      w <- which(seasons$Season == "Career")
      if(length(w) == 0) {
        seasons <- length(which(seasons$Season != ""))
      } else {
        seasons <- w - 1
      }
      
      per_game <- get_Euro_PerGame(players$Link[i], seasons)
      per_minute <- get_Euro_PerMinute(players$Link[i], seasons)
      
      games <- per_game$g
      
      college_stats <- merge(per_game, per_minute, by = "season")
      college_stats <- college_stats[, -c(grep(".x", colnames(college_stats)), grep(".y", colnames(college_stats)))]
      college_stats <- cbind(college_stats, games)
      
    } else {
      
      players$Amateur[i] <- "College"
      seasons <- tryCatch(read_html(players$Link[i]) %>% html_nodes("table") %>% html_table() %>% .[[1]])
      w <- which(seasons$Season == "Career")
      if(length(w) == 0) {
        seasons <- length(which(seasons$Season != ""))
      } else {
        seasons <- w - 1
      }
      
      per_game <- get_College_PerGame(players$Link[i], seasons)
      per_minute <- get_College_PerMinute(players$Link[i], seasons)
      per_poss <- get_College_PerPoss(players$Link[i], seasons)
      advanced <- get_College_Advanced(players$Link[i], seasons)
      
      games <- per_game$g
      school_link <- paste0("http://www.sports-reference.com", per_game$school_link)
      conf_link <- paste0("http://www.sports-reference.com", per_game$conf_link)
      
      college_stats <- merge(merge(merge(per_game, per_minute, by = "season"), per_poss, by = "season"), advanced, by = "season")
      college_stats <- college_stats[, -c(grep(".x", colnames(college_stats)), grep(".y", colnames(college_stats)))]
      college_stats <- cbind(college_stats, games, school_link, conf_link)
    }
    
    
    
    
    age <- 0
    if(month(birth) < 6) {
      years <- players$DraftYear[i] - year(birth)
      days <- as.numeric(difftime(paste0(players$DraftYear[i], "-06-01"), paste0(players$DraftYear[i], substr(birth, 5, 10))))
      age <- years + (days/365)
    } else {
      years <- players$DraftYear[i] - year(birth) - 1
      days <- as.numeric(difftime(paste0(players$DraftYear[i], substr(birth, 5, 10)), paste0(players$DraftYear[i], "-06-01")))
      age <- years + (days/365)
    }
    
    player <- data.frame(Player = players$Player[i], DraftAge = age, Position = position)
    college_stats$season <- as.character(college_stats$season)
    college_stats$Year <- 2000 + as.integer(substr(college_stats$season, nchar(college_stats$season) - 1, nchar(college_stats$season)))
    
    if(players$isEuro[i]) {
      euroPlayers <- rbind(euroPlayers, cbind(player, college_stats))
    } else {
      collegePlayers <- rbind(collegePlayers, cbind(player, college_stats))
    }
    
  }
  
  write.csv(euroPlayers, file = "EuroDraftProspects.csv", row.names = F)
  write.csv(collegePlayers, file = "CollegeDraftProspects.csv", row.names = F)
  
  dbGetQuery(cn, "Drop Table CollegeDraftProspects")
  dbGetQuery(cn, "Drop Table EuroDraftProspects")
  
  dbWriteTable(cn, "CollegeDraftProspects", collegePlayers)
  dbWriteTable(cn, "EuroDraftProspects", euroPlayers)
}