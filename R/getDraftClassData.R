#' @export
getDraftClassData <- function() {
  cn <- dbConnect(RSQLite::SQLite(), "data/NBADraft.sqlite3")
  players <- dbGetQuery(cn, "Select * from Players Where Prospect = 1")
  players$isEuro <- grepl("basketball", players$BRLink)
  
  player_measurements <- readLines("Player_data.txt")
  player_measurements <- strsplit(player_measurements, "\\t")
  
  player_measurements <- plyr::ldply(player_measurements[c(2:length(player_measurements))], function(x) {
    x[1] <- strsplit(x[1], "\\. ")[[1]][2]
    return(as.data.frame(t(cbind(unlist(x)))))
  })
  
  colnames(player_measurements) <- c("Player", "Position", "Team", "Age", "Height", "Wingspan")
  for(i in 1:ncol(player_measurements)) {
    player_measurements[,i] <- as.character(player_measurements[,i])
    if(i >= 5) {
      player_measurements[,i] <- decimalLength(player_measurements[,i])
    }
  }
  player_measurements$Age <- as.numeric(player_measurements$Age)
  
  
  players$Amateur <- ""
  collegePlayers <- data.frame()
  euroPlayers <- data.frame()
  
  for(i in 1:nrow(players)) {
    print(paste0(i, ": ", players$Player[i]))
    lines <- readLines(players$BRLink[i])
    
    # age <- lines[grep("Age:", lines)]
    # age <- as.numeric(strsplit(age, "strong> ")[[1]][2])
    
    # position <- stri_trim(lines[grep(pattern = "Position:", lines) + 2])
    # if(length(position) == 0) {
    # position <- NA
    # }
    if(players$isEuro[i]) {
      
      players$Amateur[i] <- "Euro"
      
      seasons <- tryCatch(read_html(players$BRLink[i]) %>% html_nodes("table") %>% html_table() %>% .[[1]])
      w <- which(seasons$Season == "Career")
      if(length(w) == 0) {
        seasons <- length(which(seasons$Season != ""))
      } else {
        seasons <- w - 1
      }
      
      per_game <- get_Euro_PerGame(players$BRLink[i], seasons)
      per_minute <- get_Euro_PerMinute(players$BRLink[i], seasons)
      
      games <- per_game$g
      
      college_stats <- merge(per_game, per_minute, by = "season")
      college_stats <- college_stats[, -c(grep(".x", colnames(college_stats)), grep(".y", colnames(college_stats)))]
      college_stats <- cbind(college_stats, games)
      
    } else {
      
      players$Amateur[i] <- "College"
      seasons <- tryCatch({read_html(players$BRLink[i]) %>% html_nodes("table") %>% html_table() %>% .[[1]]}, error = function(e){e})
      if(class(seasons)[1] != "simpleError") {
        w <- which(seasons$Season == "Career")
        if(length(w) == 0) {
          seasons <- length(which(seasons$Season != ""))
        } else {
          seasons <- w - 1
        }
        
        per_game <- get_College_PerGame(players$BRLink[i], seasons)
        per_minute <- get_College_PerMinute(players$BRLink[i], seasons)
        per_poss <- get_College_PerPoss(players$BRLink[i], seasons)
        advanced <- get_College_Advanced(players$BRLink[i], seasons)
        
        games <- per_game$g
        school_link <- paste0("http://www.sports-reference.com", per_game$school_link)
        conf_link <- paste0("http://www.sports-reference.com", per_game$conf_link)
        
        college_stats <- merge(merge(merge(per_game, per_minute, by = "season"), per_poss, by = "season"), advanced, by = "season")
        college_stats <- college_stats[, -c(grep(".x", colnames(college_stats)), grep(".y", colnames(college_stats)))]
        college_stats <- cbind(college_stats, games, school_link, conf_link)
      }
    }
    
    
    
    
    # age <- 0
    # if(month(birth) < 6) {
    #   years <- players$DraftYear[i] - year(birth)
    #   days <- as.numeric(difftime(paste0(players$DraftYear[i], "-06-01"), paste0(players$DraftYear[i], substr(birth, 5, 10))))
    #   age <- years + (days/365)
    # } else {
    #   years <- players$DraftYear[i] - year(birth) - 1
    #   days <- as.numeric(difftime(paste0(players$DraftYear[i], substr(birth, 5, 10)), paste0(players$DraftYear[i], "-06-01")))
    #   age <- years + (days/365)
    # }
    
    player <- data.frame(PlayerId = players$PlayerId[i], Player = players$Player[i], DraftAge = player_measurements$Age[i], Position = player_measurements$Position[i])
    college_stats$season <- as.character(college_stats$season)
    college_stats$Year <- 2000 + as.integer(substr(college_stats$season, nchar(college_stats$season) - 1, nchar(college_stats$season)))
    
    if(NA %in% colnames(college_stats)) {
      college_stats$`NA` <- NULL
    }
    
    if(players$isEuro[i]) {
      euroPlayers <- rbind(euroPlayers, cbind(player, college_stats))
    } else {
      collegePlayers <- rbind(collegePlayers, cbind(player, college_stats))
    }
    
  }
  
  write.csv(euroPlayers, file = "data/EuroDraftProspects.csv", row.names = F)
  write.csv(collegePlayers, file = "data/CollegeDraftProspects.csv", row.names = F)
  
  dbGetQuery(cn, "Drop Table CollegeDraftProspects")
  dbGetQuery(cn, "Drop Table EuroDraftProspects")
  
  dbWriteTable(cn, "CollegeDraftProspects", collegePlayers)
  dbWriteTable(cn, "EuroDraftProspects", euroPlayers)
}