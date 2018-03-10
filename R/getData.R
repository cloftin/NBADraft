
#' @export
get_data <- function() {
  
  options(stringsAsFactors = F)
  
  draftYearToTest <- 2017
  
  player_stats <- data.frame()
  
  for(i in draftYearToTest:1980) {
    temp <- readHTMLTable(rawToChar(GET(paste0("http://www.basketball-reference.com/draft/NBA_", i, ".html"))$content), stringsAsFactors = F)$stats
    temp <- temp[,-c(15:18)]
    temp <- temp %>% filter(Pk != "Pk")
    temp <- temp %>% filter(Pk != "")
    temp$draftYear <- i
    # temp <- temp %>% select(Player, VORP, G, MP, draftYear)
    for(a in c(1,2,c(6:ncol(temp)))) {
      temp[,a] <- as.numeric(temp[,a])
    }
    temp$link <- ""
    
    lines <- readLines(paste0("https://www.basketball-reference.com/draft/NBA_", i, ".html"))
    for(j in 1:nrow(temp)) {
      print(paste0(i, ": ", j/nrow(temp)))
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
  
  cn <- dbConnect(RSQLite::SQLite(), "data/NBADraft.sqlite3")
  
  dbGetQuery(cn, "Drop Table PlayerStats")
  dbWriteTable(cn, "PlayerStats", player_stats)
  
  # write.csv(player_stats, file = "player_stats.csv", row.names = F)
  
  
  # player_stats <- read.csv("player_stats.csv", header = T, stringsAsFactors = F)
  
  euroPlayers <- data.frame()
  collegePlayers <- data.frame()
  
  player_stats$Amateur <- ""
  for(i in c(2075:nrow(player_stats))) {
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
        
        per_game <- draftR::get_Euro_PerGame(college, seasons)
        per_minute <- draftR::get_Euro_PerMinute(college, seasons)
        
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
      
      college_stats$Year <- 1 + as.integer(substr(college_stats$season, 1, 4))
      if(isEuro) {
        t <- tryCatch(bind_rows(euroPlayers, cbind(player, college_stats)), error = function(e) {
          e
        })
        if(inherits(t, "error")) {
          next
        } else {
          euroPlayers <- t
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
  }
  
  setnames(collegePlayers, "player_stats$link[i]", "link")
  setnames(euroPlayers, "player_stats$link[i]", "link")
  
  collegePlayers <- merge(player_stats, collegePlayers, by="link") %>% .[order(-.$VORP),]
  euroPlayers <- merge(player_stats, euroPlayers, by="link") %>% .[order(-.$VORP),]
  
  collegePlayers$Player.y <- NULL
  collegePlayers$g <- NULL
  collegePlayers$gs <- NULL
  setnames(collegePlayers, "Player.x", "Player")
  setnames(collegePlayers, "WS", "ProWS")
  setnames(collegePlayers, "BPM", "ProBPM")
  
  euroPlayers$Player.y <- NULL
  euroPlayers$g <- NULL
  euroPlayers$gs <- NULL
  setnames(euroPlayers, "Player.x", "Player")
  
  write.csv(collegePlayers, file = "college_players.csv", row.names = F)
  write.csv(euroPlayers, file = "euro_players.csv", row.names = F)
  
  dbGetQuery(cn, "Drop Table CollegePlayers")
  dbGetQuery(cn, "Drop Table EuroPlayers")
  
  collegePlayers$mp <- NULL
  euroPlayers$mp <- NULL
  
  dbWriteTable(cn, "CollegePlayers", collegePlayers)
  dbWriteTable(cn, "EuroPlayers", euroPlayers)

  
}