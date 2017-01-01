library(XML)
library(dplyr)
library(lubridate)
library(rvest)
library(stringi)

source("get_College_Data.R")

options(stringsAsFactors = F)

player_stats <- data.frame()

for(i in 2015:2010) {
  temp <- readHTMLTable(paste0("http://www.basketball-reference.com/play-index/psl_finder.cgi?request=1&match=combined&type=totals&per_minute_base=36&per_poss_base=100&lg_id=NBA&is_playoffs=N&year_min=&year_max=&franch_id=&season_start=1&season_end=-1&age_min=0&age_max=99&height_min=0&height_max=99&shoot_hand=&birth_country_is=Y&birth_country=&birth_state=&college_id=&draft_year=", i, "&is_active=&debut_yr_nba_start=&debut_yr_nba_end=&debut_yr_aba_start=&debut_yr_aba_end=&is_hof=&is_as=&as_comp=gt&as_val=&award=&pos_is_g=Y&pos_is_gf=Y&pos_is_f=Y&pos_is_fg=Y&pos_is_fc=Y&pos_is_c=Y&pos_is_cf=Y&qual=&c1stat=&c1comp=gt&c1val=&c2stat=&c2comp=gt&c2val=&c3stat=&c3comp=gt&c3val=&c4stat=&c4comp=gt&c4val=&c5stat=&c5comp=gt&c6mult=1.0&c6stat=&order_by=vorp"))$stats
  temp <-  temp[-which(temp$To == "To"), ]
  temp$draftYear <- i
  temp <- temp %>% select(Player, VORP, G, GS, MP, draftYear)
  temp$VORP <- as.numeric(temp$VORP)
  temp$link <- ""
  
  for(j in 1:nrow(temp)) {
    print(j/nrow(temp))
    page <- readLines(paste0("http://www.basketball-reference.com/play-index/psl_finder.cgi?request=1&match=combined&type=totals&per_minute_base=36&per_poss_base=100&lg_id=NBA&is_playoffs=N&year_min=&year_max=&franch_id=&season_start=1&season_end=-1&age_min=0&age_max=99&height_min=0&height_max=99&shoot_hand=&birth_country_is=Y&birth_country=&birth_state=&college_id=&draft_year=", i, "&is_active=&debut_yr_nba_start=&debut_yr_nba_end=&debut_yr_aba_start=&debut_yr_aba_end=&is_hof=&is_as=&as_comp=gt&as_val=&award=&pos_is_g=Y&pos_is_gf=Y&pos_is_f=Y&pos_is_fg=Y&pos_is_fc=Y&pos_is_c=Y&pos_is_cf=Y&qual=&c1stat=&c1comp=gt&c1val=&c2stat=&c2comp=gt&c2val=&c3stat=&c3comp=gt&c3val=&c4stat=&c4comp=gt&c4val=&c5stat=&c5comp=gt&c6mult=1.0&c6stat=&order_by=vorp"))
    page <- page[grep(temp$Player[j], page)]
    page <- strsplit(page, "a href=\\\"")[[1]][2]
    page <- strsplit(page, ">")[[1]][1]
    page <- paste0("http://www.basketball-reference.com", gsub("\\\"", "", page))
    
    temp$link[j] <- page
  }
  
  player_stats <- rbind(player_stats, temp)
}

write.csv(player_stats, file = "player_stats.csv", row.names = F)


player_stats <- read.csv("player_stats.csv", header = T, stringsAsFactors = F)
euroPlayers <- data.frame()
collegePlayers <- data.frame()

for(i in 144:nrow(player_stats)) {
  print(paste0(i, ": ", player_stats$Player[i]))
  lines <- readLines(player_stats$link[i])
  
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
      
      seasons <- tryCatch(read_html(college) %>% html_nodes("table") %>% html_table() %>% .[[1]])
      print(seasons)
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
      college_stats <- cbind(college_stats, games)
      
    } else {
      
      seasons <- tryCatch(read_html(college) %>% html_nodes("table") %>% html_table() %>% .[[1]])
      print(seasons)
      w <- which(seasons$Season == "Career")
      if(length(w) == 0) {
        seasons <- length(which(seasons$Season != ""))
      } else {
        seasons <- w - 1
      }
      
      per_game <- get_College_PerGame(college, seasons)
      per_minute <- get_College_PerMinute(college, seasons)
      per_poss <- get_College_PerPoss(college, seasons)
      advanced <- get_College_Advanced(college, seasons)
      
      games <- per_game$g
      
      college_stats <- merge(merge(merge(per_game, per_minute, by = "season"), per_poss, by = "season"), advanced, by = "season")
      college_stats <- college_stats[, -c(grep(".x", colnames(college_stats)), grep(".y", colnames(college_stats)))]
      college_stats <- cbind(college_stats, games)
    }
    
    
    
    
    age <- 0
    if(month(birth) < 6) {
      years <- player_stats$draftYear[i] - year(birth)
      days <- as.numeric(difftime(paste0(player_stats$draftYear[i], "-06-01"), paste0(player_stats$draftYear[i], substr(birth, 5, 10))))
      age <- years + (days/365)
    } else {
      years <- player_stats$draftYear[i] - year(birth) - 1
      days <- as.numeric(difftime(paste0(player_stats$draftYear[i], substr(birth, 5, 10)), paste0(player_stats$draftYear[i], "-06-01")))
      age <- years + (days/365)
    }
    
    player <- data.frame(Player = player_stats$Player[i], DraftAge = age)
    
    if(isEuro) {
      euroPlayers <- rbind(euroPlayers, cbind(player, college_stats))
    } else {
      collegePlayers <- rbind(collegePlayers, cbind(player, college_stats))
    }
  }
}


collegePlayers <- merge(collegePlayers, player_stats, by="Player") %>% .[order(-.$VORP),]
euroPlayers <- merge(euroPlayers, player_stats, by="Player") %>% .[order(-.$VORP),]

write.csv(collegePlayers, file = "college_players.csv", row.names = F)
write.csv(euroPlayers, file = "euro_players.csv", row.names = F)

