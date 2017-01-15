get_Euro_PerGame <- function(link, seasons) {
  
  lines <- readLines(link)
  start <- grep("Per Game Table", lines)
  end <- grep("/table", lines)
  euro_stats <- lines[c(start:end[end > start][1])]
  
  columns <- euro_stats[grep("aria-label=", euro_stats)]
  columns <- unlist(lapply(strsplit(columns, "data-stat=\\\""), tail, 1))
  columns <- unlist(lapply(strsplit(columns, "\\\" scope"), head, 1))
  columns <- columns[-c(2)]
  
  stats <- euro_stats[grep("full_table", euro_stats)]
  stats <- lapply(strsplit(stats, "data-stat=\\\""), function(x) strsplit(x, split = "\\\" >"))
  stats <- unlist(stats)
  
  season <- stats[grep("years", stats)]
  season <- strsplit(season, ">") %>% lapply(., function(x) x[2]) %>% unlist()
  season <- strsplit(season, "<") %>% lapply(., function(x) x[1]) %>% unlist()
  
  stats <- stats[-grep("a href", stats)]
  stats <- stats[seq(2, length(stats), by = 2)]
  stats <- stats[-c(1,2)]
  if(length(grep("scope|lg_name_short", stats)) > 0) {
    stats <- stats[-grep("scope|lg_name_short", stats)]
  }
  stats <- unlist(lapply(strsplit(stats, "<"), function(x) head(x, 1)))
  stats <- data.frame(matrix(as.numeric(stats), nrow = seasons, byrow=T))
  stats <- cbind(season, stats)
  
  colnames(stats) <- columns
  
  return(stats)
}

get_Euro_PerMinute <- function(link, seasons) {
  
  lines <- readLines(link)
  start <- grep("Per 36 Minutes", lines)[1]
  end <- grep("/table", lines)
  euro_stats <- lines[c(start:end[end > start][1])]
  
  columns <- euro_stats[grep("aria-label=", euro_stats)]
  columns <- unlist(lapply(strsplit(columns, "data-stat=\\\""), tail, 1))
  columns <- unlist(lapply(strsplit(columns, "\\\" scope"), head, 1))
  columns <- columns[-c(2)]
  
  stats <- euro_stats[grep("full_table", euro_stats)]
  stats <- lapply(strsplit(stats, "data-stat=\\\""), function(x) strsplit(x, split = "\\\" >"))
  stats <- unlist(stats)
  
  season <- stats[grep("years", stats)]
  season <- strsplit(season, ">") %>% lapply(., function(x) x[2]) %>% unlist()
  season <- strsplit(season, "<") %>% lapply(., function(x) x[1]) %>% unlist()
  
  stats <- stats[-grep("a href", stats)]
  stats <- stats[seq(6, length(stats), by = 2)]
  if(length(grep("scope|lg_name_short", stats)) > 0) {
    stats <- stats[-grep("scope|lg_name_short", stats)]
  }
  stats <- unlist(lapply(strsplit(stats, "<"), function(x) head(x, 1)))
  stats <- data.frame(matrix(as.numeric(stats), nrow = seasons, byrow=T))
  stats <- cbind(season, stats)
  
  colnames(stats) <- columns
  
  return(stats)
}

get_College_PerGame <- function(link, seasons) {
  
  lines <- readLines(link)
  start <- grep("Per Game Table", lines)[1]
  end <- grep("/table", lines)
  poss <- lines[c(start:end[end > start][1])]
  
  columns <- poss[grep("aria-label=", poss)]
  columns <- unlist(lapply(strsplit(columns, "data-stat=\\\""), tail, 1))
  columns <- unlist(lapply(strsplit(columns, "\\\" scope"), head, 1))
  columns <- columns[-c(2:3)]
  if(length(grep("zzz|awards", columns)) > 0) {
    columns <- columns[-grep("zzz|awards", columns)]
  }
  
  stats <- poss[grep("tr id=\\\"players_per_game", poss)]
  stats <- lapply(strsplit(stats, "data-stat=\\\""), function(x) strsplit(x, split = "\\\" >"))
  stats <- unlist(stats)
  
  season <- stats[grep("seasons", stats)]
  season <- strsplit(season, ">") %>% lapply(., function(x) x[2]) %>% unlist()
  season <- strsplit(season, "<") %>% lapply(., function(x) x[1]) %>% unlist()
  
  school_link <- unlist(lapply(stats[grep("cbb/schools", stats)], function(x) strsplit(x, "href=\\\"")[[1]][2]))
  school_link <- unlist(lapply(school_link, function(x) strsplit(x, "\\\"")[[1]][1]))
  
  conf_link <- unlist(lapply(stats[grep("conferences", stats)], function(x) strsplit(x, "href=\\\"")[[1]][2]))
  conf_link <- unlist(lapply(conf_link, function(x) strsplit(x, "\\\"")[[1]][1]))
  
  stats <- stats[-grep("a href|full_table|season|age|team_id|lg_id|pos|school_name|conf_abbr|left|players_per_game|zzz", stats)]
  t <- lapply(which(stats == "g"), function(x) seq(x + 1, length(stats), by = 2))
  selection <- c()
  for(j in 1:length(t)) {
    selection <- c(selection, t[[j]][which(t[[j]] <= (which(stats == "pts_per_g")[j] + 1))])
  }
  stats <- stats[selection]
  stats <- unlist(lapply(strsplit(stats, "<"), function(x) head(x, 1)))
  stats <- data.frame(matrix(as.numeric(stats), nrow = seasons, byrow=T))
  stats <- cbind(season, stats)
  
  colnames(stats) <- columns
  
  stats <- cbind(stats, school_link, conf_link)
  
  return(stats)
}

get_College_PerMinute <- function(link, seasons) {
  
  lines <- readLines(link)
  start <- grep("Per 40 Minutes Table", lines)[1]
  end <- grep("/table", lines)
  poss <- lines[c(start:end[end > start][1])]
  
  columns <- poss[grep("aria-label=", poss)]
  columns <- unlist(lapply(strsplit(columns, "data-stat=\\\""), tail, 1))
  columns <- unlist(lapply(strsplit(columns, "\\\" scope"), head, 1))
  columns <- columns[-c(2:3)]
  
  stats <- poss[grep("tr id=\\\"players_per_min", poss)]
  stats <- lapply(strsplit(stats, "data-stat=\\\""), function(x) strsplit(x, split = "\\\" >"))
  stats <- unlist(stats)
  
  season <- stats[grep("seasons", stats)]
  season <- strsplit(season, ">") %>% lapply(., function(x) x[2]) %>% unlist()
  season <- strsplit(season, "<") %>% lapply(., function(x) x[1]) %>% unlist()
  
  school_link <- unlist(lapply(stats[grep("cbb/schools", stats)], function(x) strsplit(x, "href=\\\"")[[1]][2]))
  school_link <- unlist(lapply(school_link, function(x) strsplit(x, "\\\"")[[1]][1]))
  
  conf_link <- unlist(lapply(stats[grep("conferences", stats)], function(x) strsplit(x, "href=\\\"")[[1]][2]))
  conf_link <- unlist(lapply(conf_link, function(x) strsplit(x, "\\\"")[[1]][1]))
  
  stats <- stats[-grep("a href|full_table|season|age|team_id|lg_id|pos|school_name|conf_abbr|left|players_per_min|tr_id", stats)]
  stats <- stats[seq(2, length(stats), by = 2)]
  stats <- unlist(lapply(strsplit(stats, "<"), function(x) head(x, 1)))
  stats <- data.frame(matrix(as.numeric(stats), nrow = seasons, byrow=T))
  stats <- cbind(season, stats)
  
  colnames(stats) <- columns
  
  stats <- cbind(stats, school_link, conf_link)
  
  # Adjust Per-40 to Per-36
  for(i in c(3,4,6,7,9,10,12,13,15:21)) {
    stats[,i] <- stats[,i] * .9
  }
  
  return(stats)
}

get_College_PerPoss <- function(link, seasons) {
  
  lines <- readLines(link)
  start <- grep("Per 100 Poss Table", lines)[1]
  end <- grep("/table", lines)
  poss <- lines[c(start:end[end > start][1])]
  
  columns <- poss[grep("aria-label=", poss)]
  columns <- unlist(lapply(strsplit(columns, "data-stat=\\\""), tail, 1))
  columns <- unlist(lapply(strsplit(columns, "\\\" scope"), head, 1))
  columns <- columns[-c(2:3)]
  
  stats <- poss[grep("players_per_poss", poss)]
  stats <- lapply(strsplit(stats, "data-stat=\\\""), function(x) strsplit(x, split = "\\\" >"))
  stats <- unlist(stats)
  
  season <- stats[grep("seasons", stats)]
  season <- strsplit(season, ">") %>% lapply(., function(x) x[2]) %>% unlist()
  season <- strsplit(season, "<") %>% lapply(., function(x) x[1]) %>% unlist()
  
  school_link <- unlist(lapply(stats[grep("cbb/schools", stats)], function(x) strsplit(x, "href=\\\"")[[1]][2]))
  school_link <- unlist(lapply(school_link, function(x) strsplit(x, "\\\"")[[1]][1]))
  
  conf_link <- unlist(lapply(stats[grep("conferences", stats)], function(x) strsplit(x, "href=\\\"")[[1]][2]))
  conf_link <- unlist(lapply(conf_link, function(x) strsplit(x, "\\\"")[[1]][1]))
  
  stats <- stats[-grep("a href|full_table|season|age|team_id|lg_id|school_name|conf_abbr|left|tr_id|tr id", stats)]
  stats <- stats[seq(3, length(stats), by = 2)]
  stats <- unlist(lapply(strsplit(stats, "<"), function(x) head(x, 1)))
  stats <- data.frame(matrix(as.numeric(stats), nrow = seasons, byrow=T))
  stats <- cbind(season, stats)
  
  colnames(stats) <- columns
  
  stats <- cbind(stats, school_link, conf_link)
  
  stats$Xxx <- NULL
  
  return(stats)
}

get_College_Advanced <- function(link, seasons) {
  
  lines <- readLines(link)
  start <- grep("Advanced Table", lines)[1]
  end <- grep("/table", lines)
  poss <- lines[c(start:end[end > start][1])]
  
  columns <- poss[grep("aria-label=", poss)]
  columns <- unlist(lapply(strsplit(columns, "data-stat=\\\""), tail, 1))
  columns <- unlist(lapply(strsplit(columns, "\\\" scope"), head, 1))
  columns <- columns[-c(2:3)]
  
  stats <- poss[grep("players_advanced", poss)]
  stats <- lapply(strsplit(stats, "data-stat=\\\""), function(x) strsplit(x, split = "\\\" >"))
  stats <- unlist(stats)
  
  season <- stats[grep("seasons", stats)]
  season <- strsplit(season, ">") %>% lapply(., function(x) x[2]) %>% unlist()
  season <- strsplit(season, "<") %>% lapply(., function(x) x[1]) %>% unlist()
  
  school_link <- unlist(lapply(stats[grep("cbb/schools", stats)], function(x) strsplit(x, "href=\\\"")[[1]][2]))
  school_link <- unlist(lapply(school_link, function(x) strsplit(x, "\\\"")[[1]][1]))
  
  conf_link <- unlist(lapply(stats[grep("conferences", stats)], function(x) strsplit(x, "href=\\\"")[[1]][2]))
  conf_link <- unlist(lapply(conf_link, function(x) strsplit(x, "\\\"")[[1]][1]))
  
  stats <- stats[-grep("a href|full_table|season|age|team_id|lg_id|pos|school_name|conf_abbr|left|players_advanced|tr_id", stats)]
  stats <- stats[seq(2, length(stats), by = 2)]
  stats <- unlist(lapply(strsplit(stats, "<"), function(x) head(x, 1)))
  stats <- data.frame(matrix(as.numeric(stats), nrow = seasons, byrow=T))
  stats <- cbind(season, stats)
  
  colnames(stats) <- columns
  
  stats <- cbind(stats, school_link, conf_link)
  
  stats$Xxx <- NULL
  stats$Yyy <- NULL
  
  return(stats)
}