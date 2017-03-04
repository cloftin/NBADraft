library(XML)
library(DBI)

dxRankings <- function() {
  
  dat <- data.frame()
  for(i in 2007:2017) {
    if(i < 2016) {
      t <- readHTMLTable(paste0("http://www.draftexpress.com/nba-mock-draft/", i, "/list/"), stringsAsFactors = F)[[3]]$V3
    } else {
      t <- c(readHTMLTable(paste0("http://www.draftexpress.com/nba-mock-draft/", i, "/list/"), stringsAsFactors = F)[[4]]$V2,
             readHTMLTable(paste0("http://www.draftexpress.com/nba-mock-draft/", i, "/list/"), stringsAsFactors = F)[[5]]$V2)
    }
    t <- t[which(!is.na(t))]
    t <- unlist(lapply(t, function(x) {strsplit(x, " Â")[[1]][1]}))
    rankings <- data.frame(Player = t, Rank = c(1:60), Year = i)
    dat <- rbind(dat, rankings)
  }
  dat$Player <- as.character(dat$Player)
  
  cn <- dbConnect(RSQLite::SQLite(), "NBADraft.sqlite3")
  dbSendQuery(cn, "Drop Table DXRankings")
  dbWriteTable(cn, "DXRankings", dat)
}