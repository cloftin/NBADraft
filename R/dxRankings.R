
#' @export
dxRankings <- function() {
  
  dat <- data.frame()
  for(i in 2007:2017) {
    t <- as.data.frame(readHTMLTable(paste0("http://www.draftexpress.com/nba-mock-draft/", i, "/#list"), stringsAsFactors = F)[[1]])
    t$`#` <- NULL
    t$`+/-` <- NULL
    t$CO <- NULL
    t$Rank <- c(1:nrow(t))
    t$Year <- i
    t <- dplyr::select(t, Player, Rank, Year, Pos, HT, WT, WS)
    dat <- rbind(dat, t)
  }
  dat$Player <- as.character(dat$Player)
  
  cn <- dbConnect(RSQLite::SQLite(), "data/NBADraft.sqlite3")
  dbSendQuery(cn, "Drop Table DXRankings")
  dbWriteTable(cn, "DXRankings", dat)
}