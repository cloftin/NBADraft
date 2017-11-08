#' @export
getDXData <- function() {
  
  cn <- dbConnect(RSQLite::SQLite(), "data/NBADraft.sqlite3")
  
  dat <- data.frame()
  for(i in 2002:2017) {
    print(i)
    present <- T
    page <- 0
    while(present) {
      t <- readHTMLTable(paste0("http://www.draftexpress.com/stats.php?q=eff&year=", i, "&league=NCAA&per=per40pace&qual=all&sort2=DESC&pos=all&stage=all&min=15&conference=All&pageno=", page, "&sort=6"), stringsAsFactors = F)
      if(length(t) > 0) {
        page <- page + 1
        t <- t[[2]]
        colnames(t) <- unlist(t[1,])
        t <- t[-1,]
        t$Year <- i
        dat <- rbind(dat, t)
      } else {
        present = F
      }
    }
  }
  dat$Cmp <- NULL
  
  for(i in 3:ncol(dat)) {
    dat[,i] <- as.numeric(dat[,i])
  }
  
  dat$Team <- gsub("St. Bonaventure", "Saint Bonaventure", dat$Team)
  dat$Team <- gsub("St. John's", "Saint John's", dat$Team)
  dat$Team <- gsub("St. Francis (NY)", "Saint Francis (NY)", dat$Team)

  dat$Team <- gsub("\\.$", "", dat$Team)
  dat$Team <- gsub(" St$", " State", dat$Team)
  dat$Team <- gsub("USC", "Southern California", dat$Team)
  # dat$Team <- gsub("St\\.", " State", dat$Team)
  dat$Team <- gsub("N\\.C\\.", "North Carolina", dat$Team)
  
  dat$Team <- gsub("Mt. St. Marys", "Mount St. Mary's", dat$Team)
  dat$Team <- gsub("Miami FL", "Miami (FL)", dat$Team)
  dat$Team <- gsub("Miami OH", "Miami (OH)", dat$Team)
  dat$Team <- gsub("LSU", "Louisiana State", dat$Team)
  
  colnames(dat) <- c("Name", "Team", "games", "min_g", "pts_g", "fga_g", "pts_play", "ts_pct", "efg_pct",
                     "fta_per_fga_pct", "fg3a_per_fga_pct", "ast_g", "ast_per_fga_pct", "ast_to_ratio",
                     "ppr", "blk_g", "stl_g", "pf_g", "Year")

  dbSendQuery(cn, "Drop table DXStats")
  dbWriteTable(cn, "DXStats", dat, row.names = F)
}