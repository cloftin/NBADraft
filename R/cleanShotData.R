
#' @export
cleanShotData <- function() {
  
  cn <- dbConnect(RSQLite::SQLite(), "data/NBADraft.sqlite3")
  
  dat <- plyr::ldply(list.files("data/shot_data/", patter = ".csv"), function(fname) {
    
    t <- read.csv(paste0("shot_data/", fname), header = T, stringsAsFactors = F)
    colnames(t) <- c("Team", "Player", "FGA", "TS", "eFG", "ShotsAtRim", "FGPerAtRim", "PerAssistedAtRim", 
                     "PerShotsAs2PtJ", "FGPerOn2PtJ", "PerAssisted2PtJ", "PerShots3PtJ", "FGPerOn3PtJ",
                     "PerAssisted3PtJ", "FTAPerFGA", "FTPer")
    t$`NA` <- NULL
    
    t <- as.matrix(t)
    t[t == "---"] <- NA
    t <- as.data.frame(t)
    t[] <- lapply(t, as.character)
    t$FGA <- as.numeric(t$FGA)
    t$TS <- as.numeric(t$TS)
    
    for(i in 5:ncol(t)) {
      t[,i] <- as.numeric(gsub("%", "", t[,i]))/100
    }
    t <- t %>% filter(Player != "Total")
    
    t$Player <- paste(unlist(lapply(strsplit(t$Player, " "), function(x){tail(x,1)})),
                      unlist(lapply(strsplit(t$Player, " "), function(x) {
                        paste(x[c(1:length(x) - 1)], collapse = " ")
                      })), sep = " ")
    
    t %>% filter(substr(t$Player, nchar(t$Player), nchar(t$Player)) != " ")
    
    t$Year <- as.integer(substr(fname, 1, 4))
    return(t)
    
  })
  
  
  dbGetQuery(cn, "Drop Table ShotData")
  
  dbWriteTable(cn, "ShotData", dat)
  
}