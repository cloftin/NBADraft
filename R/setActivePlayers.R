
#' @export
setActivePlayers <- function() {
  cn <- dbConnect(RSQLite::SQLite(), "data/NBADraft.sqlite3")
  
  players <- dbGetQuery(cn, "Select * from Players")
  
  loop <- T
  counter <- 0
  while (loop) {
    cat(paste0("\nStarting loop: ", counter + 1))
    t <- readLines(paste0(
      "https://www.basketball-reference.com/play-index/draft_finder.cgi?request=1&year_min=&year_max=&round_min=&round_max=&pick_overall_min=&pick_overall_max=&franch_id=&college_id=0&is_active=Y&is_hof=&pos_is_g=Y&pos_is_gf=Y&pos_is_f=Y&pos_is_fg=Y&pos_is_fc=Y&pos_is_c=Y&pos_is_cf=Y&c1stat=&c1comp=&c1val=&c2stat=&c2comp=&c2val=&c3stat=&c3comp=&c3val=&c4stat=&c4comp=&c4val=&order_by=year_id&order_by_asc=Y&offset=", counter * 100))
    p <- t[grep("th scope=\"row\"", t)]
    if(length(p) < 100) {
      loop <- F
    } else {
      counter <- counter + 1
    }
    
    for(i in 1:length(p)) {
      paste(loop, p, sep = ":")
      player <- strsplit(strsplit(strsplit(p[i], "a href=\"/players/")[[1]][2], ".html\\\"")[[1]][1], "/")[[1]][2]
      playerid <- players[grep(player, players$BRLink),]$PlayerId
      dbSendQuery(cn, paste0("Update Players Set Active = 1 Where PlayerId = ", playerid))
    }
  }
  
}