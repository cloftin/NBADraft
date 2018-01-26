#' @export
getConferenceStats <- function() {
  
  cn <- dbConnect(RSQLite::SQLite(), "data/NBADraft.sqlite3")
  
  lines <- readLines("http://www.sports-reference.com/cbb/conferences/") 
  lines <- lines[grep("conf_name", lines)]
  lines <- lines[c(2:length(lines))]
  
  links <- unlist(lapply(lines, function(x) strsplit(strsplit(x, "a href=\\\"")[[1]][2], "\\\">")[[1]][1]))
  names <- unlist(lapply(lines, function(x) strsplit(strsplit(strsplit(x, "a href=\\\"")[[1]][2], "\\\">")[[1]][2], "</a>")[[1]][1]))
  
  links <- as.character(links[!is.na(links)])
  links <- paste0("https://www.sports-reference.com", links)
  names <- as.character(names[!is.na(names)])
  
  conferences <- data.frame(matrix(ncol = 0, nrow = length(names)))
  conferences$Conference <- names
  conferences$Link <- links
  
  dat <- data.frame()
  for(i in 1:nrow(conferences)) {
    
    table <- readLines(conferences$Link[i])
    table <- table[grep("html", table)]
    table <- table[grep("ranker", table)]
    
    table <- plyr::ldply(table, function(x) {
      x <- strsplit(x, "data-stat=")[[1]][-c(1,2)]
      season <- data.frame(season = strsplit(strsplit(x[1], ".html\\\">")[[1]][2], "<")[[1]][1])
      x <- x[-1]
      x <- lapply(x, function(x){
        stat <- strsplit(x, "\"")[[1]][2]
        value <- strsplit(strsplit(strsplit(x, "\"")[[1]][3], " >")[[1]][2], "<")[[1]][1]
        return(c(stat, value))
      })
      
      dat <- data.frame(matrix(unlist(lapply(x, tail, 1)), nrow=1, byrow=T))
      colnames(dat) <- unlist(lapply(x, head, 1))
      dat <- cbind(season, dat)
      return(dat)
    }) %>% select(season, school_count, srs, sos)

    confLines <- readLines(conferences$Link[i])
    confLines <- confLines[grep("data-stat=\\\"season\\\"", confLines)]
    confLines <- confLines[c(2:length(confLines))]
    confLines <- unlist(lapply(confLines, function(x) strsplit(strsplit(x, "data-stat=\\\"season\\\" ><a href=\\\"")[[1]][2], "\\\"")[[1]][1]))
    confLines <- paste0("http://www.sports-reference.com", confLines)
    
    table <- cbind(table, confLines)
    colnames(table) <- c("Season", "NumSchools", "ConfSRS", "ConfSOS", "ConfLink")
    
    dat <- rbind(dat, table)
    
  }
  
  dat$Season <- as.character(dat$Season)
  dat$NumSchools <- as.integer(as.character(dat$NumSchools))
  dat$ConfSRS <- as.numeric(as.character(dat$ConfSRS))
  dat$ConfSOS <- as.numeric(as.character(dat$ConfSOS))
  dat$ConfLink <- as.character(dat$ConfLink)
  
  dbGetQuery(cn, "Drop Table ConferenceStats")
  
  dbWriteTable(cn, "ConferenceStats", dat)
  
}