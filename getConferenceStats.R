library(XML)
library(DBI)
library(dplyr)

getConferenceStats <- function() {
  
  cn <- dbConnect(RSQLite::SQLite(), "NBADraft.sqlite3")
  
  lines <- readLines("http://www.sports-reference.com/cbb/conferences/") 
  lines <- lines[grep("conf_name", lines)]
  lines <- lines[c(2:length(lines))]
  
  links <- unlist(lapply(lines, function(x) strsplit(strsplit(x, "a href=\\\"")[[1]][2], "\\\">")[[1]][1]))
  names <- unlist(lapply(lines, function(x) strsplit(strsplit(strsplit(x, "a href=\\\"")[[1]][2], "\\\">")[[1]][2], "</a>")[[1]][1]))
  
  links <- as.character(links[!is.na(links)])
  links <- paste0("http://www.sports-reference.com", links)
  names <- as.character(names[!is.na(names)])
  
  conferences <- data.frame(matrix(ncol = 0, nrow = length(names)))
  conferences$Conference <- names
  conferences$Link <- links
  
  dat <- data.frame()
  for(i in 1:nrow(conferences)) {
    
    table <- data.frame(readHTMLTable(conferences$Link[i], stringsAsFactors = F))
    table <- table[,c(2, 3, 7, 8)]
    
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
  dat$NumSchools <- as.integer(dat$NumSchools)
  dat$ConfSRS <- as.numeric(dat$ConfSRS)
  dat$ConfSOS <- as.numeric(dat$ConfSOS)
  dat$ConfLink <- as.character(dat$ConfLink)
  
  dbGetQuery(cn, "Drop Table ConferenceStats")
  
  dbWriteTable(cn, "ConferenceStats", dat)
  
}