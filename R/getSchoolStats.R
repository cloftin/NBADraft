library(XML)
library(DBI)
library(dplyr)
library(stringi)

getSchoolStats <- function() {
  
  cn <- dbConnect(RSQLite::SQLite(), "NBADraft.sqlite3")
  
  dat <- data.frame()
  for(i in 2002:2017) {
    print(i)
    table <- readHTMLTable(paste0("http://www.sports-reference.com/cbb/seasons/", i, "-school-stats.html"), stringsAsFactors = F)$basic_school_stats
    table$Rk <- as.numeric(table$Rk)
    table <- table[complete.cases(table$Rk),]
    
    table <- table[, -c(9:ncol(table))]
    table <- table %>% select(School, SRS, SOS)
    colnames(table) <- c("School", "SchoolSRS", "SchoolSOS")
    table$School <- stri_trim(gsub(pattern = "\\*", "", unique(table$School)))
    
    lines <- readLines(paste0("http://www.sports-reference.com/cbb/seasons/", i, "-school-stats.html")) 
    lines <- lines[grep("data-stat=\\\"school_name\\\" ><a href='/cbb/schools", lines)]
    
    
    links <- unlist(lapply(lines, function(x) strsplit(strsplit(x, "a href='")[[1]][2], "'>")[[1]][1]))
    
    links <- as.character(links[!is.na(links)])
    
    links <- paste0("http://www.sports-reference.com", links)
    
    table$school_link <- links
    
    table$School <- as.character(table$School)
    table$SchoolSRS <- as.numeric(table$SchoolSRS)
    table$SchoolSOS <- as.numeric(table$SchoolSOS)
    table$school_link <- as.character(table$school_link)
    table$Year <- i
    dat <- rbind(dat, table)
    
  }
  
  dbGetQuery(cn, "Drop Table SchoolStats")
  
  dbWriteTable(cn, "SchoolStats", dat, row.names = F)
  
}