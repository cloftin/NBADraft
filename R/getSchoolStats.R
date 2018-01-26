#' @export
getSchoolStats <- function() {
  
  cn <- dbConnect(RSQLite::SQLite(), "data/NBADraft.sqlite3")
  
  dat <- data.frame()
  for(i in 2002:2018) {
    print(i)
    table <- readLines(paste0("https://www.sports-reference.com/cbb/seasons/", i, "-school-stats.html"))
    table <- table[c(c(grep("</thead>", table)+2):length(table))]
    table <- table[grep("html", table)]
    table <- table[grep("school_name", table)]
    
    table <- plyr::ldply(table, function(x) {
      x <- strsplit(x, "data-stat=")[[1]][-c(1,2)]
      school <- data.frame(school = strsplit(strsplit(x[1], ".html\'>")[[1]][2], "<")[[1]][1])
      x <- x[-1]
      x <- lapply(x, function(x){
        stat <- strsplit(x, "\"")[[1]][2]
        value <- strsplit(strsplit(strsplit(x, "\"")[[1]][3], " >")[[1]][2], "<")[[1]][1]
        return(c(stat, value))
      })
      
      dat <- data.frame(matrix(unlist(lapply(x, tail, 1)), nrow=1, byrow=T))
      colnames(dat) <- unlist(lapply(x, head, 1))
      dat <- cbind(school, dat)
      return(dat)
    })
    
    # table <- table[, -c(9:ncol(table))]
    table <- table %>% select(school, srs, sos)
    colnames(table) <- c("School", "SchoolSRS", "SchoolSOS")
    table$School <- stri_trim(gsub(pattern = "\\*", "", unique(table$School)))
    table$School <- gsub("&amp;", "&", table$School)
    
    lines <- readLines(paste0("http://www.sports-reference.com/cbb/seasons/", i, "-school-stats.html")) 
    lines <- lines[grep("data-stat=\\\"school_name\\\" ><a href='/cbb/schools", lines)]
    
    
    links <- unlist(lapply(lines, function(x) strsplit(strsplit(x, "a href='")[[1]][2], "'>")[[1]][1]))
    
    links <- as.character(links[!is.na(links)])
    
    links <- paste0("http://www.sports-reference.com", links)
    
    table$school_link <- links
    
    table$School <- as.character(table$School)
    table$SchoolSRS <- as.numeric(as.character(table$SchoolSRS))
    table$SchoolSOS <- as.numeric(as.character(table$SchoolSOS))
    table$school_link <- as.character(table$school_link)
    table$Year <- i
    dat <- rbind(dat, table)
    
  }
  
  dbGetQuery(cn, "Drop Table SchoolStats")
  
  dbWriteTable(cn, "SchoolStats", dat, row.names = F)
  
}