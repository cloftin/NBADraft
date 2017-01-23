seasonWeighting <- function(dat) {
  frequencies <- c(2, 3, 5, 6)
  if(nrow(dat) > 4) {
    dat <- dat[-which(dat$games == min(dat$games)),]
  }
  dat$freq <- frequencies[c(1:nrow(dat))]
  dat <- dat[rep(row.names(dat), dat$freq), ]
  dat <- as.data.frame(cbind(dat$Player[1], rbind(colMeans(dat[, c(5:(ncol(dat) - 1))]))))
  colnames(dat)[1] <- "Player"
  dat$Player <- as.character(dat$Player)
  for(i in 2:ncol(dat)) {
    dat[,i] <- as.numeric(as.character(dat[,i]))
  }
  return(dat)
}