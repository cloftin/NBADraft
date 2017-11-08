#' @export
seasonWeighting <- function(dat, invert = F, current = F) {
  colStart <- which(colnames(dat) == "DraftAge")
  if(current) {
    colStart <- 4
  }
  if(invert) {
    frequencies <- c(6, 5, 3, 2)
  } else {
    frequencies <- c(2, 3, 5, 6)
  }
  if(nrow(dat) > 4) {
    dat <- dat[-which(dat$games == min(dat$games)),]
  }
  dat$freq <- frequencies[c(1:nrow(dat))]
  dat <- dat[rep(row.names(dat), dat$freq), ]
  dat <- as.data.frame(cbind(dat[1,c(1:(colStart - 1))], rbind(colMeans(dat[, c(colStart:(ncol(dat) - 1))], na.rm = T))))
  return(dat)
}