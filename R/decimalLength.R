
#' @export
decimalLength <- function(length) {
  length <- stri_trim(length)
  length <- gsub("\\\"", "", length)
  # length <- length[which(length != "NA")]
  feet <- as.numeric(unlist(lapply(strsplit(length, "-"), head, 1)))
  inches <- round(as.numeric(unlist(lapply(strsplit(length, "-"), tail, 1)))/12, 2)
  length <- feet + inches
  return(length)
}