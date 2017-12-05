insertRow2 <- function(existingDF, newrow, r) {
  existingDF <- rbind(existingDF,newrow)
  existingDF <- existingDF[order(c(1:(nrow(existingDF)-1),r-0.5)),]
  row.names(existingDF) <- 1:nrow(existingDF)
  return(existingDF)
}