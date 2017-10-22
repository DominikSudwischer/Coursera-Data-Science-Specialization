get.by.rank <- function(v, rank)
{
  if (rank == "best") { v[1] }
  else if (rank == "worst") { v[length(v)] }
  else if (rank > length(v)) { NA }
  else { v[rank] }
}

rankall <- function(outcome, rank = "best")
{
  df <- read.table("outcome-of-care-measures.csv", sep = ",", colClasses = "character", header = TRUE)
  df <- df[, c(2, 7, 11, 17, 23)]
  colnames(df) = c("Hospital.Name", "State", "Heart.Attack", "Heart.Failure", "Pneumonia")
  if (outcome == "heart attack")
  {
    df <- df[, c(1, 2, 3)]
  }
  else if (outcome == "heart failure")
  {
    df <- df[, c(1, 2, 4)]
  }
  else if (outcome == "pneumonia")
  {
    df <- df[, c(1, 2, 5)]
  }
  else
  {
    stop("invalid outcome")
  }
  colnames(df) <- c("Hospital.Name", "State", "Outcome")
  df$Outcome <- as.numeric(df$Outcome)
  df <- df[!is.na(df$Outcome), ]
  df <- df[order(df$State, df$Outcome, df$Hospital.Name), ]
  result <- NULL
  states <- unique(df$State)
  for (state in states)
  {
    result <- c(result, get.by.rank(df[df$State == state, 1], rank))
    names(result)[length(result)] <- state
  }
  result <- data.frame(result, states)
  colnames(result) <- c("hospital", "state")
  invisible(result)
}