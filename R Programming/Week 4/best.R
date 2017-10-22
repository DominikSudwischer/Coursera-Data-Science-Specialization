best <- function(state, outcome)
{
  df <- read.table("outcome-of-care-measures.csv", sep = ",", colClasses = "character", header = TRUE)
  df <- df[df$State == state, c(2, 11, 17, 23)]
  colnames(df) = c("Hospital.Name", "Heart.Attack", "Heart.Failure", "Pneumonia")
  if (nrow(df) <= 0)
  {
    stop("invalid state")
  }
  if (outcome == "heart attack")
  {
    df <- df[, c(1, 2)]
  }
  else if (outcome == "heart failure")
  {
    df <- df[, c(1, 3)]
  }
  else if (outcome == "pneumonia")
  {
    df <- df[, c(1, 4)]
  }
  else
  {
    stop("invalid outcome")
  }
  colnames(df) <- c("Hospital.Name", "Outcome")
  df$Outcome <- as.numeric(df$Outcome)
  df <- df[!is.na(df$Outcome), ]
  df <- df[order(df$Outcome, df$Hospital.Name), ]
  print(df[1, 1])
}