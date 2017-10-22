pollutantmean <- function(directory, pollutant, id = 1:332)
{
  df <- NULL
  for (monitor in id)
  {
    df <- rbind(df, read.table(paste(directory, "/",sprintf("%03d", monitor), ".csv", sep = ""), header=TRUE, sep=","))
  }
  mean(df[, pollutant], na.rm = TRUE)
}