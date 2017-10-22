complete <- function(directory, id = 1:332)
{
  num.obs <- numeric(length(id))
  names(num.obs) <- as.character(id)
  for (monitor in id)
  {
    cur.df <- read.table(paste(directory, "/",sprintf("%03d", monitor), ".csv", sep = ""), header=TRUE, sep=",",
                         colClasses = c("Date", "numeric", "numeric", "integer"))
    num.obs[as.character(monitor)] <- sum(complete.cases(cur.df))
  }
  data.frame(id, num.obs)
}