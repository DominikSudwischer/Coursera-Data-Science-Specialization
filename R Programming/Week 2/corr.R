corr <- function(directory, treshold = 0)
{
  obs <- complete(directory)
  good.obs <- obs[obs$num.obs > treshold, "id"]
  cors <- NULL
  for (monitor in good.obs)
  {
    df <- read.table(paste(directory, "/",sprintf("%03d", monitor), ".csv", sep = ""), header=TRUE, sep=",",
                     colClasses = c("Date", "numeric", "numeric", "integer"))
    cors <- c(cors, cor(df$sulfate, df$nitrate, use = "complete.obs"))
  }
  if (!is.null(cors))
  {
    cors
  }
  else
  {
    vector("numeric")
  }
}