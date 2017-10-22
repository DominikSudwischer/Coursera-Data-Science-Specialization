## Analysis file for the course project "Getting and Cleaning Data" on coursera
## Details on the processing steps can be found in README.txt

## Read files.
subject_test <- read.table("data/test/subject_test.txt", sep = "")
X_test <- read.table("data/test/X_test.txt", sep = "")
y_test <- read.table("data/test/y_test.txt", sep = "")
subject_train <- read.table("data/train/subject_train.txt", sep = "")
X_train <- read.table("data/train/X_train.txt", sep = "")
y_train <- read.table("data/train/y_train.txt", sep = "")

## Select features to extract based on the features.txt file.
## We are only interested in files about the mean and the standard deviation (std).
## Those always end with double parantheses ("()"). If only the word "mean" is contained,
## it will not be considered in this analysis.
## We extract the rows with "mean()" or "std()" in it using RegEx.
connection <- file("data/features.txt", open = "r")
features <- readLines(connection)
close.connection(connection)
selection.vector <- grep("mean\\(\\)|std\\(\\)", features, ignore.case = TRUE)
X_test <- X_test[, selection.vector]
X_train <- X_train[, selection.vector]

## Merge X, y and the subjects
df_test <- cbind(X_test, subject_test, y_test)
df_train <- cbind(X_train, subject_train, y_train)

## Concatenate the dataframes
df <- rbind(df_test, df_train)

## We are generating some useful descriptions for variable names now.
## First, we trim the leading row number and the following whitespace.
features <- gsub("[0-9]+ ", "", features[selection.vector])
## According to the documentation, the leading "t" abbreviates "time", "f" corresponds to "frequency",
## Acc" means "Acceleration", "Gyro" is "Gyroscope" and "Mag" stands for "magnitude".
features <- sub("^t", "time.", features)
features <- sub("^f", "frequency.", features)
features <- sub("Acc", ".acceleration.", features)
features <- sub("Gyro", ".gyroscope.", features)
features <- sub("Mag$|Mag-", "magnitude.", features)
features <- gsub("-", ".", features)
features <- gsub("\\.\\.", "\\.", features)
features <- tolower(features)
names(df) <- append(append(features, "subject"), "activity")

## Last, we will simply replace the activity ids in the column "activity" by the activity name.
connection <- file("data/activity_labels.txt", open = "r")
activity.names <- readLines(connection)
close.connection(connection)
mat <- matrix(unlist(strsplit(activity.names, split = " ")), nrow = 6, byrow = TRUE)
df$activity <- mat[df$activity, 2]

## For the second part of the assignment, we will use the "dplyr" package. We need to group the 
## observations by subject and activity and calculate the mean for each single observation.
library(dplyr)
mean.df <- df %>% group_by(activity,subject) %>% summarise_all(mean)
names(mean.df)[3:length(names(mean.df))] <- paste0("mean.of.", names(mean.df)[3:length(names(mean.df))])

write.table(mean.df, file = "tidy_means.txt", row.names = FALSE)
write(names(mean.df), file = "feature_names.txt")