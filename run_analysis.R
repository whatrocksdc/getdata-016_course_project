## Loading additional libraries for data manipulation
library(data.table)
library(plyr)
library(dplyr)
library(tidyr)

## Read the features measured by the device into a data table
features <- read.table("UCI HAR Dataset/features.txt")

## Make each feature a unique value; there are duplicates
features_unique <- make.unique(as.character(features$V2))

## Modify the features to remove parentheses and substituting
## FUN (short for Function) in lieu of ()
## as () had presented problems to R during later filtering steps
features_groomed <- gsub("\\({1}\\){1}", "FUN", features$V2)

## Read in the activity type for each observation and add a descriptive label
y_test <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "activity_ID")
y_train <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "activity_ID")

## Read in the subject identifiers tied to each observation and add a descriptive label
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject_ID")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject_ID")

## Read raw data from observations into a data table, using re-named features
x_test <- as.data.table(read.table("UCI HAR Dataset/test/X_test.txt", col.names=features_groomed))
x_train <- as.data.table(read.table("UCI HAR Dataset/train/X_train.txt", col.names=features_groomed))

## Append activity and subject IDs to the left of the data table
## to properly assign each observation's features to a specific subject and activity
x_test_with_ID <- cbind(y_test$activity_ID,
                              subject_test$subject_ID, x_test)
x_train_with_ID <- cbind(y_train$activity_ID,
                               subject_train$subject_ID, x_train)

## Clean up columns with new names lost in the binding
## Note--I'm looking for ideas on how to do this better--feedback appreciated in comments
setnames(x_test_with_ID,
         old = c("V1", "V2"),
         new = c("activity_ID", "subject_ID"))
setnames(x_train_with_ID,
         old = c("V1", "V2"),
         new = c("activity_ID", "subject_ID"))

## Combine both sets of labeled and ID'd observerations
x_combined <- rbind(x_test_with_ID, x_train_with_ID)

## Select only those observations that recorded the output of the
## mean or standard deviation functions [mean() or std() in the original file]
## This does not include other functions with the string "mean" or "std" in them
x_groomed <- select(x_combined,
                    contains('subject_ID'),
                    contains('activity_ID'),
                    contains('meanFUN'),
                    contains('stdFUN'))

## Group and order by subject ID and then activity
## Applying the mean() function to each set of observations that matched every
## subject and activity pair
tidy <- summarise_each(group_by(x_groomed, subject_ID, activity_ID), funs(mean))

## Using input from the activties_label.txt file, give activities readable names
## Could have done this more elegantly, but I prefered better formatted names
tidy$activity_ID[tidy$activity_ID == 1] <- "Walking"
tidy$activity_ID[tidy$activity_ID == 2] <- "Walking Upstairs"
tidy$activity_ID[tidy$activity_ID == 3] <- "Walking Downstairs"
tidy$activity_ID[tidy$activity_ID == 4] <- "Sitting"
tidy$activity_ID[tidy$activity_ID == 5] <- "Standing"
tidy$activity_ID[tidy$activity_ID == 6] <- "Laying"

## Cleaning up column names now that our activity ID is now an activity name
setnames(tidy, old = "activity_ID", new = "Activity")

## Write output of tidy data to a tab-delimited file
write.table(tidy, file = "tidy_output.txt", row.names = FALSE, sep = "\t")
