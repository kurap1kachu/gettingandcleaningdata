library(data.table)
library(dplyr)
file_Url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
file_path = "/Users/jerryyu/Desktop/R_coursera/getting and cleaning data/projectdata.zip"
zipFile ="projectdata.zip"

download.file(file_Url, file_path, mode = "wb")

# unzip zip file containing data if data directory doesn't already exist
datapath <- "UCI HAR Dataset"
if (!file.exists(dataPath)) {
        unzip(zipFile)
}

# checking files in datapath
# files = list.files(datapath, recursive = TRUE)
# files

xtrain <- read.table(file.path(datapath, "train", "X_train.txt"), header = FALSE)
ytrain <- read.table(file.path(datapath, "train", "y_train.txt"), header = FALSE)
subtrain <- read.table(file.path(datapath, "train", "subject_train.txt"), header = FALSE)

xtest <- read.table(file.path(datapath, "test", "X_test.txt"), header = FALSE)
ytest <- read.table(file.path(datapath, "test", "y_test.txt"), header = FALSE)
subtest <- read.table(file.path(datapath, "test", "subject_test.txt"), header = FALSE)

features <- read.table(file.path(datapath, "features.txt"), header = FALSE)

activity <- read.table(file.path(datapath, "activity_labels.txt"), header = FALSE)

# Create Sanity and Column Values to the Train Data
colnames(xtrain) = features[,2]
colnames(ytrain) = "activityId"
colnames(subtrain) = "subjectId"

# Create Sanity and column values to the test data
colnames(xtest) = features[,2]
colnames(ytest) = "activityId"
colnames(subtest) = "subjectId"

# Create sanity check for the activity labels value
colnames(activity) <- c('activityId','activityType')

# merging data 
alltrain <- cbind(ytrain, subtrain, xtrain)
alltest <- cbind(ytest, subtest, xtest)
finaldata <- rbind(alltrain, alltest)

colNames <- colnames(finaldata)

mean_n_std <- (grepl("activityId", colNames) |
                      grepl("subjectId", colNames)|
                      grepl("mean..", colNames)|
                      grepl("std", colNames)
               )

mean_std <- finaldata[, mean_n_std == TRUE]

# replacing activity values with named factor level
with_Activityname <- merge(mean_std, activity, by = "activityId", all.x = TRUE)
with_Activityname$activityId <- factor(with_Activityname$activityId, level = activity[,1], labels = activity[, 2])

# group by subject and activity and summarise using mean
activity_mean <- with_Activityname %>%
        group_by(subjectId, activityId) %>%
        summarize_each(funs(mean))

# output to file "tidydata.txt"
write.table(activity_mean, "tidydata.txt", row.names = FALSE)
