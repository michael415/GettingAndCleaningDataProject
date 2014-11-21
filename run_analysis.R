# Download and unzip data set
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", dest="UCI_HAR_Dataset.zip", method="curl")
unzip("UCI_HAR_Dataset.zip")

# Change work directory to data set directory for shorter statements
setwd("UCI HAR Dataset")

# Step 1:
# Merge the training and test sets to create one data set

# Load training and test sets
subject_train <- read.table("train/subject_train.txt", sep="")
X_train <- read.table("train/X_train.txt", sep="")
y_train <- read.table("train/y_train.txt", sep="")
subject_test <- read.table("test/subject_test.txt", sep="")
X_test <- read.table("test/X_test.txt", sep="")
y_test <- read.table("test/y_test.txt", sep="")

X_combined <- rbind(X_train, X_test)
y_combined <- rbind(y_train, y_test)
subject_combined <- rbind(subject_train, subject_test)

# Step 2:
# Extract only the measurements on the mean and standard deviation for each measurement
features <- readLines("features.txt")
mean_std_deviation_cols <- c(grep("mean\\(\\)$", features), grep("std\\(\\)$", features))
X_mean_std_deviation <- X_combined[,mean_std_deviation_cols]

# Step 3:
# Appropriately label the data set with descriptive variable names
colnames <- character(length=ncol(X_combined))
for(i in 1:length(names(X_combined))) {
    colnames[i] <- substr(features[i], gregexpr(" ", features[i])[[1]][1]+1, nchar(features[i]))
}
filteredcolnames <- colnames[mean_std_deviation_cols]
colnames(X_mean_std_deviation) <- filteredcolnames

# Add activity labels as first row
X_mean_std_deviation <- cbind(activity=y_combined, X_mean_std_deviation)
colnames(X_mean_std_deviation)[1] <- "Activity"

# Step 4:
# Use descriptive activity names to name the activities in the data set
activity_labels <- read.table("activity_labels.txt")
X_mean_std_deviation$Activity <- as.factor(X_mean_std_deviation$Activity)
levels(X_mean_std_deviation$Activity) <- activity_labels[,2]

# Step 5:
# Create a second, independent tidy data set with the average of each variable
# for each activity and each subject

# First add "subject" column to data set
X_mean_std_deviation <- cbind(subject=subject_combined, X_mean_std_deviation)
colnames(X_mean_std_deviation)[1] <- "Subject"
X_mean_std_deviation[,1] <- as.factor(X_mean_std_deviation[,1])

# Then aggregate over each activity and subject
tidyset <- aggregate(. ~ Subject + Activity, X_mean_std_deviation, mean)

# Create directory for the tidy data set
dir.create("complete")

# Write it out
write.table(tidyset, file="complete/tidyset.txt")

