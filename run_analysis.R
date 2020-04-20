
library(data.table)
library(dplyr)


# reading the datas

features <- read.table("UCI HAR Dataset/features.txt", col.names = c("n","functions"))
activities <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
x_test <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
y_test <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "code")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
x_train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$functions)
y_train <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "code")


##########################################################################################
# merging datas - Merges the training and the test sets to create one data set

str(x_test)
str(x_train)


str(y_test)
str(y_train)

X <- rbind(x_train, x_test)
Y <- rbind(y_train, y_test)
Subject <- rbind(subject_train, subject_test)
Merged_Data <- cbind(Subject, Y, X)


##########################################################################################
# Extracts only the measurements on the mean and standard deviation for each measurement.


extract1 <- names(Merged_Data)
extract1 

extract_mean_sd <- grepl("subject|code|mean|std", extract1, ignore.case=TRUE)

sum(extract_mean_sd)
extract_mean_sd


extract_data1 = Merged_Data[,extract_mean_sd]

str(extract_data1)


##########################################################################################
# Uses descriptive activity names to name the activities in the data set

extract_data1$code <- activities[extract_data1$code, 2]



##########################################################################################
# Appropriately labels the data set with descriptive variable names.

names_data1<- names(extract_data1)

setnames(extract_data1, "code", "activity")

names(extract_data1[,1]) <- 'subject'
names(extract_data1[,2]) <- 'activity'
names(extract_data1)  <- gsub("[[:punct:][:blank:]]","",  names(extract_data1))
names(extract_data1) <- gsub("mean", " (Mean) ", names(extract_data1))
names(extract_data1) <- gsub("std", " (STD) ", names(extract_data1))



##########################################################################################
#  From the data set in step 4, creates a second, independent tidy data set with the average
#  of each variable for each activity and each subject.


str(extract_data1)

FinalData <- extract_data1 %>%
  group_by(subject, activity) %>%
  summarise_all(funs(mean))

write.table(FinalData, "FinalData.txt", row.name=FALSE)















