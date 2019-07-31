#File run_analysis.R

#loading libraries
library(dplyr)

#download dataset
if(!file.exists("./ProjectData")){
  dir.create("./ProjectData")
}

fileUrl<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile="./projectData/projectDataset.zip",mode = "wb")

#extracting data from zip file
if (!file.exists("UCI HAR Dataset")) { 
  unzip("./ProjectData/projectDataset.zip",exdir = "./ProjectData")
}

#loading data

#read features and activity lables
features<-read.table("./ProjectData/UCI HAR Dataset/features.txt")
activities<-read.table("./ProjectData/UCI HAR Dataset/activity_labels.txt")

#read test data
x_test<-read.table("./ProjectData/UCI HAR Dataset/test/x_test.txt")
y_test<-read.table("./ProjectData/UCI HAR Dataset/test/y_test.txt")
subject_test<-read.table("./ProjectData/UCI HAR Dataset/test/subject_test.txt")

#read training data
x_train<-read.table("./ProjectData/UCI HAR Dataset/train/X_train.txt")
y_train<-read.table("./ProjectData/UCI HAR Dataset/train/y_train.txt")
subject_train<-read.table("./ProjectData/UCI HAR Dataset/train/subject_train.txt")

#--------------------------------------------------------------------------------------

#1.Merge Training and Test datasets to create one dataset
subject_combined<-rbind(subject_train,subject_test)
y_combined<-rbind(y_train,y_test)
x_combined<-rbind(x_train,x_test)

#setting column names before column merge
colnames(subject_combined)<-"Subject"
colnames(y_combined)<-"Activity"
colnames(x_combined)<-t(features[2])

#final merge of data sets
complete_data<-cbind(subject_combined,y_combined,x_combined)

#2.Extract only the measurements on the mean and standard deviation for each measurement
mean_std<-features$V2[grep("mean\\(\\)|std\\(\\)", features$V2)]
selectedNames<-c(as.character(mean_std), "Subject", "Activity" )
tidy_data<-subset(complete_data,select=selectedNames)

#3.Use descriptive activity names to name the activities in the data set
tidy_data$Activity<-factor(tidy_data$Activity,labels=activities[,2])

#4.Appropriately labels the data set with descriptive variable names
names(tidy_data)<-gsub("Acc", "Accelerometer", names(tidy_data))
names(tidy_data)<-gsub("Gyro", "Gyroscope", names(tidy_data))
names(tidy_data)<-gsub("Mag", "Magnitude", names(tidy_data))
names(tidy_data)<-gsub("^t", "Time", names(tidy_data))
names(tidy_data)<-gsub("^f", "Frequency", names(tidy_data))
names(tidy_data)<-gsub("tBody", "TimeBody", names(tidy_data))
names(tidy_data)<-gsub("mean", "Mean", names(tidy_data), ignore.case = TRUE)
names(tidy_data)<-gsub("std", "STD", names(tidy_data), ignore.case = TRUE)
names(tidy_data)<-gsub("freq", "Frequency", names(tidy_data), ignore.case = TRUE)
names(tidy_data)<-gsub("angle", "Angle", names(tidy_data))
names(tidy_data)<-gsub("gravity", "Gravity", names(tidy_data))
names(tidy_data)<-gsub("BodyBody", "Body", names(tidy_data))

#5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject
final_tidy_data <- aggregate(. ~Subject + Activity,tidy_data, mean)
final_tidy_data<-final_tidy_data[order(final_tidy_data$Subject,final_tidy_data$Activity),]

write.table(final_tidy_data, "./ProjectData/Grouped_Tidy_Data.txt", row.name=FALSE)
write.table(final_tidy_data, file = "tidydata.txt",row.name=FALSE,quote = FALSE, sep = '\t')

