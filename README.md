Getting and Cleaning Data - Course Project

This repository contains required submissions for Getting and Cleaning Data - Course Project.

######################################################################### Raw Dataset ##########################################################################

Original dataset contains the data collected from the accelerometers of the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained: http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

For this project, raw dataset downloaded from following link: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

######################################################################### Files ################################################################################

CodeBook.md: Code book that describes the variables, the data, and any transformations or work that I performed to clean up the data

run_analysis.R:

Contains R source code which downloads the raw data and load data to R data-frames then performs the data preparation in 5 steps as described in the course project description. 1) Merges the training and the test sets to create one data set. 2) Extracts only the measurements on the mean and standard deviation for each measurement. 3) Uses descriptive activity names to name the activities in the data set 4) Appropriately labels the data set with descriptive variable names. 5) From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

Grouped_Tidy_Data.txt is generated after performing all five steps of transformations.

######################################################################### Dependencies ##########################################################################

"dplyr" R package is required to be installed on R environment in order to run run_analysis.R.