Code Book : Coursera Getting and Cleaning Data - Course Project

The data set that this code book pertains to is located in the Grouped_Tidy_Data.txt file of this repository. 
The structure of the data set is explained in the Data section, its variables are described in the Variables section.
All the transformations that were carried out to obtain the data set, starting from source data are presented in the Transformations section. 

############################################################### Data ###############################################################

Data Format of Grouped_Tidy_Data.txt:

The Grouped_Tidy_Data.txt data file is a text file which contains space-separated values. 
The first row of file contains the names of the variables. 
Remaining rows contains the values of these variables.

############################################################### Variables ###############################################################

Each row contains, for a given subject and activity, 79 averaged signal measurements.

Identifiers :

Subject - Subject identifier which indicate the subject who performed the activity, integer, ranges from 1 to 30.

Activity - Activity identifier, string with 6 possible values:

WALKING: subject was walking
WALKING_UPSTAIRS: subject was walking upstairs
WALKING_DOWNSTAIRS: subject was walking downstairs
SITTING: subject was sitting
STANDING: subject was standing
LAYING: subject was laying

Measurements are normalized and bounded within [-1,1].
Prior to normalisation, The units used for the accelerations (total and body) are 'g's (gravity of earth -> 9.80665 m/seg2).
Prior to normalisation, The gyroscope units are rad/seg.

Mean Values and Standard Deviations of time-domain body acceleration in the X, Y and Z directions: 
	TimeBodyAccelerometer.Mean...X TimeBodyAccelerometer.Mean...Y TimeBodyAccelerometer.Mean...Z
	TimeBodyAccelerometer.STD...X TimeBodyAccelerometer.STD...Y TimeBodyAccelerometer.STD...Z

Mean Values and Standard Deviations of time-domain gravity acceleration in the X, Y and Z directions: 
	TimeGravityAccelerometer.Mean...X TimeGravityAccelerometer.Mean...Y TimeGravityAccelerometer.Mean...Z
	TimeGravityAccelerometer.STD...X TimeGravityAccelerometer.STD...Y TimeGravityAccelerometer.STD...Z

Mean Values and Standard Deviations of time-domain body acceleration jerk (derivation of the acceleration in time) in the X, Y and Z directions: 
	TimeBodyAccelerometerJerk.Mean...X TimeBodyAccelerometerJerk.Mean...Y TimeBodyAccelerometerJerk.Mean...Z
	TimeBodyAccelerometerJerk.STD...X TimeBodyAccelerometerJerk.STD...Y TimeBodyAccelerometerJerk.STD...Z

Mean Values and Standard Deviations of time-domain body angular velocity in the X, Y and Z directions: 
	TimeBodyGyroscope.Mean...X TimeBodyGyroscope.Mean...Y TimeBodyGyroscope.Mean...Z
	TimeBodyGyroscope.STD...X TimeBodyGyroscope.STD...Y TimeBodyGyroscope.STD...Z

Mean Values and Standard Deviations of time-domain body angular velocity jerk (derivation of the angular velocity in time) in the X, Y and Z directions: 
	TimeBodyGyroscopeJerk.Mean...X TimeBodyGyroscopeJerk.Mean...Y TimeBodyGyroscopeJerk.Mean...Z
	TimeBodyGyroscopeJerk.STD...X TimeBodyGyroscopeJerk.STD...Y TimeBodyGyroscopeJerk.STD...Z

Mean Value and Standard Deviation of deviation of the time-domain magnitude of body acceleration: 
	TimeBodyAccelerometerMagnitude.Mean.. TimeBodyAccelerometerMagnitude.STD..

Mean Value and Standard Deviation of the time-domain magnitude of gravity acceleration: 
	TimeGravityAccelerometerMagnitude.Mean.. TimeGravityAccelerometerMagnitude.STD..

Mean Value and Standard Deviation of the time-domain magnitude of body acceleration jerk (derivation of the acceleration in time): 
	TimeBodyAccelerometerJerkMagnitude.Mean.. TimeBodyAccelerometerJerkMagnitude.STD..
		
Mean Value and Standard Deviation of the time-domain magnitude of body angular velocity: 
	TimeBodyGyroscopeMagnitude.Mean.. TimeBodyGyroscopeMagnitude.STD..

Mean Value and Standard Deviation of the time-domain magnitude of body angular velocity jerk (derivation of the angular velocity in time): 
	TimeBodyGyroscopeJerkMagnitude.Mean.. TimeBodyGyroscopeJerkMagnitude.STD..

Mean Values and Standard Deviations of the frequency-domain body acceleration in the X, Y and Z directions: 
	FrequencyuencyBodyAccelerometer.Mean...X FrequencyuencyBodyAccelerometer.Mean...Y FrequencyuencyBodyAccelerometer.Mean...Z
	FrequencyuencyBodyAccelerometer.STD...X FrequencyuencyBodyAccelerometer.STD...Y FrequencyuencyBodyAccelerometer.STD...Z

Weighted average of the frequency components of the frequency-domain body acceleration in the X, Y and Z directions: 
	FrequencyuencyBodyAccelerometer.MeanFrequency...X FrequencyuencyBodyAccelerometer.MeanFrequency...Y FrequencyuencyBodyAccelerometer.MeanFrequency...Z

Mean Values and Standard Deviations of frequency-domain body acceleration jerk (derivation of the acceleration in time) in the X, Y and Z directions: 
	FrequencyuencyBodyAccelerometerJerk.Mean...X FrequencyuencyBodyAccelerometerJerk.Mean...Y FrequencyuencyBodyAccelerometerJerk.Mean...Z
	FrequencyuencyBodyAccelerometerJerk.STD...X FrequencyuencyBodyAccelerometerJerk.STD...Y FrequencyuencyBodyAccelerometerJerk.STD...Z

Weighted average of the frequency components of the frequency-domain body acceleration jerk (derivation of the acceleration in time) in the X, Y and Z directions: 
	FrequencyuencyBodyAccelerometerJerk.MeanFrequency...X FrequencyuencyBodyAccelerometerJerk.MeanFrequency...Y FrequencyuencyBodyAccelerometerJerk.MeanFrequency...Z

Mean Values and Standard Deviations of frequency-domain body angular velocity in the X, Y and Z directions: 
	FrequencyuencyBodyGyroscope.Mean...X FrequencyuencyBodyGyroscope.Mean...Y FrequencyuencyBodyGyroscope.Mean...Z
	FrequencyuencyBodyGyroscope.STD...X FrequencyuencyBodyGyroscope.STD...Y FrequencyuencyBodyGyroscope.STD...Z

Weighted average of the frequency components of the frequency-domain body angular velocity in the X, Y and Z directions: 
	FrequencyuencyBodyGyroscope.MeanFrequency...X FrequencyuencyBodyGyroscope.MeanFrequency...Y FrequencyuencyBodyGyroscope.MeanFrequency...Z

Mean Value, standard deviation, and weighted average of the frequency components of the frequency-domain magnitude of body acceleration: 
	FrequencyuencyBodyAccelerometerMagnitude.Mean.. FrequencyuencyBodyAccelerometerMagnitude.STD.. FrequencyuencyBodyAccelerometerMagnitude.MeanFrequency..

Mean Value, standard deviation, and weighted average of the frequency components of the frequency-domain magnitude of body acceleration jerk (derivation of the acceleration in time): 
	FrequencyuencyBodyAccelerometerJerkMagnitude.Mean.. FrequencyuencyBodyAccelerometerJerkMagnitude.STD.. FrequencyuencyBodyAccelerometerJerkMagnitude.MeanFrequency..

Mean Value, standard deviation, and weighted average of the frequency components of the frequency-domain magnitude of body angular velocity: 
	FrequencyuencyBodyGyroscopeMagnitude.Mean.. FrequencyuencyBodyGyroscopeMagnitude.STD.. FrequencyuencyBodyGyroscopeMagnitude.MeanFrequency..

Mean Value, standard deviation, and weighted average of the frequency components of the frequency-domain magnitude of body angular velocity jerk (derivation of the angular velocity in time): 
	FrequencyuencyBodyGyroscopeJerkMagnitude.Mean.. FrequencyuencyBodyGyroscopeJerkMagnitude.STD.. FrequencyuencyBodyGyroscopeJerkMagnitude.MeanFrequency..

Mean Values of Additional vectors obtained by averaging the signals in a signal window sample : 
	Angle.TimeBodyAccelerometerMean.Gravity. Angle.TimeBodyAccelerometerJerkMean..GravityMean. Angle.TimeBodyGyroscopeMean.GravityMean. Angle.TimeBodyGyroscopeJerkMean.GravityMean. Angle.X.GravityMean. Angle.Y.GravityMean. Angle.Z.GravityMean.

############################################################### Transformations ###############################################################

The source data is downloaded from https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip.

run_analysis.R R script performs following transformations to the source data:

1)Merges the training and the test sets to create one data set. 
2)Extracts only the measurements on the mean and standard deviation for each measurement. 
3)Uses descriptive activity names to name the activities in the data set. 
4)Appropriately labels the data set with descriptive variable names. Also Special characters (i.e. (, ), and -) were removed. 
5)From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.