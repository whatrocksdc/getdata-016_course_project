# Introduction

This is a code book that describes the variables, the data, and any transformations or work that are performed to clean up the data

# Data Source
* Original data:  (https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip)
* Original descriptions:  (http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones)

# Script

The script referencing the data in this code book is 'run_analysis.R'

# Data Background

The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video*recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low*pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain. See 'features_info.txt' for more details. 

The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ. These time domain signals (prefix 't' to denote time) were captured at a constant rate of 50 Hz. Then they were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise. Similarly, the acceleration signal was then separated into body and gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc-XYZ) using another low pass Butterworth filter with a corner frequency of 0.3 Hz. 

Subsequently, the body linear acceleration and angular velocity were derived in time to obtain Jerk signals (tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ). Also the magnitude of these three-dimensional signals were calculated using the Euclidean norm (tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag). 

Finally a Fast Fourier Transform (FFT) was applied to some of these signals producing fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. (Note the 'f' to indicate frequency domain signals). 

These signals were used to estimate variables of the feature vector for each pattern:  
'-XYZ' is used to denote 3-axial signals in the X, Y and Z directions.

tBodyAcc-XYZ
tGravityAcc-XYZ
tBodyAccJerk-XYZ
tBodyGyro-XYZ
tBodyGyroJerk-XYZ
tBodyAccMag
tGravityAccMag
tBodyAccJerkMag
tBodyGyroMag
tBodyGyroJerkMag
fBodyAcc-XYZ
fBodyAccJerk-XYZ
fBodyGyro-XYZ
fBodyAccMag
fBodyAccJerkMag
fBodyGyroMag
fBodyGyroJerkMag

The set of variables that were estimated from these signals are: 

mean(): Mean value
std(): Standard deviation

# Data output

* "subject_ID"
    * The character identifier of the individual subject from the group of 30 who performed the tests
* "Activity"
    * The name of the activity performed by the subject
    * The names were drawn from the features.txt file
    * More appealing names were inserted into the final tidy dataset in lieu of their corresponding numbers        
* "tBodyAcc.meanFUN.X"
    * The mean of all 'tBodyAcc-mean()-X' observations performed by an individual subject performing an individual activity
* "tBodyAcc.meanFUN.Y"
    * The mean of all 'tBodyAcc-mean()-Y' observations performed by an individual subject performing an individual activity
* "tBodyAcc.meanFUN.Z"
    * The mean of all 'tBodyAcc-mean()-Z' observations performed by an individual subject performing an individual activity
* "tGravityAcc.meanFUN.X"
    * The mean of all 'tGravityAcc-mean()-X' observations performed by an individual subject performing an individual activity
* "tGravityAcc.meanFUN.Y"
    * The mean of all 'tGravityAcc-mean()-Y' observations performed by an individual subject performing an individual activity
* "tGravityAcc.meanFUN.Z"
    * The mean of all 'tGravityAcc-mean()-Z' observations performed by an individual subject performing an individual activity
* "tBodyAccJerk.meanFUN.X"
    * The mean of all 'tBodyAccJerk-mean()-X' observations performed by an individual subject performing an individual activity
* "tBodyAccJerk.meanFUN.Y"
    * The mean of all 'tBodyAccJerk-mean()-Y' observations performed by an individual subject performing an individual activity
* "tBodyAccJerk.meanFUN.Z"
    * The mean of all 'tBodyAccJerk-mean()-Z' observations performed by an individual subject performing an individual activity
* "tBodyGyro.meanFUN.X"
    * The mean of all 'tBodyGyro-mean()-X' observations performed by an individual subject performing an individual activity
* "tBodyGyro.meanFUN.Y"
    * The mean of all 'tBodyGyro-mean()-Y' observations performed by an individual subject performing an individual activity
* "tBodyGyro.meanFUN.Z"
    * The mean of all 'tBodyGyro-mean()-Z' observations performed by an individual subject performing an individual activity
* "tBodyGyroJerk.meanFUN.X"
    * The mean of all 'tBodyGyroJerk-mean()-X' observations performed by an individual subject performing an individual activity
* "tBodyGyroJerk.meanFUN.Y"
    * The mean of all 'tBodyGyroJerk-mean()-Y' observations performed by an individual subject performing an individual activity
* "tBodyGyroJerk.meanFUN.Z"
    * The mean of all 'tBodyGyroJerk-mean()-Z' observations performed by an individual subject performing an individual activity
* "tBodyAccMag.meanFUN"
    * The mean of all 'tBodyAccMag-mean()' observations performed by an individual subject performing an individual activity
* "tGravityAccMag.meanFUN"
    * The mean of all 'tGravityAccMag-mean()' observations performed by an individual subject performing an individual activity
* "tBodyAccJerkMag.meanFUN"
    * The mean of all 'tBodyAccJerkMag-mean()' observations performed by an individual subject performing an individual activity
* "tBodyGyroMag.meanFUN"
    * The mean of all 'tBodyGyroMag-mean()' observations performed by an individual subject performing an individual activity
* "tBodyGyroJerkMag.meanFUN"
    * The mean of all 'tBodyGyroJerkMag-mean()' observations performed by an individual subject performing an individual activity
* "fBodyAcc.meanFUN.X"
    * The mean of all 'fBodyAcc-mean()-X' observations performed by an individual subject performing an individual activity
* "fBodyAcc.meanFUN.Y"
    * The mean of all 'fBodyAcc-mean()-Y' observations performed by an individual subject performing an individual activity
* "fBodyAcc.meanFUN.Z"
    * The mean of all 'fBodyAcc-mean()-Z' observations performed by an individual subject performing an individual activity
* "fBodyAccJerk.meanFUN.X"
    * The mean of all 'fBodyAccJerk-mean()-X' observations performed by an individual subject performing an individual activity
* "fBodyAccJerk.meanFUN.Y"
    * The mean of all 'fBodyAccJerk-mean()-Y' observations performed by an individual subject performing an individual activity
* "fBodyAccJerk.meanFUN.Z"
    * The mean of all 'fBodyAccJerk-mean()-Z' observations performed by an individual subject performing an individual activity
* "fBodyGyro.meanFUN.X"
    * The mean of all 'fBodyGyro-mean()-X' observations performed by an individual subject performing an individual activity
* "fBodyGyro.meanFUN.Y"
    * The mean of all 'fBodyGyro-mean()-Y' observations performed by an individual subject performing an individual activity
* "fBodyGyro.meanFUN.Z"
    * The mean of all 'fBodyGyro-mean()-Z' observations performed by an individual subject performing an individual activity
* "fBodyAccMag.meanFUN"
    * The mean of all 'fBodyAccMag-mean()' observations performed by an individual subject performing an individual activity
* "fBodyBodyAccJerkMag.meanFUN"
    * The mean of all 'fBodyBodyAccJerkMag-mean()' observations performed by an individual subject performing an individual activity
* "fBodyBodyGyroMag.meanFUN"
    * The mean of all 'fBodyBodyGyroMag-mean()' observations performed by an individual subject performing an individual activity
* "fBodyBodyGyroJerkMag.meanFUN"
    * The mean of all 'fBodyBodyGyroJerkMag-mean()' observations performed by an individual subject performing an individual activity
* "tBodyAcc.stdFUN.X"
    * The mean of all 'tBodyAcc-std()-X' observations performed by an individual subject performing an individual activity
* "tBodyAcc.stdFUN.Y"
    * The mean of all 'tBodyAcc-std()-Y' observations performed by an individual subject performing an individual activity
* "tBodyAcc.stdFUN.Z"
    * The mean of all 'tBodyAcc-std()-Z' observations performed by an individual subject performing an individual activity
* "tGravityAcc.stdFUN.X"
    * The mean of all 'tGravityAcc-std()-X' observations performed by an individual subject performing an individual activity
* "tGravityAcc.stdFUN.Y"
    * The mean of all 'tGravityAcc-std()-Y' observations performed by an individual subject performing an individual activity
* "tGravityAcc.stdFUN.Z"
    * The mean of all 'tGravityAcc-std()-Z' observations performed by an individual subject performing an individual activity
* "tBodyAccJerk.stdFUN.X"
    * The mean of all 'tBodyAccJerk-std()-X' observations performed by an individual subject performing an individual activity
* "tBodyAccJerk.stdFUN.Y"
    * The mean of all 'tBodyAccJerk-std()-Y' observations performed by an individual subject performing an individual activity
* "tBodyAccJerk.stdFUN.Z"
    * The mean of all 'tBodyAccJerk-std()-Z' observations performed by an individual subject performing an individual activity
* "tBodyGyro.stdFUN.X"
    * The mean of all 'tBodyGyro-std()-X' observations performed by an individual subject performing an individual activity
* "tBodyGyro.stdFUN.Y"
    * The mean of all 'tBodyGyro-std()-Y' observations performed by an individual subject performing an individual activity
* "tBodyGyro.stdFUN.Z"
    * The mean of all 'tBodyGyro-std()-Z' observations performed by an individual subject performing an individual activity
* "tBodyGyroJerk.stdFUN.X"
    * The mean of all 'tBodyGyroJerk-std()-X' observations performed by an individual subject performing an individual activity
* "tBodyGyroJerk.stdFUN.Y"
    * The mean of all 'tBodyGyroJerk-std()-Y' observations performed by an individual subject performing an individual activity
* "tBodyGyroJerk.stdFUN.Z"
    * The mean of all 'tBodyGyroJerk-std()-Z' observations performed by an individual subject performing an individual activity
* "tBodyAccMag.stdFUN"
    * The mean of all 'tBodyAccMag-std()' observations performed by an individual subject performing an individual activity
* "tGravityAccMag.stdFUN"
    * The mean of all 'tGravityAccMag-std()' observations performed by an individual subject performing an individual activity
* "tBodyAccJerkMag.stdFUN"
    * The mean of all 'tBodyAccJerkMag-std()' observations performed by an individual subject performing an individual activity
* "tBodyGyroMag.stdFUN"
    * The mean of all 'tBodyGyroMag-std()' observations performed by an individual subject performing an individual activity
* "tBodyGyroJerkMag.stdFUN"
    * The mean of all 'tBodyGyroJerkMag-std()' observations performed by an individual subject performing an individual activity
* "fBodyAcc.stdFUN.X"
    * The mean of all 'fBodyAcc-std()-X' observations performed by an individual subject performing an individual activity
* "fBodyAcc.stdFUN.Y"
    * The mean of all 'fBodyAcc-std()-Y' observations performed by an individual subject performing an individual activity
* "fBodyAcc.stdFUN.Z"
    * The mean of all 'fBodyAcc-std()-Z' observations performed by an individual subject performing an individual activity 
* "fBodyAccJerk.stdFUN.X"
    * The mean of all 'fBodyAccJerk-std()-X' observations performed by an individual subject performing an individual activity
* "fBodyAccJerk.stdFUN.Y"
    * The mean of all 'fBodyAccJerk-std()-Y' observations performed by an individual subject performing an individual activity
* "fBodyAccJerk.stdFUN.Z"
    * The mean of all 'fBodyAccJerk-std()-Z' observations performed by an individual subject performing an individual activity
* "fBodyGyro.stdFUN.X"
    * The mean of all 'fBodyGyro-std()-X' observations performed by an individual subject performing an individual activity
* "fBodyGyro.stdFUN.Y"
    * The mean of all 'fBodyGyro-std()-Y' observations performed by an individual subject performing an individual activity
* "fBodyGyro.stdFUN.Z"
    * The mean of all 'fBodyGyro-std()-Z' observations performed by an individual subject performing an individual activity
* "fBodyAccMag.stdFUN"
    * The mean of all 'fBodyAccMag-std()' observations performed by an individual subject performing an individual activity
* "fBodyBodyAccJerkMag.stdFUN"
    * The mean of all 'fBodyBodyAccJerkMag-std()' observations performed by an individual subject performing an individual activity
* "fBodyBodyGyroMag.stdFUN"
    * The mean of all 'fBodyBodyGyroMag-std()' observations performed by an individual subject performing an individual activity
* "fBodyBodyGyroJerkMag.stdFUN"
    * The mean of all 'fBodyBodyGyroJerkMag-std()' observations performed by an individual subject performing an individual activity