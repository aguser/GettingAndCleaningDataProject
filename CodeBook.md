---
title: "CodeBook"
author: "ArturG"
date: "Sunday, March 22, 2015"
output: html_document
---

The run_analysis R script was created to execute following steps related to the project from Getting and Cleaning Data Course:
1. Merges the training and the test sets to create one data set.
2. Extracts only the measurements on the mean and standard deviation for each measurement. 
3. Uses descriptive activity names to name the activities in the data set
4. Appropriately labels the data set with descriptive variable names. 
5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

The script details:

I. Key Variables:
dataActivityTest  - test data describing activities
dataActivityTrain - train data describing activities
dataSubjectTrain - test data describing subjects
dataSubjectTest  - train data describing subjects
mainDataTest  - test data, sensor signals (accelerometer and gyroscope) after pre-processing 
mainDataTrain - train data, sensor signals (accelerometer and gyroscope) after pre-processing
mainData - it contains all data (all variables above are merged)


II. Data:
Here are the input data sets for the project: 
https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

The dataset includes the following files:
=========================================

- 'README.txt'

- 'features_info.txt': Shows information about the variables used on the feature vector.

- 'features.txt': List of all features.

- 'activity_labels.txt': Links the class labels with their activity name.

- 'train/X_train.txt': Training set.

- 'train/y_train.txt': Training labels.

- 'test/X_test.txt': Test set.

- 'test/y_test.txt': Test labels.

The following files are available for the train and test data. Their descriptions are equivalent. 

- 'train/subject_train.txt': Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30. 

- 'train/Inertial Signals/total_acc_x_train.txt': The acceleration signal from the smartphone accelerometer X axis in standard gravity units 'g'. Every row shows a 128 element vector. The same description applies for the 'total_acc_x_train.txt' and 'total_acc_z_train.txt' files for the Y and Z axis. 

- 'train/Inertial Signals/body_acc_x_train.txt': The body acceleration signal obtained by subtracting the gravity from the total acceleration. 

- 'train/Inertial Signals/body_gyro_x_train.txt': The angular velocity vector measured by the gyroscope for each window sample. The units are radians/second. 


III. Transformations:
The key transformations used in the script are following:
- rbind (mergind rows), cbind(merging columns), joining, subsetting, summing and aggregating tables (sqldf and other functions were used)