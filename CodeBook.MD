# Coursera - Getting and Cleaning Data
## Date: 6/19/15
## Owner:  Kevin Creese
## Project - Tidy Data

## Description
This document provides details realted to the origonal data set, variable descriptions, and transformations performed against this data based off of project requirements outlined by Coursera for their course in "Getting and Cleaning Data".  

### Data Source
The initial study which produced this data was performed by The UCI Machine Learning Repository, and the details surrounding their study can be found at [here]
(http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones)

*The data used to create this tidy data set can be found [here]
(https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip)

### Data Set Information (copied from their README.txt)
The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain. See 'features_info.txt' for more details. 

### Attribute Information
For each record in the dataset it is provided:
	* Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration.
	* Triaxial Angular velocity from the gyroscope.
	* A 561-feature vector with time and frequency domain variables. 
	* Its activity label. 
	* An identifier of the subject who carried out the experiment.


### Part 1 - Merge the training and test sets to create one data set.  
NOTE:  Data is pulled from the extracted data in the "UCI HAR Dataset"  Please ensure that your working directory is set to the "...\UCI HAR Dataset".

Data Tables Created from Data Files:
activityLabels ... activity_labels.txt
features       ... features.txt
X_TrainSet     ... /train/X_train.txt
Y_TrainLbl     ... /train/Y_train.txt
subjectTrain   ... /train/subject_train.txt
X_TestSet      ... /test/X_test.txt
Y_TestLbl      ... /test/Y_test.txt
subjectText    ... /test/subject_test.txt


Additional steps included assigning column names that could help help merge simular data into a seperate data sets

DS_Train - used to collect the Y_TrainLbl, subjectTrain, and X_TrainSet
DS_Test  - used to collect the Y_TestLbl, subjectTest, and X_TestSet
DS_Final - used to merge these data sets together

### Part 2 - Extract only the measurements on the mean and standard deviation for each measurement. 
Set a datatable called MyColumns and used that to set logical vectors that contained a true value for the activity, subject, mean, and standard deviation.

Used the MyColumns table to subset and keep only the columns we wanted to evaluate

### Part 3 - Use descriptive activity names to name the activities in the data set.
Merged data sets DS_final and activityLabels by the column labels "activityId".
When this was complete I updated the DC_Final dataset.

### Part 4 - Appropriately label the data set with descriptive activity names.
I used the gsub function based off of some recommendations I searched out on the web and decided to use "_" as a means of cleaning up the word segments that I found.  I looped through each column label and conducted a search for a segment I could elaborate on and then updated the DS_Final table with my results.  

### Part 5 - Create a second, independent tidy data set with the average of each variable for each activity and each subject. 

Created DS_Variables and subset the "Activity_Type" field that now has a "cleaned up name".
Used DS_Variables to produce the a new data set called DS_Mean which summarized the data by the mean which produced ony the data set with the avearge of each variable for each activity and subject.

Then I Updated the data set for activityLabels to use the same column headers so that I could merge into a final data set called DS_Tidy.