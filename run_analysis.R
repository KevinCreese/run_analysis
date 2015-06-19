# run_analysis.R
# Created: 6/13/15
# Owner: Kevin Creese

# Obtain Data from https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
# You should create one R Script called run_analysis.R that does the following.
#     1. Merges the training and the test sets to create one data set.
#     2. Extracts only the measurements on the mean and standard deviation for each measurement
#     3.  Uses description activity names to name the activities in the data set
#     4. Appropriately label the data set with descriptive variable names.
#     5.  From the data set in step 4, create a second, independent tidy data set with the average of each
#         variable for each activity and each subject.


######## PART 1 - Merge the Training and test sets to create one data set ############

## Clean Up My Stuff
rm(list=ls())

## Set Working directory
if(getwd() != "C:/Users/kevincre/Desktop/Projects/Data Science Certificate/3-GettingAndCleaningData/Project/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset") { 
  setwd("C:/Users/kevincre/Desktop/Projects/Data Science Certificate/3-GettingAndCleaningData/Project/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset")
}

## Import all .txt data into nice little table clusters that we can manipulate
activityLabels <- read.table("activity_labels.txt", header=FALSE, sep=" ")
features       <- read.table("features.txt", header=FALSE, sep = " " )
# Train tables
X_TrainSet     <- read.table("./train/X_train.txt", header=FALSE)
Y_TrainLbl     <- read.table("./train/Y_train.txt", header=FALSE)
subjectTrain   <- read.table("./train/subject_train.txt", header=FALSE)
# Test tables
X_TestSet      <- read.table("./test/X_test.txt", header=FALSE)
Y_TestLbl      <- read.table("./test/Y_test.txt", header=FALSE)
subjectTest    <- read.table("./test/subject_test.txt", header=FALSE)

## Assign header details to help merge data
colnames(activityLabels)   = c("activityId","activityType")
colnames(subjectTrain)     = "subjectID"
colnames(subjectTest)      = "subjectID"
colnames(X_TrainSet)       = features[,2]
colnames(X_TestSet)        = features[,2]
colnames(Y_TrainLbl)       = "activityId"
colnames(Y_TestLbl)        = "activityId"

## Create a dataset for training by merging Y_TrainLbl, subjectTrain, and X_TrainSet
DS_Train    <- cbind(Y_TrainLbl,subjectTrain,X_TrainSet)

## Create a dataset for test by mercing Y_TestLbl, subjectTest, and X_TestSet
DS_Test     <- cbind(Y_TestLbl,subjectTest,X_TestSet)

## Row Combine both data sets to create one final dataset to analyze
DS_Final    <- rbind(DS_Train,DS_Test)
View(DS_Final, "Part 1") ; # Confirm I got what I wanted

## Capture all of the column names so that we can search for specific column variables
colNames    <- colnames(DS_Final)

##### END PART 1 #####


#############  Part 2 - Extracts only the measurements on the mean and standard deviation for each measurement ###########

# We need to group together ID's, mean, and stddev columns if they are found and ignore the others
#  We will use True/false statements to complete that task as we search for the names we want to evalute
MyColumns   <- (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & 
                !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames))

# Now we can subset our DS_Final dataset by the values we want to evaluate
DS_Final    <- DS_Final[MyColumns==TRUE]
View(DS_Final, "Part 2"); # Confirm that I got what I wanted

##### END PART 2 #####


#############  Part 3 - Uses description activity names to name the activities in the data set ############
# Set the activityLables to the DS_Final so that we have good descriptions via the merge function
DS_Final   <- merge(DS_Final,activityLabels,by="activityId",all.x=TRUE)

# Ensure added column is added to the list of column names
colNames    <- colnames(DS_Final)

View(DS_Final, "Part 3")

##### END PART 3 #####

############# Part 4 - Appropriately label the data set with the descriptive activity names. #############
# The names in the DS_Final still look like "fBodyBodyGyroMag-std()"  We need to search through these names
# and then re-lable each column to better describe the data we are looking at

# I'm going to loop through the column names.  If there is a better way please update my project with comments,
#    but I think looping will work

for (i in 1:length(colNames)) {
  colNames[i] <- gsub("\\()","",colNames[i])
  colNames[i] <- gsub("^(t)","Time_",colNames[i])
  colNames[i] <- gsub("^(f)","Frequence_",colNames[i])
  colNames[i] <- gsub("-std","StdDev",colNames[i])
  colNames[i] <- gsub("-mean","Mean",colNames[i])
  colNames[i] <- gsub("([Bb]ody,[Bb]ody|[Bb]ody)","Body_",colNames[i])
  colNames[i] <- gsub("Mag","Magnitude_",colNames[i])
  colNames[i] <- gsub("activityId","Activity_ID",colNames[i])
  colNames[i] <- gsub("subjectID","Subject_ID",colNames[i])
  colNames[i] <- gsub("Gravity","Gravity_",colNames[i])
  colNames[i] <- gsub("Acc","Acc_",colNames[i])
  colNames[i] <- gsub("Jerk","Jerk_",colNames[i])
  colNames[i] <- gsub("Gyro","Gyro_",colNames[i])
  colNames[i] <- gsub("activityType","Activity_Type",colNames[i])
  
}
# Assign the new names back to the Data set
colnames(DS_Final) <- colNames
# I wanted to move the Activity Type to the Front of my data
DS_Final <- DS_Final[c(21,1:20)]

View(DS_Final, "Part 4")

##### END PART 4 #####

############# Part 5 - Create a second, independant tidy data set with the average of each variable for each activity and each subject.  #############################################
# We need to analyse just the variables first

DS_Variables <- DS_Final[,names(DS_Final) != "Activity_Type"]

# Sumamryize the DS_Variables table to identify the mean for each variable
DS_Means <- aggregate(DS_Variables[,names(DS_Variables) != c("Subject_ID","Activity_ID")], 
                      by=list(Activity_ID=DS_Variables$Activity_ID,Subject_ID = DS_Variables$Subject_ID), mean)

# Clean up duplicate columns
DS_Means <- DS_Means[c(3:length(colNames))]

# Reset activityLabels with new "more descriptive" name
colnames(activityLabels)   = c("Activity_ID","Activity_Type")

# Make my tidy data set
DS_Tidy <- merge(DS_Means,activityLabels,by="Activity_ID", all.x=TRUE)

# Move Activity_Type to the first column, because I like the data better that way
DS_Tidy <- DS_Tidy[c(length(DS_Tidy),1:length(DS_Tidy)-1)]

View(DS_Tidy, "Part 5")
write.table(DS_Tidy, "./TidyData.txt",row.names=TRUE,sep="\t")
