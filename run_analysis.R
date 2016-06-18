##########################################################################################################
#
# Description:
#   runAnalysis.r - Code to download data from web , merge the training and test sets to create one dataset,
#                   extracts meaningful measurements and subsequently creates a tidy data set with the 
#                   average of each variable for each activity and subject.
#
# Author:
#   [1] Biswajeeban Dash, BD, +917328054605
#
# History:
#   [1]2016-06-18 - Initial Version
#
# Files URL:
#   https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
#
##########################################################################################################


# Clean up workspace
rm(list=ls())

# set working directory where you want to download the datasets
setwd("C:/Users/bdash/Desktop/Coursera/R")

# download the file and put the file in the data folder
if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile="./data/Dataset.zip")

# Unzip the files
unzip(zipfile="./data/Datasets.zip",exdir="./data")

# create a variable to store path till UCI HAR Dataset - unzipped files are in this folder
path_uci <- file.path("./data", "UCI HAR Dataset")

# Read the data from files required for this project and store them in tables 
# Run list.files on path_uci to see for yourselves the name of all files stored in the folder
# import features.txt, activity_labels.txt, subject_train.txt, subject_test.txt, x_train.txt, x_test.txt,
# y_train.txt, t_test.txt
# Since Features consists in x_train and x_test and Activity variables in Y, we name accordingly
FeaturesNames  = read.table(file.path(path_uci,"features.txt"),header=FALSE) 
ActivityLabels = read.table(file.path(path_uci,"activity_labels.txt"),header=FALSE)

SubjectTrain   = read.table(file.path(path_uci,"train","subject_train.txt"),header=FALSE)
SubjectTest    = read.table(file.path(path_uci,"test","subject_test.txt"),header=FALSE)

FeaturesTrain  = read.table(file.path(path_uci,"train","x_train.txt"),header=FALSE)
FeaturesTest   = read.table(file.path(path_uci,"test","x_test.txt"),header=FALSE)

ActivityTrain  = read.table(file.path(path_uci,"train","y_train.txt"),header=FALSE)
ActivityTest   = read.table(file.path(path_uci,"test","y_test.txt"),header=FALSE)

# 1. Merge the training and the test sets to create one data set.

  # concatenate different types of tables(subject, activity, features) by rows - stack them up
  dataSubject  = rbind(SubjectTrain, SubjectTest)
  dataActivity = rbind(ActivityTrain, ActivityTest)
  dataFeatures = rbind(FeaturesTrain, FeaturesTest)

  # rename the variable to more meaningful names rather than V1 and V2 etc.
  names(dataSubject)  = c("subject")
  names(dataActivity) = c("activity")
  names(dataFeatures) = FeaturesNames$V2

  # Merge columns to get the final data - all data has same number of rows = 2947 + 7352
  dataFinal = cbind(dataSubject,dataActivity,dataFeatures)


# 2. Extract only the measurements on the mean and standard deviation for each measurement. 

  # subset names of features by measurements on the mean and standard deviation
  # find the names of the features where mean() and std() is used by using grep command
  FeaturesNamesSubset = FeaturesNames$V2[grep("mean\\(\\)|std\\(\\)",FeaturesNames$V2)]

  # subset the final data by selected names of features(subsetted above) along with subject and activity
  selectedNames = c(as.character(FeaturesNamesSubset),"subject","activity")
  dataFinal = subset(dataFinal, select=selectedNames)


# 3. Use descriptive activity names to name the activities in the data set

  # factorize variable activity in the final data by using descriptive names from ActivityLabels
  dataFinal$activity = factor(dataFinal$activity,labels = c("WALKING","WALKING_UPSTAIRS","WALKING_DOWNSTAIRS","SITTING","STANDING","LAYING"))


# 4. Appropriately label the data set with descriptive activity names. 

  # to use descriptive activity names - substitute the variables names by following rules
  # 1. prefix t is replaced y time
  # 2. Acc is replaced by Accelerometer
  # 3. Gyro is replaced by Gyroscope
  # 4. prefix f is replaced by frequency
  # 5. Mag is replaced by Magnitude
  # 6. BodyBody is replaced by Body
  # 7. Remove ()
  # 8. -std is replaced by -StdDev
  # 9. -mean is replaced by -Mean
  names(dataFinal) = gsub("^t", "time", names(dataFinal))
  names(dataFinal) = gsub("^f", "frequency", names(dataFinal))
  names(dataFinal) = gsub("Acc", "Accelerometer", names(dataFinal))
  names(dataFinal) = gsub("Gyro", "Gyroscope", names(dataFinal))
  names(dataFinal) = gsub("Mag", "Magnitude", names(dataFinal))
  names(dataFinal) = gsub("BodyBody", "Body", names(dataFinal))
  names(dataFinal) = gsub("\\()","", names(dataFinal))
  names(dataFinal) = gsub("-std","-StdDev", names(dataFinal))
  names(dataFinal) = gsub("-mean", "-Mean", names(dataFinal))


# 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject. 

  # include plyr package
  library(plyr)

  # aggregate the final dataset by average value(mean) on the combination of subject and activity
  dataFinal2 = aggregate(. ~subject + activity, dataFinal, mean)

  # order the final dataset by subject and then by activity before writing
  dataFinal2 = dataFinal2[order(dataFinal2$subject,dataFinal2$activity),]

  # Export the tidy dataset to a text file 
  write.table(dataFinal2, file = "./data/tidyData.txt",row.names=TRUE,sep='\t')
