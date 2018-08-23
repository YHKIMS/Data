#Hi, This is my first assignment so please be generous about my assignment..


#create directory("data"), download and unzip the zip-file


if(!file.exists("./data")){dir.create("data")}
fileUrl<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile="./data/Assignment.zip")
unzip("./data/Assignment.zip",exdir='./data')


# create train-data and combind it with its labels(6 activities) and subject(1 to 30)

train<-read.table('./data/UCI HAR Dataset/train/X_train.txt') ## read x_train
train_subject<-read.table('./data/UCI HAR Dataset/train/subject_train.txt') ##read subject(1-30) of train-data
train_activity<-read.table('./data/UCI HAR Dataset/train/y_train.txt') ##read activity(6activities) of train-data
train_dataset<-cbind(train,train_subject,train_activity) ##combind train-data, subject of train-data and activity of train-data


# Appropriately labels the train-data set with descriptive variable name(QUESTION 4-continue)

features<-read.table('./data/UCI HAR Dataset/features.txt') ##
features<-features[,-1]
features<-as.character(features)
names(train_dataset)<-c(features,'subject','activities')

# create test-data and combind it with its labels(6 activities) and subject(1 to 30)

test<-read.table('./data/UCI HAR Dataset/test/X_test.txt') ## read x_test
test_subject<-read.table('./data/UCI HAR Dataset/test/subject_test.txt') ##read subject(1-30) of test-data
test_activity<-read.table('./data/UCI HAR Dataset/test/y_test.txt') ##read activity(6activities) of test-data
test_dataset<-cbind(test,test_subject,test_activity) ##combind train-data, subject of train-data and activity of train-data


# Appropriately labels the test-data set with descriptive variable name(QUESTION 4-Done)

names(test_dataset)<-c(features,'subject','activities')

# Merges the training and test datasets to create one data set(QUESTION1) : full_dataset

full_dataset<-rbind(train_dataset,test_dataset)


# Extracts only the measurements on the mean and standard deviation for each measurement(QUESTION2) : full_dataset_sub

full_dataset_sub<-full_dataset[,c(grepl('mean|std',names(full_dataset)),562,563)]


# Uses descriptive activity names to name the activities in the dataset(QUESTION3)

full_dataset_sub$activities<-as.factor(full_dataset_sub$activities)
activity_name<-read.table('./data/UCI HAR Dataset/activity_labels.txt')
activity_name<-activity_name[,-1]
activity_name<-as.character(activity_name)
library(plyr)
full_dataset_sub$activities<-revalue(full_dataset_sub$activities,c('1'=activity_name[1],'2'=activity_name[2],'3'=activity_name[3],'4'=activity_name[4],'5'=activity_name[5],'6'=activity_name[6]))


# create 2nd, independent tidy dataset with the average of each variable for each activity and each subject(QUESTION5) : full_dataset_sub_second

library(dplyr)
full_dataset_sub_second<-full_dataset_sub%>%
        group_by(activities,subject)%>%
        summarise_at(vars(-c(activities,subject)),funs(mean(.,na.rm = T)))





