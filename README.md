# GettingCleaninngDataWeek4Assignment
##Week 4 project Getting and Cleaning data

##step 0 setting environment  by loading libraries
library(tidyverse)
library(tidyr)
library(dplyr)
library(stringr)

##Step 1. Getting data from assignment here  https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip . and then use read.table function
to load data into R

X_test <- read.table("E:\\DataAnalysis\\UCI HAR Dataset\\test\\X_test.txt", header = TRUE)
Y_test <- read.table("E:\\DataAnalysis\\UCI HAR Dataset\\test\\Y_test.txt", header = TRUE)
X_train <-read.table("E:\\DataAnalysis\\UCI HAR Dataset\\train\\X_train.txt", header = TRUE)
Y_train <-read.table("E:\\DataAnalysis\\UCI HAR Dataset\\train\\y_train.txt", header = TRUE)
activity_labels <-read.table("E:\\DataAnalysis\\UCI HAR Dataset\\activity_labels.txt", header = FALSE)
features<-read.table("E:\\DataAnalysis\\UCI HAR Dataset\\features.txt", header = FALSE)
subject_train<-read.table("E:\\DataAnalysis\\UCI HAR Dataset\\train\\subject_train.txt", header = TRUE)
subject_test<-read.table("E:\\DataAnalysis\\UCI HAR Dataset\\test\\subject_test.txt", header = TRUE)


##Correct activity_labels , feature and subject tables by rename columns that easy to use

activity_labels<-rename(activity_labels,"no."=V1,"test"=V2)
features<-rename(features,"no."=V1,"feature"=V2)
subject_train <- rename(subject_train,"subject_id" = X1)
subject_test <- rename(subject_test,"subject_id" = X2)

##Label Y_train and Y_test with descirptive activity labels from activity_labels

Y_trainlabel <-
    Y_train %>%
	mutate(labels=case_when(X5 == "1"  ~ "WALKING",
							X5 == "2"  ~ "WALKING_UPSTAIRS",
							X5 == "3"  ~ "WALKING_DOWNSTAIRS",
							X5 == "4"  ~ "SITTING",
							X5 == "5"  ~ "STANDING",
							X5 == "6"  ~ "LAYING",))
Y_trainlabel<-Y_trainlabel[,-1]
Y_testlabel <-
	Y_test %>%
	mutate(labels=case_when(X5 == "1"  ~ "WALKING",
							X5 == "2"  ~ "WALKING_UPSTAIRS",
							X5 == "3"  ~ "WALKING_DOWNSTAIRS",
							X5 == "4"  ~ "SITTING",
							X5 == "5"  ~ "STANDING",
							X5 == "6"  ~ "LAYING",))
Y_testlabel<-Y_testlabel[,-1]
################################################################################

##Identify measurements on the mean and standard deviation for each measurement
### Label X_test and X_train columns name which measurements on mean and S.D. ##
by surching which measurments in features dataset contain word "mean" or"std"
 featuresmean<-grep("mean",features[,2])
 featuresSD<-grep("std",features[,2])

###Rename X_train columns which measurements on mean
for(i in seq(from = 1, to =length(featuresmean))){
 	label<-as.character()
 	label<-features[featuresmean[i],2]
 	names(X_train)[names(X_train) == colnames(X_train[featuresmean[i]])] <- label
}
###Rename X_train columns which measurements on S.D.
for(i in seq(from = 1, to =length(featuresSD))){
 	label<-as.character()
 	label<-features[featuresSD[i],2]
 	names(X_train)[names(X_train) == colnames(X_train[featuresSD[i]])] <- label
 }

###Rename X_test columns which measurements on mean
 for(i in seq(from = 1, to =length(featuresmean))){
 	label<-as.character()
 	label<-features[featuresmean[i],2]
 	names(X_test)[names(X_test) == colnames(X_test[featuresmean[i]])] <- label
 }
###Rename X_test columns which measurements on S.D.
 for(i in seq(from = 1, to =length(featuresSD))){
 	label<-as.character()
 	label<-features[featuresSD[i],2]
 	names(X_test)[names(X_test) == colnames(X_test[featuresSD[i]])] <- label
 } 
 
 ##################################################################################
 ### Select only columns of interest####
 
columns_of_interest <-c(featuresmean,featuresSD)
columns_of_interest <-as.numeric(columns_of_interest)
columns_of_interest <-sort(columns_of_interest)

X_train_interest<-X_train[,columns_of_interest]
X_test_interest<-X_test[,columns_of_interest]

###############################################################################

##Merge test and train table with subject and descriptive activity name label ## column bind with Subject
## and rename column name of activity name label for next step merge

Train<-cbind(subject_train,Y_trainlabel,X_train_interest)
Train<-rename(Train,"label"= Y_trainlabel)

Test<-cbind(subject_test,Y_testlabel,X_test_interest)
Test<-rename(Test,"label"= Y_testlabel)

###Merge Test table with Train table by rbind
Data<-rbind(Train,Test)

###Creates an independent tidy data set with the average of each variable for each activity and each subject
### By using group_by and summarise .
Tidy_Data <- Data %>%
	group_by(subject_id,label)%>%
	summarise(across(is.numeric,mean,na.rm = TRUE))
### Finish###
