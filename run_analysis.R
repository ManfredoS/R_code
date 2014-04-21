##
##
## Getting and cleaning data
## 
## Script for peer assessment "run_analysis.R"
## 21.04.14 ms
###############################################
## 1) Merge the training and the test sets to create one data set.
###############################################

## load packages for reshaping the data sets:
require(Hmisc)           
require(plyr)
require(reshape2)
##

## load and check feature discription (x variable names)
list.files("./data/UCI HAR Dataset")
features <- read.table(file="./data/UCI HAR Dataset/features.txt",header=FALSE)

## load and combine train data:
list.files(c("./data/UCI HAR Dataset/train","./data/UCI HAR Dataset/test"))
##
y_train <- read.table(file="./data/UCI HAR Dataset/train/y_train.txt",header=FALSE);names(y_train) <- "Y" 
x_train <- read.table(file="./data/UCI HAR Dataset/train/x_train.txt",header=FALSE);names(x_train) <- features[,2]
subject_train <- read.table(file="./data/UCI HAR Dataset/train/subject_train.txt",header=FALSE); names(subject_train) <- "Subject"
##
## combind the 3 data.sets and add info, which set they´re from:
train <- cbind("train",subject_train,y_train,x_train);names(train)[1] <- "Set"   

## load and combine test data:
y_test <- read.table(file="./data/UCI HAR Dataset/test/y_test.txt",header=FALSE);names(y_test) <- "Y" 
x_test <- read.table(file="./data/UCI HAR Dataset/test/x_test.txt",header=FALSE);names(x_test) <- features[,2]
subject_test <- read.table(file="./data/UCI HAR Dataset/test/subject_test.txt",header=FALSE); names(subject_test) <- "Subject"
## combind the 3 data.sets and add info, which set they´re from:
test <- cbind("test",subject_test,y_test,x_test);names(test)[1] <- "Set"   
## check if all columns match:
all.equal(names(train),names(test))

## bind the two data sets together:
combi <- rbind(train,test);paste(dim(combi)[1],"rows",dim(combi)[2],"columns",sep=" ")
## [1] "10299 rows 564 columns"
## make Subject a factor:
combi$Subject <- as.factor(combi$Subject)

###############################################
## 2) Extract only the measurements on the mean and standard deviation for each measurement. 
###############################################

## check if NAs are present:
which(is.na(combi))
## make a data frame for the means and sds for all features(variables)
Means <- data.frame(sapply(combi[,4:length(colnames(combi))],mean));names(Means) <- "Means"
SDs <- data.frame(sapply(combi[,4:length(colnames(combi))],sd));names(SDs) <- "SDs"
## bind the two:
MSD <- merge(Means,SDs,by=0);names(MSD)[1] <- "Feature_variable";MSD[,1] <- as.factor(MSD[,1])

###############################################
## 3) Use descriptive activity names to name the activities in the data set
###############################################


###############################################
## 4) Appropriately label the data set with descriptive activity names. 
###############################################




## 5) Create a second, independent tidy data set with the average of each variable for each activity and each subject. 
