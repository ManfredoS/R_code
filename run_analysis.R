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
## make set, subject and Y factors:
for(i in 1:3){combi[,i] <- as.factor(combi[,i])}

###############################################
## 2) Extract only the measurements on the mean and standard deviation for each measurement. 
###############################################

## check if NAs are present:
which(is.na(combi))
## only take the rownames containing mean() and std():
MSD <- combi[,c(1:3,grep("mean()",colnames(combi)),grep("std()",colnames(combi)))]

###############################################
## 3) Use descriptive activity names to name the activities in the data set
###############################################

## reading the activity description:
activities <- read.table(file="./data/UCI HAR Dataset/activity_labels.txt",header=FALSE)
## make data frame containing the meaning of the activity coding:
Activities <- data.frame(Activity=activities[,2][combi$Y])

###############################################
## 4) Appropriately label the data set with descriptive activity names. 
###############################################

## add activity label to activity coding within data set:
combi2 <- cbind(Activities,MSD) 
combi2[1:5,1:5]

###############################################
## 5) Create a second, independent tidy data set with the average of each variable for each activity and each subject. 
###############################################

## make variable subject_activity
combi2$Sub_Act <- paste(combi$Subject,combi2$Activity,combi2$Y,sep=":")

## assign each subject_activity combination the average within the selected (mean, std) variables:
combi3 <- data.frame(Combination=unique(combi2$Sub_Act));rownames(combi3) <- combi3[,1]
for(i in 5:(length(combi2)-1)){
DF <- data.frame(with(combi2,tapply(combi2[,i],combi2$Sub_Act,mean)))
names(DF) <- names(combi2)[i]
combi3 <- cbind(combi3,DF)
}
rownames(combi3) <- 1:length(rownames(combi3))

## setup the tidy data set by splitting and adding the joined variable:
combi3$Combination <- as.character(combi3$Combination)
Unsplit <- data.frame(matrix(unlist(strsplit(combi3$Combination, ":")),ncol=3,byrow=TRUE))
Prep <- cbind(Unsplit,combi3)
names(Prep)[1:3] <- c("Subject","Activity","Y")

## final tidy data set: 
Tidy <- Prep[,-4]
dim(Tidy);Tidy[1:10,1:5]


## save data set to the folder:
write.table(Tidy,"Tidy.txt",sep=";",row.names=FALSE)
