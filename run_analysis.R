########## Project Deliverables ##########
# You should create one R script called run_analysis.R that does the following.
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names.
# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject

########## Genral Assumptions ##########
# The source zip file was downloaded and extracted in the workspace directory
# The "test" and "train" subdirectories are in the workspace directory
# The data files are in their respective subdirectories
# The matrixStats, graphics and grDevices packages have been installed
# The worksapce/lobal Environment has been cleared and without residuals


setwd("~/$R/UCI HAR Dataset/") ####### Remove ########
rm(list = ls())


library(matrixStats)
library(stringr)

########## PART ONE ##########
# 1. Merges the training and the test sets to create one data set.
# Load the observations, the list of subjects and the list of activities
print("PART ONE: Please be patient while getting /train' and 'test' and creating one data set named 'data'\n")
trainObs <- read.delim("train/X_train.txt", sep = "", header = FALSE)
trainSub <- read.delim("train/subject_train.txt", sep = "", header = FALSE)
trainAct <- read.delim("train/y_train.txt", sep = "", header = FALSE)
testObs <- read.delim("test/X_test.txt", sep = "", header = FALSE)
testSub <- read.delim("test/subject_test.txt", sep = "", header = FALSE)
testAct <- read.delim("test/y_test.txt", sep = "", header = FALSE)
cat(" Dimensions of train are [", dim(trainObs), "] and for test are [", dim(testObs), "].\n")
cat(" Dimensions of trainSub are [", dim(trainSub), "] and for testSub are [", dim(testSub), "].\n")
cat(" Dimensions of testAct are [", dim(trainAct), "] and for testAct are [", dim(testAct), "].\n")

data <- rbind(trainObs, testObs)
rm(trainObs)
rm(testObs)
subjects <- rbind(trainSub, testSub)
rm(trainSub)
rm(testSub)
activities <- rbind(trainAct, testAct)
rm(trainAct); rm(testAct)
cat(" Dimensions of data are [", dim(data), "].\n")
# summary(data)
# summary(activities)
# summary(subjects)

########## PART TWO ##########
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
# This assumes the mean and std columns of each grouping is the deliverable
# Also assume no activities and subjects are required until Part Three
cat("PART TWO: Calculating means and standard deviations\n")
colLabels <- read.delim("features.txt", sep = "", header = FALSE, stringsAsFactors = FALSE)
dataLabels <- colLabels$V2
names(data) <- dataLabels
stdCols <-str_detect (dataLabels, as.character("std"))
meanCols <-str_detect (dataLabels, as.character("mean"))
stdData<-data[, stdCols]
meanData <- data[meanCols]
smallData <- cbind(meanData, stdData)
dim(smallData)
summary(smallData)
print("The above was a summary of the mean and standard deviation measurments")
rm(meanData);rm(stdData); rm(smallData); rm(meanCols); rm(stdCols)

########## PART THREE ##########
# 3. Uses descriptive activity names to name the activities in the data set
# convert the activities into activity labels
cat("PART THREE: convert the activities into activity labels\n")
act_labels <- c('WALKING','WALKING_UPSTAIRS','WALKING_DOWNSTAIRS','SITTING','STANDING','LAYING')
act_list <- list(activities$V1)
act_list <- as.integer(act_list[[1]])
act_fac <- factor(act_list, labels = act_labels)
acts <- as.data.frame(act_fac)
rm(act_list)
data <- cbind(acts, data)
cat(" added the activity labels to the first column of the data\n")
data <- cbind(subjects, data)
cat(" added the subjects to the first column of the data\n")
cat(" Dimensions of data are now [", dim(data), "] after adding subjects and activities.\n")
#cat(" Activity labels: ", act_labels, "\n")

dataLabels <- c('subjects', 'activities', dataLabels)
#cat("labels data frame:\n")
#print(dataLabels)
rm(act_labels);rm(act_fac); rm(act_list); rm(acts); rm(colLabels)

########## PART FOUR ##########
# 4. Appropriately labels the data set with descriptive variable names.
cat('PART FOUR: label columns of data set with variable names\n')
dataLabels <-str_replace_all(dataLabels, "std", "standarddeviation")
dataLabels <-str_replace_all(dataLabels, "mean", "meanaverage")
dataLabels <-str_replace_all(dataLabels, "mad", "medianabsolutedeviation")
dataLabels <-str_replace_all(dataLabels, "sma", "signal-magnitudearea")
dataLabels <-str_replace_all(dataLabels, "iqr", "interquartilerange")
dataLabels <-str_replace_all(dataLabels, "arC", "autorregressc")
dataLabels <-str_replace_all(dataLabels, "[()]", "")
dataLabels <-str_replace_all(dataLabels, "-", "")
dataLabels <-str_replace_all(dataLabels, ",", "-")
dataLabels <-str_replace_all(dataLabels, "BodyBody", "BBody")
#dataLabels <-str_replace_all(dataLabels, "[a-zA-Z+-]", "")
names(data) <- dataLabels
#rm(meanData);rm(stdData); rm(smallData); rm(meanCols); rm(stdCols); rm(shortLabels)
print(dataLabels)


########## PART FIVE ##########
# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject
cat("PART FIVE: Creates a second, independent tidy data set with the average of each variable for each activity and each subject\n")
data$subjects <- factor(data$subjects)
data$activities <- factor(data$activities)
trimdata<- data[, 3:563]
meandata <- aggregate(trimdata[, ], by = list(Subjects = data$subjects, Activities = data$activities), mean, simplify = TRUE)
write.table(meandata, file = "tidyMeansData.txt", append = FALSE, quote = TRUE, sep = " ", eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE, qmethod = c("escape", "double"), fileEncoding = "")

######## Clean Global Environment ########
rm(list = ls())
print("The tidy data file name 'tidyMeansData.txt' has been created in your workspace")

