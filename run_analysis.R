### set working directory
getwd()
setwd("C:/Users/user/Desktop/DataScience/Getting and Cleaning Data/P1/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset")
getwd()

#Read the Activity, Subject and mainData files
dataActivityTest  <- read.table("./test/y_test.txt",header = FALSE)
dataActivityTrain <- read.table("./train/y_train.txt",header = FALSE)
dataSubjectTrain <- read.table("./train/subject_train.txt",header = FALSE)
dataSubjectTest  <- read.table("./test/subject_test.txt",header = FALSE)
mainDataTest  <- read.table("./test/X_test.txt",header = FALSE)
mainDataTrain <- read.table("./train/X_train.txt",header = FALSE)

###Merges the training and the test sets to create one data set.

#1. UNION ALL on tables = concatenate by rows
dataSubject<- sqldf("select * from dataSubjectTrain UNION ALL select * from  dataSubjectTest")
names(dataSubject) <- c("subjects")

dataActivity<- sqldf("select * from dataActivityTrain UNION ALL select * from  dataActivityTest")
names(dataActivity) <- c("activities")

mainData<- sqldf("select * from mainDataTrain UNION ALL select * from  mainDataTest")
mainColumnsNames <- read.table("./features.txt",header = FALSE)
names(mainData) <- mainColumnsNames$V2
names(mainData)


#2. merging 3 data sets = merge columns
mainData = cbind(mainData, dataSubject)
mainData = cbind(mainData, dataActivity)

names(mainData)

###Extracts only the measurements on the mean and standard deviation for each measurement. 

# finding columns numbers where names are related to the mean or standard deviation
mainColumnsNamesVec <- grep("-(mean|std)\\(\\)", mainColumnsNames[, 2])
# adding to the vector numbers of two last columns related to the subjects and activities
mainColumnsNamesVec <- append(mainColumnsNamesVec ,c(562,563))
# subsetting the mainData using the vector
mainData <- mainData[,mainColumnsNamesVec]
str(mainData)


###Uses descriptive activity names to name the activities in the data set
#Readinf activity names from “activity_labels.txt”
getwd()
activitiesNames <- read.table("./activity_labels.txt",header = FALSE)
head(activitiesNames)
head(mainData)
tail(mainData)
names(mainData)
# adding column with Activities names
mainData <- sqldf("select * FROM mainData LEFT OUTER JOIN activitiesNames ON mainData.activities = activitiesNames.V1") 

# removing redundant columns related to the activities
mainData = mainData[,c(1:(length(mainData)-3),length(mainData))]
names(mainData) <- replace(names(mainData), length(names(mainData)), 'activities')
head(mainData)
tail(mainData)

###Appropriately labels the data set with descriptive variable names. 

#Replacing: Mag -> Magnitude, Acc -> Accelerometer, Gyro -> Gyroscope, prefix t ->  time, prefix f -> frequency
names(mainData)<-gsub("Mag", "Magnitude", names(mainData))
names(mainData)<-gsub("Acc", "Accelerometer", names(mainData))
names(mainData)<-gsub("Gyro", "Gyroscope", names(mainData))

names(mainData)<-gsub("^t", "time", names(mainData))
names(mainData)<-gsub("^f", "frequency", names(mainData))


###From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
library('plyr')
averagesMainData <- ddply(mainData, .(subjects, activities), function(x) colMeans(x[, 1:66]))
write.table(averagesMainData, file = "tidydata.txt",row.name=FALSE)


