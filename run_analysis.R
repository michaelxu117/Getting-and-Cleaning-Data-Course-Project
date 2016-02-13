
## 1. Merge the training and the test sets to create one data set

setwd('/Users/michael_xu/Documents/UCI HAR Dataset/')  

features <- read.table('./features.txt',header=FALSE)
activity_labels <- read.table('./activity_labels.txt',header=FALSE)
subject_train <- read.table('./train/subject_train.txt',header=FALSE)
x_train <- read.table('./train/x_train.txt',header=FALSE)
y_train <- read.table('./train/y_train.txt',header=FALSE)


colnames(activity_labels) <- c('activityId','activityType')
colnames(subject_train) <- "subjectId"
colnames(x_train) <- features[,2]
colnames(y_train) <- "activityId"


training <- cbind(y_train,subject_train,x_train)

subject_test <- read.table('./test/subject_test.txt',header=FALSE)
x_test <- read.table('./test/x_test.txt',header=FALSE)
y_test <- read.table('./test/y_test.txt',header=FALSE)

colnames(subject_test) <- "subjectId"
colnames(x_test) <- features[,2] 
colnames(y_test) <- "activityId"

test <- cbind(y_test,subject_test,x_test) 


training_test <- rbind(training,test)


colNames <- colnames(training_test)


## 2. Extracts only the measurements on the mean and standard deviation for each measurement

logical_vector <- (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames));

final_data <- training_test[logical_vector==TRUE]



## 3. Uses descriptive activity names to name the activities in the data set

final_data <- merge(final_data,activity_labels,by='activityId',all.x=TRUE)

colNames <- colnames(final_data)



# 4. Appropriately labels the data set with descriptive variable name 

for (i in 1:length(colNames)) 
  {
    colNames[i] <- gsub("\\()","",colNames[i])
    colNames[i] <- gsub("-std$","StdDev",colNames[i])
    colNames[i] <- gsub("-mean","Mean",colNames[i])
    colNames[i] <- gsub("^(t)","time",colNames[i])
    colNames[i] <- gsub("^(f)","freq",colNames[i])
    colNames[i] <- gsub("([Gg]ravity)","Gravity",colNames[i])
    colNames[i] <- gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
    colNames[i] <- gsub("[Gg]yro","Gyro",colNames[i])
    colNames[i] <- gsub("AccMag","AccMagnitude",colNames[i])
    colNames[i] <- gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
    colNames[i] <- gsub("JerkMag","JerkMagnitude",colNames[i])
    colNames[i] <- gsub("GyroMag","GyroMagnitude",colNames[i])
  }


colnames(final_data) <- colNames


## 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject 

finalDataNoActivityType <- final_data[,names(final_data) != 'activityType'];

data2 <- aggregate(finalDataNoActivityType[,names(finalDataNoActivityType) != c('activityId','subjectId')],by=list(activityId=finalDataNoActivityType$activityId,subjectId = finalDataNoActivityType$subjectId),mean);

data2 <- merge(data2,activity_labels,by='activityId',all.x=TRUE);

write.table(data2, './data2.txt',row.names=TRUE,sep='\t')
