# Final Project: Getting and Cleaning Data
   
   ## 0. Downloading dataset from the given url
   if(!file.exists("./data")){dir.create("./data")}
   fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
   download.file(fileUrl,destfile="./data/Dataset.zip")
   
   # Unzip dataSet to /data directory
   unzip(zipfile="./data/Dataset.zip",exdir="./data")
   
    
 # 1.Merges the training and the test sets to create one data set
    ## Import training data from the given files

   features <- read.table('./data/UCI HAR Dataset/features.txt',header=FALSE)
   activityLabels <- read.table('./data/UCI HAR Dataset/activity_labels.txt',header=FALSE)
   colnames(activityLabels) <- c("activityId","activityType")
   subjectTrain <- read.table('./data/UCI HAR Dataset/train/subject_train.txt',header=FALSE)
   colnames(subjectTrain) <- "subjectId"
   x.Train <- read.table('./data/UCI HAR Dataset/train/x_train.txt',header=FALSE); colnames(x.Train) <- features[,2]
  
   x.Train <- read.table('./data/UCI HAR Dataset/train/x_train.txt',header=FALSE); colnames(x.Train) <- features[,2]
   y.Train <- read.table('./data/UCI HAR Dataset/train/y_train.txt',header=FALSE); colnames(y.Train) <- "activityId"
 
    ##Merging data and importing test data
   trainingSet = cbind(y.Train,subjectTrain,x.Train)
   subjectTest <- read.table('./test/subject_test.txt',header=FALSE)
   colnames(subjectTest) <- "subjectId"
   x.Test <- read.table('./data/UCI HAR Dataset/test/x_test.txt',header=FALSE); colnames(x.Test) <- features[,2]
   y.Test <- read.table('./data/UCI HAR Dataset/test/y_test.txt',header=FALSE); colnames(y.Test) <- "activityId"
   testSet = cbind(y.Test,subjectTest,x.Test)
  
   ##Combine training set into one merged data and creating columns for subsetting
   MergedDataSet = rbind(trainingSet,testSet)
   columns <- colnames(MergedDataSet)
  
  
 # 2. Extract only the measurements on the mean and standard deviation for each measurement
   ## Create a vector that indentifies the ID, mean & stddev
   vector <- (grepl("activity..",columns) | grepl("subject..",columns) | grepl("-mean..",columns) 
              & !grepl("-meanFreq..",columns) & !grepl("mean..-",columns) | 
                                  grepl("-std..",columns) & !grepl("-std()..-",columns))
  
   ##Update MergedDataSet from previous column
    MergedDataSet <- MergedDataSet[vector==TRUE]
  
 # 3. Use descriptive activity names to name the activities in the data set
   ## Adding descriptive activities
   MergedDataSet <- merge(MergedDataSet,activityLabels,by='activityId',all.x=TRUE)
   MergedDataSet$activityId <-activityLabels[,2][match(MergedDataSet$activityId, activityLabels[,1])]
   columns <- colnames(MergedDataSet)
 
 # 4. Appropriately label the data set with descriptive activity names
   ## Making Tidy for column names
     for (i in 1:length(columns)) 
                {
                 columns[i] <- gsub("\\()","",columns[i])                   
                 columns[i] <- gsub("-std$","StdDev",columns[i])
                 columns[i] <- gsub("-mean","Mean",columns[i])
                 columns[i] <- gsub("^(t)","time",columns[i])
                 columns[i] <- gsub("^(f)","freq",columns[i])
                 columns[i] <- gsub("([Gg]ravity)","Gravity",columns[i])
                 columns[i] <- gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",columns[i])
                 columns[i] <- gsub("[Gg]yro","Gyro",columns[i])
                 columns[i] <- gsub("AccMag","AccMagnitude",columns[i])
                 columns[i] <- gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",columns[i])
                 columns[i] <- gsub("JerkMag","JerkMagnitude",columns[i])
                 columns[i] <- gsub("GyroMag","GyroMagnitude",columns[i])
                  }
      colnames(MergedDataSet) <- columns
      MergedDataSet <- MergedDataSet[,names(MergedDataSet) != 'activityType']

 # 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject
     tidyData <- aggregate(MergedDataSet[,names(MergedDataSet) != c('activityId','subjectId')],by=list (activityId=MergedDataSet$activityId, subjectId=MergedDataSet$subjectId),mean)
     write.table(tidyData, './ResultTidyData.txt',row.names=FALSE,sep='\t')
  
 