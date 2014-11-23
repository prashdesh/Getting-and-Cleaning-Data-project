

setwd("c:/users/prash_000/Documents/Coursera/Getting_cleaning_data/project/") 
# read the training set from csv file
 trainingSet = read.csv("UCI HAR Dataset/train/X_train.txt", sep="", header=FALSE) 
# A 561-feature vector with time and frequency domain variables. 

# read y train in 562
trainingSet[,562] = read.csv("UCI HAR Dataset/train/Y_train.txt", sep="", header=FALSE) 
# read sample_Train in 563 
trainingSet[,563] = read.csv("UCI HAR Dataset/train/subject_train.txt", sep="", header=FALSE) 
 

# read into testing vector
 testingSet = read.csv("UCI HAR Dataset/test/X_test.txt", sep="", header=FALSE) 
# read y train in 562 
testingSet[,562] = read.csv("UCI HAR Dataset/test/Y_test.txt", sep="", header=FALSE) 
# read y train in 562
testingSet[,563] = read.csv("UCI HAR Dataset/test/subject_test.txt", sep="", header=FALSE) 
 

# read activity labels
 activityLabels = read.csv("UCI HAR Dataset/activity_labels.txt", sep="", header=FALSE) 
 

 # Read features and make the feature names better suited for R with some substitutions 
 features = read.csv("UCI HAR Dataset/features.txt", sep="", header=FALSE) 
 features[,2] = gsub('-mean', 'Mean', features[,2]) 
 features[,2] = gsub('-std', 'Std', features[,2]) 
 features[,2] = gsub('[-()]', '', features[,2]) 
 

 # Merge training and test sets together into one
 MergedData = rbind(trainingSet, testingSet) 
 

 # Get only the data on mean and std. dev. 
 selectedColumns <- grep(".*Mean.*|.*Std.*", features[,2]) 
 # First reduce the features table to what we want 
 features <- features[selectedColumns,] 
 # Now add the last two columns (subject and activity) 
 selectedColumns <- c(selectedColumns, 562, 563) 
 # And remove the unwanted columns from MergedData 
 MergedData <- MergedData[,selectedColumns] 
 # Add the column names (features) to MergedData 
 colnames(MergedData) <- c(features$V2, "Activity", "Subject") 
 colnames(MergedData) <- tolower(colnames(MergedData)) 
 

 currentActivity = 1 
 for (currentActivityLabel in activityLabels$V2) { 
   MergedData$activity <- gsub(currentActivity, currentActivityLabel, MergedData$activity) 
   currentActivity <- currentActivity + 1 
 } 
 

 MergedData$activity <- as.factor(MergedData$activity) 
 MergedData$subject <- as.factor(MergedData$subject) 
 

 tidy = aggregate(MergedData, by=list(activity = MergedData$activity, subject=MergedData$subject), mean) 
 # Remove the subject and activity column, since a mean of those has no use 
 tidy[,90] = NULL 
 tidy[,89] = NULL 
 write.table(tidy, "tidy.txt", sep="\t") 


