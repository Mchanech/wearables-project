require(reshape2)
require(plyr)

run_analysis <- function() {
    
    ## Read in the data, assuming 'UCI HAR Dataset' is in the working directory
    
    trainData <- read.table("UCI\ HAR\ Dataset//train//X_train.txt")
    testData <- read.table("UCI\ HAR\ Dataset//test//X_test.txt")
    
    data <- rbind(trainData,testData) # Merge data
    
    trainSubj <- read.table("UCI\ HAR\ Dataset//train//subject_train.txt")
    testSubj <- read.table("UCI\ HAR\ Dataset//test//subject_test.txt")
    
    subjects <- rbind(trainSubj,testSubj) # Merge subject lists
    
    trainActivity <- read.table("UCI\ HAR\ Dataset//train//y_train.txt")
    testActivity <- read.table("UCI\ HAR\ Dataset//test//y_test.txt")
    
    
    ## Merge activity lists and assign labels 
    activities <- rbind(trainActivity,testActivity) 
    activityLabels <- read.table("UCI\ HAR\ Dataset//activity_labels.txt",sep=" ")
    for (i in 1:length(activities)) {
        activities[i] <- activityLabels[i,2]
    }
    
    
    ## Align data with subject and activity lists, then label columns
    allData <- cbind(data, subjects, activities)
    names(allData) <- read.table("UCI\ HAR\ Dataset//features.txt",sep=" ")[,2]
    names(allData)[length(names(allData))-1] <- "SubjectID"
    names(allData)[length(names(allData))] <- "Activity"
    
    
    ## Extract only measurements involving means and standard deviations
    justMeansAndStDevs <- allData[, grep("[Mm]ean|[Ss]td", colnames(allData))]
    justMeansAndStDevs <- justMeansAndStDevs[, grep("^[^a]", colnames(justMeansAndStDevs))]
    
    
    ## Move Subject ID and Activity columns to the left for readability
    justMeansAndStDevs <- cbind(allData$SubjectID, allData$Activity, justMeansAndStDevs)
    
    
    ## Label the columns with descriptive variable names
    names(justMeansAndStDevs)[1] <- "SubjectID"
    names(justMeansAndStDevs)[2] <- "Activity"
    names(justMeansAndStDevs) <- gsub("^f", "Fourier", names(justMeansAndStDevs))
    names(justMeansAndStDevs) <- gsub("^t", "Time", names(justMeansAndStDevs))
    names(justMeansAndStDevs) <- gsub("\\(\\)", "", names(justMeansAndStDevs))
    names(justMeansAndStDevs) <- gsub("-std", "StdDev", names(justMeansAndStDevs))
    names(justMeansAndStDevs) <- gsub("-mean", "Mean", names(justMeansAndStDevs))
    names(justMeansAndStDevs) <- gsub("\\-", "", names(justMeansAndStDevs))
    names(justMeansAndStDevs) <- gsub("BodyBody", "Body", names(justMeansAndStDevs))
    names(justMeansAndStDevs) <- gsub("Acc", "Accel", names(justMeansAndStDevs))
    
    
    ## Create a data frame with all of the averages per subject
    ## and a data frame with all of the averages per activity
    subjectAverages <- data.frame()
    activityAverages <- data.frame()
    for (i in 3:length(names(justMeansAndStDevs))) {
        subjectAverages <- tapply(justMeansAndStDevs[,i], justMeansAndStDevs$SubjectID, mean)
    }
    for (i in 3:length(names(justMeansAndStDevs))) {
        activityAverages <- tapply(justMeansAndStDevs[,i], justMeansAndStDevs$Activity, mean)
    }
    allAverages <- list(subjectAverages, activityAverages)
}
