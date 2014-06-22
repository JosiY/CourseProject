# setwd("C:/Users/Josephine/Desktop/Getting and Cleaning Data")

# Read original txt files
subjectTest <- read.table("./wearable_data/UCI HAR Dataset/test/subject_test.txt", sep ="", header = TRUE)
xTest <- read.table("./wearable_data/UCI HAR Dataset/test/X_test.txt", sep ="", header = TRUE)
yTest <- read.table("./wearable_data/UCI HAR Dataset/test/y_test.txt", sep ="", header = TRUE)

subjectTrain <- read.table("./wearable_data/UCI HAR Dataset/train/subject_train.txt", sep =",", header = TRUE)
xTrain <- read.table("./wearable_data/UCI HAR Dataset/train/X_train.txt", sep ="", header = TRUE)
yTrain <- read.table("./wearable_data/UCI HAR Dataset/train/y_train.txt", sep ="", header = TRUE)


# read features" and activity_labels txt files
features <- read.table("./wearable_data/UCI HAR Dataset/features.txt", sep ="", header = TRUE)
actLabels <- read.table("./wearable_data/UCI HAR Dataset/activity_labels.txt", sep ="", header = TRUE)

# rename the names of actLabels
names(actLabels) <-c("labels","activity")
# create one new row 
newrow  <- data.frame(labels= 1,activity= "WALKING")
# combine the new row and actLabels to form a complete actLabel dataset
actLabels <- rbind(newrow, actLabels)
# renames xTest and xTrain txt files with row names of features
names(xTest)  <- features[,2]
names(xTrain) <- features[,2]

# column bind the subjectTest, yTest, xTest
testSubYX <- cbind(subjectTest, yTest, xTest)

# renames first and second cols in testSubYX and trainSubYX as "subject" and "labels"
names(testSubYX)[1:2] <- c("subject","labels")
trainSubYX <- cbind(subjectTrain, yTrain, xTrain)
names(trainSubYX)[1:2] <- c("subject","labels")

# row bind testSubYX and trainSubYX
comTestTrain <- rbind(testSubYX,trainSubYX)

# remove all features in comTestTrain which do not contain mean values
colsMean <- grep("mean()", names(comTestTrain))
cols_Mean<-c,2(1,colsMean)
dfMean <-comTestTrain[,cols_Mean]

# remove all features in comTestTrain which do not contain std values
colsStd <- grep("std()", names(comTestTrain))
dfStd <-comTestTrain[,colsStd]

# column bind dfMean and dfStd
dfMeanStd <-cbind(dfMean,dfStd)

# merge different datasets
dfMerge<- merge(dfMeanStd,actLabels,by.x="subject", by.y="labels", all=TRUE)

# Reorder columns
cols <- c(1,2,grep("activity", names(dfMerge)))
dfMerge <- dfMerge[, c(1,2,grep("activity", names(dfMerge)),(1:ncol(dfMerge))[-cols])]

# Fill missing entries in activity column
dfMerge$activity <- actLabels[dfMerge$labels,2]

# Remove redundant labels column
dfMerge <- dfMerge[,-2]

# Rename columns
names(dfMerge) <-gsub("\\(\\)", "", names(dfMerge))
names(dfMerge) <-gsub("\\-", "", names(dfMerge))
names(dfMerge) <-gsub("Freq","freq", names(dfMerge))

# aggregate the dataset by subject and activity and calculate average values
aggdata <- aggregate(dfMerge[,-c(1,2)], by=list(dfMerge$subject,dfMerge$activity), FUN=mean)

# rename columns with discriptive labels
theNames <- names(aggdata)
theNames[1:2] = c("subject","activity")
tmp <- unlist(lapply(theNames[-(1:2)], function(x)
        { if (substring(x,1,1) == "t")
          { paste("meanOfTimeDomain", substring(x,2,nchar(x)),"")    
          } else
          { paste("meanOfFrequencyDomain", substring(x,2,nchar(x)),"")          
          }
        }))
tmp <- gsub("BodyBody","Body",gsub(" ", "", tmp))
theNames = c(theNames[1:2],tmp)
names(aggdata) <- theNames

# write output
fileName = "tidyData_run_analysis.txt"
write.table(aggdata,fileName, row.names = FALSE)
