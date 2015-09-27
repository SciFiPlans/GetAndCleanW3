#Reading train and test data from files

file1 <- "UCI HAR Dataset/train/X_train.txt"
file2 <- "UCI HAR Dataset/test/X_test.txt"
traindata <- read.table(file1)
testdata <- read.table(file2)

#Merging the dataframes by adding the rows
mergedd <- rbind(traindata,testdata)

#Mean and standard deviation of each column
meanmd <- apply(mergedd,2,mean)
sdmd   <- apply(mergedd,2,sd)

#Reading the codes for the activities
file11 <- "UCI HAR Dataset/train/y_train.txt"
file21 <- "UCI HAR Dataset/test/y_test.txt"
trainactivities <- read.table(file11)
testactivities  <- read.table(file21)

#Merging them in a single df same order as the data
mergedactivities <- rbind(trainactivities,testactivities)

#Creating an empty character vector
act <- vector(mode = "character", length = dim(mergedactivities)[[1]])

#Filling the vector with the descriptive names for the activities
for(i in 1:dim(mergedactivities)[[1]]){
	if(mergedactivities[i,1] == 1) {
		act[i] <- "WALKING"
	} else if(mergedactivities[i,1] == 2) {
		act[i] <- "WALKING_UPSTAIRS"
	} else if(mergedactivities[i,1] == 3) {
		act[i] <- "WALKING_DOWNSTAIRS"
	} else if(mergedactivities[i,1] == 4) {
		act[i] <- "SITTING"
	} else if(mergedactivities[i,1] == 5) {
		act[i] <- "STANDING"
	} else if(mergedactivities[i,1] == 6) {
		act[i] <- "LAYING"
	} else {
		act[i] <- NA
	}
}

#Adding the vector as a new column to the data df
mergedd$Activity <- as.factor(act)
mergedd$ActivityCode <- mergedactivities$V1

#Read the names of the columns for labelling
file0 <- "UCI HAR Dataset/features.txt"
labels <- read.table(file0)

#Change the names of the columns
colnames(mergedd) <- c(as.character(labels[,"V2"]),"Activity","ActivityCode")


#Reading the codes for subjects
file12 <- "UCI HAR Dataset/train/subject_train.txt"
file22 <- "UCI HAR Dataset/test/subject_test.txt"
trainsubject <- read.table(file12)
testsubject  <- read.table(file22)

#Merging them in a single df same order as the data
mergedsubjects <- rbind(trainsubject,testsubject)

#Adding it to the data
mergedd$Subject <- mergedsubjects

#Creating an empty dataframe preserving labels
newdata <- mergedd[mergedd$Activity == 1,]

for(j in 1:max(mergedactivities)) { #Activities
	for(k in 1:max(mergedsubjects)) { #Subjects
		dummydf <- mergedd[mergedd$ActivityCode == j & mergedd$Subject == k,]
		dummymeanvect <- colMeans(dummydf[,1:(dim(mergedd)[2]-3)])
		newdata <- rbind(newdata,c(k,j,dummymeanvect))
	}
}

write.table(newdata, "results.txt", row.name = FALSE)

