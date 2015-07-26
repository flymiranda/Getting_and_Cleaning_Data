# Here are the data for the project: 
  
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
# Download the file. Then put it in your local folder. for example, it is "c:\rstudio"
# unzip the file. then setworkdirectory to "c:\rstudio\UCI HAR Dataset"
# You should create one R script called run_analysis.R that does the following. 
# 1. Merges the training and the test sets to create one data set.
tbl_xtrain <- read.table("train/X_train.txt")
tbl_xtest <- read.table("test/X_test.txt")
dtXtrain <- rbind(tbl_xtrain, tbl_xtest)

tbl_strain <- read.table("train/subject_train.txt")
tbl_stest <- read.table("train/subject_test.txt")
dtSubject <- rbind(tbl_strain, tbl_stest)

tbl_ytrain <- read.table("train/y_train.txt")
tbl_ytest <- read.table("test/y_test.txt")
dtYtrain <- rbind(tbl_ytrain, tbl_ytest)
# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
dtFeatures <- read.table("features.txt")
indices_features <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])
X <- X[, indices_features]
names(X) <- features[indices_features, 2]
names(X) <- gsub("\\(|\\)", "", names(X))
names(X) <- tolower(names(X))

# 3. Uses descriptive activity names to name the activities in the data set
dtActivities <- read.table("activity_lables.txt")
dtActivities[, 2] = gsub("_", "", tolower(as.character(dtActivities[, 2])))
dtYtrain[,1] = dtActivities[dtYtrain[,1], 2]
names(dtActivities) <- "activity"
# 4. Appropriately labels the data set with descriptive variable names. 
names(dtSubject) <- "subject"
data <- cbind(dtSubject, dtYtrain, dtXtrain)
write.table(cleaned, "merged_clean_data.txt")
# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
uSubjects = unique(dtSubject)[,1]
lengthSubjects = length(uSubjects)
lengthActivities = length(dtActivities[,1])
cols = dim(cleaned)[2]
ret = cleaned[1:(lengthSubjects * lengthActivities), ]

row = 1
for (sub in 1:lengthSubjects) {
  for (ac in 1:lengthActivities) {
    ret[row, 1] = uSubjects[sub]
    ret[row, 2] = dtActivities[ac, 2]
    x <- cleaned[cleaned$sbuject== sub & cleaned$activity == dtActivities[ac, 2],]
    ret[row, 3:cols] <- colMeans(x[, 3:cols])
    row = row + 1
  }
}
write.table(ret, "data_set_with_the_average.txt")