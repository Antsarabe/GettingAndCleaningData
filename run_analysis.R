#Downloading files
data_url<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(data_url,destfile="./project_dataset.zip")
unzip(zipfile = "project_dataset.zip")

#Step 1: Merging the training and the test sets to create one data set

#Read the training data set
X_train <- read.table("./UCI HAR Dataset/train/X_train.txt")
Y_train <- read.table("./UCI HAR Dataset/train/Y_train.txt")
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")

#Reading the test data set
Y_test <- read.table("./UCI HAR Dataset/test/Y_test.txt")
X_test <- read.table("./UCI HAR Dataset/test/X_test.txt")
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")

#Combine all datas
X <- rbind(X_train,X_test)
Y <- rbind(Y_train,Y_test)
subject <- rbind(subject_train,subject_test)

#Merge the training and the test sets
features<-read.table("./UCI HAR Dataset/features.txt")
names(X) <- features[,2]
names(Y)<-"Activity"
names(subject)<-"Subject"
merged_data<-cbind(subject,Y,X)

#Step 2: Extracting only the measurements on the mean and the standard deviation for each measurement
#Select the columns with a header containing "mean" or "std" and select only those columns in a new data set

col_names <- names(merged_data)[grep("Subject|Activity|mean..$|std..$",names(merged_data))]
extracted_data<-merged_data[col_names]

#Step 3: Using descriptive activity names to name the activitiey in the data set

activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")
names(activity_labels)<-c("Activity_id","Activity")
extracted_data[["Activity"]] <- factor(extracted_data$Activity, 
                                          levels = activity_labels[["Activity_id"]] , 
                                          labels = activity_labels[["Activity"]])

#Step 4: Appropriately labelling the data set with descriptive variable names
#Create a function
cleanColnames <- function(x) {
      x <- gsub('^(t)', 'time', x)
      x <- gsub('^(f)', 'freq', x)
      x <- gsub('-mean\\()$', 'Mean', x)
      x <- gsub('-std\\()$', 'StdDev', x)
      x <- gsub('BodyBody', 'Body', x)
      x <- gsub('Mag', 'Magnitude', x)
}
names(extracted_data)<-sapply(col_names, cleanColnames)

#Step 5: Creating a second, independent tidy data set 
#with the average of each varable for each activity and each subject
library(dplyr)
final_tidy_data <- extracted_data %>%
       group_by(Subject, Activity) %>%
       summarise_all(funs(mean))
write.table(final_tidy_data, '~/UCI HAR Dataset/tidy_data.csv',row.names=FALSE,sep=",")

