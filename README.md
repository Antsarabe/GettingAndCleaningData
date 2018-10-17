This is the course project for [Getting and Cleaning Data] course on Coursera. Code is written in R in file. Source file is run_analysis.R. This R script does the following things:

    1/ Download the dataset and unzip all datas
        
    2/ Load all the files of UCI HAR Dataset folder that will be used to load data: 
    test/subject_test.txt 
    test/X_test.txt 
    test/y_test.txt 
    train/subject_train.txt 
    train/X_train.txt 
    train/y_train.txt

    3/ Load activity, subject and feature info and read data from the files into the variables.

    4/ Merge the training and the test sets to create one data set:
        Concatenate the data tables by rows.
        set names to variables.
        Merge columns to get the data frame merged_data for all data.

    5/ Extract only the measurements on the mean and standard deviation for each measurement:
        Subset column names by measurements on the mean and standard deviation.
        Subset the data frame merged_data by selected column names and assign it to a new data frame extracted_data

    6/ Use descriptive activity names to name the activities in the data set:
        Read descriptive activity names from activity_labels.txt
        Factorize variable activity in the data frame extracted_data using descriptive activity names.

    7/ Appropriately labels the data set with descriptive variable names:
        Create a function that substitute:
          ^(t) by 'time'
          ^(f) by 'freq'
          -mean by 'Mean'
          -std by 'StdDev'
          BodyBody by 'Body'
          Mag by 'Magnitude'
        Apply the function to the column names of the data frame extracted_data

    8/ Creates a independent tidy dataset that consists of the average (mean) value of each variable for each subject and activity pair

Final output file is tidydata.txt
