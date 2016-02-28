##Set working directory
setwd("C:/Users/Peter/Google Drive/Documents/Coursera/Data Science/03_Getting&CleaningData/Assignment")

##download file
if(!file.exists("data.zip")) {
fileurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileurl, destfile = "data.zip", method = "auto")
unzip("data.zip")}

##function to move a variable to the first column of the dataframe
reorder <- function(df, var) {
  col_idx <- grep(var, names(df))
  df[, c(col_idx, (1:ncol(df))[-col_idx])]} 

## read in lable and subject id dataframes
features_list <- read.table("./UCI HAR Dataset/features.txt")
activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")
activity_train_labels <- read.table("./UCI HAR Dataset/train/y_train.txt")
subject_train_id <- read.table("./UCI HAR Dataset/train/subject_train.txt")
activity_test_labels <- read.table("./UCI HAR Dataset/test/y_test.txt")
subject_test_id <- read.table("./UCI HAR Dataset/test/subject_test.txt")

##read in training and test dataframes
training_set <- read.table("./UCI HAR Dataset/train/X_train.txt")
test_set <- read.table("./UCI HAR Dataset/test/X_test.txt")


##only keep mean() and std() variables
varstokeep <- grep("\\bmean()\\b|\\bstd()\\b", features_list$V2)
training_set <- training_set[,varstokeep]
test_set <- test_set[,varstokeep]
features_list <- features_list[varstokeep,]


## Change the column names of the training & test data to those of the features list
list <- as.vector(features_list[,2])
colnames(training_set) <- list
colnames(test_set) <- list

## Add the activty type numbers to the training and test datasets
list <- as.vector(activity_train_labels[,1])
training_set$activity_train_labels = list
list <- as.vector(activity_test_labels[,1])
test_set$activity_test_labels = list

## Merge activty lables onto the new activty type number variable in both datasets
## Also create an order variable to return the row order after merging
list <- c(1:7352)
training_set$order = list
list <- c(1:2947)
test_set$order = list

training_set <- merge(training_set, activity_labels, by.x = "activity_train_labels" , by.y = "V1")
test_set <- merge(test_set, activity_labels, by.x = "activity_test_labels" , by.y = "V1")

##reorder and rename the activity lables variable, and delete the activity number variable
training_set <- training_set[order(training_set$order),]
test_set <- test_set[order(test_set$order),]
training_set <- reorder(training_set, "V2")
test_set <- reorder(test_set, "V2")
names(training_set)[names(training_set)=="V2"] <- "activity_type"
names(test_set)[names(test_set)=="V2"] <- "activity_type"
training_set$activity_train_labels <- NULL
test_set$activity_test_labels <- NULL

## Add the subject id varaible to both datasets
list <- as.vector(subject_train_id[,1])
training_set$subject_id = list
training_set <- reorder(training_set, "subject_id")
list <- as.vector(subject_test_id[,1])
test_set$subject_id = list
test_set <- reorder(test_set, "subject_id")

##clear up work envioment
rm(activity_test_labels,activity_train_labels, activity_labels, features_list, subject_test_id, subject_train_id, list, varstokeep)

## append test and training datasets 

full_data <- rbind(training_set, test_set)
full_data$order <- NULL
full_data <- full_data[order(full_data$subject_id),]

## Clean up the variable names to remove "()" and replace "-" with "_"
varnames <- gsub("\\(|\\)","",names(full_data))
varnames <- gsub("-","_",varnames)
colnames(full_data) <- varnames

## collpase dataset
library(doBy)
varnames <- as.vector(names(full_data))
varnames <- varnames[3:68]
collapse1 <- summaryBy(list(varnames, c("subject_id", "activity_type")),  data=full_data)
rm(varnames)

## Export the datasets
if(!file.exists("./Clean_Data")) {dir.create("./Clean_Data")}

write.table(full_data, "./Clean_data/clean_dataset.txt")
write.table(collapse1, "./Clean_data/clean_dataset_summary.txt")

library(openxlsx)
wb <- createWorkbook()
addWorksheet(wb, "clean data summary")
addWorksheet(wb, "clean data full")
writeData(wb, "clean data summary", collapse1)
writeData(wb, "clean data full", full_data)

saveWorkbook(wb, file = "./Clean_data/clean_dataset.xlsx", overwrite = TRUE)



