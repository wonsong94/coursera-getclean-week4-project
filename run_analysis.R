## IMPORT LIBRARY
library(dplyr)
## SET DIRECTORY FOR DATASET
setwd("/Users/songmichael/Desktop/Coursera/part3_w4/final/final/coursera-getclean-week4-project/UCI HAR Dataset")

## DEFINE FUNCTION FOR READING FILES
get_data <- function(name) {
  X <- read.table(paste("./", name, "/X_", name, ".txt", sep=''))
  ## Extracts only the measurements on the mean and standard deviation for each measurement.
  body_acc_x <- read.table(paste("./", name, "/Inertial Signals/body_acc_x_", name, ".txt", sep="")) %>% get_mean_sd()
  body_acc_y <- read.table(paste("./", name, "/Inertial Signals/body_acc_y_", name, ".txt", sep="")) %>% get_mean_sd()
  body_acc_z <- read.table(paste("./", name, "/Inertial Signals/body_acc_z_", name, ".txt", sep="")) %>% get_mean_sd()
  
  body_gyro_x <- read.table(paste("./", name, "/Inertial Signals/body_gyro_x_", name, ".txt", sep="")) %>% get_mean_sd()
  body_gyro_y <- read.table(paste("./", name, "/Inertial Signals/body_gyro_y_", name, ".txt", sep="")) %>% get_mean_sd()
  body_gyro_z <- read.table(paste("./", name, "/Inertial Signals/body_gyro_z_", name, ".txt", sep="")) %>% get_mean_sd()
  
  total_acc_x <- read.table(paste("./", name, "/Inertial Signals/total_acc_x_", name, ".txt", sep="")) %>% get_mean_sd()
  total_acc_y <- read.table(paste("./", name, "/Inertial Signals/total_acc_y_", name, ".txt", sep="")) %>% get_mean_sd()
  total_acc_z <- read.table(paste("./", name, "/Inertial Signals/total_acc_z_", name, ".txt", sep="")) %>% get_mean_sd()
  
  subject_train <- read.table(paste("./", name, "/subject_", name, ".txt", sep=''))
  
  y <- read.table(paste("./", name, "/y_", name, ".txt", sep=''))
  
  ## SET EACH RESULT TO MATRIX AND RETURN A LIST OF ALL VARIABLES
  output <- list(as.matrix(subject_train), as.matrix(y),
                 body_acc_x, body_acc_y, body_acc_z,
                 body_gyro_x, body_gyro_y, body_gyro_z,
                 total_acc_x, total_acc_y, total_acc_z,
                 as.matrix(X))
}

## DEFINE FUNCTION FOR CALCULATING MEAN AND STANDARD DEVIATION
get_mean_sd <- function(x) {
  cbind(apply(x, 1, mean), apply(x, 1, sd))
}

## LOAD TRAIN DATASET (output = list of all data)
train_data <- get_data("train")
test_data <- get_data("test")

## TASK 1: MERGE TRAINING AND TEST DATA SET INTO ONE DATA SET
merged_data <- mapply(function(x,y) {rbind(x, y)}, train_data, test_data)

## MERGE ALL DATA INTO ONE SINGLE DATA SET
final <- Reduce(function (x, y) cbind(x, y), merged_data)

## NAME ALL VALUES TO DESCRIPTIVE VARIABLES
## ALL DESCRIPTIVE NAMES ARE SAVED IN feature_updated.txt
colnames(final) <- as.vector(read.table("feature_updated.txt")[2])$V2

write.table(head(final, 300), file='tidy_data.txt', row.names = FALSE)
