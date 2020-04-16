library(dplyr)

# read test data set
# for the read commands to work, you need the test and train folders in your wd
X_test<-read.delim(paste0(getwd(),"/test/X_test.txt"), header = FALSE, sep = "")
Y_test<-read.delim(paste0(getwd(),"/test/Y_test.txt"), header = FALSE)
subject_test<-read.delim(paste0(getwd(),"/test/subject_test.txt"), header = FALSE)

#read train data set
X_train<-read.delim(paste0(getwd(),"/train/X_train.txt"), header = FALSE, sep = "")
Y_train<-read.delim(paste0(getwd(),"/train/Y_train.txt"), header = FALSE)
subject_train<-read.delim(paste0(getwd(),"/train/subject_train.txt"), header = FALSE)

#read table variables and activity labels
variables<-read.delim("features.txt", header = FALSE, sep = "")
activity<- read.delim("activity_labels.txt", header = FALSE, sep = "")




# 1. Merges the training and the test sets to create one data set.
X<-rbind(X_test,X_train)
Y<-rbind(Y_test,Y_train)
subject<-rbind(subject_test,subject_train)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
m_stdev_var_names<-variables[grep("mean[^a-z,A-z]|std", variables$V2),]
X<-X[,m_stdev_var_names$V1]

# 3. Uses descriptive activity names to name the activities in the data set
Y$activity_label<-factor(Y$V1, labels = as.character(activity$V2))

# 4. Appropriately labels the data set with descriptive variable names.
names(X)<-m_stdev_var_names$V2

# 5. From the data set in step 4, creates a second, independent tidy data set with the 
#average of each variable for each activity and each subject.
names(subject)<-"subject"
joint<-cbind(subject$subject,Y$activity_label,X)
tidy_mean<- joint%>%
  group_by(subject$subject,Y$activity_label) %>%
  summarise_each(funs(mean))
names(tidy_mean)[1:2]<-c("subject", "activity")

write.table(tidy_mean, "tidy_data.txt", row.names = FALSE)

