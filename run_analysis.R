library(plyr)
library(dplyr)
library(reshape2)


#read features
features<-read.csv('features.txt',sep = " ", header = FALSE, stringsAsFactors = FALSE)
str(features)
#create column names based on features$V2
col_names<-as.list(features$V2)
length(col_names)

#create list for column names that contains 'mean' and 'std'
#will be used to extract columns containing mean and std
mean_names<-col_names[grep('mean()', col_names)]
std_names<-col_names[grep('std()', col_names)]
mean_std_names<-as.character(append(mean_names,std_names))

#read activity label 
activity_label<-read.table('activity_labels.txt', header = FALSE, stringsAsFactors = FALSE,
                           blank.lines.skip = TRUE)

#add column names for activity label. It will be useful for joining
colnames(activity_label)<-c('activity_id', 'activity_label')

###Compiling test data
#read X_test - comprised of the recorded data
X_test<-read.table('test/X_test.txt', header = FALSE, stringsAsFactors = FALSE, 
                            blank.lines.skip = TRUE)
#give column names for X_test
colnames(X_test)<-c(col_names)
col_match<-match(mean_std_names, names(X_test))
X_test<-X_test[,col_match]
#read Y_test - comprised of activity id
Y_test<-read.table('test/Y_test.txt', header = FALSE, stringsAsFactors = FALSE, 
                   blank.lines.skip = TRUE)
#provide column name activity_id
colnames(Y_test)<-'activity_id'

#read subject_test - comprised of subject information
subject_test<-read.table('test/subject_test.txt', header = FALSE, stringsAsFactors = FALSE, 
                   blank.lines.skip = TRUE)
#provide column name subject
colnames(subject_test)<-'subject'

###binding and joining various tables
##since data is binded according to rows, it is important to maintain original order
##when combining data
#column bind subject_test, Y_test
meta_data_test<-cbind(subject_test, Y_test)
#join is used because it maintains original order versus merge.
meta_data_test<-join(meta_data_test, activity_label, by = 'activity_id', type = 'left')
#final column bind of test_data_1 and X_test
test_data_final<-cbind(meta_data_test, X_test)


#read x_train data
X_train<-read.table('train/X_train.txt', header = FALSE, stringsAsFactors = FALSE, 
                   blank.lines.skip = TRUE)
#add column names to x_train
colnames(X_train)<-c(col_names)
col_match<-match(mean_std_names, names(X_train))
X_train<-X_train[,col_match]

#read y_train data
Y_train<-read.table('train/Y_train.txt', header = FALSE, stringsAsFactors = FALSE, 
                    blank.lines.skip = TRUE)
#add column name to y_train data
colnames(Y_train)<-'activity_id'

#read subject train data
subject_train<-read.table('train/subject_train.txt', header = FALSE, stringsAsFactors = FALSE, 
                          blank.lines.skip = TRUE)
#add column names to subject
colnames(subject_train)<-'subject'

#column bind subject and y_train data
meta_data_train<-cbind(subject_train, Y_train)

#use join to join meta_data_train and activity label
meta_data_train<-join(meta_data_train, activity_label, by = 'activity_id', type = 'left')

#cbind metadata with x_train data
train_data_final<-cbind(meta_data_train,X_train)

#final rowbind to combine train and test data
final_data<-rbind(test_data_final, train_data_final)

final_data$activity_id<-factor(final_data$activity_id, levels = activity_label$activity_id, labels = activity_label$activity_label)
final_data$activity_label<-NULL
final_data$subject<-as.factor(final_data$subject)

final_data.melted<-melt(final_data, id = c('subject', 'activity_id'))
final_data.mean<-dcast(final_data.melted, subject + activity_id ~ variable , mean)

write.table(final_data.mean, "tidy.txt", row.names = FALSE, quote = FALSE)

