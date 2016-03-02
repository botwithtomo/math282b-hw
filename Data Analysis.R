# load packages
library(data.table)
library(dplyr)

# read data
x_train = fread("./UCI HAR Dataset/train/X_train.txt")
y_train = fread("./UCI HAR Dataset/train/y_train.txt")
x_test = fread("./UCI HAR Dataset/test/X_test.txt")
y_test = fread("./UCI HAR Dataset/test/y_test.txt")
variable = fread("./UCI HAR Dataset/features.txt")
test_subject = fread("./UCI HAR Dataset/test/subject_test.txt")
train_subject = fread("./UCI HAR Dataset/train/subject_train.txt")

#set names for our data
names(x_train) = variable$V2
names(x_test)= variable$V2
names(x_train) =tolower(names(x_train))
names(x_test) = tolower(names(x_test))
names(train_subject) = "subject"
names(test_subject) ="subject"
names(y_train) ="activity"
names(y_test) ="activity"

# find out which data variable contains mean or std
data11 = select(x_train,contains("mean"))
data12 = select(x_train,contains("std"))
data21 = select(x_test,contains("mean"))
data22 = select(x_test,contains("std"))

# combine our training and test
train = cbind(train_subject,y_train,data11,data12)
test = cbind(test_subject,y_test,data21,data22)
total = rbind(train,test)

# sub 1-6 with our descriptive variable 
total$activity = gsub(1,"WALKING",total$activity)
total$activity = gsub(2,"WALKING_UPSTAIRS",total$activity)
total$activity = gsub(3,"WALKING_DOWNSTAIRS",total$activity)
total$activity = gsub(4,"SITTING",total$activity)
total$activity = gsub(5,"STANDING",total$activity)
total$activity = gsub(6,"LAYING",total$activity)

# make our variable names look better
names(total) = gsub("\\()","",names(total))
names(total) = gsub("mean","Mean",names(total))
names(total) = gsub("std","STD",names(total))
names(total) = gsub("-x$","-X",names(total))
names(total) = gsub("-y$","-Y",names(total))
names(total) = gsub("-z$","-Z",names(total))

# get independent tidy data set with the average of each variable for each activity and each subject.
# there should be a better and quicker way to do it, but I come up with this one
# this one is pretty cool tho

	bysummary_subject = by(total,total$subject,summary)
	subject_mean = data.frame()
	for (i in 1:length(bysummary_subject)){
		subject_mean[i,1]=names(bysummary_subject)[i]
		for(m in 3:88) {	
			temp=bysummary_subject[[i]][4,m]
			temp=strsplit(temp,":")
			temp=as.numeric(temp[[1]][2])
			subject_mean[i,m-1] = temp
		}
	}
names(subject_mean) =c("Activity/Subject",names(total)[-c(1,2)])

	bysummary_activity = by(total,total$activity,summary)
	activity_mean = data.frame()
	for (i in 1:length(bysummary_activity)){
		activity_mean[i,1]=names(bysummary_activity)[i]
		for (m in 3:88){
			temp = bysummary_activity[[i]][4,m]
			temp=strsplit(temp,":")
			temp=as.numeric(temp[[1]][2])
			activity_mean[i,m-1]=temp
		}
	}
names(activity_mean) =c("Activity/Subject",names(total)[-c(1,2)])
data= rbind(activity_mean,subject_mean)
write.table(total,sep="\t",file="rawdata.txt")
write.table(data,sep="\t",file="tidydata.txt")