
# required packages
packages <- c("data.table", "reshape2")
sapply(packages, require, character.only=TRUE, quietly=TRUE)

# set the path for use later in case we need to return
path <- getwd()

# download the data set and create the directory if it doesn't exist yet
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
f <- "Dataset.zip"
download.file(url, file.path(path, f))

# Change this to use unzip
unzip(file.path(path, f))

# set the directory of all the extracted files and list them to get the list
pathIn <- file.path(path, "UCI HAR Dataset")
list.files(pathIn, recursive=TRUE)

# get the test and train subjet information
dtSubjectTrain <- fread(file.path(pathIn, "train", "subject_train.txt"))
dtSubjectTest  <- fread(file.path(pathIn, "test" , "subject_test.txt" ))

# get the test and train activity information
dtActivityTrain <- fread(file.path(pathIn, "train", "y_train.txt"))
dtActivityTest  <- fread(file.path(pathIn, "test" , "y_test.txt" ))

# read in all the data
fileToDataTable <- function (f) {
	df <- read.table(f)
	dt <- data.table(df)
}
dtTrain <- fileToDataTable(file.path(pathIn, "train", "X_train.txt"))
dtTest  <- fileToDataTable(file.path(pathIn, "test" , "X_test.txt" ))

# start combine all the data together with rbind
dtSubject <- rbind(dtSubjectTrain, dtSubjectTest)
setnames(dtSubject, "V1", "subject")
dtActivity <- rbind(dtActivityTrain, dtActivityTest)
setnames(dtActivity, "V1", "activityNum")
dt <- rbind(dtTrain, dtTest)

# merge the data with cbind
dtSubject <- cbind(dtSubject, dtActivity)
dt <- cbind(dtSubject, dt)

# set the key
setkey(dt, subject, activityNum)

# mean and standard deviation 
dtFeatures <- fread(file.path(pathIn, "features.txt"))
setnames(dtFeatures, names(dtFeatures), c("featureNum", "featureName"))

# use grep to subset needed information
dtFeatures <- dtFeatures[grepl("mean\\(\\)|std\\(\\)", featureName)]

# turn everything into a vector
dtFeatures$featureCode <- dtFeatures[, paste0("V", featureNum)]
head(dtFeatures)
dtFeatures$featureCode

# subset everything again using the variable names that we want to select from
select <- c(key(dt), dtFeatures$featureCode)
dt <- dt[, select, with=FALSE]

# tidy data has readable activity names, do go ahead and set those
dtActivityNames <- fread(file.path(pathIn, "activity_labels.txt"))
setnames(dtActivityNames, names(dtActivityNames), c("activityNum", "activityName"))
dt <- merge(dt, dtActivityNames, by="activityNum", all.x=TRUE)
setkey(dt, subject, activityNum, activityName)

# rshape the data using melt
dt <- data.table(melt(dt, key(dt), variable.name="featureCode"))

# merge in the other data
dt <- merge(dt, dtFeatures[, list(featureNum, featureCode, featureName)], by="featureCode", all.x=TRUE)
dt$activity <- factor(dt$activityName)
dt$feature <- factor(dt$featureName)

# helper function to grep through things to get the data we want
grepthis <- function (regex) {
  grepl(regex, dt$feature)
}

# case for two categories
n <- 2
y <- matrix(seq(1, n), nrow=n)
x <- matrix(c(grepthis("^t"), grepthis("^f")), ncol=nrow(y))
dt$featDomain <- factor(x %*% y, labels=c("Time", "Freq"))
x <- matrix(c(grepthis("Acc"), grepthis("Gyro")), ncol=nrow(y))
dt$featInstrument <- factor(x %*% y, labels=c("Accelerometer", "Gyroscope"))
x <- matrix(c(grepthis("BodyAcc"), grepthis("GravityAcc")), ncol=nrow(y))
dt$featAcceleration <- factor(x %*% y, labels=c(NA, "Body", "Gravity"))
x <- matrix(c(grepthis("mean()"), grepthis("std()")), ncol=nrow(y))
dt$featVariable <- factor(x %*% y, labels=c("Mean", "SD"))

# case with only one category
dt$featJerk <- factor(grepthis("Jerk"), labels=c(NA, "Jerk"))
dt$featMagnitude <- factor(grepthis("Mag"), labels=c(NA, "Magnitude"))

# case for three categories
n <- 3
y <- matrix(seq(1, n), nrow=n)
x <- matrix(c(grepthis("-X"), grepthis("-Y"), grepthis("-Z")), ncol=nrow(y))
dt$featAxis <- factor(x %*% y, labels=c(NA, "X", "Y", "Z"))

# sanity check for all the right combinations
r1 <- nrow(dt[, .N, by=c("feature")])
r2 <- nrow(dt[, .N, by=c("featDomain", "featAcceleration", "featInstrument", "featJerk", "featMagnitude", "featVariable", "featAxis")])
r1 == r2


# create the data set with the average of each variable for each
setkey(dt, subject, activity, featDomain, featAcceleration, featInstrument, featJerk, featMagnitude, featVariable, featAxis)
dtTidy <- dt[, list(count = .N, average = mean(value)), by=key(dt)]

# save the data set
f <- file.path(path, "project_dataset.txt")
write.table(dtTidy, f, quote=FALSE, sep="\t", row.names=FALSE)


